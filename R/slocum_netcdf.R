# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a glider file in a Slocum netcdf format
#'
#' \strong{This is an unsupported and provisional function}, written to handle a particular
#' file created in Spring of 2023 by MUN researchers, as part of an OTN
#' project.
#'
#' The sample file does not use standard names for variables, but
#' the `nameMap` argument facilitates renaming for storage
#' in the returned object. (Renaming simplifies later analysis, e.g.
#' permitting direct use of algorithms in the `oce` package,
#' which assume that salinity is named `"salinity"`, etc.)
#' The original names of data items are retained in the metadata
#' of the returned object, so that the `[[` operator in the `oce`
#' package can retrieve the data using either the original name
#' or the more standard name.
#'
#' @param file Name of a netcdf file.
#'
#' @param nameMap List used to rename data columns. The default is tailored
#' to a particular file, and is subject to change without notice, as that
#' file is explored.
#'
#' @param debug an integer controlling how much information is printed during
#' processing.  If this is 0, then only errors and warnings will be printed.  If it
#' is 1, then the function entry and exit are signalled, and a progress bar
#' is shown.  If it exceeds 1, then detailed notes are provided (but the
#' progress bar is not shown).
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Dan Kelley with help from Chantelle Layton and Cameron Richardson.
#'
#' @family functions to read glider data
#' @family functions to read netcdf glider data
#'
## @importFrom ncdf4 nc_open ncatt_get ncvar_get nc_close
#' @importFrom utils capture.output
#'
#' @md
#'
#' @export
read.glider.slocum.netcdf <- function(file,
    nameMap=list(
        salinity="glider_record/sci_rbrctd_salinity_00",
        SA="absolute_salinity",
        temperature="glider_record/sci_rbrctd_temperature_00",
        CT="conservative_temperature",
        pressure="glider_record/sci_rbrctd_seapressure_00", # CJR had pressure here
        time="glider_record/sci_rbrctd_timestamp",
        conductivity="glider_record/sci_rbrctd_conductivity_00",
        #depth="glider_record/sci_rbrctd_depth_00", # CJR read this
        latitude="glider_record/m_gps_lat",
        longitude="glider_record/m_gps_lon",
        profileLat="profile_lat",
        profileLon="profile_lon",
        density="density",
        O2="oxygen_concentration",
        u="u",
        v="v",
        vxi="glider_record/m_initial_water_vx",
        vyi="glider_record/m_initial_water_vy",
        vy="glider_record/m_water_vy",
        vx="glider_record/m_water_vx",
        id="profile_id"),
    debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {\n", unindent=1, sep="")
    if (missing(file))
        stop("must provide `file'")
    if (length(file) != 1)
        stop("file must have length 1")
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop("must install.packages(\"ncdf4\") to read this data type")
    capture.output({ # capture the output to silence warning about missing-value byte length
        f <- ncdf4::nc_open(file)
    })
    res <- new("glider")
    # get all global attributes AKA metadata (we capture output to
    # avoid a warning about 8-byte integers.  This occurs in a sample
    # file from MUN, which has deployment_id equal to 33LL according
    # to ncdump at the CLI interface.  (I assume that LL means long integer.)
    capture.output({
        attributes <- ncdf4::ncatt_get(f, 0)
    })
    # change attribute names from snake-case to camel-case
    attributeNames <- names(attributes)
    attributeNamesCamel <- unlist(lapply(attributeNames, toCamelCase))
    names(attributes) <- attributeNamesCamel
    # assign attributes to metadata
    res@metadata <- attributes
    # make space for units and flags in metadata
    res@metadata$units <- list()
    res@metadata$flags <- list()
    res@metadata$flagScheme <- list()
    knowFlagScheme <- FALSE # change this if we find any _qc variables
    # get all data, including dimensions
    data <- list()
    dataNames <- c(names(f$var), names(f$dim))
    dataType <- c(rep("var", length(f$var)), rep("dim", length(f$dim)))
    dataNamesOriginal <- list()
    fixVector <- function(x, fillValue=NULL)
    {
        n <- length(x)
        nan <- is.nan(x)
        nnan <- sum(nan, na.rm=TRUE)
        if (nnan > 0L) {
            gliderDebug(debug-1, "    ", nnan, " (", round(100*nnan/n, 4), "%) of values are NA\n", sep="")
            x[nan] <- NA
        }
        is9999 <- x == 9999.00
        nis9999 <- sum(is9999, na.rm=TRUE)
        if (nis9999 > 0L) {
            gliderDebug(debug-1, "    ", nis9999, " (", round(100*nis9999/n, 4), "%) of values to NA\n", sep="")
            x[is9999] <- NA
        }
        as.vector(x)
    }
    pb <- NULL
    ndataNames <- length(dataNames)
    if (debug == 1)
        pb <- txtProgressBar(min=1, max=ndataNames, style=3)
    for (i in seq_len(ndataNames)) {
        gliderDebug(debug-1, "netcdf item named \"", dataNames[i], "\":\n", sep="")
        # see if it is a remapped name
        w <- which(dataNames[i] == nameMap)
        newName <- if (length(w) > 0) names(nameMap)[w] else toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        capture.output(d <- try(ncdf4::ncvar_get(f, dataNames[i]), silent=TRUE))
        capture.output(fillValue <- try(ncdf4::ncatt_get(f, dataNames[i])$"_FillValue", silent=TRUE))
        if (!inherits(d, "try-error")){
            isTime <- grepl(".*[t,T]ime.*", newName) & !grepl(".*[q,Q]c.*", newName) & !grepl(".*[f,F]lag.*", newName)
            if (isTime) {
                data[[newName]] <- numberAsPOSIXct(fixVector(d, fillValue = fillValue))
                gliderDebug(debug-1, "    converted to POSIXct\n", sep="")
            } else {
                if (grepl("^.*Qc$", newName)) {
                    if (!knowFlagScheme) {
                        res@metadata$flagScheme$name <- "IOOS"
                        res@metadata$flagScheme$mapping <- list()
                        ftypes <- strsplit(ncdf4::ncatt_get(f,
                                gsub("Qc$", "_qc", newName))$flag_meaning," ")[[1]]
                        for (i in seq_along(ftypes))
                            res@metadata$flagScheme$mapping[[ftypes[i]]] <- as.numeric(i)
                        # recognize, or guess, the good code
                        res@metadata$flagScheme$default <- as.numeric(seq_along(ftypes))
                        w <- which("good_data" == ftypes)
                        if (length(w) == 1L) {
                            res@metadata$flagScheme$default <- res@metadata$flagScheme$default[-w]
                        } else {
                            res@metadata$flagScheme$default <- res@metadata$flagScheme$default[-2L]
                            warning("assuming flag=2 means 'good data'")
                        }
                        knowFlagScheme <- TRUE
                    }
                    res@metadata$flags[[gsub("Qc$", "", newName)]] <- fixVector(d, fillValue=fillValue)
                } else {
                    # not QC
                    data[[newName]] <- fixVector(d, fillValue = fillValue)
                    # handle units
                    capture.output(unit <- try(ncdf4::ncatt_get(f, dataNames[i])$units, silent=TRUE))
                    if (!inherits(unit, "try-error") && !is.null(unit) && nchar(trimws(unit)) > 0L) {
                        if (unit != "1"){
                            newUnit <- switch(unit,
                                "Celsius" = list(unit = expression(degree*C), scale="ITS-90"),
                                "kg m-3" = list(unit = expression(kg/m^3), scale=""),
                                "ug l-1" = list(unit=expression(mu*g/l), scale=""),
                                "S m-1" = list(unit=expression(S/m), scale=""),
                                "degrees_north" = list(unit=expression(degree*N), scale=""),
                                "degrees_east" = list(unit=expression(degree*E), scale=""),
                                "m" = list(unit=expression(m), scale=""),
                                "m-1" = list(unit=expression(m^-1), scale=""), # or should it be 1/m?
                                "degrees" = list(unit=expression(degree), scale=""),
                                "m s-1" = list(unit=expression(m/s), scale=""),
                                "dbar" = list(unit=expression(dbar), scale=""),
                                "nm" = list(unit=expression(nm), scale=""),
                                "umol kg-1" = list(unit=expression(mu*mol/kg), scale=""),
                                "percent" = list(unit=expression("%"), scale=""),
                                "rad" = list(unit=expression(rad), scale=""),
                                "mg m-3" = list(unit=expression(mg/m^3), scale=""),
                                "ppb" = list(unit=expression(ppb), scale=""),
                                "Hz" = list(unit=expression(Hz), scale=""),
                                "km" = list(unit=expression(km), scale="")
                                )
                        } else {
                            # for unit == 1
                            newUnit <- switch(newName,
                                "salinity" = list(unit=expression(), scale="PSS-78"), # need to check on scale
                                "backscatter700" = list(unit=expression(), scale=""),
                                "profileIndex" = list(unit=expression(), scale=""),
                                "profileDirection" = list(unit=expression(), scale="")
                                )
                        }
                        if(is.null(newUnit)){
                            newUnit <- list(unit=expression(), scale="")
                            gliderDebug(debug-1, "    storing unit as \"", unit, "\"\n", sep="")
                        }
                        res@metadata$units[[newName]] <- newUnit
                    }
                }
            }
            if (newName != dataNames[i])
                gliderDebug(debug-1, "    renaming \"", newName, "\" for storage\n", sep="")
        } else {
            message("Could not read \"", newName, "\", proceeding to next variable\"")
        }
        if (debug == 1)
            setTxtProgressBar(pb, i)
    }
    if (debug == 1)
        setTxtProgressBar(pb, i)
    inData <- names(dataNamesOriginal) %in% names(data)
    res@data <- as.data.frame(data) # FIXME: only do if items in list are same length
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- dataNamesOriginal[inData]
    gliderDebug(debug, "} # read.glider.slocum.netcdf", unindent=1, sep="")
    ncdf4::nc_close(f)
    res@metadata$type <- "slocum"
    res
}

