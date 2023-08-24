# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a glider file in a Slocum netcdf format
#'
#' This is a provisional function, written to handle a particular
#' file created in Spring of 2023 by MUN researchers, as part of an OTN
#' project.  Please consult the \dQuote{Slocum Gliders} vignette for more
#' information about this file.
#'
#' Since different files might use different naming conventions for data,
#' it is helpful to establish a mapping between names in the file and the
#' names that are to be stored in the return object.  This is what the
#' `nameMap` argument is for.  The file may contain information that
#' will guide the user as to which of various possibilities to use,
#' although reference to other documents, on consultation with the
#' data provider may be required.  Again, see the \dQuote{Slocum Gliders}
#' vignette for more on this topic.
#'
#' @param file Name of a netcdf file.
#'
#' @param nameMap either a character value or a list.  In the first case,
#' the only permitted possibility is `nameMap="?"`, which instructs
#' `read.glider.slocum.netcdf()` to return a vector of variable names
#' as determined by scanning the file. Examining this list can be a useful
#' first step in the exploration of data file.  Once the variables are known, it makes
#' sense to use the list form of `nameMap`, perhaps starting with the default
#' value.
#'
#' @param readAll logical value indicating whether to read all the data
#' columns in the file.  The default, FALSE, means to only
#' read columns that appear in `nameMap`.  Using FALSE can yield significant
#' decreases in processing time and memory usage.  For the 2 Gb test file
#' used during the coding of this function (which, admittedly, seems
#' to have a great deal of duplication or near-duplication of data),
#' setting `readAll` to FALSE drops the reading time from 12 s to 1.5s,
#' and the size of the resultant value from 2.0Gb to 0.28 Gb.
#'
#' @param debug an integer controlling how much information is printed during
#' processing.  If this is 0, then only errors and warnings will be printed.  If it
#' is 1, then the function entry and exit are signalled, and a line
#' is printed for each variable read.
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Dan Kelley, aided by Cameron Richardson on data names.
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
    readAll=FALSE,
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
        oxygen="oxygen_concentration",
        u="u",
        v="v",
        vxi="glider_record/m_initial_water_vx",
        vyi="glider_record/m_initial_water_vy",
        vy="glider_record/m_water_vy",
        vx="glider_record/m_water_vx",
        id="profile_id"),
    debug)
{
    debug <- min(1L, max(0L, as.integer(debug))) # make 0L or 1L
    if (missing(file))
        stop("must provide `file'")
    if (length(file) != 1)
        stop("file must have length 1")
    gliderDebug(debug, "read.glider.slocum.netcdf(file=\"", file, "\", ...) {\n", unindent=1, sep="")
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
            gliderDebug(debug, "    ", nnan, " (", round(100*nnan/n, 4),
                "%) of values are NA\n", sep="")
            x[nan] <- NA
        }
        is9999 <- x == 9999.00
        nis9999 <- sum(is9999, na.rm=TRUE)
        if (nis9999 > 0L) {
            gliderDebug(debug, "    ", nis9999, " (", round(100*nis9999/n, 4),
                "%) of values are 9999, so set to NA\n", sep="")
            x[is9999] <- NA
        }
        as.vector(x)
    }
    if (identical(nameMap, "?"))
        return(dataNames)
    pb <- NULL
    if (!readAll) {
        if (length(nameMap) < 1L)
            stop("if readAll is FALSE, then nameMap must contain at least one item")
        gliderDebug(debug-1, "only reading the ", length(nameMap), " items named in the nameMap argument\n")
        willRead <- unname(unlist(nameMap))
        keep <- names(f$var) %in% willRead
        dataNames <- dataNames[keep]
    }
    ndataNames <- length(dataNames)
    for (i in seq_len(ndataNames)) {
        # see if it is a remapped name
        w <- which(dataNames[i] == nameMap)
        newName <- if (length(w) > 0) names(nameMap)[w] else toCamelCase(dataNames[i])
        gliderDebug(debug, "storing netcdf variable \"", dataNames[i], "\" as \"", newName, "\"\n", sep="")
        dataNamesOriginal[[newName]] <- dataNames[i]
        capture.output(d <- try(ncdf4::ncvar_get(f, dataNames[i]), silent=TRUE))
        capture.output(fillValue <- try(ncdf4::ncatt_get(f, dataNames[i])$"_FillValue", silent=TRUE))
        if (!inherits(d, "try-error")){
            isTime <- grepl(".*[t,T]ime.*", newName) & !grepl(".*[q,Q]c.*", newName) & !grepl(".*[f,F]lag.*", newName)
            if (isTime) {
                data[[newName]] <- numberAsPOSIXct(fixVector(d, fillValue = fillValue))
                gliderDebug(debug, "    converted to POSIXct\n", sep="")
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
    }
    inData <- names(dataNamesOriginal) %in% names(data)
    res@data <- as.data.frame(data) # FIXME: only do if items in list are same length
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- dataNamesOriginal[inData]
    gliderDebug(debug, "} # read.glider.slocum.netcdf\n", unindent=1, sep="")
    ncdf4::nc_close(f)
    res@metadata$type <- "slocum"
    res
}

