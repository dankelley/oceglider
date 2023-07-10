## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a Slocum Glider file
#'
#' These files do not use standard names for variables, but
#' the `nameMap` argument facilitates renaming for storage
#' in the returned object. (Renaming simplifies later analysis, e.g.
#' permitting direct use of algorithms in the `oce` package,
#' which assume that salinity is named `"salinity"`, etc.)
#' The original names of data items are retained in the metadata
#' of the returned object, so that the `[[` operator in the `oce`
#' package can retrieve the data using either the original name
#' (e.g. `x[["sci_water_temp"]]`) or the more standard
#' name (e.g. `x[["temperature"]]`).
#'
#' @param file A connection or a character string giving the name of the file to load.
#'
#' @template debug
#'
#' @param nameMap List used to rename data columns. See \dQuote{Details}.
#'
#' @return An oce object holding the data, with variables renamed as
#' described in \dQuote{Details}, and with `salinity` added,
#' as calculated by [oce::swSCTp()] which uses the UNESCO
#' algorithm and assumes that the conductivity values are stored in S/m
#' units.
#'
#' @examples
#' library(oceanglider)
#' if (file.exists("~/slocum.csv")) {
#'     g <- read.glider.slocum("~/slocum.csv")
#'     summary(g)
#'
#'     # 1. Plot time-depth trace, colour-coded for temperature
#'     par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # thin margins
#'     cm <- colormap(z=g[['temperature']])
#'     drawPalette(colormap=cm, cex.axis=3/4)
#'     t <- g[["time"]]
#'     p <- g[["depth"]]
#'     plot(t, p, ylim=rev(range(p)), xlab="Time", ylab="Pressure [dbar]",
#'          col=cm$zcol, cex=1/2, pch=20)
#'     mtext(paste("Temperature, from", t[1]), cex=3/4)
#'
#'     # 2. Plot distance-depth trace, colour-coded for temperature
#'     dist <- geodDist(g[['longitude']],g[['latitude']],alongPath=TRUE)
#'     par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # thin margins
#'     cm <- colormap(z=g[['temperature']])
#'     drawPalette(colormap=cm, cex.axis=3/4)
#'     p <- g[["depth"]]
#'     plot(dist, p, ylim=rev(range(p)), xlab="Distance [km]", ylab="Pressure [dbar]",
#'          col=cm$zcol, cex=1/2, pch=20)
#'     mtext(paste("Temperature, from", t[1]), cex=3/4)
#'}
#'
#' @family functions for slocum gliders
#' @family functions to read glider data
#'
#' @importFrom utils read.csv
#' @importFrom methods new
#' @importFrom oce numberAsPOSIXct swSCTp
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
read.glider.slocum <- function(file, debug,
    nameMap=list(conductivity="sci_water_cond",
        temperature="sci_water_temp",
        pressure="sci_water_pressure",
        longitude="lon",
        latitude="lat",
        depth="i_depth"))
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(file))
        stop("must provide `file'")
    filename <- ""
    if (is.character(file)) {
        filename <- normalizePath(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    data <- utils::read.csv(filename, header=TRUE)
    names <- names(data)
    nameMapNames <- names(nameMap)
    gliderDebug(debug, 'original data names: "', paste(names, collapse='", "'), '"\n')
    rval <- methods::new("glider")
    rval@metadata$type <- "slocum"
    rval@metadata$dataNamesOriginal <- list()
    for (iname in seq_along(names)) {
        if (names[iname] %in% nameMap) {
            newName <- nameMapNames[which(names[iname] == nameMap)]
            rval@metadata$dataNamesOriginal[[newName]] <- names[iname]
            names[iname] <-  newName
        } else {
            rval@metadata$dataNamesOriginal[[names[iname]]] <- names[iname]
        }
    }
    gliderDebug(debug, 'new data names: "', paste(names, collapse='", "'), '"\n')
    names(data) <- names
    salinity <- with(data,
        oce::swSCTp(conductivity, temperature, pressure,
            conductivityUnit="S/m", eos="unesco"))
    data$salinity <- salinity
    data$time <- oce::numberAsPOSIXct(data$unix_timestamp, "unix")
    rval@data$payload1 <- as.data.frame(data)
    rval@metadata$filename <- filename
    # FIXME add to dataNamesOriginal as for CTD data type
    rval
}

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
#' (e.g. `x[["sci_water_temp"]]`) or the more standard
#' name (e.g. `x[["temperature"]]`).
#'
#' @param file Name of a netcdf file.
#'
#' @param nameMap List used to rename data columns. The default is tailore
#' to a particular file, and is subject to change without notice, as that
#' file is explored.
#'
#' @template debug
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
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {", unindent=1, sep="")
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

    # get all global attributes aka metadata
    gliderDebug(debug, "reading and renaming attributes (aka metadata)\n")
    attributes <- ncdf4::ncatt_get(f, 0)
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
    gliderDebug(debug, "reading and renaming data\n")
    fixVector <- function(x, fillValue=NULL)
    {
        # check for nan's
        nan <- is.nan(x)
        gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" has \"", length(which(nan == TRUE)), "\" NaN values \n", sep="")
        x[nan] <- NA
        # check for fillValues (if provided)
        if(!is.null(fillValue)){
          isFillValue <- x == fillValue
          gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" has \"", length(which(isFillValue == TRUE)), "\"", fillValue, " values \n", sep="")
          x[isFillValue] <- NA
        }
        # check for 9999.00 values
        is9999 <- x == 9999.00
        gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" has \"", length(which(is9999 == TRUE)), "\" 9999.00 values \n", sep="")
        x[is9999] <- NA
        as.vector(x)
    }
    for (i in seq_along(dataNames))  {
        newName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        capture.output(d <- try(ncdf4::ncvar_get(f, dataNames[i]), silent = TRUE))
        capture.output(fillValue <- try(ncdf4::ncatt_get(f, dataNames[i])$`_FillValue`, silent = TRUE))
        gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" try-error logical is \"", inherits(d, "try-error"), "\"\n", sep="")

        if(!inherits(d, "try-error")){
            isTime <- grepl(".*[t,T]ime.*", newName) & !grepl(".*[q,Q]c.*", newName) & !grepl(".*[f,F]lag.*", newName)
            if (isTime) {
                data[[newName]] <- numberAsPOSIXct(fixVector(d, fillValue = fillValue))
                gliderDebug(debug, "i=", i, " ... time name \"", dataNames[i], "\" converted to \"", newName, "\" converted from integer to POSIXct\n", sep="")
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
                    #message(" <", gsub("Qc$", "", newName), ">")
                    res@metadata$flags[[gsub("Qc$", "", newName)]] <- fixVector(d, fillValue=fillValue)
                } else {
                    #message("  NOT qc")
                    data[[newName]] <- fixVector(d, fillValue = fillValue)
                    gliderDebug(debug, "i=", i, " ... length of \"", dataNames[i], "\" is \"", length(data[[newName]]), "\"\n", sep="")
                    gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" converted to \"", newName, "\"\n", sep="")
                    # Handle units. Note that ncatt_get() prints a message for things that
                    # lack attributes, and its 'quiet' argument does not silence them, so
                    # we discard the output.
                    capture.output(unit <- try(ncdf4::ncatt_get(f, dataNames[i])$units, silent=TRUE))
                    if (!inherits(unit, "try-error") && !is.null(unit)) {
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
                            message("FIXME: store \"", newName, " unit \"", unit, "\"")
                        }
                        res@metadata$units[[newName]] <- newUnit
                    }
                }
            }
        } else {
            message("Could not read \"", newName, "\", proceeding to next variable\"")
        }
    }
    inData <- names(dataNamesOriginal) %in% names(data)
    res@data <- as.data.frame(data) # FIXME: only do if items in list are same length
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- dataNamesOriginal[inData]
    gliderDebug(debug, "} # read.glider.slocum.netcdf", unindent=1, sep="")
    ncdf4::nc_close(f)
    res@metadata$type <- "slocum"
    res
}

