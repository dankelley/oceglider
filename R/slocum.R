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
#'     drawPalette(colormap=cm, cex=3/4)
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
#'     drawPalette(colormap=cm, cex=3/4)
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
#' @export
#'
#' @author Dan Kelley
#'
#' @md
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
    ## FIXME add to dataNamesOriginal as for CTD data type
    rval
}

