## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# http://gliders.oceantrack.org/data/slocum/m80_2017-12-16_view_sci_water.csv

#' Download and Cache a Slocum Glider File
#'
#' If the file is already present in \code{destdir}, then it is not
#' downloaded again. The default \code{destdir} is the present directory,
#' but it probably makes more sense to use something like \code{"~/data/glider"}
#' to make it easy for scripts in other directories to use the cached data.
#' The file is downloaded with \code{\link{download.file}}.
#'
#' @param mission Character value indicating the mission name.
#' @param year Numeric value indicating the year of the mission start.
#' @param month Numeric value indicating the month of the mission start.
#' @param day Numeric value indicating the day of the mission start.
#' @param item Character value indicating the type of data sought.
#' @param server String specifying the online server.
#' @template filenames
#' @template debug
#'
#' @return A character value indicating the filename of the result; if
#' there is a problem, the result will be the empty string.
#'
#' @examples
#'\dontrun{
#' ## The download takes several seconds.
#' library(oceanglider)
#' gfile <- download.glider.slocum(destddir="~/data/glider")
#'}
#' @family functions for slocum gliders
#' @family functions to download data
#' @importFrom utils download.file
#' @export
download.glider.slocum <- function(mission="m80", year=2017, month=12, day=16,
                                   item="view_sci_water.csv",
                                   server="http://gliders.oceantrack.org/data/slocum",
                                   destdir=".", destfile, force=FALSE, dryrun=FALSE,
                                   debug=getOption("gliderDebug", 0))
{
    destfile <- sprintf("%s_%04d-%02d-%02d_%s", mission, year, month, day, item)
    destpath <- paste(destdir, destfile, sep="/")
    if (0 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        source <- sprintf("%s/%s", server, destfile)
        bad <- utils::download.file(source, destfile)
        if (!bad && destdir != ".")
            system(paste("mv", destfile, destpath))
    } else {
        message("Not downloading ", destfile, " because it is already present in ", destdir)
    }
    if (destdir == ".") destfile else destpath
}

#' Read a Slocum Glider file
#'
#' These files do not use standard names for variables, but
#' the \code{nameMap} argument facilitates renaming for storage
#' in the returned object. (Renaming simplifies later analysis, e.g.
#' permitting direct use of algorithms in the \code{oce} package,
#' which assume that salinity is named \code{"salinity"}, etc.)
#' The original names of data items are retained in the metadata
#' of the returned object, so that the \code{[[} operator in the \code{oce}
#' package can retrieve the data using either the original name
#' (e.g. \code{x[["sci_water_temp"]]}) or the more standard
#' name (e.g. \code{x[["temperature"]]}).
#'
#' @param file A connection or a character string giving the name of the file to load.
#'
#' @param nameMap List used to rename data columns. See \dQuote{Details}.
#'
#' @template debug
#'
#' @return An oce object holding the data, with variables renamed as
#' described in \dQuote{Details}, and with \code{salinity} added,
#' as calculated by \code{oce::\link[oce]{swSCTp}} which uses the UNESCO
#' algorithm and assumes that the conductivity values are stored in S/m
#' units.
#'
#' @author Dan Kelley
#'
#' @examples
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
#'
#'     # 3. Plot first two yos in CTD format, with yos isolated crudely.
#'     yos <- ctdFindProfiles(as.ctd(g))
#'     plot(yos[[1]])
#'     plot(yos[[2]])
#'}
#' @family functions for slocum gliders
#' @family functions to read glider data
#' @importFrom utils read.csv
#' @importFrom methods new
#' @importFrom oce numberAsPOSIXct swSCTp
#' @export
read.glider.slocum <- function(file,
                               nameMap=list(conductivity="sci_water_cond",
                                            temperature="sci_water_temp",
                                            pressure="sci_water_pressure",
                                            longitude="lon",
                                            latitude="lat",
                                            depth="i_depth"),
                               debug=getOption("gliderDebug", 0))

{
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
    rval <- methods::new("oce")
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
    rval@data <- data
    rval@metadata$filename <- filename
    ## FIXME add to dataNamesOriginal as for CTD data type
    rval
}

