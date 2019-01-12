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


#' Print a debugging message
#'
#' Many glider functions decrease the \code{debug} level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#' @param \dots items to be supplied to \code{\link{cat}}, which does the
#' printing.  Almost always, this should include a trailing newline.
#' @param unindent Number of levels to un-indent, e.g. for start and end lines
#' from a called function.
#' @author Dan Kelley
#' @importFrom utils flush.console
#' @export
gliderDebug <- function(debug=0, ..., unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (n > 0)
            cat(paste(rep("  ", n), collapse=""))
        cat(...)
    }
    utils::flush.console()
    invisible()
}

#' Read a Slocum Glider file
#'
#' @param name Character value giving the name of the file
#'
#' @return An oce object holding the data.
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' g <- read.glider.slocum("~/data/glider/m80_2017-12-16_view_sci_water.csv")
#'}
#' @family functions for slocum gliders
#' @family functions to read glider data
#' @importFrom utils read.csv
#' @importFrom methods new
#' @importFrom oce numberAsPOSIXct swSCTp
#' @export
read.glider.slocum <- function(name)
{
    rval <- methods::new("oce")
    data <- utils::read.csv(name, header=TRUE)
    names <- names(data)
    print(names)
    names <- gsub("sci_water_cond", "conductivity", names)
    names <- gsub("sci_water_temp", "temperature", names)
    names <- gsub("sci_water_pressure", "pressure", names)
    names <- gsub("lon", "longitude", names)
    names <- gsub("lat", "latitude", names)
    names <- gsub("i_depth", "depth", names)
    names(data) <- names
    print(names)
    salinity <- with(data,
                     oce::swSCTp(conductivity, temperature, pressure,
                                 conductivityUnit="S/m", eos="unesco"))
    data$salinity <- salinity
    data$time <- oce::numberAsPOSIXct(data$unix_timestamp, "unix")
    rval@data <- data
    rval@metadata$filename <- name
    rval
}

