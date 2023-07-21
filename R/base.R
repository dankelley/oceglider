#' oceGlider: A Package for Processing Ocean Glider Data
#'
#' This package was written with two particular dataset types
#' in mind, from SeaExplorer and Slocum devices. There is a good
#' chance that the functions provided here (a) will fail on
#' other types and (b) function names and arguments will change
#' as more datasets are examined by the author.
#'
#' @importFrom methods new
#' @importFrom oce handleFlags oceDebug setFlags subset summary
#' @docType package
#' @name oceGlider
NULL

#' A class to hold glider information
#' @export
glider <- setClass("glider", contains="oce")

setMethod(f="initialize",
    signature="glider",
    definition=function(.Object, filename) {
        if (!missing(filename))
            .Object@metadata$filename <- filename
        .Object@metadata$type <- "?"
        .Object@metadata$subtype <- "?"
        .Object@metadata$level <- NA # unknown at start
        .Object@processingLog$time <- as.POSIXct(Sys.time())
        .Object@processingLog$value <- "create 'glider' object"
        return(.Object)
    })

#' Retrieve Part of a glider Object
#'
#' Retrieve something contained in a glider object, or something that can
#' be computed from what is contained there.
#'
#' First, a check is done to see if the object's metadata contains an item
#' with name given by `i`. If this is true, then that value is returned.
#'
#' Otherwise, the item is sought somewhere within the `data` slot.
#' The procedure is somewhat subtle, and depends on the data type.
#'
#' For objects read by [read.glider.slocum()] ...
#' **FIXME: write more here, but only when we handle the slocum data
#' in the form it has as of 2019; the code is 2 years old and the data file
#' format used in local laboratories seems to have changed, possibly twice,
#' in the meantime.**
#'
#' For objects of type `seaexplorer`, i.e. as read by
#' [read.glider.seaexplorer.realtime()] and
#' [read.glider.seaexplorer.delayed()]. the `data` slot
#' may contain multiple items.  In some cases, there will be an item
#' named `glider` and another named `payload1`. In others,
#' the first of these may be missing.  (Also, it seems likely that
#' the package will be updated to include multiple payloads, when
#' users start deploying such gliders.)
#'
#' If `j` is not specified, then
#' `i` is sought first in `payload1`, with
#' `glider` being checked thereafter. For example, this means that a
#' thermometer within the payload will be preferred to one attached to
#' the body of the glider. This selection
#' process can be controlled by setting `j` to either `"glider"`
#' or `"payload1"`.  For example, both `x[["temperature"]]` and
#' `x[["temperature","payload1"]]` retrieve values from
#' the payload thermistor, while `x[["temperature","glider"]]` retrieves
#' values from the glider thermister. For clarity of code, it might make
#' sense to always specify `j`.
#'
#' In addition to retrieving data stored in the object, `\[\[` can also
#' return the following.
#'
#'\itemize{
#'
#' \item the full `data` slot, with e.g. `x[["data"]]`
#'
#' \item the `glider` item in `data` slot, with e.g. `x[["glider"]]`
#'
#' \item the `payload1` item in `data` slot, with e.g. `x[["payload1"]]`
#'
#' \item the Absolute Salinity is returned with e.g.
#' `x[["SA"]]`. This is computed with
#' [gsw::gsw_SA_from_SP()], based on the water properties
#' stored in the object. (See also the item for Conservative Temperature)
#'
#' \item the sigma-theta density anomaly calculated using
#' [oce::swSigmaTheta()] on the water properties stored in the object,
#' with e.g. `x[["sigmaTheta"]]`. This obeys the setting of the
#' equation of state, established with e.g. `options(oceEOS="gsw")` for the
#' TEOS-10/GSW variant or `options(oceEOS="unesco")` for the
#' older UNESCO variant.
#'
#' \item the Conservative Temperature is returned with e.g.
#' `x[["CT"]]`. This is computed with
#' [gsw::gsw_CT_from_t()], based on the water properties
#' stoed in the object. (See also the item for Absolute Salinity.)
#'
#' \item the sigma0 density anomaly is returned with e.g.
#' `x[["sigma0"]]`. This is computed with
#' [oce::swSigma0()]  based on the water properties
#' stored in the object.
#' Note that the computation depends on the setting of the equation of state,
#' set up with `options(oceEOS="gsw")` for the TEOS-10/GSW variant
#' or with `options(oceEOS="unesco")` for the older UNESCO variant.
#'
#' \item the spiciness0 water property is returned with e.g.
#' `x[["spiciness0"]]`. This is computed with
#' [gsw::gsw_spiciness0()], based on the water properties
#' stored in the object. (Note that this is the TEOS-10/GSW variant.)
#'
#' \item glider object containing just the data for a particular yo,
#' e.g. `x[["yo",1]]` yields the first yo.
#'
#'}
#'
#' @param x A glider object, i.e. one inheriting from [glider-class].
#'
#' @param i Character value that names the item to be retrieved.
#'
#' @param j Optional character value specifying the data-stream to be used.
#'
#' @param ... Optional additional information (ignored).
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oceGlider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
#' g <- read.glider.seaexplorer.delayed(directory)
#' hist(g[["salinity"]])
#'
#' @importFrom oce swSigmaTheta swSigma0 swSpice
#' @importFrom gsw gsw_CT_from_t gsw_SA_from_SP gsw_spiciness0
#'
#' @export
#'
#' @md
setMethod(f="[[",
    signature(x="glider", i="ANY", j="ANY"),
    definition=function(x, i, j, ...) {
        #. message("in [[, i='", i, "'")
        #.debug <- getOption("gliderDebug", default=0)
        # gliderDebug(debug, "glider [[ {\n", unindent=1)
        if (missing(i))
            stop("Must name a glider item to retrieve, e.g. '[[\"temperature\"]]'", call.=FALSE)
        i <- i[1]                # drop extras if more than one given
        if (!is.character(i))
            stop("glider item must be specified by name", call.=FALSE)
        if (i == "filename")
            return(x@metadata$filename)
        else if (i == "data")
            return(x@data)
        else if (i == "metadata")
            return(x@metadata)
        else if (i == "yo" && !missing(j)) { # NOTE: not 'yoNumber'
            lines <- which(x@data$payload1$yoNumber == j)
            x@data$payload1 <- x@data$payload1[lines, ]
            for (f in names(x@metadata$flags$payload1)) {
                x@metadata$flags$payload1[[f]] <- x@metadata$flags$payload1[[f]][lines]
            }
            return(x)
        }
        type <- x@metadata$type
        if (is.null(type))
            stop("'type' is NULL")
        if (length(grep("Flag$", i))) {
            # returns a list
            where <- "payload1"
            return(if ("flags" %in% names(x@metadata)) x@metadata$flags[[where]][[gsub("Flag$", "", i)]] else NULL)
        }
        # FIXME (DK) recognize "Unit$" as done in oce.
        if (i == "type")
            return(type)
        if (i == "sigmaTheta")
            return(swSigmaTheta(salinity=x[["salinity"]],
                    temperature=x[["temperature"]],
                    pressure=x[["pressure"]],
                    longitude=x[["longitude"]],
                    latitude=x[["latitude"]]))
        if (i == "sigma0")
            return(swSigma0(salinity=x[["salinity"]],
                    temperature=x[["temperature"]],
                    pressure=x[["pressure"]],
                    longitude=x[["longitude"]],
                    latitude=x[["latitude"]]))
        if (i == "SA")
            return(gsw_SA_from_SP(SP=x[["salinity"]], p=x[["pressure"]],
                    longitude=x[["longitude"]], latitude=x[["latitude"]]))
        if (i == "CT") {
            t <- x[["temperature"]]
            SP <- x[["salinity"]] # stored as practical salinity
            p <- x[["pressure"]]
            SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=x[["longitude"]], latitude=x[["latitude"]])
            return(gsw_CT_from_t(SA, t, p))
        }
        if (i == "spiciness0") {
            t <- x[["temperature"]]
            SP <- x[["salinity"]] # stored as practical salinity
            p <- x[["pressure"]]
            # SA <- gsw::gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
            # CT <- gsw::gsw_CT_from_t(SA, t, p)
            # return(gsw::gsw_spiciness0(SA=SA, CT=CT))
            SA <- gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
            CT <- gsw_CT_from_t(SA, t, p)
            return(gsw_spiciness0(SA=SA, CT=CT))
        }
        #. message("it is a seaexplorer")
        if (i == "glider")
            return(x@data$glider)
        if (i == "payload")
            return(x@data$payload)
        if (missing(j)) {
            #. message("j is missing")
            if (i %in% names(x@metadata)) {
                #. message("i in metadata")
                return(x@metadata[[i]])
            } else if (i %in% names(x@data)) {
                #. message("i in data")
                return(x@data[[i]])
            } else {
                #. message("returning i from within payload")
                if (i %in% names(x@data[["payload1"]]))
                    return(x@data$payload1[[i]])
                else
                    return(x@data$glider[[i]]) # what if there is no glider?
                return(x@data$payload[[i]])
            }
        }
        #. message("j is not missing. j='", j, "'")
        # if (j == "glider")
        #     return(x@data$glider)
        # if (j == "payload")
        #     return(x@data$payload1)
        # if (j == "payload1")
        #     return(x@data$payload1)
        # if (j == "payload2")
        #     return(x@data$payload2)
        # stop("type='", type, "' not permitted; it must be 'seaexplorer' or 'slocum'")
        warning("[[", i, ",", j, "]] not understood, so returning NULL", sep="")
        return(NULL)
    })

#' Convert a string from snake_case to camelCase
#'
#' @param s character value to be converted.
#'
#' @return CamelCase version of `s`.
#'
#' @examples
#' toCamelCase("profile_direction") # "profileDirection"
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
toCamelCase <- function(s)
{
    s <- strsplit(s, "")[[1]]
    r <- NULL
    n <- length(s)
    i <- 1
    while (i <= n) {
        if (s[i] == "_") {
            if (i < n)
                r <- c(r, toupper(s[i+1]))
            else
                warning("trailing underscores are ignored")
            i <- i + 2
        } else {
            r <- c(r, s[i])
            i <- i + 1
        }
    }
    paste(r, collapse="")
}



#' Convert lon and lat from a combined degree+minute formula
#'
#' Data from Seaexplorers save longitude and latitude in a combined
#' format, in which e.g. 45deg 30.1min is saved as 4530.100, as
#' illustrated in the example.
#'
#' @param x Numerical value in degree+minute notation (see \dQuote{Examples}).
#'
#' @return Numerical value in decimal degrees.
#'
#' @examples
#' degreeMinute(4530.100) # 45+30.100/60
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
degreeMinute <- function(x)
{
    s <- sign(x)
    x <- abs(x)
    degree <- floor(x / 100)
    minute <- x - 100 * degree
    s * (degree + minute / 60)
}



#' Print a debugging message
#'
#' Many glider functions decrease the `debug` level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#'
#' @param \dots items to be supplied to [cat()], which does the
#' printing.  Almost always, this should include a trailing newline.
#'
#' @param unindent Number of levels to un-indent, e.g. for start and end lines
#' from a called function.
#'
#' @author Dan Kelley
#'
#' @importFrom utils flush.console
#'
#' @md
#'
#' @export
gliderDebug <- function(debug=0, ..., unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (n > 0)
            cat(paste(rep("  ", n), collapse=""), sep="")
        cat(...)
    }
    utils::flush.console()
    invisible(NULL)
}

#' Check whether a URL exists, backtracing by separators, if not
#'
#' This uses [RCurl::url.exists()] to see if the indicated URL exists.
#' If not, an attempt is made to find a lower-level URL that does exist.
#' This is done by progressively removing items separated by `"/"`
#' in `url`
#'
#' @param url Character value specifying the URL. If no `/` is present
#' at the end of this string, then it is added before checks are done.
#'
#' @param quiet Logical value indicating whether to print a suggestion
#' for an alternative website, in the case where `url` does not exist.
#'
#' @return A logical value indicating whether the website indicated
#' by `url` exists.
#'
#' @author Dan Kelley
#'
## @importFrom RCurl url.exists
#'
#' @md
#'
#' @export
urlExists <- function(url, quiet=FALSE)
{
    # tack a '/' on the end, if not there already
    urlOrig <- url
    if (0 == length(grep("/$", url)))
        url <- paste(url, "/", sep="")
    if (!requireNamespace("RCurl", quietly=TRUE))
        stop("must install.packages(\"RCurl\") to read this data type")
    exists <- RCurl::url.exists(url)
    if (exists) {
        return(TRUE)
    } else {
        while (!quiet && TRUE) {
            w <- which('/'==strsplit(url,'')[[1]])
            if (length(w) > 1) {
                url <- strtrim(url, w[length(w) - 1])
                if (RCurl::url.exists(url)) {
                    if (!quiet)
                        cat("'", urlOrig, "' does not exist, but '", url, "' does\n", sep="")
                    break
                }
            }
        }
        return(FALSE)
    }
}

# a helper function to simplify code in read.glider.netcdf()
getAtt <- function(f, varid=0, attname=NULL, default=NULL)
{
    if (is.null(attname))
        stop("must give attname")
    # message(attname)
    t <- try(ncdf4::ncatt_get(f, varid=varid, attname=attname), silent=TRUE)
    if (inherits(t, "try-error")) {
        NULL
    } else {
        if (t$hasatt) t$value else default
    }
}


#' Read a glider file in netcdf format
#'
#' \strong{This is a provisional function, written to handle some
#' particular files available to the author.}
#'
#' The data are copied directly from the file, except that `time`
#' is converted from an integer to a POSIX time. Variable names containing
#' underscores are renamed as e.g. `profile_direction`
#' to `profileDirection`, although the \code{\link{[[,glider-method}}
#' mechanism works with either name, e.g. if `g` is a glider object, then
#' `g[["profileDirection"]]` and
#' `g[["profile_direction"]]` give the same result.
#'
#' @param file Name of a netcdf file.
#'
#' @template debug
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(oceGlider)
#'
#' # NOTE: these files are of order 100Meg, so they are
#' # not provided with the package as samples. In both
#' # examples, we plot a map and then an incidence-TS plot.
#'
#' # Seaexplorer data, from DFO (January 2019)
#' g <- read.glider.netcdf("~/Dropbox/glider_dfo.nc")
#' # Remove spurious times, from a year before deployment
#' g <- subset(g, time > as.POSIXct("2018-01-01"))
#' # Remove any observation with bad salinity
#' g <- subset(g, is.finite(g[["salinity"]]))
#' plot(g, which="map")
#' ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
#'               longitude=g[["longitude"]], latitude=g[["latitude"]])
#' plotTS(ctd, useSmoothScatter=TRUE)
#'
#' # Slocum data,from Dalhousie CEOTR rdapp (April 2019)
#' g <- read.glider.netcdf("~/Dropbox/glider_erdapp.nc")
#' # Remove any observation with bad salinity
#' g <- subset(g, is.finite(g[["salinity"]]))
#' plot(g, which="map")
#' ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
#'               latitude=g[["latitude"]], longitude=g[["longitude"]])
#' plotTS(ctd, useSmoothScatter=TRUE)
#'}
#'
#' @family functions to read glider data
#' @family functions to read netcdf glider data
#'
## @importFrom ncdf4 nc_open ncatt_get ncvar_get
#'
#' @md
#'
#' @export
read.glider.netcdf <- function(file, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    oce::oceDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {", unindent=1, sep="")
    if (missing(file))
        stop("must provide `file'")
    if (length(file) != 1)
        stop("file must have length 1")
    capture.output({
        f <- ncdf4::nc_open(file)
    })
    res <- new("glider")

    # Next demonstrates how to detect this filetype.
    instrument <- getAtt(f, attname="instrument", default="?")
    instrumentManufacturer <- getAtt(f, attname="instrument_manufacturer", default="?")
    instrumentModel <- getAtt(f, attname="instrument_model", default="?")
    type <- getAtt(f, attname="platform_type", default="?")
    if (type == "Slocum Glider")
        type <- "slocum"
    res@metadata$type <- type
    data <- list()
    # FIXME get units
    # FIXME change some variable names from snake-case to camel-case
    dataNames <- names(f$var)
    data$time <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
    dataNamesOriginal <- list()
    #? if (!"time" %in% dataNames)
    #?     dataNamesOriginal$time <- "-"
    # Get all variables, except time, which is not listed in f$var
    oce::oceDebug(debug, "reading and renaming data\n")
    for (i in seq_along(dataNames))  {
        newName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        if (dataNames[i] == "time") {
            data[["time"]] <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
            oce::oceDebug(debug, "i=", i, " ... time converted from integer to POSIXct\n", sep="")
        } else {
            data[[newName]] <- as.vector(ncdf4::ncvar_get(f, dataNames[i]))
            oce::oceDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" converted to \"", newName, "\"\n", sep="")
            dataNames[i] <- newName
        }
    }
    # gliderDebug(debug, "dataNames:", paste(dataNames, collapse=";"), "\n")
    # names(data) <- if ("time" %in% dataNames) dataNames else c("time", dataNames)
    res@data$payload1 <- as.data.frame(data)
    # head(res@data$payload1$time)
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- list(payload1=dataNamesOriginal)
    oce::oceDebug(debug, "} # read.glider.netcdf", unindent=1, sep="")
    res
}

#' Read a glider data file
#'
#' This is a high-level function that passes control to [read.glider.netcdf()]
#' if the first argument is a string ending with `".nc"`, to
#' [read.glider.seaexplorer.realtime()] if it is a vector of strings, any
#' of which contains the text `"pld1.sub."` followed by one or more digits, or to
#' [read.glider.seaexplorer.delayed()] if it is a vector of strings, any
#' contains the text `"pld1.raw."` followed by one or more digits.
#'
#' @param file Character value giving the name of the file.
#'
#' @param ... Extra parameters passed to more specific `read.*` functions.
#'
#' @template debug
#'
#' @author Dan Kelley
#'
#' @return A `glider` object, i.e. one inheriting from [glider-class].
#'
#' @md
#'
#' @export
read.glider <- function(file, debug, ...)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    oce::oceDebug(debug, 'read.glider() {', unindent=1, sep="")
    if (!is.character(file))
        stop("'file' must be a character value (or values) giving filename(s)")
    if (length(file) == 1 && length(grep(".nc$", file))) {
        res <- read.glider.netcdf(file=file, debug=debug-1, ...)
    } else if (0 != length(grep("pld1.sub", file))) {
        res <- read.glider.seaexplorer.realtime(file, debug=debug-1, ...)
    } else if (0 != length(grep("pld1.raw", file))) {
        res <- read.glider.seaexplorer.delayed(file, debug=debug-1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    oce::oceDebug(debug, '} # read.glider()', unindent=1, sep="")
    res
}

#' Convert data to glider format
#'
#' This function returns a glider object that holds data as provided
#' in the `data` argument, with units as provided by the `units`
#' argument. The `units` argument is optional, making the function
#' easy to use in interactive sessions, but production code ought to
#' be written with units fully specified.
#'
#' @param type Character value giving the type of glider, e.g.
#' be either `seaexplorer` or `slocum`.
#'
#' @param data A data frame containing the data. This is copied straight into
#' the `payload1` item in the `data` slot of the returned value,
#' \emph{without} name translation. For most functions in this package to work,
#' `data` ought to have items named `longitude`,
#' `latitude`, `salinity`, `temperature` and
#' `pressure`.
#'
#' @param units A list holding units, with names corresponding to the
#' names in the data. See the example for the format to be used
#' for `units`, but note that there are several items in this
#' dataset that are not given units, in this example.
#'
#' @examples
#' library(oceGlider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
#' g <- read.glider.seaexplorer.delayed(directory)
#' data <- g[["payload"]]
#' units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
#'      salinity=list(unit=expression(), scale="PSS-78"),
#'      pressure=list(unit=expression(dbar), scale=""),
#'      longitude=list(unit=expression(degree*E), scale=""),
#'      latitude=list(unit=expression(degree*N), scale=""))
#' gg <- as.glider("seaexplorer", data, units)
#' par(mfrow=c(2, 1))
#' plot(g, which="p")
#' plot(gg, which="p")
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
as.glider <- function(type, data, units)
{
    if (missing(type))
        stop("'type' must be given")
    if (missing(data))
        stop("'data' must be given")
    res <- new("glider")
    res@metadata$type <- type
    streamname <- "payload1"
    res@metadata$dataNamesOriginal <- list(names=names(data))
    res@data[[streamname]] <- data
    if (!missing(units)) {
        res@metadata$units <- list(payload1=units)
    }
    res
}

