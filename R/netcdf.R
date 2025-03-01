#' oceglider: A Package for Processing Ocean Glider Data
#'
#' This package was written with two particular dataset types
#' in mind, from SeaExplorer and Slocum devices. There is a good
#' chance that the functions provided here (a) will fail on
#' other types and (b) function names and arguments will change
#' as more datasets are examined by the author.
#'
#' @importFrom methods new
#' @importFrom oce subset summary
#'
#' @name oceglider
#' @docType package
#' @keywords internal
"_PACKAGE"
NULL

#' A class to hold glider information
#'
#' As of 2024-06-23, it is not clear whether the best policy is for
#' this to inherit from 'oce' or to define itself here.  Doing the
#' former might seem sensible, since it might reduce the need for code
#' here, but it seems to cause problems (e.g. inability to build
#' vignettes in the standard way) that I have trouble understanding.
#' We ran into similar problems in the argoFloats package too, and
#' there we decided to define the object fully in the new package. For
#' now, that's the approach used here also.
#'
#' @author Dan Kelley
#'
#' @export
glider <- setClass("glider", slots = c(metadata = "list", data = "list", processingLog = "list"))
# glider <- setClass("glider", contains = "oce")

setMethod(
    f = "initialize",
    signature = "glider",
    definition = function(.Object, filename) {
        if (!missing(filename)) {
            .Object@metadata$filename <- filename
        }
        .Object@metadata$type <- "?"
        .Object@metadata$subtype <- "?"
        .Object@metadata$level <- NA # unknown at start
        .Object@processingLog$time <- as.POSIXct(Sys.time())
        .Object@processingLog$value <- "create 'glider' object"
        return(.Object)
    }
)

# _ #' Saturation of O2 in sea water
# _ #'
# _ #' Computes the solubility (saturation) of Oxygen in sea water. Based on the
# _ #' Matlab function `SW_SATO2` from the CSIRO seawater toolbox.
# _ #'
# _ #' @author Chantelle Layton
# _ #' @param temperature temperature
# _ #' @param salinity salinity
# _ #'
# _ #' DEK note: the result is in mL/L.
# _ swSatO2 <- function(temperature, salinity) {
# _     Tk <- 273.15 + temperature * 1.00024
# _     # constants for Eqn (4) of Weiss 1970
# _     a1 <- -173.4292
# _     a2 <- 249.6339
# _     a3 <- 143.3483
# _     a4 <- -21.8492
# _     b1 <- -0.033096
# _     b2 <- 0.014259
# _     b3 <- -0.0017000
# _     lnC <- a1 +
# _         a2 * (100 / Tk) +
# _         a3 * log(Tk / 100) +
# _         a4 * (Tk / 100) +
# _         salinity * (b1 + b2 * (Tk / 100) + b3 * ((Tk / 100)^2))
# _     exp(lnC)
# _ }

#' Convert a string from snake_case to camelCase
#'
#' Convert a snake-case string (i.e., one constructed
#' with words separated by underlines) to a camel-case
#' string (i.e. one in which the words are strung together,
#' with upper-case for the first letter of all but the
#' first word).  See \sQuote{Examples}.
#'
#' @param s character value to be converted.
#'
#' @return [toCamelCase()] returns a camelCase version of `s`.
#'
#' @examples
#' toCamelCase("profile_direction") # "profileDirection"
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
toCamelCase <- function(s) {
    s <- strsplit(s, "")[[1]]
    r <- NULL
    n <- length(s)
    i <- 1
    while (i <= n) {
        if (s[i] == "_") {
            if (i < n) {
                r <- c(r, toupper(s[i + 1]))
            } else {
                warning("trailing underscores are ignored")
            }
            i <- i + 2
        } else {
            r <- c(r, s[i])
            i <- i + 1
        }
    }
    paste(r, collapse = "")
}



#' Convert longitude and latitude from a combined degree+minute formula
#'
#' SeaExplorer gliders save longitude and latitude with e.g.
#' 4530.100 standing for 45deg 30.1min (which is 45.50167
#' in conventional decimal format).  This function converts SeaExplorer
#' coordinate values to conventional decimal values.
#'
#' @param x Numerical value in degree+minute notation (see \dQuote{Examples}).
#'
#' @return [degreeMinute()] returns a numerical value in decimal degrees.
#'
#' @examples
#' degreeMinute(4530.100) # 45+30.100/60
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
degreeMinute <- function(x) {
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
#' @param unindent Number of levels to unindent, e.g. for start and end lines
#' from a called function.
#'
#' @author Dan Kelley
#'
#' @importFrom utils flush.console
#'
#' @md
#'
#' @export
gliderDebug <- function(debug = 0, ..., unindent = 0) {
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- debug - unindent
        if (n > 0) {
            cat(paste(rep("  ", n), collapse = ""), sep = "")
        }
        cat(...)
    }
    utils::flush.console()
    invisible(NULL)
}


# a helper function to simplify code in read.glider.netcdf()
getAtt <- function(f, varid = 0, attname = NULL, default = NULL) {
    if (is.null(attname)) {
        stop("must give attname")
    }
    # message(attname)
    t <- try(ncdf4::ncatt_get(f, varid = varid, attname = attname), silent = TRUE)
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
#' is converted from an integer to a POSIX time, and variables
#' may be renamed according to the `rename` parameter. The code
#' was last tested on 2025-03-01 using the file downloaded
#' from the URL given in Reference 1.
#'
#' @param file character value holding the name of a netcdf file
#' that holds glider data.
#'
#' @param saveGlobalAttributes logical value indicating whether to
#' read the entirety of the global attributes stored within the
#' file into the `metadata` slot in a list named `globalAttributes`.
#'
#' @param rename an indication of how (or if) to rename variables. This is
#' needed for most practical work in the oce package, which expects
#' standardized names, such as `"temperature"`, as opposed to the names stored
#' in glider files. There are three choices for `rename`. (a) It can be
#' logical, with TRUE (the default) meaning to use names as defined in
#' `system.file("extdata/dictionaries/seaexplorerDict.csv",package="oceglider")`
#' or FALSE, meaning not to rename variables. (b) It can be the name of a CSV
#' file that is in the same format as the file above-named file.  (c)
#' It can be a two-column data frame in which column 1 holds
#' variable names in the glider file and column 2 holds the corresponding
#' names to be used in the return value.
#'
#' @template debug
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Dan Kelley
#'
## @examples
## \dontrun{
## library(oceglider)
##
## # NOTE: these files are of order 100Meg, so they are
## # not provided with the package as samples. In both
## # examples, we plot a map and then an incidence-TS plot.
##
## # Seaexplorer data, from DFO (January 2019)
## g <- read.glider.netcdf("~/Dropbox/glider_dfo.nc")
## # Remove spurious times, from a year before deployment
## g <- subset(g, time > as.POSIXct("2018-01-01"))
## # Remove any observation with bad salinity
## g <- subset(g, is.finite(g[["salinity"]]))
## plot(g, which = "map")
## ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
##     longitude = g[["longitude"]], latitude = g[["latitude"]]
## )
## plotTS(ctd, useSmoothScatter = TRUE)
##
## # Slocum data,from Dalhousie CEOTR rdapp (April 2019)
## g <- read.glider.netcdf("~/Dropbox/glider_erdapp.nc")
## # Remove any observation with bad salinity
## g <- subset(g, is.finite(g[["salinity"]]))
## plot(g, which = "map")
## ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
##     latitude = g[["latitude"]], longitude = g[["longitude"]]
## )
## plotTS(ctd, useSmoothScatter = TRUE)
## }
#'
#' @family functions to read glider data
#' @family functions to read netcdf glider data
#'
#' @importFrom oce as.unit
#' @importFrom ncdf4 nc_close nc_open ncatt_get ncatt_get ncvar_get
#'
#' @md
#'
#' @references
#' 1. <https://cproof.uvic.ca/gliderdata/deployments/dfo-marvin1003/dfo-marvin1003-20230929/L0-timeseries/dfo-marvin1003-20230929_delayed.nc>
#'
#'
#' @export
read.glider.netcdf <- function(file, saveGlobalAttributes = TRUE,
                               rename = TRUE,
                               debug = getOption("gliderDebug", default = 0)) {
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) BEGIN\n", unindent = 1, sep = "")
    if (missing(file)) {
        stop("must provide file")
    }
    if (length(file) != 1) {
        stop("can only read one file at a time")
    }
    res <- new("glider")
    capture.output({
        f <- ncdf4::nc_open(file)
    })
    on.exit(ncdf4::nc_close(f))
    # get all global attributes (see https://github.com/dankelley/oceglider/issues/125)
    res@metadata$globalAttributes <- list()
    if (saveGlobalAttributes) {
        for (name in names(ncatt_get(f, ""))) {
            newname <- toCamelCase(name)
            gliderDebug(debug, "global attribute '", name, "' stored as '", newname, "'\n",
                sep = ""
            )
            res@metadata$globalAttributes[[newname]] <- ncatt_get(f, varid = 0, attname = name)$value
        }
    }
    # Next demonstrates how to detect this filetype.
    res@metadata$instrument <- getAtt(f, attname = "instrument", default = "?")
    res@metadata$instrumentManufacturer <- getAtt(f, attname = "instrument_manufacturer", default = "?")
    res@metadata$instrumentModel <- getAtt(f, attname = "instrument_model", default = "?")
    type <- getAtt(f, attname = "platform_type", default = "?")
    if (!is.null(type)) {
        type <- gsub("Glider$", "", type)
        type <- tolower(type)
        type <- trimws(type)
    }
    res@metadata$type <- type
    data <- list()
    # FIXME get units
    # FIXME change some variable names from snake-case to camel-case
    dataNames <- names(f$var)
    data$time <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
    dataNamesOriginal <- list()
    # ? if (!"time" %in% dataNames)
    # ?     dataNamesOriginal$time <- "-"
    # Get all variables, except time, which is not listed in f$var
    gliderDebug(debug, "reading and renaming data, plus collecting units\n")
    units <- list()
    # set up renaming convention
    if (is.character(rename)) {
        if (!file.exists(rename)) {
            stop("there is no file named '", rename, "'")
        }
        nameDict <- read.csv(rename, header = FALSE, col.names = c("gliderName", "oceName"))
        rename <- TRUE
    } else if (is.data.frame(rename)) {
        nameDict <- rename
        names(nameDict) <- c("gliderName", "oceName")
        rename <- TRUE
    } else if (is.logical(rename)) {
        nameDict <- read.csv(system.file("extdata/dictionaries/cproofDict.csv", package = "oceglider"),
            header = FALSE, col.names = c("gliderName", "oceName")
        )
    }
    if (debug > 0 && !is.null(rename)) {
        cat("next is head(nameDict):\n")
        print(head(nameDict))
    }

    for (i in seq_along(dataNames)) {
        w <- which(dataNames[i] == nameDict$gliderName)
        # gliderDebug(debug, "processing ", dataNames[i], "...\n")
        if (length(w) == 1L) {
            oceName <- nameDict$oceName[w]
            # gliderDebug(debug, dataNames[i], " -> ", oceName, "\n")
        } else {
            oceName <- dataNames[i]
        }
        # oceName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[oceName]] <- dataNames[i]
        gliderDebug(debug, "reading ", dataNames[i], " as ", oceName, "\n", sep = "")
        if (dataNames[i] == "time") {
            data[["time"]] <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
            gliderDebug(debug, "i=", i, " ... time converted from integer to POSIXct\n", sep = "")
        } else {
            data[[oceName]] <- as.vector(ncdf4::ncvar_get(f, dataNames[i]))
            # Infer the units, if sufficient information is available
            tmp <- ncdf4::ncatt_get(f, dataNames[i], "units")
            unit <- if (tmp$hasatt) {
                oce::as.unit(tmp$value, default = NULL)
            } else {
                list(unit = expression(), scale = "")
            }
            gliderDebug(debug, "    unit: ", as.character(unit$unit), "\n", sep = "")
            units[[oceName]] <- unit
            dataNames[i] <- oceName
        }
    }
    res@data$payload1 <- as.data.frame(data)
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- list(payload1 = dataNamesOriginal)
    res@metadata$units <- units
    res@metadata$dataAreStreamed <- TRUE
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("read.glider.netcdf()", sep = "")
    )

    gliderDebug(debug, "read.glider.netcdf() END", unindent = 1, sep = "")
    res
}

#' Read a glider data file
#'
#' This is a high-level function that passes control to [read.glider.netcdf()]
#' if the first argument is a string ending with `".nc"` or to
#' [read.glider.seaexplorer.raw()] if it is a vector of strings, any contains
#' the text `"pld1.raw."` followed by one or more digits.
#'
#' @param file character value giving the name of the file.
#'
#' @param ... extra parameters passed to more specific `read.*` functions.
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
read.glider <- function(file, debug, ...) {
    if (missing(debug)) {
        debug <- getOption("gliderDebug", default = 0)
    }
    gliderDebug(debug, "read.glider() START", unindent = 1, sep = "")
    if (!is.character(file)) {
        stop("'file' must be a character value (or values) giving filename(s)")
    }
    if (length(file) == 1 && length(grep(".nc$", file))) {
        res <- read.glider.netcdf(file = file, debug = debug - 1, ...)
    } else if (0 != length(grep("pld1.raw", file))) {
        res <- read.glider.seaexplorer.raw(file, debug = debug - 1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, "read.glider() END", unindent = 1, sep = "")
    res
}
