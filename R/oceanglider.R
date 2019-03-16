#' oceanglider: A Package for Processing Ocean Glider Data
#'
#' This package was written with two particular dataset types
#' in mind, from SeaExplorer and Slocum devices. There is a good
#' chance that the functions provided here (a) will fail on
#' other types and (b) function names and arguments will change
#' as more datasets are examined by the author.
#'
#' The downloading functions require a base URL, and the default argument is to
#' use values stored as options.  For the author, this is done by putting the following
#' in the top-level \code{.Rprofile} file:
#'
#'\preformatted{
#' options(slocumURL="")
#' options(seaexplorerURL="")
#'}
#'
#' @import knitr
#' @docType package
#' @name oceanglider
NULL

#' Class for Glider Objects
#'
#' @author Dan Kelley
#'
#' @export
setClass("glider", contains="oce")
setMethod(f="initialize",
          signature="glider",
          definition=function(.Object, filename) {
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'glider' object"
              return(.Object)
          })


#' Trim a glider Object
#'
#' Return a trimmed version of a glider object.
#'
#' At the moment, this only works for SeaExplorer data (i.e. cases in which
#' \code{x[["type"]]=="seaexplorer"}).
#'
#' The permitted values of \code{method} are:
#'\itemize{
#'
#' \item \code{"ascending"}, which retains only \code{glider}
#' data entries for which the \code{NavState} equals 117, and
#' only \code{payload} data entries for which
#' \code{NAV_RESOURCE} is 117.
#'
#' \item \code{"descending"}, which retains only \code{glider}
#' data entries for which the \code{NavState} equals 100, and
#' only \code{payload} data entries for which
#' \code{NAV_RESOURCE} is 100.
#'
#'}
#'
#' @param x A \code{glider} object, i.e. one inheriting from
#' \code{\link{glider-class}}.
#'
#' @param method An expression indicating how to subset \code{x}. See
#' \dQuote{Details}.
#'
#' @return A \code{\link{glider-class}} object that
#' has been trimmed to contain only the data specified by
#' \code{subset}.
#'
#' @author Dan Kelley
#'
#' @examples
#' files <- system.file("extdata",
#'                      c("sea024.32.gli.sub.200.gz",
#'                        "sea024.32.pld1.sub.200.gz"), package="oceanglider")
#' d <- read.glider.seaexplorer(files)
#' summary(gliderTrim(d, "ascending"))
#' summary(gliderTrim(d, "descending"))
#'
#' @export
gliderTrim <- function(x, method)
{
    if (!inherits(x, "glider"))
        stop("function is only for objects of class 'glider'")
    if (missing(method))
        stop("give method")
    res <- x
    if (method == "ascending") {
        res@data$glider <- subset(res@data$glider, res@data$glider$NavState == 117)
        res@data$payload <- subset(res@data$payload, res@data$payload$NAV_RESOURCE == 117)
    } else if (method == "descending") {
        res@data$glider <- subset(res@data$glider, res@data$glider$NavState == 100)
        res@data$payload <- subset(res@data$payload, res@data$payload$NAV_RESOURCE == 100)
    } else {
        stop("method must be either \"ascending\" or \"descending\"")
    }
    res
}

#' Subset an oceanglider Object
#'
#' Note that \code{NA} values in the \code{subset} value will be dropped from
#' the return value, mimicking the behaviour of the base \code{link{subset}}
#' function.
#'
#' @param x an oceanglider object, i.e. one inheriting from the \code{\link{oceanglider-class}}.
#' @param subset a logical expression indicating how to take the subset.
#' @param ... ignored
#' @return An oceanglider object.
#' @examples
#'\dontrun{
#' # Example 1. remove wild salinities
#' library(oceanglider)
#' g <- read.glider(filename)
#' gg <- subset(g, 0 < salinity & salinity < 40)
#' par(mfrow=c(2, 1))
#' hist(g[["salinity"]], main="S original")
#' hist(gg[["salinity"]], main="S cleaned")
#'}
#'
#' @author Dan Kelley
#'
#' @export
setMethod(f="subset",
          signature="glider",
          definition=function(x, subset, ...) {
              if (missing(subset))
                  stop("must give 'subset'")
              keep <- eval(substitute(subset), x@data, parent.frame())
              keep[is.na(keep)] <- FALSE
              ##message("percent keep ", round(sum(keep)/length(keep)*100, 2), "%")
              res <- x
              for (i in seq_along(x@data))
                  res@data[[i]] <- res@data[[i]][keep]
              for (i in seq_along(x@metadata$flags))
                  res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
              res@processingLog <- processingLogAppend(res@processingLog,
                                                        paste(deparse(match.call(call=sys.call(sys.parent(1)))),
                                                              sep="", collapse=""))
              res
          })


#' @title Retrieve Part of a glider Object
#'
#' @description
#' First, a check is done to see if the object's metadata contains an item
#' with name given by \code{i}. If this is true, then that value is returned.
#' Otherwise, the item is sought in the \code{data} slot. This is straightforward
#' for objects read by \code{\link{read.glider.slocum}}, which has no
#' repeated data items, but trickier for objects read by
#' \code{\link{read.glider.seaexplorer}}, since it stores data from the glider
#' and the payload separately, in items called \code{glider} and
#' \code{payload}, respectively. If \code{j} is not specified, then
#' \code{i} is sought first in the \code{payload} component, with
#' \code{glider} being checked thereafter. (For example, this means that the
#' payload thermometer is preferred to the glider thermometer.) This selection
#' process can be controlled by setting \code{j} to either \code{"glider"}
#' or \code{"payload"}.  For example, both \code{x[["temperature"]]} and
#' \code{x[["temperature","payload"]]} retrieves values from
#' the payload thermistor, while \code{x[["temperature","glider"]]} retrieve
#' values from the glider thermister.
#'
#' @param x A glider object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param i Character value that names the item to be retrieved.
#'
#' @param j Optional character value specifying the data-stream to be used.
#'
#' @param ... Optional additional information (ignored).
#'
#' @author Dan Kelley
#'
#' @export
setMethod(f="[[",
          signature(x="glider", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              ##. message("in [[, i='", i, "'")
              debug <- getOption("gliderDebug", default=0)
              gliderDebug(debug, "glider [[ {\n", unindent=1)
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
              type <- x@metadata$type
              if (is.null(type))
                  stop("'type' is NULL")
              if (i == "type")
                  return(type)
              if (type == "seaexplorer") {
                  ##. message("it is a seaexplorer")
                  if (i == "glider")
                      return(x@data$glider)
                  if (i == "payload")
                      return(x@data$payload)
                  if (i == "yo")
                      return(x@metadata$yo)
                  if (missing(j)) {
                      ##. message("j is missing")
                      if (i %in% names(x@metadata)) {
                          ##. message("i in metadata")
                          return(x@metadata[[i]])
                      } else if (i %in% names(x@data)) {
                          ##. message("i in data")
                          return(x@data[[i]])
                      } else {
                          ##. message("returning i from within payload")
                          return(x@data$payload[[i]])
                      }
                  }
                  ##. message("j is not missing. j='", j, "'")
                  if (j == "glider")
                      return(x@data$glider[[i]])
                  if (j == "payload")
                      return(x@data$payload[[i]])
                  return(NULL)
              } else if (type == "slocum") {
                  return(x@data[[i]])
              } else {
                  stop("type='", type, "' not permitted; it must be 'seaexplorer' or 'slocum'")
              }
              if (missing(j)) {
                  if (i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else {
                      return(x@data[[i]]) # FIXME: extend this for 'j'
                  }
              } else {
                  message("FIXME: code [[ to handle j")
              }
          })

#' Plot a glider Object
#'
#' This is a limited function that is intended for quick views of a dataset.
#' More serious analysis is best done by extracting data and using whatever
#' graphical methods work well for the task at hand.
#'
#' The form of the plot is set by the \code{which} argument, as follows.
#'
#'\itemize{
#'
#'\item \code{which=0} or \code{which="map"}: plot a map of sampling locations. This
#' can be quite slow with the default plot type, so try e.g.
#' \code{plot(g, type="l")} to speed things up for a quick look at the data.
#' In many cases, that quick look might be followed by the drawing of
#' a larger view, including a coastline, with functions provided for
#' \code{coastline} objects in the \CRANpkg{oce} package.
#'
#'\item \code{which=1} or \code{which="p"}: time-series plot
#' of pressure versus time. This
#' is done using \code{\link[oce]{oce.plot.ts}} in the \CRANpkg{oce} package,
#' which also makes the other time-series plots listed below.
#'
#'\item \code{which=2} or \code{which="T"}: time-series temperature plot
#'
#'\item \code{which=3} or \code{which="S"}: time-series salinity plot
#'
#'\item \code{which=4} or \code{which="TS"}: temperature-salinity diagram,
#' with dots for data and labels indicating density anomaly; see
#' \code{\link[oce]{plotTS}} in the \CRANpkg{oce} package for
#' details.
#'
#'}
#'
#' @param x A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param which Integer or character value specifying which style is
#' to be used; see \dQuote{Details}.
#'
#' @param ... Ignored in the present version.
#'
#' @importFrom oce oce.plot.ts plotTS resizableLabel
#' @importFrom graphics plot
#'
#' @examples
#'\dontrun{
#' library(glider)
#' # These files are much too large to provide, so no sample
#' # file is provided.
#' g <- read.glider(filename)
#'
#' # Example 1. Pressure-time plot
#' plot(g, which="p")
#'
#' # Example 2. Pressure-time plot, with dots (which slows things down!)
#' plot(g, which="p", type="p", cex=0.5)
#'
#' # Example 3. Pressure-time plot, colour-coded for temperature
#' # (using an oce function to define the color map) and arranged
#' # with high pressure at the bottom, to make a time-pressure
#' # section plot of temperature. Several arguments
#' # are passed to oce.plot.ts(), and users may find it
#' # agreeable to simply call that function directly.
#' cm <- colormap(g[["temperature"]])
#' ylim <- rev(range(g[["pressure"]], na.rm=TRUE))
#' par(mar=c(2, 3.5, 2, 4))
#' drawPalette(colormap=cm)
#' plot(g, which="p", type="p", cex=1/3, col=cm$zcol, ylim=ylim,
#'      mar=c(2, 3.5, 2, 4))
#'}
setMethod(f="plot",
          signature="glider",
          definition=function(x, which, ...) {
              if (which == 0 || which == "map") {
                  lat <- x[["latitude"]]
                  lon <- x[["longitude"]]
                  asp <- 1/cos(mean(lat*pi/180))
                  plot(x[["longitude"]], x[["latitude"]], asp=asp,
                       xlab=resizableLabel("longitude"),
                       ylab=resizableLabel("latitude"), ...)
              } else if (which == 1 || which == "p") {
                  oce.plot.ts(x[["time"]], x[["pressure"]], ylab=resizableLabel("p"), ...)
              } else if (which == 2 || which == "T") {
                  oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), ...)
              } else if (which == 3 || which == "S") {
                  oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), ...)
              } else if (which == 4 || which == "TS") {
                  plotTS(x, ...)
              } else {
                  stop("which=", which, " is not permitted; see ?\"plot,glider-method\"")
              }
          })

#' Summarize a glider Object
#' @param object A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom oce threenum processingLogShow
#' @importFrom methods callNextMethod
#'
#' @export
setMethod(f="summary",
          signature="glider",
          definition=function(object, ...) {
              ##mnames <- names(object@metadata)
              cat("Glider Summary\n-----------\n\n")
              if (2 == length(object@metadata$filename)) {
                  cat(sprintf("* Input files:          \"%s\"\n", object@metadata$filename[1]))
                  cat(sprintf("                        \"%s\"\n", object@metadata$filename[2]))
              } else if (1 == length(object@metadata$filename)) {
                  cat(sprintf("* Input file:           \"%s\"\n", object@metadata$filename))
              } else {
                  cat("* Input file:  UNKNOWN\n")
              }
              type <- object@metadata$type
              cat(sprintf("* Type:                 %s\n", type))
              cat(sprintf("* Yo:                   %d\n", object@metadata$yo))
              if (!is.null(type) && type == "seaexplorer") {
                  if (2 == sum(c("glider", "payload") %in% names(object@data))) {
                      ## Glider data, two-stream format
                      ndata <- length(object@data$glider)
                      threes <- matrix(nrow=ndata, ncol=4)
                      for (i in 1:ndata)
                          threes[i, ] <- oce::threenum(object@data$glider[[i]])
                      if (!is.null(threes)) {
                          rownames(threes) <- paste("    ", names(object@data$glider))
                          OriginalName <- unlist(lapply(names(object@data$glider), function(n)
                                                        if (n %in% names(object@metadata$dataNamesOriginal$glider))
                                                            object@metadata$dataNamesOriginal$glider[[n]] else "-"))
                          threes <- cbind(threes, OriginalName)
                          colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "OriginalName")
                          cat("* Data from glider's sensors:\n")
                          owidth <- options('width')
                          options(width=150) # make wide to avoid line breaks
                          print(threes, quote=FALSE)
                          options(width=owidth$width)
                          cat("\n")
                      }
                      ## Payload data
                      ndata <- length(object@data$payload)
                      threes <- matrix(nrow=ndata, ncol=4)
                      for (i in 1:ndata)
                          threes[i, ] <- threenum(object@data$payload[[i]])
                      if (!is.null(threes)) {
                          rownames(threes) <- paste("    ", names(object@data$payload))
                          OriginalName <- unlist(lapply(names(object@data$payload), function(n)
                                                        if (n %in% names(object@metadata$dataNamesOriginal$payload))
                                                            object@metadata$dataNamesOriginal$payload[[n]] else "-"))
                          threes <- cbind(threes, OriginalName)
                          colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "OriginalName")
                          cat("* Data from payload's sensors:\n")
                          owidth <- options('width')
                          options(width=150) # make wide to avoid line breaks
                          print(threes, quote=FALSE)
                          options(width=owidth$width)
                          cat("\n")
                      }
                  } else {
                      return(invisible(callNextMethod())) # summary
                  }
              }
              processingLogShow(object)
          })


#' Convert a string from snake_case to camelCase
#'
#' @param s Character value
#'
#' @return CamelCase version of \code{s}
#'
#' @examples
#' expect_equal("profileDirection", toCamelCase("profile_direction"))
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
#' @param x Numerical value in degree+minute notation, e.g.
#'
#' @return Numerical value in decimal degrees.
#'
#' @examples
#' expect_equal(45+30.100/60, degreeMinute(4530.100))
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
            cat(paste(rep("  ", n), collapse=""), sep="")
        cat(...)
    }
    utils::flush.console()
    invisible(NULL)
}

#' Check whether a URL exists, backtracing by separators, if not
#'
#' This uses \code{[RCurl]{url.exits}} to see if the indicated URL exists.
#' If not, an attempt is made to find a lower-level URL that does exist.
#' This is done by progressively removing items separated by \code{"/"}
#' in \code{url}
#'
#' @param url Character value specifying the URL. If no \code{/} is present
#' at the end of this string, then it is added before checks are done.
#'
#' @param quiet Logical value indicating whether to print a suggestion
#' for an alternative website, in the case where \code{url} does not exist.
#'
#' @return A logical value indicating whether the website indicated
#' by \code{url} exists.
#'
#' @importFrom RCurl getURL
#' @export
urlExists <- function(url, quiet=FALSE)
{
    ## tack a '/' on the end, if not there already
    urlOrig <- url
    if (0 == length(grep("/$", url)))
        url <- paste(url, "/", sep="")
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

#' Download and Cache a Glider File
#'
#' This function visits a server that holds glider data,
#' looking for one or more files matching a given pattern. If such
#' files are found on the server, then those files that do
#' not already exist locally are downloaded
#' using \code{\link[utils]{download.file}}, which produces
#' messages to indicate the course of its progress. If no
#' files on the server match the pattern, then the return value
#' is \code{NULL}; otherwise, it is a vector of character values
#' giving the names of the files that have been downloaded.
#'
#' @param url Character value providing the web location of the
#' server.
#'
#' @param pattern A specification of the yos to be downloaded. This
#' only makes sense for seaexplorer files, since slocum files (at
#' least those the author works with) combine all yos together.
#' There are three choices for \code{pattern}. First, it may be
#' a vector of integers indicating the \code{yo}
#' numbers that are desired. Second, it may be a single character
#' value specifying a \code{\link{regexp}} pattern that will be
#' used to identify the name(s) of the desired file(s). Third,
#' it may be \code{NA} which means to download/cache all \code{pld1}
#' (payload) and \code{gli} (glider) files in the indicated server
#' directory.
#'
#' @param destdir Character value indicating the directory in which
#' to save the downloaded data.
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @return Either a vector of character values for the full-path names of the
#' desired files (whether they were downloaded or already present
#' locally), or \code{NULL} if the server had no files
#' matching the indicated pattern.
#'
#' @author Dan Kelley
#'
#' @examples
#' # 1. Download and read yo 200 of glider SEA024 on mission 32.
#'\dontrun{
#' url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
#' files <- download.glider(url, "\\.200\\.gz$",
#'                          destdir="~/data/glider/SEA024/M32")
#' if (2 == length(files)) {
#'     g <- read.glider.seaexplorer(files)
#'     summary(g)
#' }
#'
#' # 2. Download yo 100, 101, and 102.
#' files <- download.glider(url, pattern=100:102,
#'                          destdir="~/data/glider/SEA024/M32")
#'
#' # 3. Download all yos from this mission.
#' files <- download.glider(url, pattern=NA,
#'                          destdir="~/data/glider/SEA024/M32")
#'}
#'
#' @family functions to download data
#' @importFrom RCurl getURL
#' @importFrom utils download.file
#' @export
download.glider <- function(url, pattern, destdir=".", debug=0)
{
    if (missing(url))
        stop("must supply url")
    if (missing(pattern))
        stop("must supply pattern")
    patternIsNA <- FALSE
    patternIsNumeric <- FALSE
    if (is.na(pattern[1])) {
        patternIsNA <- TRUE
    } else {
        patternIsNumeric <- is.numeric(pattern[1])
    }
    if (!urlExists(url=url, quiet=FALSE))
        stop("no such url")
    ## Ensure url ends with "/", specifying a directory.
    if ("/" != substr(url, nchar(url), nchar(url)))
        url <- paste(url, "/", sep="")
    filesString <- RCurl::getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
    files <- strsplit(filesString, "\n")[[1]]
    if (patternIsNA) {
        regexp <- paste("^sea[0-9]+\\.*(pld1)|(gli)*\\.sub\\.[0-9]+\\.gz$",sep="")
        files <- files[grep(regexp, files)]
        if (0 == length(files)) {
            cat(url, " has no pld1 or gli files\n")
            return(NULL)
        }
    } else if (patternIsNumeric) {
        fileSubset <- NULL
        for (yo in pattern) {
            regexp <- paste("^sea[0-9]+\\.*(pld1)|(gli)*\\.sub\\.", yo, "\\.gz$",sep="")
            fileSubset <- c(fileSubset, files[grep(regexp, files)])
        }
        files <- fileSubset
        if (0 == length(files)) {
            cat(url, " has no files with yo number as specified by pattern\n")
            return(NULL)
        }
    } else {
        w <- grep(pattern, files)
        wlen <- length(w)
        if (wlen == 0) {
            cat(url, " has no files matching pattern \"", pattern, "\"", sep="")
            return(NULL)
        }
        files <- files[w]
    }
    gliderDebug(debug, 'Found files: "', paste(files, collapse='", "'), '"\n', sep="")
    destfiles <- paste(destdir, files, sep="/")
    for (i in seq_along(files)) {
        if (!file.exists(destfiles[i])) {
            path <- paste(url, files[i], sep="")
            utils::download.file(path, destfiles[i])
            gliderDebug(debug, 'downloaded "', destfiles[i], '"\n', sep="")
        } else {
            gliderDebug(debug, 'already have "', destfiles[i], '", so not downloading\n', sep="")
        }
    }
    destfiles
}

#' Read a glider file in netcdf format
#'
#' \strong{This is a provisional function, written to handle a particular level-1 file
#' provided to the author by DFO colleagues in mid January, 2019.} This only works
#' for files with global attribute \code{instrument} set to \code{Glider},
#' \code{instrument_manufacturer} set to \code{Alseamar}, and
#' \code{instrument_model} set to \code{SeaExplorer}, although this restriction
#' will likely be lifted as data from other instruments becomes available.
#'
#' The data are copied directly from the file, except that \code{time}
#' is converted from an integer to a POSIX time. Variable names containing
#' underscores are renamed as e.g. \code{profile_direction}
#' to \code{profileDirection}, although the \code{\link{[[,glider-method}}
#' mechanism works with either name, e.g. if \code{g} is a glider object, then
#' \code{g[["profileDirection"]]} and
#' \code{g[["profile_direction"]]} give the same result.
#'
#' @param file Name of a netcdf file.
#'
#' @template debug
#'
#' @return A glider object, i.e. one inheriting from \code{\link{glider-class}}.
#' (This class inherits from \code{\link[oce]{oce-class}} in the
#' \code{oce} package.)
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(oceanglider)
#' g <- read.glider.netcdf("GLI2018_SEA019_054DM_L1.nc")
#' ## Remove spurious times (cannot be year 2009)
#' g <- subset(g, time > as.POSIXct("2018-01-01"))
#' ## Remove bad data
#' g <- subset(g, is.finite(g[["salinity"]]))
#' ## Focus on ascent phase (profileDirection==-1)
#' g <- subset(g, profileDirection==-1)
#' # CTD-style plot of whole dataset
#' ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
#'               longitude=g[["longitude"]], latitude=g[["latitude"]])
#' plot(ctd, type=rep("p", 4)) # 'type' gives dots
#' # CTD-style plot of a particular profile
#' plot(as.ctd(subset(g, profileIndex==200)))
#'}
#'
#' @family functions to read glider data
#' @importFrom ncdf4 nc_open ncatt_get ncvar_get
#' @export
read.glider.netcdf <- function(file, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {", unindent=1, sep="")
    if (missing(file))
        stop("must provide `file'")
    if (length(file) != 1)
        stop("file must have length 1")
    f <- nc_open(file)
    res <- new("glider")
    ## Next demonstrates how to detect this filetype.
    instrument <- ncatt_get(f, varid=0, attname="instrument")
    if (is.null(instrument) || "Glider" != instrument$value)
        stop("glider files must have a global attribute 'instrument' equal to 'Glider'")
    instrumentManufacturer <- ncatt_get(f, varid=0, attname="instrument_manufacturer")
    if (is.null(instrumentManufacturer) || "Alseamar" != instrumentManufacturer$value)
        stop("global attribute 'instrument_manufacturer' must be 'Alseamar' but it is '",
             instrumentManufacturer$value, "'")
    instrumentModel <- ncatt_get(f, varid=0, attname="instrument_model")
    if (is.null(instrumentModel) || "SeaExplorer" != instrumentModel$value)
        stop("global attribute 'instrument_model' must be 'SeaExplorer' but it is '", instrumentModel$value, "'")
    res@metadata$type <- "seaexplorer"
    data <- list()
    ## FIXME get units
    ## FIXME change some variable names from snake-case to camel-case
    dataNames <- names(f$var)
    data$time <- numberAsPOSIXct(as.vector(ncvar_get(f, "time")))
    dataNamesOriginal <- list()
    dataNamesOriginal$time <- "-"
    ## Get all variables, except time, which is not listed in f$var
    for (i in seq_along(dataNames))  {
        newName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        data[[newName]] <- as.vector(ncvar_get(f, dataNames[i]))
        gliderDebug(debug, "data name \"", dataNames[i], "\" converted to \"", newName, "\"", sep="")
        dataNames[i] <- newName
    }
    names(data) <- c("time", dataNames) # names now in CamelCase, not snake_case.
    res@data <- data
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    gliderDebug(debug, "} # read.glider.netcdf", unindent=1, sep="")
    res
}

#' Read a glider data file
#'
#' This is a high-level function that passes control to \code{\link{read.glider.netcdf}}
#' if the first argument is a string ending with \code{".nc"}, or to
#' \code{\link{read.glider.seaexplorer}} if it is a string (or vector of strings)
#' ending in \code{".gz"}.
#'
#' @param file Character value giving the name of the file.
#'
#' @param ... Extra parameters passed to more specific \code{read.*} functions.
#'
#' @template debug
#'
#' @return A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#' @export
read.glider <- function(file, debug, ...)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    gliderDebug(debug, 'read.glider() {', unindent=1, sep="")
    if (!is.character(file))
        stop("'file' must be a character value (or values) giving filename(s)")
    if (length(file) == 1 && length(grep(".nc$", file))) {
        res <- read.glider.netcdf(file=file, debug=debug-1, ...)
    } else if (length(grep(".gz$", file[1]))) {
        res <- read.glider.seaexplorer(file, debug=debug-1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, '} # read.glider()', unindent=1, sep="")
    res
}
