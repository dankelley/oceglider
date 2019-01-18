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


#' Subset a glider Object
#'
#' Return a subset of a glider object.
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
              debug <- getOption("gliderDebug", default=0)
              gliderDebug(debug, "glider [[ {\n", unindent=1)
              if (missing(i))
                  stop("Must name a glider item to retrieve, e.g. '[[\"temperature\"]]'", call.=FALSE)
              i <- i[1]                # drop extras if more than one given
              if (!is.character(i))
                  stop("glider item must be specified by name", call.=FALSE)
              if (i == "filename")
                  return(x@metadata$filename)
              type <- x@metadata$type
              if (is.null(type))
                  stop("'type' is NULL")
              if (i == "type")
                  return(type)
              if (type == "seaexplorer") {
                  if (i == "glider")
                      return(x@data$glider)
                  if (i == "payload")
                      return(x@data$payload)
                  if (i == "yo")
                      return(x@metadata$yo)
                  if (missing(j))
                      return(x@data$payload[[i]])
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

#' Summarize a glider Object
#' @param object A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param ... Further arguments passed to or from other methods.
#' @importFrom oce threenum processingLogShow
#' @export
setMethod(f="summary",
          signature="glider",
          definition=function(object, ...) {
              ##mnames <- names(object@metadata)
              cat("Glider Summary\n-----------\n\n")
              cat(sprintf("* Input files:          \"%s\"\n", object@metadata$filename[1]))
              cat(sprintf("                        \"%s\"\n", object@metadata$filename[2]))
              type <- object@metadata$type
              cat(sprintf("* Type:                 %s\n", type))
              cat(sprintf("* Yo:                   %d\n", object@metadata$yo))
              if (type == "seaexplorer") {
                  ## Glider data
                  ndata <- length(object@data$glider)
                  threes <- matrix(nrow=ndata, ncol=4)
                  for (i in 1:ndata) {
                      threes[i, ] <- oce::threenum(object@data$glider[[i]])
                  }
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
                  for (i in 1:ndata) {
                      threes[i, ] <- threenum(object@data$payload[[i]])
                  }
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
                  cat("FIXME: summarize (slocum)\n")
              }
              processingLogShow(object)
          })


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

