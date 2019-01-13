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


#' Convert lon and lat from a combined degree+minute formula
#'
#' Data from Seaexplorers save longitude and latitude in a combined
#' format, in which e.g. 45deg 30.1min is saved as 4530.1
#'
#' @param x Numerical value in degree+minute notation, e.g.
#'
#' @return Numerical value in decimal degrees.
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
#' @param pattern Character value indicating a pattern for
#' the name(s) of the desired file(s).
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @return Either a vector of character values for the names of the
#' desired files (whether they were downloaded or already present
#' locally), or \code{NULL} if the server had no files
#' matching the indicated pattern.
#'
#' @author Dan Kelley
#'
#' @examples
#' # Download and read the second yo of mission 25 of instrument SEA024.
#' url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25"
#' yo2file <- download.glider(url, "pld1.sub.2.gz$")
#' if (!is.null(yo2file)) {
#'     yo2 <- read.glider.seaexplorer(yo2file)
#' }
#' ##? # Download (or use cache for) a set files
#' ##? download.glider.seaexplorer(yo=download.glider.seaexplorer(yo="?"))
#'
#' @family functions to download data
#' @importFrom RCurl getURL
#' @importFrom utils download.file
#' @export
download.glider <- function(url, pattern, debug=0)
{
    if (missing(url))
        stop("must supply url")
    if (missing(pattern))
        stop("must supply pattern")
    if (!urlExists(url=url, quiet=FALSE))
        stop("no such url")
    ## Ensure url ends with "/", specifying a directory.
    if ("/" != substr(url, nchar(url), nchar(url)))
        url <- paste(url, "/", sep="")
    filesString <- RCurl::getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
    files <- strsplit(filesString, "\n")[[1]]
    w <- grep(pattern, files)
    wlen <- length(w)
    if (wlen == 0) {
        cat(url, " has no files matching pattern \"", pattern, "\"", sep="")
        return(NULL)
    }
    files <- files[w]
    gliderDebug(debug, 'Found files: "', paste(files, collapse='", "'), '"\n', sep="")
    for (file in files) {
        if (!file.exists(file)) { # look in destination
            path <- paste(url, file, sep="")
            utils::download.file(path, file)
            gliderDebug(debug, 'downloaded "', file, '"\n', sep="")
        } else {
            gliderDebug(debug, 'already have "', file, '" locally, so not downloading\n', sep="")
        }
    }
    files
}

