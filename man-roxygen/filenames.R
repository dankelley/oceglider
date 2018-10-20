#' @param destdir Optional string indicating the directory in which to store
#' downloaded files. If not supplied, \code{"."} is used, i.e. the data file
#' is stored in the present working directory.  It can be
#' helpful to create a top-level directory called \code{data},
#' with a subdirectory for each file type used (e.g. \code{~/data/glider}
#' to hold glider files).
#'
#' @param destfile Optional string indicating the name of the file.
#' If not supplied, then the file name is constructed from the other
#' parameters of the function call, so that subsequent calls with
#' the same parameters will yield the same result; this is useful
#' for caching.
#'
#' @param force A logical value that indicates whether to force the download,
#' even if the pathname constructed from \code{destdir} and \code{destfile}
#' already exists.
#'
#' @param dryrun A logical value that indicates whether to return the constructed
#' web query, without attempting to download the file. This can be helpful in
#' designing responses to changing URLs.


