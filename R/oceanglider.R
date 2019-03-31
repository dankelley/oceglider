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
#' @importFrom methods new
#' @import knitr
#' @importFrom oce subset summary
#' @docType package
#' @name oceanglider-class
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

seaexplorerNavState <- list("not navigating"=105,
                            "surfacing"=115,
                            "at surface"=116,
                            "inflecting down"=110,
                            "descent"=100,
                            "inflecting up"=118,
                            "ascent"=117)
# +105 means the glider is not navigating yet;
# +115 means the glider is surfacing in preparation for communication;
# +116 means the glider is at the surface, acquiring a GPS signal, and is communicating;
# +110 means the glider is inflecting downward;
# +100 means the glider has ballast set to be descending;
# +118 means the glider has ballast adjusted to reduce density, so will be inflecting upward;
# +117 means the glider has ballast set to be ascending.


##OLD #' Trim a glider Object
##OLD #'
##OLD #' Return a trimmed version of a glider object.
##OLD #'
##OLD #' At the moment, this only works for SeaExplorer data (i.e. cases in which
##OLD #' \code{x[["type"]]=="seaexplorer"}).
##OLD #'
##OLD #' The permitted values of \code{method} are:
##OLD #'\itemize{
##OLD #'
##OLD #' \item \code{"ascending"}, which retains only \code{glider}
##OLD #' data entries for which the \code{navState} equals 117, and
##OLD #' only \code{payload} data entries for which
##OLD #' \code{NAV_RESOURCE} is 117.
##OLD #'
##OLD #' \item \code{"descending"}, which retains only \code{glider}
##OLD #' data entries for which the \code{navState} equals 100, and
##OLD #' only \code{payload} data entries for which
##OLD #' \code{NAV_RESOURCE} is 100.
##OLD #'
##OLD ## \item \code{"length"}, which requires also that
##OLD ## \code{parameters} be specified. This retains only \code{glider}
##OLD ## yos with more than \code{parameters$minlength} depth levels.
##OLD #'
##OLD #'}
##OLD #'
##OLD #' @param x A \code{glider} object, i.e. one inheriting from
##OLD #' \code{\link{glider-class}}.
##OLD #'
##OLD #' @param method An expression indicating how to subset \code{x}. See
##OLD #' \dQuote{Details}.
##OLD #'
##OLD ## @param parameters A list containing extra parameters. At present,
##OLD ## this is only used if \code{method="length"}, and must contain
##OLD ## an element named \code{minimum}, an integer specifying how many
##OLD ## levels a yo must havee to avoid being discarding.
##OLD #'
##OLD #' @return A \code{\link{glider-class}} object that
##OLD #' has been trimmed to contain only the data specified by
##OLD #' \code{subset}.
##OLD #'
##OLD #' @author Dan Kelley
##OLD #'
##OLD #' @examples
##OLD #' files <- system.file("extdata/seaexplorer/sub",
##OLD #'                      c("sea024.32.gli.sub.200.gz",
##OLD #'                        "sea024.32.pld1.sub.200.gz"), package="oceanglider")
##OLD #' d <- read.glider.seaexplorer.sub(files)
##OLD #' summary(gliderTrim(d, "ascending"))
##OLD #' summary(gliderTrim(d, "descending"))
##OLD #'
##OLD #' @section Caution:
##OLD #' This function may be subsumed into \code{\link{subset,glider-method}}, because it
##OLD #' does similar things, and users are more likely to guess the name of the latter.
##OLD #'
##OLD #' @export
##OLD gliderTrim <- function(x, method)#, parameters)
##OLD {
##OLD     if (!inherits(x, "glider"))
##OLD         stop("function is only for objects of class 'glider'")
##OLD     if (missing(method))
##OLD         stop("give method")
##OLD     res <- x
##OLD     if (method == "ascending") {
##OLD         res@data$glider <- subset(res@data$glider, res@data$glider$navState == 117)
##OLD         res@data$payload <- subset(res@data$payload, res@data$payload$NAV_RESOURCE == 117)
##OLD     } else if (method == "descending") {
##OLD         res@data$glider <- subset(res@data$glider, res@data$glider$navState == 100)
##OLD         res@data$payload <- subset(res@data$payload, res@data$payload$NAV_RESOURCE == 100)
##OLD     ## } else if (method == "length") {
##OLD     ##     if (missing(parameters))
##OLD     ##         stop("must give parameters, if method=\"length\"")
##OLD     ##     if (!is.list(parameters))
##OLD     ##         stop("parameters must be a list")
##OLD     ##     minimum <- parameters$minimum
##OLD     ##     if (is.null(minimum))
##OLD     ##         stop("parameters must contain an item named \"minimum\"")
##OLD     ##     if (x@metadata$type != "seaexplorer")
##OLD     ##         stop("method only works for seaexplorer data; contact the package authors, if you need this for other types")
##OLD     ##     if (!"payload" %in% names(x@data))
##OLD     ##         stop("only works for 'raw' datasets, not for 'sub' ones; contact package authors, if you need to handle sub data")
##OLD     ##     gs <- split(x@data$payload, x[["yoNumber"]])
##OLD     ##     keepYo <- unlist(lapply(gs, function(yo) {
##OLD     ##                             n <- length(yo[["pressure"]])
##OLD     ##                             n >= parameters$minimum } ) )
##OLD     ##     ##. message("sum(keepYo)=", sum(keepYo), " length(keepYo)=", length(keepYo))
##OLD     ##     keepLevel <- unlist(lapply(gs, function(yo) {
##OLD     ##                                n <- length(yo[["pressure"]])
##OLD     ##                                rep(n >= parameters$minimum, n) } ) )
##OLD     ##     ## gsk <- gs[keep]
##OLD     ##     ## ## FIXME: do.call() seems slow; try expanding 'keep' and then index x@data$payload.
##OLD     ##     ## res@data$payload <- do.call(rbind.data.frame, x@data$payload[keep, ])
##OLD     ##     res@metadata$yo <- x@metadata$yo[keepYo]
##OLD     ##     res@data$payload <- x@data$payload[keepLevel, ]
##OLD     } else {
##OLD         stop("method \"", method, "\" is not permitted; see ?gliderTrim for choices")
##OLD     }
##OLD     res
##OLD }

#' Subset a glider Object
#'
#' Select a portion of an glider object, specified according to one
#' of several possible schemes, based on the form of the argument named
#' \code{subset}. Note that the schemes cannot be combined, so
#' nested calls must be used to accomplish combinations.
#'
#' Scheme 1: if \code{subset} is a logical expression written
#' in terms of data that are stored in the yos, then this expression is applied
#' to the \code{payload1} item in the \code{data} slot of the object
#' (see Example 1).
#'
#' Scheme 2: if \code{subset} is a logical expression containing
#' the word \code{"levels"}, then the expression is used as a filter
#' to select yos (see Example 2). Typically, this might be used to avoid
#' short yos.
#'
#' Scheme 3: If \code{subset} is the string \code{"ascending"}, then
#' only ascending segments of yos are retained. This is done
#' by selecting for \code{navState==117} in both the \code{glider}
#' and \code{payload1} streams of the \code{data} slot of the object.
#'
#' Scheme 4: If \code{subset} is the string \code{"descending"}, then
#' only descending segments of yos are retained. This is done
#' by selecting for \code{navState==100} in both the \code{glider}
#' and \code{payload1} streams of the \code{data} slot of the object.
#'
#' @param x an oceanglider object, i.e. one inheriting from the \code{\link{glider-class}}.
#'
#' @param subset a logical expression or a character string that indicates
#' how to take the subset. See \dQuote{Details}.
#'
#' @param ... Additional arguments, of which the only one permitted at the
#' moment is \code{debug}, an integer indicating the level of debugging information
#' to be permitted.
#'
#' @return An oceanglider object.
#'
#' @examples
#'\dontrun{
#'
#' # Example 1. remove wild salinities
#' library(oceanglider)
#' g <- read.glider(filename)
#' gg <- subset(g, 0 < salinity & salinity < 40)
#' par(mfrow=c(2, 1))
#' hist(g[["salinity"]], main="S original")
#' hist(gg[["salinity"]], main="S cleaned")
#'
#' # Example 2. remove short yos
#' gg <- subset(g, levels > 4)
#'
#' # Example 3. retain only ascending portions of yos
#' gascending <- subset(g, "ascending")
#'
#' # Example 4. retain only descending portions of yos
#' gdescending <- subset(g, "descending")
#'}
#'
#' @author Dan Kelley
#'
#' @export
#' @aliases subset,glider-method
#' @section Bugs:
#' The 'ascending' and 'descending' methods do not work. This seems
#' to be a problem of exporting classes using roxygen2 tags. I am looking
#' into this.  DK 2019-03-28.
setMethod(f="subset",
          signature="glider",
          definition=function(x, subset, ...) {
              if (missing(subset))
                  stop("must give 'subset'")
              dots <- list(...)
              debug <- if ("debug" %in% names(dots)) dots$debug else getOption("gliderDebug",0)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              gliderDebug(debug, "subset,glider-method() {\n", unindent=1)
              gliderDebug(debug, "subsetString is ", subsetString, "\n", sep="")
              if (x[["type"]] == "seaexplorer") {
                  gliderDebug(debug, "type is seaexplorer\n")
                  if (!"payload1" %in% names(x@data))
                      stop("In subset,glider-method() : cannot subset seaexplorer objects that lack a 'payload1' item in the data slot", call.=FALSE)
                  if (is.character(substitute(subset))) {
                      gliderDebug(debug, "subset is character\n")
                      ## subset is a character string
                      if (subset == "ascending") {
                          res <- x
                          res@data$glider <- subset(res@data$glider, res@data$glider$navState == 117)
                          res@data$payload1 <- subset(res@data$payload1, res@data$payload1$navState == 117)
                      } else if (subset == "descending") {
                          res <- x
                          res@data$glider <- subset(res@data$glider, res@data$glider$navState == 100)
                          res@data$payload1 <- subset(res@data$payload1, res@data$payload1$navState == 100)
                      }
                  } else {
                      gliderDebug(debug, "subset is a logical expression\n")
                      ## subset is a logical expression
                      if (1 == length(grep("levels", subsetString))) {
                          if (!"payload1" %in% names(x@data))
                              stop("In subset,glider-method() : only works for 'raw' datasets, not for 'sub' ones; contact package authors, if you need to handle sub data", call.=FALSE)
                          s <- split(x@data$payload1, x[["yoNumber"]])
                          warning("In subset,glider-method() : only subsetting 'payload1'; contact package authors, if your data have other streams", call.=FALSE)
                          levels <- as.integer(lapply(s, function(ss) length(ss[["pressure"]])))
                          keepYo <- eval(substitute(subset), list(levels=levels))
                          ##message("sum(keepYo)=", sum(keepYo), " length(keepYo)=", length(keepYo))
                          res <- x
                          res@metadata$yo <- x@metadata$yo[keepYo]
                          keepLevel <- unlist(lapply(seq_along(s), function(si) rep(keepYo[si], levels[si])))
                          ## NOTE: the following was a much slower (10s of seconds compared to perhaps 1s or less)
                          ## res@data$payload <- do.call(rbind.data.frame, x@data$payload[keepYo, ])
                          res@data$glider <- x@data$glider[keepLevel, ]
                          res@data$payload1 <- x@data$payload1[keepLevel, ]
                      } else {
                          warning("evaluating in the context of payload1 only; cannot evaluate in glider context yet")
                          keep <- eval(substitute(subset), x@data[["payload1"]], parent.frame())
                          keep[is.na(keep)] <- FALSE
                          res <- x
                          res@data[["payload1"]] <- x@data[["payload1"]][keep,]
                          for (i in seq_along(x@metadata$flags)) {
                              res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
                          }
                      }
                  }
              } else {
                  ## warning("subsetting of non-seaexplorer has not been tested yet")
                  keep <- eval(substitute(subset), x@data, parent.frame())
                  keep[is.na(keep)] <- FALSE
                  res <- x
                  res@data <- subset(x@data, keep)
                  for (i in seq_along(x@metadata$flags)) {
                      res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
                  }
              }
              res@processingLog <- processingLogAppend(res@processingLog,
                                                       paste(deparse(match.call(call=sys.call(sys.parent(1)))),
                                                             sep="", collapse=""))
              gliderDebug(debug, "} # subset,glider-method\n", sep="", unindent=1)
              res
          })


#' Retrieve Part of a glider Object
#'
#' Retrieve something contained in a glider object, or something that can
#' be computed from what is contained there.
#'
#' First, a check is done to see if the object's metadata contains an item
#' with name given by \code{i}. If this is true, then that value is returned.
#'
#' Otherwise, the item is sought somewhere within the \code{data} slot.
#' The procedure is somewhat subtle, and depends on the data type.
#'
#' For objects read by \code{\link{read.glider.slocum}} ...
#' \emph{FIXME: write more here, but only when we handle the slocum data
#' in the form it has as of 2019; the code is 2 years old and the data file
#' format used in local laboratories seems to have changed, possibly twice,
#' in the meantime.}
#'
#' For objects of type \code{seaexplorer}, i.e. as read by
#' \code{\link{read.glider.seaexplorer.sub}} and
#' \code{\link{read.glider.seaexplorer.raw}}. the \code{data} slot
#' may contain multiple items.  In some cases, there will be an item
#' named \code{glider} and another named \code{payload1}. In others,
#' the first of these may be missing.  (Also, it seems likely that
#' the package will be updated to include multiple payloads, when
#' users start deploying such gliders.) If \code{j} is not specified, then
#' \code{i} is sought first in \code{payload1}, with
#' \code{glider} being checked thereafter. For example, this means that a
#' thermometer within the payload will be preferred to one attached to
#' the body of the glider. This selection
#' process can be controlled by setting \code{j} to either \code{"glider"}
#' or \code{"payload1"}.  For example, both \code{x[["temperature"]]} and
#' \code{x[["temperature","payload1"]]} retrieve values from
#' the payload thermistor, while \code{x[["temperature","glider"]]} retrieves
#' values from the glider thermister.
#'
#' In addition to retrieving data stored in the object, \code{\[\[} can also
#' return the following.
#'
#'\itemize{
#'
#' \item the full \code{data} slot, with e.g. \code{x[["data"]]}
#'
#' \item the \code{glider} item in \code{data} slot, with e.g. \code{x[["glider"]]}
#'
#' \item the \code{payload1} item in \code{data} slot, with e.g. \code{x[["payload1"]]}
#'
#' \item the Conservative Temperature calculated from water properties, with e.g. 
#' \code{x[["CT"]]}
#'
#' \item the Absolute Salinity calculated from water properties, with e.g. 
#' \code{x[["SA"]]}
#'
#' \item the sigma-theta density anomaly calculated using
#' \code{\link[oce]{swSigmaTheta}} on the water properties stored in the object,
#' with e.g. \code{x[["sigmaTheta"]]}. This obeys the setting of the
#' equation of state, set up \code{\link{options}(oceEOS="gsw")} for the
#' TEOS-10/GSW variant or \code{\link{options}(oceEOS="unesco")} for the
#' older UNESCO variant.
#' 
#' \item the sigma0 density anomaly calculated using \code{\link[oce]{swSigma0}}
#' using the water properties stored in the object, with e.g. 
#' \code{x[["sigma0"]]}. This obeys the setting of the equation of state,
#' set up \code{\link{options}(oceEOS="gsw")} for the TEOS-10/GSW variant
#' or \code{\link{options}(oceEOS="unesco")} for the older UNESCO variant.
#'
#' \item the spiciness0 water property calculated using
#' \code{\link[gsw]{gsw_spiciness0}}
#' using the water properties stored in the object. (Note that this
#' is the TEOS-10/GSW variant.)
#'
#' \item data for a given yo, with e.g. \code{x[["yo", 1]]} for the first
#' yo.
#'
#'}
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
#' @importFrom oce swSigmaTheta swSigma0 swSpice
#' @importFrom gsw gsw_CT_from_t gsw_SA_from_SP gsw_spiciness0
#' @export
setMethod(f="[[",
          signature(x="glider", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              ##. message("in [[, i='", i, "'")
              ##.debug <- getOption("gliderDebug", default=0)
              ## gliderDebug(debug, "glider [[ {\n", unindent=1)
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
              if (i == "sigmaTheta") {
                  return(swSigmaTheta(salinity=x[["salinity"]],
                                      temperature=x[["temperature"]],
                                      pressure=x[["pressure"]],
                                      longitude=x[["longitude"]],
                                      latitude=x[["latitude"]]))
              } else if (i == "sigma0") {
                  return(swSigma0(salinity=x[["salinity"]],
                                  temperature=x[["temperature"]],
                                  pressure=x[["pressure"]],
                                  longitude=x[["longitude"]],
                                  latitude=x[["latitude"]]))
              } else if (i == "spiciness0") {
                  t <- x[["temperature"]]
                  SP <- x[["salinity"]] # stored as practical salinity
                  p <- x[["pressure"]]
                  ## SA <- gsw::gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
                  ## CT <- gsw::gsw_CT_from_t(SA, t, p)
                  ## return(gsw::gsw_spiciness0(SA=SA, CT=CT))
                  SA <- gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
                  CT <- gsw_CT_from_t(SA, t, p)
                  return(gsw_spiciness0(SA=SA, CT=CT))
              }
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
#' \item \code{which=5} or \code{which="navState"}: time-series of the
#' navigation state, stored as the \code{navState} item within
#' the \code{payload1} element of the \code{data} slot. The meanings
#' of the states are:
#' \code{navState=105} means the glider is not navigating yet;
#' \code{navState=115} means the glider is surfacing
#' in preparation for communication;
#' \code{navState=116} means the glider is at the surface,
#' acquiring a GPS signal, and is communicating;
#' \code{navState=110} means the glider is inflecting downward;
#' \code{navState=100} means the glider has ballast set to be descending;
#' \code{navState=118} means the glider has ballast adjusted to reduce density,
#' so will be inflecting  upward; and
#' \code{navState=117} means the glider has ballast set to be ascending.
#' Lines and notes in the plot border indicate these states and meanings.
#'
#'}
#'
#' @param x A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param which Integer or character value specifying which style is
#' to be used; see \dQuote{Details}.
#'
#' @template debug
#'
#' @param ... ignored.
#'
#' @importFrom oce oce.plot.ts plotTS resizableLabel
#' @importFrom graphics abline par plot text
#'
#' @examples
#' library(oceanglider)
#' files <- system.file("extdata/seaexplorer/sub",
#'                      c("sea021.49.gli.sub.100.gz",
#'                        "sea021.49.pld1.sub.100.gz"), package="oceanglider")
#' g <- read.glider.seaexplorer.sub(files)
#'
#' # Example 1. A single yo of (low-resolution) "sub" data
#' plot(g, which="p")
#' plot(g, which="S")
#' plot(g, which="T")
#' plot(g, which="TS")
#' plot(g, which="map")
#' plot(g, which="navState")
#'
#' # FIXME: replace the remnants given below with interesting examples using
#' # FIXME: raw data, when we get read.glider.seaexplorer.raw() working.
#'
#'\dontrun{
#' # These files are much too large to provide, so no sample
#' # file is provided.
#' g <- read.glider(filename)
#'
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
#'
#' @export
setMethod(f="plot",
          signature="glider",
          definition=function(x, which, debug, ...) {
              debug <- if (!missing(debug)) debug else getOption("gliderDebug",0)
              gliderDebug(debug, "plot,glider-method {\n", sep="", unindent=1)
              if (which == 0 || which == "map") {
                  gliderDebug(debug, "map plot\n", sep="")
                  latitude <- x[["latitude"]]
                  longitude <- x[["longitude"]]
                  asp <- 1 / cos(mean(latitude*pi/180))
                  plot(longitude, latitude, asp=asp,
                       xlab=resizableLabel("longitude"),
                       ylab=resizableLabel("latitude"), ...)
              } else if (which == 1 || which == "p") {
                  gliderDebug(debug, "pressure time-series plot\n", sep="")
                  oce.plot.ts(x[["time"]], x[["pressure"]], ylab=resizableLabel("p"), debug=debug-1, ...)
              } else if (which == 2 || which == "T") {
                  oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), debug=debug-1, ...)
              } else if (which == 3 || which == "S") {
                  oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), debug=debug-1, ...)
              } else if (which == 4 || which == "TS") {
                  plotTS(x, debug=debug-1, ...)
              } else if (which == 5 || which == "navState") {
                  oce.plot.ts(x[["time"]], x[["navState"]],
                              xlab="Time", ylab="navState", type="p",
                              mar=c(3, 3, 1, 7))
                  for (ii in seq_along(seaexplorerNavState)) {
                      abline(h=seaexplorerNavState[[ii]], col="darkgray")
                  }
                  # labels in margin, not rotated so we can read them.
                  oxpd <- par("xpd")
                  par(xpd=NA)
                  tmax <- par("usr")[2] + 0.00 * diff(par("usr")[1:2])
                  for (ii in seq_along(seaexplorerNavState)) {
                      text(tmax, seaexplorerNavState[[ii]],
                           sprintf(" %d: %s", seaexplorerNavState[[ii]],
                                   names(seaexplorerNavState[ii])),
                           col="darkgray", cex=0.75, xpd=TRUE, pos=4)
                  }
                  par(xpd=oxpd)
              } else {
                  stop("which=", which, " is not permitted; see ?\"plot,glider-method\"")
              }
              gliderDebug(debug, "} # plot,glider-method\n", sep="", unindent=1)
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
              cat("Glider Summary\n--------------\n\n")
              nfiles <- length(object@metadata$filename)
              if (nfiles == 0) {
                  cat("* Input file: (none)\n")
              } else if (nfiles == 1) {
                  cat("* Input file:\n")
                  cat("    ", object@metadata$filename[1], "\n", sep="")
              } else if (nfiles == 2) {
                  cat("* Input files:\n")
                  cat("    ", object@metadata$filename[1], "\n", sep="")
                  cat("    ", object@metadata$filename[2], "\n", sep="")
              } else {
                  cat("* Input files:\n")
                  cat("    ", object@metadata$filename[1], "\n", sep="")
                  cat("    ", object@metadata$filename[2], "\n", sep="")
                  cat("    (and ", nfiles - 2, " others)\n", sep="")
              }
              metadataNames <- names(object@metadata)
              type <- object@metadata[["type"]]
              cat("* Type:    ", type, "\n", sep="")
              if ("subtype" %in% metadataNames)
                  cat("* Subtype: ", object@metadata[["subtype"]], "\n", sep="")
              nyo <- length(object@metadata$yo)
              if (nyo == 0)
                  cat("* Yo:      (none)\n")
              if (nyo == 1)
                  cat(sprintf("* Yo:      %d\n", object@metadata$yo))
              else if (nyo > 1)
                  cat(sprintf("* Yo:      %d values, between %d and %d\n",
                              nyo, object@metadata$yo[1], object@metadata$yo[nyo]))
              if (!is.null(type) && type == "seaexplorer") {
                  for (streamName in names(object@data)) {
                      stream <- object@data[[streamName]]
                      ## Make a list, so following code looks more like oce code.
                      if (is.data.frame(stream))
                          stream <- as.list(stream)
                      ndata <- length(stream)
                      threes <- matrix(nrow=ndata, ncol=4)
                      for (i in 1:ndata)
                          threes[i, ] <- oce::threenum(stream[[i]])
                      if (!is.null(threes)) {
                          rownames(threes) <- paste("    ", names(stream))
                          OriginalName <- unlist(lapply(names(stream), function(n)
                                                        if (n %in% names(object@metadata$dataNamesOriginal[[streamName]]))
                                                            object@metadata$dataNamesOriginal[[streamName]][[n]] else "-"))
                          threes <- cbind(threes, OriginalName)
                          colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "OriginalName")
                          cat("* Data within the \"", streamName, "\" stream:\n", sep="")
                          owidth <- options('width')
                          options(width=150) # make wide to avoid line breaks
                          print(threes, quote=FALSE)
                          options(width=owidth$width)
                          cat("\n")
                      }
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
#'     g <- read.glider.seaexplorer.sub(files)
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
#' if the first argument is a string ending with \code{".nc"}, to
#' \code{\link{read.glider.seaexplorer.sub}} if it is a vector of strings, any
#' of which contains the text \code{".sub."} followed by one or more digits, or to
#' \code{\link{read.glider.seaexplorer.raw}} if it is a vector of strings, any
#' contains the text \code{".raw."} followed by one or more digits.
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
    } else if (0 != length(grep(".sub.[0-9]+", file))) {
        res <- read.glider.seaexplorer.sub(file, debug=debug-1, ...)
    } else if (0 != length(grep(".raw.[0-9]+", file))) {
        res <- read.glider.seaexplorer.raw(file, debug=debug-1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, '} # read.glider()', unindent=1, sep="")
    res
}
