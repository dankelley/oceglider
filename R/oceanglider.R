#' oceanglider: A Package for Processing Ocean Glider Data
#'
#' This package was written with two particular dataset types
#' in mind, from SeaExplorer and Slocum devices. There is a good
#' chance that the functions provided here (a) will fail on
#' other types and (b) function names and arguments will change
#' as more datasets are examined by the author.
#'
#' @importFrom methods new
#' @import knitr
#' @importFrom oce handleFlags setFlags subset summary
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

#' Signal erroneous application to non-oce objects
#' @param object A vector, which cannot be the case for \code{oce} objects.
#' @param flags Ignored.
#' @param actions Ignored.
#' @param debug Ignored.
setMethod("handleFlags",
          signature=c(object="vector", flags="ANY", actions="ANY", debug="ANY"),
          definition=function(object, flags=list(), actions=list(), debug=getOption("gliderDebug", 0)) {
              stop("handleFlags() can only be applied to objects inheriting from \"glider\"")
          })

#' Handle Flags in glider Objects
#'
#' This function may be used to set suspicious data to \code{NA},
#' or some other value, based on the values of corresponding data-quality
#' flags.
#'
#' The flags are stored within the object as a \code{\link{list}}
#' named \code{payload1}, which is stored within a list named \code{flags}
#' that is stored in the object's \code{metadata} slot. Both
#' \code{flags} and \code{flags$payload1} are set up when the object is
#' created, but values are inserted into \code{flags$payload1} are
#' inserted later, when the data are read by one of the \code{read.glider*}
#' functions.
#'
#' For example, \code{\link{read.glider.seaexplorer.delayed}}
#' sets \code{flags$payload1$salinity} to be a vector of length
#' matching the data stored in \code{data$payload1$salinity}, and 
#' does the same for temperature and some other things that are typically
#' assessed as part of quality-assessment procdures.  When these
#' things are set up, they are also assigned numerical values, one for
#' each element in the data set.  The initial value is set to 
#' value 2, which means \code{not_evaluated}
#' in the IOOS 2017 quality-control scheme (see [1] table 2).
#'
#' These numerical values provide a way to edit a dataset in an
#' convenient and traceable way, through the appropriate setting
#' of the \code{flags} and \code{actions} arguments. Flag values
#' may be altered with \code{\link{setFlags,glider-method}}, as
#' illustrated in the \dQuote{Examples} section.
#'
#' @param object A \code{glider} object, i.e. an object that inherits
#' from \code{\link{glider-class}}.
#'
#' @param flags A \code{\link{list}} specifying flag values upon which
#' actions will be taken. This can take two forms. In the first, the
#' list has named elements each containing a vector of integers. For example,
#' salinities flagged with values of 1 or 3 through 9 would be specified
#' by \code{flags=list(salinity=c(2,3,4,9))}. Several data items can be specified,
#' e.g. \code{flags=list(salinity=c(2,3,4,9),temperature=c(2,3,4,9))} indicates
#' that the actions are to take place for both salinity and temperature.
#' In the second form, \code{flags} is a list with unnamed vectors, and
#' this means to apply the actions to all the data entries; thus,
#' \code{flags=list(c(2,3,4,9))} means to apply not just to salinity and temperature,
#' but also to everything else for which flags have been set up. If \code{flags}
#' is not provided, then \code{\link{defaultFlags}} is called on
#' \code{object}, to try to determine a conservative default.
#'
#' @param actions An optional \code{\link{list}} that contains items with
#' names that match those in the \code{flags} argument.  If \code{actions}
#' is not supplied, the default will be to set all values identified by
#' \code{flags} to \code{NA}; this can also be specified by
#' specifying \code{actions=list("NA")}. It is also possible to specify
#' functions that calculate replacement values. These are provided
#' with \code{object} as the single argument, and must return a
#' replacement for the data item in question.
#'
#' @param debug An optional integer specifying the degree of debugging, with
#' value 0 meaning to skip debugging and 1 or higher meaning to print some
#' information about the arguments and the data. It is usually a good idea to set
#' this to 1 for initial work with a dataset, to see which flags are being
#' handled for each data item. If not supplied, this defaults to the value of
#' \code{\link{getOption}("gliderDebug", 0)}.
#'
#' @examples
#' library(oceanglider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceanglider")
#' g <- read.glider.seaexplorer.delayed(directory)
#'
#' # The histogram motivates a crude limit for anomalously low salinity.
#' par(mfrow=c(1, 2), mar=c(3, 3, 1, 1), mgp=c(2, 0.75, 0))
#' hist(g[["salinity"]], breaks=100, xlab="Original Salinity", main="")
#' abline(v=31, col=2)
#'
#' # Flag value 3 means 'suspect' in the IOOS scheme [1, table]; other
#' # flags are pass=1, not_evaluated=2 (the default as read), fail=4, and missing=9.
#' g2 <- setFlags(g, "salinity", g[["salinity"]]<31, 3)
#' g3 <- handleFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
#' hist(g3[["salinity"]], breaks=100, xlab="Trimmed Salinity", main="")
#'
#' @references
#'\enumerate{
#' \item U.S. Integrated Ocean Observing System.
#' "Manual for the Use of Real-Time Oceanographic Data Quality Control Flags, Version 1.1,"
#' 2017. \url{https://cdn.ioos.noaa.gov/media/2017/12/QARTOD-Data-Flags-Manual_Final_version1.1.pdf}.
#'}
#'
#' @author Dan Kelley
#'
#' @family functions relating to data-quality flags
#' @export
setMethod("handleFlags",
          signature=c(object="glider", flags="ANY", actions="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, debug=getOption("gliderDebug", 0)) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              if (is.null(flags)) {
                  flags <- c(3, 4, 9)
                  if (is.null(flags))
                      stop("must supply 'flags', or use initializeFlagScheme() on the glider object first")
              }
              if (is.null(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags)))
                  stop("names of flags and actions must match")
              handleFlagsInternalOceanglider(object, flags, actions, debug)
          })

#' Low-level function to handle flags (temporary code)
#'
#' **Important note.** This function is provided only because the CRAN version
#' of the oce package does not export \code{handleFlagsInternal}. Once a
#' the CRAN oce is updated, this function will be removed.
#'
#' @param object An \code{oceanglider} object, i.e. an object inheriting
#' from \code{\link{oceanglider-class}}.
#'
#' @param flags A \code{\link{list}} that associates integer values
#" with names, e.g. \code{list(good=1, bad=2)}.
#'
#' @param actions A character vector, which is lengthened to match
#' the length of \code{flags}. The most common value is \code{"NA"},
#' which means to set flaggd values to the missing-value cod, \code{NA}.
#'
#' @param debug An integer specifying the debugging level, with value
#' \code{0} meaning to act silently, and higher values meaning to print
#' some debugginf information.
#'
#' @author Dan Kelley
#'
#' @export
handleFlagsInternalOceanglider <- function(object, flags, actions, debug) {
    gliderDebug(debug, "handleFlagsInternal() {\n", sep="", unindent=1)
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    ## Permit e.g. flags=c(1,3)
    if (!is.list(flags))
        flags <- list(flags)
    if (missing(actions)) {
        warning("no actions supplied (internal error; report to developer)")
        return(object)
    }
    if (missing(debug))
        debug <- 0
    if (any(names(flags) != names(actions)))
        stop("names of flags must match those of actions")
    ##> schemeMappingNames <- names(object@metadata$flagScheme$mapping)
    ##> if (is.character(flags[[1]])) {
    ##>     for (f in flags[[1]]) {
    ##>         if (!(f %in% schemeMappingNames))
    ##>             stop("flag \"", f, "\" is not part of the flagScheme mapping; try one of: \"",
    ##>                  paste(schemeMappingNames, collapse="\", \""), "\"")
    ##>     }
    ##>     flags <- as.numeric(object@metadata$flagScheme$mapping[flags[[1]]])
    ##>     browser()
    ##> }
    gliderDebug(debug, "flags=", paste(as.vector(flags), collapse=","), "\n")
    if (length(object@metadata$flags)) {
        all <- is.null(names(flags[1])) # "ALL" %in% names(flags)
        gliderDebug(debug, "all=", all, "\n")
        ## if (all && length(flags) > 1)
        ##    stop("if first flag is unnamed, no other flags can be specified")
        if (all && (length(actions) > 1 || !is.null(names(actions)))) {
            stop("if flags is a list of a single unnamed item, actions must be similar")
        }
        where <- "payload1"
        for (name in names(object@data[[where]])) {
            flagsObject <- object@metadata$flags[[where]]
            gliderDebug(debug, "unique(flagsObject) for ", name, ":\n")
            if (debug > 0)
                print(table(flagsObject))
            if (!is.null(flagsObject)) {
                dataItemLength <- length(object@data[[where]][[name]])
                ##> message("name: ", name, ", flags: ", paste(object@metadata$flags[[name]], collapse=" "))
                flagsThis <- if (all) flags[[1]] else flags[[name]]
                ##> message("flagsThis:");print(flagsThis)
                gliderDebug(debug, "before converting to numbers, flagsThis=", paste(flagsThis, collapse=","), "\n")
                actionsThis <- if (all) actions[[1]] else actions[[name]]
                if (name %in% names(object@metadata$flags[[where]])) {
                    gliderDebug(debug, "name: \"", name, "\"\n", sep="")
                    actionNeeded <- object@metadata$flags[[where]][[name]] %in% flagsThis
                    ##> if (name == "salinity") browser()
                    ##gliderDebug(debug, "actionNeeded: ", paste(actionNeeded, collapse=" "))
                    if (any(actionNeeded)) {
                        gliderDebug(debug, "  \"", name, "\" has ", dataItemLength, " data, of which ",
                                    sum(actionNeeded), " are flagged\n", sep="")
                        if (debug > 1) {
                            message("\nactionsThis follows...")
                            print(actionsThis)
                        }
                        if (is.function(actionsThis)) {
                            object@data[[where]][[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                        } else if (is.character(actionsThis)) {
                            if (actionsThis == "NA") {
                                object@data[[where]][[name]][actionNeeded] <- NA
                            } else {
                                stop("the only permitted character action is 'NA'")
                            }
                        } else {
                            stop("action must be a character string or a function")
                        }
                    } else {
                        gliderDebug(debug, "  no action needed, since no \"", name, "\" data are flagged as stated\n", sep="")
                    }
                }
            } else {
                gliderDebug(debug, "\"", name, "\" is not the subject of flags\n", sep="")
            }
        }
    }
    object@processingLog <- processingLogAppend(object@processingLog,
                                                paste("handleFlags(flags=",
                                                      substitute(flags, parent.frame()),
                                                      ", actions=",
                                                      substitute(actions, parent.frame()),
                                                      ")", collapse=" ", sep=""))
    gliderDebug(debug, "} # handleFlagsInternalOceanglider()\n", sep="", unindent=1)
    object
}


#' Set data-quality flags within a glider object
#'
#' This function changes specified entries in the data-quality
#' flags of \code{glider} objects. Those flags are stored within
#' a list named \code{flags$payload1} that resides in the \code{metadata}
#' slot.
#'
#' @param object A glider object, i.e. an object inheriting from \code{\link{glider-class}}.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's \code{data} slot, an error is reported.
#'
#' @param i There are three choices for \code{i}. First, if
#' \code{i=="all"}, then any existing flags for the named item are discarded, and
#' replaced with the new \code{value}.  Second, if \code{i} is a vector of
#' integers, then flags are set to \code{value} at indices given by \code{i}.
#' Third, if it is a logical vector of the same length as the data, then just
#' those indices that match \code{TRUE} values in \code{i} are set to \code{value}.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags
#'
#' @seealso See \code{\link{handleFlags,glider-method}} for an example of use.
#'
#' @author Dan Kelley
#' @export
setMethod("setFlags",
          c(object="glider", name="ANY", i="ANY", value="ANY", debug="ANY"),
          function(object, name=NULL, i=NULL, value=NULL, debug=0) {
              res <- setFlagsInternalOceanglider(object, name, i, value, debug-1)
              res
          })


setFlagsInternalOceanglider <- function(object, name=NULL, i=NULL, value=NULL, debug=getOption("gliderDebug", 0))
{
    gliderDebug(debug, "setFlagsInternalOceanglider(object, name='", name, "', value=", value,
                ", i=c(", paste(head(i), collapse=","), "...), debug=", debug, ") {\n", sep="",
                unindent=1)
    res <- object
    ## Ensure proper argument setup.
    if (is.null(name))
        stop("must supply a name")
    if (is.null(i))
        stop("must supply 'i'")
    setAll <- length(i) == 1 && i == "all"
    if (is.null(value))
        stop("must supply 'value'")
    if (length(name) > 1)
        stop("must specify one 'name' at a time")
    where <- "payload1"
    if ("flags" %in% names(object@metadata) && where %in% names(object@metadata$flags)) {
        if (!(name %in% names(object@metadata$flags[[where]])))
            stop("object has no flag for \"", name, "\"; try one of: \"", paste(names(object@metadata$flags[[where]]), collapse=" "), "\"")
        if (is.logical(i) && length(i) != length(res@metadata$flags[[where]][[1]]))
            stop("length of 'i' (", length(i), ") does not match length of object@data$payload1[[1]] (",
                 length(res@metadata$flags[[where]][[1]]))
        if (setAll)
            i <- seq_along(object@data[[where]][[1]])
        ## Permit 'value' to be a character string, if a scheme already
        ## exists and 'value' is one of the stated flag names.
        valueOrig <- value
        if (is.character(value)) {
            if (is.null(res@metadata$flagScheme)) {
                stop("cannot have character 'value' because initializeFlagScheme() has not been called on object")
            } else {
                if (value %in% names(res@metadata$flagScheme$mapping))
                    value <- res@metadata$flagScheme$mapping[[value]]
                else
                    stop("value=\"", value, "\" is not defined in the object's flagScheme; try one of: \"",
                         paste(names(res@metadata$flagScheme$mapping), "\", \""), "\"", sep="")
            }
        }
        ## Finally, apply the value
        res@metadata$flags[[where]][[name]][i] <- value
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("setFlags(object, name=\"", name, "\",",
                                                   "i=c(", paste(head(i, collapse=",")), "...),",
                                                   "value=", valueOrig, ")", collapse="", sep=""))
    gliderDebug(debug, "} # setFlagsInternalOceanglider\n", sep="", unindent=1)
    res
}


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
#' the word \code{"yolength"}, then the expression is used as a filter
#' to select yos based on the number of samples they
#' contain (see Example 2). Typically, this might be used to avoid
#' very short yos that might have been inferred erroneously
#' by the glider instrumentation.
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
#' gg <- subset(g, yolength > 4)
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
              gliderDebug(debug, "subsetString is \"", subsetString, "\"\n", sep="")

              gliderDebug(debug, "type is seaexplorer\n")
              if (!"payload1" %in% names(x@data))
                  stop("In subset,glider-method() : cannot subset seaexplorer objects that lack a 'payload1' item in the data slot", call.=FALSE)
              if (is.character(substitute(subset))) {
                  gliderDebug(debug, "subset is character\n")
                  ## subset is a character string
                  if (subset == "ascending") {
                      res <- x
                      keep <- res@data$glider$navState == 117
                      res@data$glider <- subset(res@data$glider, keep)
                      res@data$payload1 <- subset(res@data$payload1, keep)
                      for (i in seq_along(x@metadata$flags[["payload1"]])) {
                          res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keep]
                      }
                  } else if (subset == "descending") {
                      res <- x
                      keep <- res@data$glider$navState == 100
                      res@data$glider <- subset(res@data$glider, keep)
                      res@data$payload1 <- subset(res@data$payload1, keep)
                      for (i in seq_along(x@metadata$flags[["payload1"]])) {
                          res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keep]
                      }
                  }
              } else {
                  gliderDebug(debug, "subset is a logical expression\n")
                  ## subset is a logical expression
                  if (1 == length(grep("yolength", subsetString))) {
                      if (!"payload1" %in% names(x@data))
                          stop("In subset,glider-method() : only works for 'raw' datasets, not for 'sub' ones; contact package authors, if you need to handle sub data", call.=FALSE)
                      s <- split(x@data$payload1, x[["yoNumber"]])
                      ## warning removed for issue (https://github.com/dankelley/oceanglider/issues/41)
                      ## warning("In subset,glider-method() : only subsetting 'payload1'; contact package authors, if your data have other streams", call.=FALSE)
                      thisYolength <- as.integer(lapply(s, function(ss) length(ss[["pressure"]])))
                      keepYo <- eval(substitute(subset), list(yolength=thisYolength))
                      ##message("sum(keepYo)=", sum(keepYo), " length(keepYo)=", length(keepYo))
                      res <- x
                      res@metadata$yo <- x@metadata$yo[keepYo]
                      keepData <- unlist(lapply(seq_along(s), function(si) rep(keepYo[si], thisYolength[si])))
                      ## NOTE: the following was a much slower (10s of seconds compared to perhaps 1s or less)
                      ## res@data$payload <- do.call(rbind.data.frame, x@data$payload[keepYo, ])
                      res@data$glider <- x@data$glider[keepData, ]
                      res@data$payload1 <- x@data$payload1[keepData, ]
                      for (i in seq_along(x@metadata$flags[["payload1"]])) {
                          res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keepData]
                      }
                  } else {
                      ##warning("evaluating in the context of payload1 only; cannot evaluate in glider context yet")
                      keep <- eval(substitute(subset), x@data[["payload1"]], parent.frame())
                      keep[is.na(keep)] <- FALSE
                      gliderDebug(debug, "keeping", sum(keep), "of", length(keep), "elements\n")
                      res <- x
                      res@data[["payload1"]] <- x@data[["payload1"]][keep,]
                      for (i in seq_along(x@metadata$flags[["payload1"]])) {
                          res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keep]
                      }
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
#' \code{\link{read.glider.seaexplorer.realtime}} and
#' \code{\link{read.glider.seaexplorer.delayed}}. the \code{data} slot
#' may contain multiple items.  In some cases, there will be an item
#' named \code{glider} and another named \code{payload1}. In others,
#' the first of these may be missing.  (Also, it seems likely that
#' the package will be updated to include multiple payloads, when
#' users start deploying such gliders.)
#'
#' If \code{j} is not specified, then
#' \code{i} is sought first in \code{payload1}, with
#' \code{glider} being checked thereafter. For example, this means that a
#' thermometer within the payload will be preferred to one attached to
#' the body of the glider. This selection
#' process can be controlled by setting \code{j} to either \code{"glider"}
#' or \code{"payload1"}.  For example, both \code{x[["temperature"]]} and
#' \code{x[["temperature","payload1"]]} retrieve values from
#' the payload thermistor, while \code{x[["temperature","glider"]]} retrieves
#' values from the glider thermister. For clarity of code, it might make
#' sense to always specify \code{j}.
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
#' \item the Absolute Salinity is returned with e.g.
#' \code{x[["SA"]]}. This is computed with
#' \code{\link[gsw]{gsw_SA_from_SP}}, based on the water properties
#' stoed in the object. (See also the item for Conservative Temperature)
#'
#' \item the sigma-theta density anomaly calculated using
#' \code{\link[oce]{swSigmaTheta}} on the water properties stored in the object,
#' with e.g. \code{x[["sigmaTheta"]]}. This obeys the setting of the
#' equation of state, set up \code{\link{options}(oceEOS="gsw")} for the
#' TEOS-10/GSW variant or \code{\link{options}(oceEOS="unesco")} for the
#' older UNESCO variant.
#'
#' \item the Conservative Temperatuer is returned with e.g.
#' \code{x[["CT"]]}. This is computed with
#' \code{\link[gsw]{gsw_CT_from_t}}, based on the water properties
#' stoed in the object. (See also the item for Absolute Salinity.)
#'
#' \item the sigma0 density anomaly is returned with e.g.
#' \code{x[["sigma0"]]}. This is computed with
#' \code{\link[oce]{swSigma0}}  based on the water properties
#' stored in the object.
#' Note that the computation depends on the setting of the equation of state,
#' set up \code{\link{options}(oceEOS="gsw")} for the TEOS-10/GSW variant
#' or \code{\link{options}(oceEOS="unesco")} for the older UNESCO variant.
#'
#' \item the spiciness0 water property is returned with e.g.
#' \code{x[["spiciness0"]]}. This is computed with
#' \code{\link[gsw]{gsw_spiciness0}}, based on the water properties
#' stoed in the object. (Note that this is the TEOS-10/GSW variant.)
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
              else if (i == "yoNumber" && !missing(j)) {
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
                  ## returns a list
                  where <- "payload1"
                  return(if ("flags" %in% names(x@metadata)) x@metadata$flags[[where]][[gsub("Flag$", "", i)]] else NULL)
              }
              ## FIXME (DK) recognize "Unit$" as done in oce.
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
                  ## SA <- gsw::gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
                  ## CT <- gsw::gsw_CT_from_t(SA, t, p)
                  ## return(gsw::gsw_spiciness0(SA=SA, CT=CT))
                  SA <- gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
                  CT <- gsw_CT_from_t(SA, t, p)
                  return(gsw_spiciness0(SA=SA, CT=CT))
              }
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
                      if (i %in% names(x@data[["payload1"]]))
                          return(x@data$payload1[[i]])
                      else
                          return(x@data$glider[[i]]) # what if there is no glider?
                      return(x@data$payload[[i]])
                  }
              }
              ##. message("j is not missing. j='", j, "'")
              if (j == "glider")
                  return(x@data$glider)
              if (j == "payload")
                  return(x@data$payload1)
              if (j == "payload1")
                  return(x@data$payload1)
              if (j == "payload2")
                  return(x@data$payload2)
              stop("type='", type, "' not permitted; it must be 'seaexplorer' or 'slocum'")
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
#' of the \code{navState} values for \code{seaexplorer} data
#' are:
#'
#' \itemize{
#'
#' \item \code{105}: glider is not navigating yet
#'
#' \item \code{115}: glider is surfacing, with ballast and
#' centre of gravity being adjusted to put antenna out
#' of the water
#'
#' \item \code{116}: glider is at the surface,
#' acquiring a GPS signal, and communicating
#'
#' \item \code{110}: ballast and centre of mass are
#' adjusted to cause glider to inflect downward
#'
#' \item \code{100}: ballast is in diving position; adjustments
#' may be made to adjust pitch and heading
#'
#' \item \code{118}: target depth or altitude has been achieved,
#' so ballast and centre of mass are adjusted to inflect glider
#' upwards
#'
#' \item \code{117}: glider is ascending, with controls being
#' adjusted for desired pitch and heading
#'
#'}
#'
#' Lines and notes in the plot border indicate these states, both
#' numerically and with phrases, as inferred by
#' \code{\link{navStateCodes}}.
#'
#'}
#'
#' @param x A \code{glider} object, i.e. one inheriting from \code{\link{glider-class}}.
#'
#' @param which Integer or character value specifying which style is
#' to be used; see \dQuote{Details}.
#'
#' @param type Type of plot, as defined in \code{\link{par}}, e.g. \code{"p"} (the
#' default) for points, \code{"l"} for connected line segments, or \code{"o"}
#' for an overlay of points and lines, etc. The default is \code{"o"}, which is
#' perhaps the best for short sequences.
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
#'
#' ## Examples 1: a single yo of low-resolution real-time data
#' dirRealtime <- system.file("extdata/seaexplorer/sub", package="oceanglider")
#' g <- read.glider.seaexplorer.realtime(dirRealtime, yo=100)
#' plot(g, which="p")
#' plot(g, which="S")
#' plot(g, which="T")
#' plot(g, which="TS") # note odd connections between points
#' plot(g, which="map")
#' plot(g, which="navState")
#'
#' # Example 2: navState and pressure history of some delayed-mode yos,
#' # from a deployment in which sampling was supposed to be
#' # suppressed during the descending phases of motion.
#' dirRaw <- system.file("extdata/seaexplorer/raw", package="oceanglider")
#' g <- read.glider.seaexplorer.delayed(dirRaw)
#' plot(g, which="navState")
#'
#' # Note: colormap and drawPalette are oce functions.
#' cm <- colormap(g[["temperature"]])
#' # Note the setting of mar, here and in th plot.
#' par(mar=c(2, 3.5, 2, 4))
#' drawPalette(colormap=cm)
#' plot(g, which="p", type="p", cex=1/3, col=cm$zcol, mar=c(2, 3.5, 2, 4))
#'
#' @export
setMethod(f="plot",
          signature="glider",
          definition=function(x, which, type="o", debug, ...) {
              dots <- list(...)
              debug <- if (!missing(debug)) debug else getOption("gliderDebug",0)
              gliderDebug(debug, "plot,glider-method {\n", sep="", unindent=1)
              if (which == 0 || which == "map") {
                  gliderDebug(debug, "map plot\n", sep="")
                  latitude <- x[["latitude"]]
                  longitude <- x[["longitude"]]
                  asp <- 1 / cos(mean(latitude*pi/180))
                  plot(longitude, latitude, asp=asp,
                       xlab=resizableLabel("longitude"),
                       ylab=resizableLabel("latitude"), type=type, ...)
              } else if (which == 1 || which == "p") {
                  gliderDebug(debug, "pressure time-series plot\n", sep="")
                  p <- x[["pressure"]]
                  if ("ylim" %in% names(dots)) oce.plot.ts(x[["time"]], p, ylab=resizableLabel("p"), debug=debug-1, type=type, ...)
                  else oce.plot.ts(x[["time"]], p, ylab=resizableLabel("p"), ylim=rev(range(p, na.rm=TRUE)), debug=debug-1, type=type, ...)
              } else if (which == 2 || which == "T") {
                  oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), debug=debug-1, type=type, ...)
              } else if (which == 3 || which == "S") {
                  oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), debug=debug-1, type=type, ...)
              } else if (which == 4 || which == "TS") {
                  plotTS(x, debug=debug-1, type=type, ...)
              } else if (which == 5 || which == "navState") {
                  ns <- navStateCodes(x)
                  oce.plot.ts(x[["time"]], x[["navState"]], ylab="navState",
                              mar=c(2, 3, 1, 9), type=type, ...)
                  for (ii in seq_along(ns)) {
                      abline(h=ns[[ii]], col="blue")
                  }
                  # labels in margin, not rotated so we can read them.
                  oxpd <- par("xpd")
                  par(xpd=NA)
                  tmax <- par("usr")[2] + 0.00 * diff(par("usr")[1:2])
                  for (ii in seq_along(ns)) {
                      text(tmax, ns[[ii]],
                           sprintf(" %d: %s", ns[[ii]],
                                   names(ns[ii])),
                           col="blue", cex=0.75, xpd=TRUE, pos=4)
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
#' @importFrom oce dataLabel threenum processingLogShow
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
              if (nyo == 1)
                  cat(sprintf("* Yo:      %d\n", object@metadata$yo))
              else if (nyo > 1)
                  cat(sprintf("* Yo:      %d values, between %d and %d\n",
                              nyo, object@metadata$yo[1], object@metadata$yo[nyo]))
              for (streamName in names(object@data)) {
                  stream <- object@data[[streamName]]
                  ## order names alphabetically (easier with long lists of unfamiliar names)
                  o <- names(stream)
                  stream <- stream[, o]
                  ## Make a list, so following code looks more like oce code.
                  if (is.data.frame(stream))
                      stream <- as.list(stream)
                  ndata <- length(stream)
                  threes <- matrix(nrow=ndata, ncol=4)
                  for (i in 1:ndata)
                      threes[i, ] <- oce::threenum(stream[[i]])
                  if ("units" %in% metadataNames) {
                      units <- object@metadata$units[[streamName]]
                      unitsNames <- names(object@metadata$units[[streamName]])
                      units <- unlist(lapply(seq_along(object@metadata$units[[streamName]]),
                                             function(i) {
                                                 u <- object@metadata$units[[streamName]][[i]]
                                                 if (0 == length(u[1][[1]])) {
                                                     if (2 == length(u)) return(u[2]) else return("")
                                                 }
                                                 if (length(u) == 1) {
                                                     res <- if (is.expression(u)) as.character(u) else u
                                                 } else if (length(u) == 2) {
                                                     res <- if (nchar(u[2])) paste(u[[1]], u[[2]], sep=", ") else u[[1]]
                                                 } else {
                                                     res <- ""
                                                 }
                                                 res <- as.character(res)[1] # the [1] is in case the unit is mixed up
                                                 ## Clean up notation, by stages. (The order may matter.)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+C", "\u00B0C", res)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+F", "\u00B0F", res)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+E", "\u00B0E", res)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+W", "\u00B0W", res)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+N", "\u00B0N", res)
                                                 if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+S", "\u00B0S", res)
                                                 if (nchar(res)) res <- gsub("percent", "%", res)
                                                 if (nchar(res)) res <- gsub("degree", "\u00B0", res)
                                                 if (nchar(res)) res <- gsub("^,[ ]*", "", res)
                                                 if (nchar(res)) res <- gsub("mu . ", "\u03BC", res)
                                                 if (nchar(res)) res <- gsub("per . mil", "\u2030", res)
                                                 if (nchar(res)) res <- gsub("10\\^\\(-8\\)[ ]*\\*", "10\u207B\u2078", res)
                                                 if (nchar(res)) res <- gsub("\\^2", "\u00B2", res)
                                                 if (nchar(res)) res <- gsub("\\^3", "\u00B3", res)
                                                 res
                                             }))
                      names(units) <- unitsNames
                      rownames(threes) <- paste("    ", oce::dataLabel(names(stream), units), sep="")
                  } else {
                      rownames(threes) <- paste("    ", names(stream), sep="")
                  }
                  if (!is.null(threes)) {
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
              ## Get flags specifically from metadata; using [["flags"]] could extract
              ## it from data, if present there and not in metadata (as e.g. with
              ## the data("glider") that is provided with oce).
              flags <- object@metadata$flags[["payload1"]]
              if (length(flags)) {
                  if (!is.null(object@metadata$flagScheme)) {
                      cat("* Data-quality Flag Scheme\n\n")
                      cat("    name    \"", object@metadata$flagScheme$name, "\"\n", sep="")
                      cat("    mapping ", gsub(" = ", "=", as.character(deparse(object@metadata$flagScheme$mapping,
                                                                                   width.cutoff=400))), "\n\n", sep="")
                  }
                  cat("* Data-quality Flags\n\n")
                  width <- 1 + max(nchar(names(flags)))
                  for (name in names(flags)) {
                      padding <- rep(" ", width - nchar(name))
                      if (!all(is.na(flags[[name]]))) {
                          cat("    ", name, ":", padding, sep="")
                          flagTable <- table(flags[[name]])
                          flagTableLength <- length(flagTable)
                          if (flagTableLength) {
                              for (i in seq_len(flagTableLength)) {
                                  cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep="")
                                  if (i != flagTableLength) cat(", ") else cat("\n")
                              }
                          }
                      }
                  }
                  cat("\n")
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

# a helper function to simplify code in read.glider.netcdf()
getAtt <- function(f, varid=0, attname=NULL, default=NULL)
{
    if (is.null(attname))
        stop("must give attname")
    ##message(attname)
    t <- try(ncatt_get(f, varid=varid, attname=attname), silent=TRUE)
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
    instrument <- getAtt(f, attname="instrument", default="?")
    instrumentManufacturer <- getAtt(f, attname="instrument_manufacturer", default="?")
    instrumentModel <- getAtt(f, attname="instrument_model", default="?")
    type <- getAtt(f, attname="platform_type", default="?")
    if (type == "Slocum Glider")
        type <- "slocum"
    res@metadata$type <- type
    data <- list()
    ## FIXME get units
    ## FIXME change some variable names from snake-case to camel-case
    dataNames <- names(f$var)
    data$time <- numberAsPOSIXct(as.vector(ncvar_get(f, "time")))
    dataNamesOriginal <- list()
    ##? if (!"time" %in% dataNames)
    ##?     dataNamesOriginal$time <- "-"
    ## Get all variables, except time, which is not listed in f$var
    gliderDebug(debug, "reading and renaming data\n")
    for (i in seq_along(dataNames))  {
        newName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        if (dataNames[i] == "time") {
            data[["time"]] <- numberAsPOSIXct(as.vector(ncvar_get(f, "time")))
            gliderDebug(debug, "i=", i, " ... time converted from integer to POSIXct\n", sep="")
        } else {
            data[[newName]] <- as.vector(ncvar_get(f, dataNames[i]))
            gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" converted to \"", newName, "\"\n", sep="")
            dataNames[i] <- newName
        }
    }
    ##gliderDebug(debug, "dataNames:", paste(dataNames, collapse=";"), "\n")
    ##names(data) <- if ("time" %in% dataNames) dataNames else c("time", dataNames)
    res@data$payload1 <- as.data.frame(data)
    ##head(res@data$payload1$time)
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- list(payload1=dataNamesOriginal)
    gliderDebug(debug, "} # read.glider.netcdf", unindent=1, sep="")
    res
}

#' Read a glider data file
#'
#' This is a high-level function that passes control to \code{\link{read.glider.netcdf}}
#' if the first argument is a string ending with \code{".nc"}, to
#' \code{\link{read.glider.seaexplorer.realtime}} if it is a vector of strings, any
#' of which contains the text \code{"pld1.sub."} followed by one or more digits, or to
#' \code{\link{read.glider.seaexplorer.delayed}} if it is a vector of strings, any
#' contains the text \code{"pld1.raw."} followed by one or more digits.
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
    } else if (0 != length(grep("pld1.sub", file))) {
        res <- read.glider.seaexplorer.realtime(file, debug=debug-1, ...)
    } else if (0 != length(grep("pld1.raw", file))) {
        res <- read.glider.seaexplorer.delayed(file, debug=debug-1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, '} # read.glider()', unindent=1, sep="")
    res
}

#' Convert data to glider format
#'
#' This function returns a glider object that holds data as provided
#' in the \code{data} argument, with units as provided by the \code{units}
#' argument. The \code{units} argument is optional, making the function
#' easy to use in interactive sessions, but production code ought to
#' be written with units fully specified.
#'
#' @param type Character value giving the type of glider, e.g.
#' be either \code{seaexplorer} or \code{slocum}.
#'
#' @param data A data frame containing the data. This is copied straight into
#' the \code{payload1} item in the \code{data} slot of the returned value,
#' \emph{without} name translation. For most functions in this package to work,
#' \code{data} ought to have items named \code{longitude},
#' \code{latitude}, \code{salinity}, \code{temperature} and
#' \code{pressure}.
#'
#' @param units A list holding units, with names corresponding to the
#' names in the data. See the example for the format to be used
#' for \code{units}, but note that there are several items in this
#' dataset that are not given units, in this example.
#'
#' @examples
#' library(oceanglider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceanglider")
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
