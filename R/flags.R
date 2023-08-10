#' Handle Flags in glider Objects
#'
#' This function may be used to set suspicious data to `NA`,
#' or some other value, based on the values of corresponding data-quality
#' flags.
#'
#' The flags are stored within the object as a [list]
#' named `payload1`, which is stored within a list named `flags`
#' that is stored in the object's `metadata` slot. Both
#' `flags` and `flags$payload1` are set up when the object is
#' created, but values are inserted into `flags$payload1` are
#' inserted later, when the data are read by one of the `read.glider*`
#' functions.
#'
#' For example, [read.glider.seaexplorer.delayed()]
#' sets `flags$payload1$salinity` to be a vector of length
#' matching the data stored in `data$payload1$salinity`, and
#' does the same for temperature and some other things that are typically
#' assessed as part of quality-assessment procdures.  When these
#' things are set up, they are also assigned numerical values, one for
#' each element in the data set.  The initial value is set to
#' value 2, which means `not_evaluated`
#' in the IOOS 2017 quality-control scheme (see table 2 of reference 1).
#'
#' These numerical values provide a way to edit a dataset in an
#' convenient and traceable way, through the appropriate setting
#' of the `flags` and `actions` arguments. Flag values
#' may be altered with [setFlags,glider-method()], as
#' illustrated in the \dQuote{Examples} section.
#'
#' @param object An object of [glider-class].
#'
#' @param flags A `list` specifying flag values upon which
#' actions will be taken. This can take two forms. In the first, the
#' list has named elements each containing a vector of integers. For example,
#' salinities flagged with values of 3 ("suspect"), 4 ("fail")
#' or 9 ("missing") would be specified by `flags=list(salinity=c(3,4,9))`.
#' Several data items can be specified,
#' e.g. `flags=list(salinity=c(3,4,9),temperature=c(3,4,9))` indicates
#' that the actions are to take place for both salinity and temperature.
#' In the second form, `flags` is a list with unnamed vectors, and
#' this means to apply the actions to all the data entries; thus,
#' `flags=list(c(3,4,9))` means to apply not just to salinity and temperature,
#' but also to everything else for which flags have been set up. If `flags`
#' is not provided, then [defaultFlags()] is called on
#' `object`, to try to determine a conservative default.
#'
#' @param actions An optional `list` that contains items with
#' names that match those in the `flags` argument.  If `actions`
#' is not supplied, the default will be to set all values identified by
#' `flags` to `NA`; this can also be specified by
#' specifying `actions=list("NA")`. It is also possible to specify
#' functions that calculate replacement values. These are provided
#' with `object` as the single argument, and must return a
#' replacement for the data item in question.
#'
#' @param where An optional string that permits data and flags to be stored
#' indirectly, e.g. with data in `object@data[[where]]` instead of
#' in `object@data`, and flags in `object@metadata$flags[[where]]` instead of in
#' `object@metadata$flags`. If `where` is NULL, the second forms are used. This
#' scheme is needed because SeaExplorer data are stored in this manner.
#'
#' @param debug An optional integer specifying the degree of debugging, with
#' value 0 meaning to skip debugging and 1 or higher meaning to print some
#' information about the arguments and the data. It is usually a good idea to set
#' this to 1 for initial work with a dataset, to see which flags are being
#' handled for each data item. If not supplied, this defaults to the value of
#' `\link{getOption}("gliderDebug",0)`.
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceglider")
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
#'
#' @export
#'
#' @md
setMethod("handleFlags",
    signature=c(object="glider", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
    definition=function(object, flags=NULL, actions=NULL, where="payload1", debug=getOption("gliderDebug", 0)) {
        # DEVELOPER 1: alter the next comment to explain your setup
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
        # handleFlags(object=object, flags=flags, actions=actions, where=where, debug=debug)
        handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
    })


## NOT EXPORTED #' Low-level function to handle flags
## NOT EXPORTED #'
## NOT EXPORTED #' @param object An `oceglider` object, i.e. an object inheriting
## NOT EXPORTED #' from [glider-class].
## NOT EXPORTED #'
## NOT EXPORTED #' @param flags A `list` that associates integer values
## NOT EXPORTED #" with names, e.g. `list(good=1, bad=2)`.
## NOT EXPORTED #'
## NOT EXPORTED #' @param actions A character vector, which is lengthened to match
## NOT EXPORTED #' the length of `flags`. The most common value is `"NA"`,
## NOT EXPORTED #' which means to set flaggd values to the missing-value code, `NA`.
## NOT EXPORTED #'
## NOT EXPORTED #' @param where An optional string that allows the user to over-ride
## NOT EXPORTED #' the automated detection of where data and flags exist, within
## NOT EXPORTED #' `object`.  If `object[["type"]]` is `"seaexplorer"`, this will
## NOT EXPORTED #' default to `payload1`; otherwise, it defaults to `NULL`. Users
## NOT EXPORTED #' are advised *not* to set `where`, and it is only included here
## NOT EXPORTED #' so that `handleFlagsoceglider` behaves like [oce::handleFlags()].
## NOT EXPORTED #'
## NOT EXPORTED #' @param debug An integer specifying the debugging level, with value
## NOT EXPORTED #' `0` meaning to act silently, and higher values meaning to print
## NOT EXPORTED #' some debugging information.
## NOT EXPORTED #'
## NOT EXPORTED #' @author Dan Kelley
## NOT EXPORTED #'
## NOT EXPORTED #' @export
## NOT EXPORTED #' @md
handleFlagsInternal <- function(object, flags, actions, where, debug)
{
    if (missing(debug))
        debug <- 0
    oce::oceDebug(debug, "handleFlagsInternal() {\n", sep="", unindent=1)
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    # Permit e.g. flags=c(1,3)
    if (!is.list(flags))
        flags <- list(flags)
    if (missing(actions))
        actions <- "NA"
    if (missing(where))
       where <- if (object[["type"]] == "seaexplorer") "payload1" else NULL
    oce::handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
}

#' Set data-quality flags within a glider object
#'
#' This function changes specified entries in the data-quality
#' flags of `glider` objects. Those flags are stored within
#' a list named `flags$payload1` that resides in the `metadata`
#' slot.
#'
#' @param object A glider object, i.e. an object inheriting from `glider-class`.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's `data` slot, an error is reported.
#'
#' @param i There are three choices for `i`. First, if
#' `i=="all"`, then any existing flags for the named item are discarded, and
#' replaced with the new `value`.  Second, if `i` is a vector of
#' integers, then flags are set to `value` at indices given by `i`.
#' Third, if it is a logical vector of the same length as the data, then just
#' those indices that match `TRUE` values in `i` are set to `value`.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags
#'
#' @seealso See [handleFlags,glider-method()] for an example of use.
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
setMethod("setFlags",
    c(object="glider", name="ANY", i="ANY", value="ANY", debug="ANY"),
    function(object, name=NULL, i=NULL, value=NULL, debug=0) {
        setFlagsInternaloceglider(object, name, i, value, debug-1)
    })


setFlagsInternaloceglider <- function(object, name=NULL, i=NULL, value=NULL, debug=getOption("gliderDebug", 0))
{
    oce::oceDebug(debug, "setFlagsInternaloceglider(object, name='", name, "', value=", value,
        ", i=c(", paste(head(i), collapse=","), "...), debug=", debug, ") {\n", sep="",
        unindent=1)
    res <- object
    # Ensure proper argument setup.
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
        # Permit 'value' to be a character string, if a scheme already
        # exists and 'value' is one of the stated flag names.
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
        # Finally, apply the value
        res@metadata$flags[[where]][[name]][i] <- value
    }
    res@processingLog <- processingLogAppend(res@processingLog,
        paste("setFlags(object, name=\"", name, "\",",
            "i=c(", paste(head(i, collapse=",")), "...),",
            "value=", valueOrig, ")", collapse="", sep=""))
    oce::oceDebug(debug, "} # setFlagsInternaloceglider\n", sep="", unindent=1)
    res
}

