#' Summarize a glider Object
#'
#' @param object A `glider` object, i.e. one inheriting from `glider-class`.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom oce dataLabel threenum processingLogShow
#' @importFrom methods callNextMethod
#'
#' @export
#'
#' @aliases summary.glider
#'
#' @md
setMethod(f="summary",
    signature="glider",
    definition=function(object, ...) {
        # mnames <- names(object@metadata)
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
        # 44 https://github.com/dankelley/oceglider/issues/44
        # 44 nyo <- length(object@metadata$yo)
        # 44 if (nyo == 1)
        # 44     cat(sprintf("* Yo:      %d\n", object@metadata$yo))
        # 44 else if (nyo > 1)
        # 44     cat(sprintf("* Yo:      %d values, between %d and %d\n",
        # 44                 nyo, object@metadata$yo[1], object@metadata$yo[nyo]))
        payload1Exists <- "payload1" %in% names(object@data)
        stream <- if (object[["type"]] == "seaexplorer") object@data$payload1 else object@data
        if (payload1Exists) {
            odataName <- "payload1"
            odata <- object@data[[odataName]]
        } else {
            odataName <- ""
            odata <- object@data
        }
        # order names alphabetically (easier with long lists of unfamiliar names)
        o <- order(names(odata))
        odata <- odata[, o]
        # Make a list, so following code looks more like oce code.
        if (is.data.frame(odata))
            odata <- as.list(odata)
        ndata <- length(odata)
        threes <- matrix(nrow=ndata, ncol=3)
        odataNames <- names(odata)
        if ("time" %in% odataNames) {
            from <- min(odata$time, na.rm=TRUE)
            to <- max(odata$time, na.rm=TRUE)
            nt <- length(odata$time)
            deltat <- mean(diff(as.numeric(odata$time)), na.rm=TRUE)
            if (is.na(deltat)) {
                cat("* Time:               ", format(from), "\n")
            } else {
                cat("* Time ranges from", format(from), "to", format(to), "with", nt, "samples and mean increment", deltat, "s\n")
            }
        }
        for (i in 1:ndata)
            threes[i, ] <- oce::threenum(odata[[i]])
        if ("units" %in% metadataNames) {
            if (payload1Exists) {
                units <- object@metadata$units$payload1[o]
                unitsNames <- names(object@metadata$units$payload1[o])
            } else {
                units <- object@metadata$units[o]
                unitsNames <- names(object@metadata$units[o])
            }
            units <- unlist(lapply(seq_along(units),
                    function(i) {
                        u <- units[[i]]
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
                        # Clean up notation, by stages. (The order may matter.)
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
            rownames(threes) <- paste("    ", oce::dataLabel(names(odata), units), sep="")
        } else {
            rownames(threes) <- paste("    ", names(odata), sep="")
        }
        if (!is.null(threes)) {
            dim <- if (payload1Exists) {
                as.vector(lapply(object@data$payload1, function(x) length(x)))
            } else {
                as.vector(lapply(object@data, function(x) length(x)))
            }
            if (payload1Exists) {
                OriginalName <- unlist(lapply(names(odata), function(n)
                        if (n %in% names(object@metadata$dataNamesOriginal$payload1))
                            object@metadata$dataNamesOriginal$payload1[[n]] else "-"))
            } else {
                OriginalName <- unlist(lapply(names(odata), function(n)
                        if (n %in% names(object@metadata$dataNamesOriginal))
                            object@metadata$dataNamesOriginal[[n]] else "-"))
            }
            threes <- cbind(threes, dim, OriginalName)
            colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "OriginalName")
            if (object[["type"]] == "seaexplorer") {
                cat("* Data Overview (of the \"payload1\" stream):\n", sep="")
            } else {
                cat("* Data Overview:\n", sep="")
            }
            owidth <- options('width')
            options(width=150) # make wide to avoid line breaks
            if ("time" %in% odataNames)
                threes <- threes[-which("time" == odataNames), , drop=FALSE]
            print(as.data.frame(threes), digits=5)
            options(width=owidth$width)
            cat("\n")
        }
        # Get flags specifically from metadata; using [["flags"]] could extract
        # it from data, if present there and not in metadata (as e.g. with
        # the data("glider") that is provided with oce).
        flags <- if (object@metadata$type == "ioos")
            object@metadata$flags
        else
            object@metadata$flags[["payload1"]]
        if (length(flags)) {
            if (!is.null(object@metadata$flagScheme)) {
                cat("* Data-quality Flag Scheme\n\n")
                cat("    name    \"", object@metadata$flagScheme$name, "\"\n", sep="")
                cat("    mapping ", gsub(" = ", "=", as.character(deparse(object@metadata$flagScheme$mapping,
                                width.cutoff=400))), "\n", sep="")
                cat("    default ", gsub(" = ", "=", as.character(deparse(object@metadata$flagScheme$default, width.cutoff=400))), "\n\n", sep="")
            }
            flagNames <- names(flags)
            if (is.null(flagNames)) {
                cat("* Data-quality Flags (one flag applies to all data)\n\n")
                flagTable <- table(flags)
                flagTableLength <- length(flagTable)
                if (flagTableLength) {
                    for (i in seq_len(flagTableLength)) {
                        cat("    \"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep="")
                        if (i != flagTableLength) cat(", ") else cat("\n")
                    }
                }
            } else {
                cat("* Data-quality Flags\n\n")
                width <- 1 + max(nchar(flagNames))
                for (name in flagNames) {
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
                    } else {
                        cat("    ", name, ":", padding, "\"NA\" ", length(flags[[name]]), "\n", sep="")
                    }
                }
            }
            cat("\n")
        }
        processingLogShow(object)
    })

