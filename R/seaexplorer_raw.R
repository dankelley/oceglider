issue40 <- TRUE # read fractional seconds? (https://github.com/dankelley/oceglider/issues/40)

#' Read Raw SeaExplorer Data
#'
#' Reads raw CSV files produced by a SeaExplorer glider. This function can
#' output either "Level 0" or "Level 1" type data. Level 0 is simply the raw
#' data as written in the CSV files with very processing done except
#' renaming variables to the oce convention (e.g. `"GPCTD_CONDUCTIVITY"`
#' is renamed as `"conductivity"`), converting timestamp strings
#' into times, and converting longitude and latitude into decimal
#' degrees.
#'
#'
#' Level 1 processing performs a number of steps to give an
#' "analysis ready" dataset, including
#'
#' \enumerate{
#'
#' \item If the dataset lacks a dead-reckoning column, surface longitude and
#' latitude readings are interpolated with respect to time, to provide a crude
#' estimate subsurface positions.
#'
## \item Removal of the first few sensor values from when the glider
## is in `navState=118` (inflecting up) or `navState=110`
## (inflecting down). The reason for this is that when the glider is
## set to sample on alternating profiles, when the CTD is powered up
## the first sample output to the payload computer is the *last*
## sample recorded before power down.
#'
#' \item Interpolation, depending on the value of `interpolateToCTD`. If
#' `interpolateToCTD` is `TRUE`, then any "extra" sensors are interpolated
#' to the times for which there is CTD data. Otherwise, NAs for *all*
#' the sensors are interpolated to a common time, corresponding to the
#' raw time stamps output from the various sensors. A caution -- this
#' will produce an apparent "upsampling" of each sensor, so that the
#' apparent sample rate is higher. For example, if a Wetlabs FLBBCD
#' sensor sampled, but there is no corresponding GP-CTD sample from
#' the same time, the CTD parameters will be interpolated from the
#' ones before and after. This has the disadvantage of interpolating
#' values that were not measured, but has the advantage of assigning
#' pressures to values measured by sensors not integrated into the CTD
#' (e.g. Wetlabs FLBBCD, Rinko O2). Following the interpolation, any
#' rows with duplicated times are removed.
#'
#' \item calculation of Practical salinity from conductivity, temperature
#' and pressure using [oce::swSCTp()].
#'
#' }
#'
#' In any case, a flag scheme is set up according to the IOOS classification system (see
#' Table 2 of reference 1), as follows.
#'
#' \tabular{llll}{
#' \strong{Name}   \tab \strong{Value} \tab \strong{IOOS Name}          \tab \strong{Description}\cr
#' `pass`          \tab 1              \tab Pass                        \tab Data has passed quality control (QC) tests\cr
#' `not_evaluated` \tab 2              \tab Not Evaluated               \tab Data has not been QC tested\cr
#' `suspect`       \tab 3              \tab Suspect or of High Interest \tab Data is considered to be of suspect or high interest\cr
#' `fail`          \tab 4              \tab Fail                        \tab Data is considered to have failed on one or more QC tests\cr
#' `missing`       \tab 9              \tab Missing Data                \tab Data are missing; using a placeholder\cr
#' }
#'
#' @param directory The directory in which the delayed-mode SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to read. If
#' this is not provided, [read.glider.seaexplorer.raw()] will read all yo
#' numbers for which files are present in `dir`.
#'
#' @param pattern a character value used to find files to read. The
#' default, `"pld1"`, will match files with that string in the name. The
#' best choices for this depends on the setup of files. The search
#' for files is done with `list.files(directory, pattern)`, and so
#' it can be a good idea to try that first, to learn how to specify
#' the desired files.  Also, try calling with `debug=1` to get some
#' indication of the files that get read or with `debug=2` to get
#' a full listing.
#'
#' @param level A numeric value specifying the processing level, 0 or
#' 1. See Details.
#'
#' @param interpolateToCTD A logical indicating whether all sensors should be
#' interpolated to the CTD times to obtain a common time base, or whether all
#' sensors should simply be interpolated for all time stamps (which was the
#' default behaviour before 2019-12-08)
#'
## @param removeTimeSincePowerOn numeric value indicating the number of
## seconds of data to trim, after the glider sensors are powered on.
## One way to determine this is to read the whole sequence,
## and then plot say the first 10 minutes of salinity and temperature
## data, looking for a transition between aphysical values, which
## might take the form of zero salinity, followed by a relatively
## rapid ramp-up to values that seem more oceanographic.
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
#' @param progressBar a logical value that controls whether to indicate the
#' progress made in reading and interpreting the data.  This can be useful,
#' since the work can be slow. The default is to show progress in
#' interactive sessions, but not in scripts.
#'
#' @param missingValue numeric value that indicates bad data. Any data items
#' equaling this value are converted to NA. The default is 9999. To avoid
#' changing values to NA, call the function with `missingValue=NULL`.
#'
#' @template debug
#'
#' @section Historical notes:
#'
#' 1. Until package version 0.1-14, released on 2025-02-10, longitude and
#'    latitude were interpolated between surface values for the case where
#'    `level` is given as 0. This behaviour was changed for issue 127, at
#'    <https://github.com/dankelley/oceglider/issues/127>).
#'
#' 2. Until package version 0.1-16, released on 2025-02-15, data could be
#'    erased for a while after power-on events, as controlled by a parameter
#'    named `removeTimeSincePowerOn`. Now, a similar action may be accomplished
#'    by calling `deleteStartupData()` on the return object from the present
#'    function.
#'
#' @template seaexplorer_names
#'
#' @examples
#' library(oceglider)
#'
#' directory <- system.file("extdata/sea_explorer/delayed_raw",
#'     package =
#'         "oceglider"
#' )
#' g <- read.glider.seaexplorer.raw(directory, progressBar = FALSE)
#' plot(g, which = "p")
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @importFrom stats approx median
#' @importFrom utils read.delim flush.console head setTxtProgressBar tail txtProgressBar
#'
#' @section History of changes to this function:
#'
#' * In version 0.1.16, the `removeTimeSincePowerOn` parameter was removed. The
#' new scheme is for the user to call `deleteStartupData()` on the return
#' value, to accomplish a similar thing.
#'
#' @author Clark Richards, Chantelle Layton and Dan Kelley
#'
#' @md
#'
#' @export
read.glider.seaexplorer.raw <- function(directory, pattern = "pld1.raw",
                                        yo, level = 1, interpolateToCTD = TRUE,
                                        # removeTimeSincePowerOn = 0,
                                        rename = TRUE,
                                        progressBar = interactive(),
                                        missingValue = 9999,
                                        debug = getOption("gliderDebug", default = 0)) {
    if (missing(directory)) {
        stop("must provide 'directory', in which glider files reside")
    }
    gliderDebug(debug, "read.glider.seaexplorer.raw(\"", directory, "\", ...) START\n", sep = "", unindent = 1)
    if (level != 0 && level != 1) {
        stop("Level must be either 0 or 1")
    }
    # navfiles <- dir(directory, pattern = "*gli*", full.names = TRUE) # FIXME: not used
    #>pattern <- paste0("*.", type, ".raw.*")
    filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
    # pld2files <- list.files(directory, pattern = "*.pld2.raw.*", full.names = TRUE)
    nfiles <- length(filenames)
    if (0L == nfiles) {
        stop("no files found in directory \"", directory, "\" that match the pattern \"", pattern, "\")")
    }
    # Note the removal of .gz at the end of filenames. This is so we can find yo numbers
    # as the last period-separated item in the filename.
    yoNumbers <- gsub(".gz$", "", filenames) |>
        strsplit(".", fixed = TRUE) |>
        lapply(tail, 1) |>
        unlist() |>
        as.integer()
    # Put the files into yo order. We must do this because the filenames
    # recovered by list.files() will not be in yo order (e.g. 100 will come
    # before 99), owing to insufficient use of leading zeros in the filenames.
    o <- order(yoNumbers)
    yoNumbers <- yoNumbers[o]
    filenames <- filenames[o]
    if (debug > 1) {
        print(data.frame(file = filenames, yoNumber = yoNumbers))
    } else if (debug == 1) {
        print(data.frame(file = filenames, yoNumber = yoNumbers) |> head())
    }
    if (missing(yo)) {
        yo <- yoNumbers
    }
    # cat(oce::vectorShow(filenames))
    # cat(oce::vectorShow(yo))
    # cat(oce::vectorShow(yoNumbers))
    y <- yoNumbers %in% yo
    # cat(oce::vectorShow(y))
    files <- filenames[y]
    # cat(oce::vectorShow(files))
    if (length(files) == 0) {
        stop("no files in directory '", directory, "'", sep = "")
    }
    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "delayed" # FIXME: this is fixed to delayed-mode ... maybe we don't want that
    res@metadata$directory <- directory
    res@metadata$pattern <- pattern
    # IOOS gives mapping=list(good = 1, not_evaluated = 2, suspect = 3, fail = 4, missing = 9)
    res <- initializeGliderFlagScheme(res, name = "IOOS")
    # res@metadata$level <- level
    res@metadata$filename <- directory
    ## 44 https://github.com/dankelley/oceglider/issues/44
    ## 44 res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list()

    showProgressBar <- identical(progressBar, TRUE)
    if (showProgressBar) {
        cat("* Reading", length(files), "files...\n")
        pb <- txtProgressBar(0, length(files), 0, style = 3) # start at 0 to allow for a single yo
    }
    ds <- list() # stores one entry per file; we expand this at the bottom of the loop
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
        nameDict <- read.csv(system.file("extdata/dictionaries/seaexplorerDict.csv", package = "oceglider"), header = FALSE, col.names = c("gliderName", "oceName"))
    }
    if (debug > 0 && !is.null(rename)) {
        cat("next is head(nameDict):\n")
        print(head(nameDict))
    }
    for (i in seq_along(files)) {
        if (showProgressBar) {
            setTxtProgressBar(pb, i)
        }
        gliderDebug(debug, "i=", i, ", file=\"", files[i], "\"\n", sep = "")
        d <- utils::read.delim(files[i], sep = ";", stringsAsFactors = FALSE, row.names = NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        gliderDebug(debug > 3, "i=", i, "  (position 1) \n")
        # cat("is next sensible\n")
        # print(head(nameDict))
        if (rename) {
            # NB we cannot e.g. convert longitude into decimal degrees if
            # we don't know which column holds longitude.
            gliderDebug(debug && i == 1L, "renaming variables for first file (others not reported)\n")
            for (row in seq_len(nrow(nameDict))) {
                gliderName <- nameDict$gliderName[row]
                oceName <- nameDict$oceName[row]
                gliderDebug(debug && i == 1L, "   ", gliderName, "->", oceName, "\n")
                namesTmp <- names(d)
                # message(oce::vectorShow(namesTmp, n = -1))
                # message(oce::vectorShow(gliderName, n = -1))
                if (gliderName %in% namesTmp) {
                    gname <- getNextName(oceName, namesTmp)
                    names(d) <- gsub(gliderName, gname, namesTmp)
                    res@metadata$dataNamesOriginal[[gname]] <- gliderName
                }
            }
            gliderDebug(debug && i == 1L, "  finished renaming for first file (others not reported)\n")
        }
        ds[[i]] <- d
        gliderDebug(debug, "first stage completed for file", i, "of", nfiles, "\n")
    }
    dall <- do.call(rbind.data.frame, ds)
    # convert some weird formats
    if ("time" %in% names(dall)) {
        gliderDebug(debug, "convert time to POSIXct object\n")
        dall$time <- as.POSIXct(dall$time, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")
    }
    if ("latitude" %in% names(dall)) {
        gliderDebug(debug, "convert latitude to decimal degrees\n")
        gliderDebug(debug, "before: ", head(dall$latitude, 3) |> paste(collapse = " "), "\n")
        dall$latitude <- degreeMinute(dall$latitude)
        gliderDebug(debug, "after: ", head(dall$latitude, 3) |> paste(collapse = " "), "\n")
    }
    if ("longitude" %in% names(dall)) {
        gliderDebug(debug, "convert longitude to decimal degrees\n")
        gliderDebug(debug, "before: ", head(dall$longitude, 3) |> paste(collapse = " "), "\n")
        dall$longitude <- degreeMinute(dall$longitude)
        gliderDebug(debug, "after: ", head(dall$longitude, 3) |> paste(collapse = " "), "\n")
    }
    # dall[["x"]] <- NULL # get rid of the weird last column
    if (showProgressBar) {
        flush.console()
    }
    if (rename) { # cannot do things in this block without renaming, because we do not know what e.g. holds lon and lat
        # First remove all duplicated lon/lat
        # Change behaviour for level=0, according to issue
        # https://github.com/dankelley/oceglider/issues/127
        dallNames <- names(dall)
        if (level > 0) {
            if (!"deadReckoning" %in% dallNames) {
                if (3L == sum(c("time", "longitude", "latitude") %in% dallNames)) {
                    gliderDebug(debug, "manipulating longitude and latitude, since no deadReckoning data are available...\n")
                    gliderDebug(debug, "  step 1: set duplicated lon,lat to NA\n")
                    dall$longitude[which(duplicated(dall$longitude))] <- NA
                    dall$latitude[which(duplicated(dall$latitude))] <- NA
                    gliderDebug(debug, "step 2: set lon,lat to NA if not at surface\n")
                    inRadioContact <- dall$navState == 116L
                    dall$longitude[!inRadioContact] <- NA
                    dall$latitude[!inRadioContact] <- NA
                    gliderDebug(debug, "  step 3: interpolate between at-surface lon and lat values\n")
                    tmplon <- try(approx(dall$time, dall$longitude, dall$time)$y, silent = TRUE)
                    tmplat <- try(approx(dall$time, dall$latitude, dall$time)$y, silent = TRUE)
                    if (!inherits(tmplon, "try-error") && !inherits(tmplat, "try-error")) {
                        dall$longitude <- tmplon
                        dall$latitude <- tmplat
                        gliderDebug(debug, "... finished interpolating lon,lat between surfacings\n")
                    } else {
                        warning("insufficient data to interpolate lon,lat between surfacings\n")
                    }
                } else {
                    gliderDebug(debug, "object does not contain 'time', 'longitude' and 'latitude'\n")
                }
            } else {
                gliderDebug(debug, "using dead-reckoning for longitude and latitude values\n")
            }
        }
        # Trim out any empty rows (no data at all)
        sub <- dall[, which(!dallNames %in% c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber"))]
        naRows <- apply(sub, 1, function(x) sum(is.na(x)))
        ok <- naRows < dim(sub)[2]
        dall <- dall[ok, ]
    }
    if (!is.null(missingValue)) {
        dall[dall == missingValue] <- NA
    }
    if (level == 0) {
        res@data <- dall
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste("read.glider.seaexplorer.raw(directory=", directory, ", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep = "")
        )
    } else if (level == 1) {
        # Interpolate NAs
        if (rename) {
            ctd <- which(!is.na(dall$temperature)) # indices of measure CTD points
            n <- length(names(dall)) - length(c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber"))
            if (showProgressBar) {
                cat("* Interpolating NAs...\n")
                pb <- txtProgressBar(1, n, 1, style = 3)
            }
            i <- 1
            for (var in names(dall)) {
                if (!(var %in% c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber"))) {
                    if (showProgressBar) {
                        setTxtProgressBar(pb, i)
                    }
                    if (!all(is.na(dall[[var]]))) { # in case the entire field is missing, e.g. oxygenFrequency
                        dall[[var]] <- approx(dall[["time"]], dall[[var]], dall[["time"]])$y
                    }
                    i <- i + 1
                }
            }
            # We want to interpolate non-CTD fields to the time stamps
            # for which there is actual measured CTD data. Since we've
            # already interpolated to all existing time stamps, we can
            # just remove the ones that were *not* from the CTD
            if (interpolateToCTD) {
                dall <- dall[ctd, ]
            }
        }
        if (showProgressBar) {
            cat("\n")
            flush.console()
        }
        # Remove duplicated times
        dall <- dall[!duplicated(dall), ]
        # Calculate salinity
        if (rename) {
            if (3L == sum(c("conductivity", "temperature", "pressure") %in% names(dall))) {
                gliderDebug(debug, "computing salinity\n")
                dall$salinity <- with(dall, swSCTp(conductivity, temperature, pressure, conductivityUnit = "S/m"))
                dall$salinity[dall$salinity > 40] <- NA
            }
        }
        res@data <- dall
    }
    # Insert units
    res@metadata$units <- list()
    dataNames <- names(res@data)
    if ("salinity" %in% dataNames) {
        res@metadata$units$salinity <- list(unit = expression(), scale = "PSS-78")
    } # FIXME: is this modern?
    if ("temperature" %in% dataNames) {
        res@metadata$units$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
    }
    if ("pressure" %in% dataNames) {
        res@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
    }
    if ("longitude" %in% dataNames) {
        res@metadata$units$longitude <- list(unit = expression(degree * E), scale = "")
    }
    if ("latitude" %in% dataNames) {
        res@metadata$units$latitude <- list(unit = expression(degree * N), scale = "")
    }
    if ("heading" %in% dataNames) {
        res@metadata$units$heading <- list(unit = expression(degree), scale = "")
    }
    if ("pitch" %in% dataNames) {
        res@metadata$units$pitch <- list(unit = expression(degree), scale = "")
    }
    if ("roll" %in% dataNames) {
        res@metadata$units$roll <- list(unit = expression(degree), scale = "")
    }
    # set up flags to value 2, which means not-checked
    len <- length(res@data[[1]]) # all have same length
    for (name in dataNames) {
        res@metadata$flags[[name]] <- rep(2, len)
    }
    res@metadata$dataAreStreamed <- FALSE
    # BOOKMARK END
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste0(
            "read.glider.seaexplorer.raw(directory=\"", directory, "\", pattern=\"", pattern, "\"\", yo=", head(yo, 1),
            ":", tail(yo, 1), ", level=", level, ")"
        )
    )
    gliderDebug(debug, "read.glider.seaexplorer.raw() END")
    res
}
