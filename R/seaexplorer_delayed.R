issue40 <- TRUE # read fractional seconds? (https://github.com/dankelley/oceGlider/issues/40)

#' Read delayed-mode SeaExplorer glider data
#'
#' Reads delayed-mode CSV files produced by a SeaExplorer glider,
#' as detected by the presence of `".raw."` in their names.
#' Such delayed-mode data are the full resolution data stored on
#' the glider and downloaded after recovery.
#' (Use [read.glider.seaexplorer.realtime()] instead
#' of this, to read data as transmitted by the glider while
#' it is in the field.)
#'
#' This function can output either "Level 0" or "Level 1" type
#' data. Level 0 is simply the raw data as written in the CSV files
#' with no processing done, other than to remove longitude and
#' latitude for samples where the glider wasn't actually communicating
#' with satellites and then interpolate between surface values.
#'
#' Level 1 processing performs a number of steps to give an
#' "analysis ready" dataset, including
#'
#' \enumerate{
#'
#' \item Interpolation of the surface longitude and latitude to give
#' an estimate of the subsurface positions. This is a crude estimate of
#' subsurface location and should be taken only as a first guess.
#'
#' \item Removal of the first few sensor values from when the glider
#' is in `navState=118` (inflecting up) or `navState=110`
#' (inflecting down). The reason for this is that when the glider is
#' set to sample on alternating profiles, when the CTD is powered up
#' the first sample output to the payload computer is the *last*
#' sample recorded before power down.
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
#' \item Calculate Practical salinity from conductivity, temperature
#' and pressure using `swSCTp()`.
#'
#' }
#'
#' @section Flag Scheme:
#' A flag scheme is set up according to the IOOS classification system (see
#' Table 2 of reference 1), as follows.
#'
#' \tabular{llll}{
#' \strong{Name}         \tab \strong{Value} \tab \strong{IOOS Name}            \tab \strong{Description}\cr
#' `pass`           \tab 1              \tab Pass                          \tab Data has passed quality control (QC) tests\cr
#' `not_evaluated`  \tab 2              \tab Not Evaluated                 \tab Data has not been QC tested\cr
#' `suspect`        \tab 3              \tab Suspect or of High Interest   \tab Data is considered to be of suspect or high interest\cr
#' `fail`           \tab 4              \tab Fail                          \tab Data is considered to have failed on one or more QC tests\cr
#' `missing`        \tab 9              \tab Missing Data                  \tab Data are missing; using a palceholder\cr
#' }
#'
#' @param directory The directory in which the delayed-mode SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If this is not provided, `read.glider.seaexplorer.delayed`
#'     will read all yo numbers for which files are present in `dir`.
#'
#' @param level A numeric value specifying the processing level, 0 or
#'     1. See Details.
#'
#' @param interpolateToCTD A logical indicating whether all sensors should
#'     be interpolated to the CTD times to obtain a common time base,
#'     or whether all sensors should simply be interpolated for all
#'     time stamps (which was the default behavious before 2019-12-08)
#'
#' @param removeTimeSincePowerOn Amount of time to remove data after
#'     the CTD is powered on. This is to remove spurious data that can
#'     occur when the glider doesn't sample every yo, and water
#'     trapped within the CTD needs to be flushed out before good data
#'     are obtained.
#'
#' @param progressBar either a logical or character value that controls
#'     whether/how to indicate the progress made in reading and interpreting
#'     the data.  This can be useful, since the work can be slow.  If `progressBar`
#'     is a logical value, then it indicates whether to show textual progress
#'     with [txtProgressBar()].  If `progressBar` is the character value `"shiny"`,
#'     then [shiny::setProgress()] and [shiny::incProgress()] will be used,
#'     on the assumption that the call to `read.glider.seaexplorer.realtime()`
#'     was made within the context of a call to [shiny::withProgress()].
#'     The default is to use the value returned by [interactive()], i.e.
#'     to use a textual progress indicator, but only in interactive mode.
##'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @examples
#' library(oceGlider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
#' g <- read.glider.seaexplorer.delayed(directory)
#' plot(g, which="p")
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @importFrom stats approx median
#' @importFrom utils read.delim flush.console head setTxtProgressBar tail txtProgressBar
## @importFrom shiny incProgress setProgress
#'
#' @author Clark Richards and Dan Kelley
#'
#' @md
#'
#' @export
read.glider.seaexplorer.delayed <- function(directory, yo, level=1, interpolateToCTD=TRUE, removeTimeSincePowerOn=0, progressBar=interactive(), debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(directory))
        stop("must provide 'directory', in which glider files reside")
    if (is.character(progressBar) && progressBar == "shiny") {
        if (!requireNamespace("shiny", quietly=TRUE))
            stop("cannot have progressBar=\"shiny\" unless the \"shiny\" package is installed")
    }
    gliderDebug(debug, "read.glider.seaexplorer.delayed(\"", directory, "\", ...) {\n", sep="", unindent=1)
    if (level != 0 & level != 1)
        stop("Level must be either 0 or 1")
    navfiles <- dir(directory, pattern='*gli*', full.names=TRUE) # FIXME: not used
    pld1files <- dir(directory, pattern='*.pld1.raw.*', full.names=TRUE)
    pld2files <- dir(directory, pattern='*.pld2.raw.*', full.names=TRUE)
    if (length(pld2files))
        warning("pld2 files are ignored by this function; contact developers if you need to read them")
    # Note the removal of .gz at the end of filenames. This is to permit both compressed
    # and uncompressed files.  (For example, the files stored within inst/extdata/ in the
    # present package have been gzipped to save space, even though the original files were
    # not gzipped.)
    yoNumber <- as.numeric(unlist(lapply(strsplit(gsub(".gz$","",pld1files), '.', fixed=TRUE), tail, 1)))
    o <- order(yoNumber)
    yoNumber <- yoNumber[o]
    pld1files <- pld1files[o]

    if (missing(yo))
        yo <- yoNumber

    y <- yoNumber %in% yo
    files <- pld1files[y]
    if (length(files) == 0)
        stop("no .pld1. files in directory '", directory, "'", sep="")

    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "delayed"
    res <- initializeFlagScheme(res, name="IOOS",
        mapping=list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9))
    res@metadata$level <- level
    res@metadata$filename <- directory
    ##44 https://github.com/dankelley/oceGlider/issues/44
    ##44 res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list(glider=list(), payload1=list())

    pld1 <- list()
    if (is.logical(progressBar) && progressBar) {
        cat('* Reading', length(files), 'files...\n')
        pb <- txtProgressBar(0, length(files), 0, style=3) # start at 0 to allow for a single yo
    } else if (is.character(progressBar) && progressBar == "shiny") {
        shiny::setProgress(0, paste("reading", length(files), "files"))
    }
    nfiles <- length(files)
    for (i in seq_len(nfiles)) {
        if (is.logical(progressBar) && progressBar)
            setTxtProgressBar(pb, i)
        else if (is.character(progressBar) && progressBar == "shiny")
            shiny::incProgress(1 / nfiles)

        d <- utils::read.delim(files[i], sep=';', stringsAsFactors=FALSE, row.names=NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        # Rename items in payload1 data.
        gliderDebug(debug > 3, 'i=',i,' (position 1) \n')
        if ("NAV_RESOURCE" %in% names(d)) {
            names(d) <- gsub("NAV_RESOURCE", "navState", names(d))
            res@metadata$dataNamesOriginal$payload1$navState <- "NAV_RESOURCE"
        }
        if ("NAV_DEPTH" %in% names(d)) {
            names(d) <- gsub("NAV_DEPTH", "pressureNav", names(d))
            res@metadata$dataNamesOriginal$payload1$pressureNav <- "NAV_DEPTH"
        }
        if ("NAV_LONGITUDE" %in% names(d)) {
            names(d) <- gsub("NAV_LONGITUDE", "longitude", names(d))
            d$longitude <- degreeMinute(d$longitude)
            res@metadata$dataNamesOriginal$payload1$longitude <- "NAV_LONGITUDE"
        }
        if ("NAV_LATITUDE" %in% names(d)) {
            names(d) <- gsub("NAV_LATITUDE", "latitude", names(d))
            d$latitude <- degreeMinute(d$latitude)
            res@metadata$dataNamesOriginal$payload1$latitude <- "NAV_LATITUDE"
        }
        gliderDebug(debug > 3, 'i=',i,' (position 2) \n')
        if ("GPCTD_TEMPERATURE" %in% names(d)) {
            names(d) <- gsub("GPCTD_TEMPERATURE", "temperature", names(d))
            res@metadata$dataNamesOriginal$payload1$temperature <- "GPCTD_TEMPERATURE"
        }
        if ("GPCTD_PRESSURE" %in% names(d)) {
            names(d) <- gsub("GPCTD_PRESSURE", "pressure", names(d))
            res@metadata$dataNamesOriginal$payload1$pressure <- "GPCTD_PRESSURE"
        }
        if ("GPCTD_CONDUCTIVITY" %in% names(d)) {
            names(d) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(d))
            res@metadata$dataNamesOriginal$payload1$conductivity <- "GPCTD_CONDUCTIVITY"
        }
        if ("GPCTD_DOF" %in% names(d)) {
            names(d) <- gsub("GPCTD_DOF", "oxygenFrequency", names(d))
            res@metadata$dataNamesOriginal$payload1$oxygenFrequency <- "GPCTD_DOF"
        }
        gliderDebug(debug > 3, 'i=',i,' (position 3) \n')
        if ("FLBBCD_CHL_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CHL_COUNT", "chlorophyllCount", names(d))
            res@metadata$dataNamesOriginal$payload1$chlorophyllCount <- "FLBBCD_CHL_COUNT"
        }
        if ("FLBBCD_CHL_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CHL_SCALED", "chlorophyll", names(d))
            res@metadata$dataNamesOriginal$payload1$chlorophyll <- "FLBBCD_CHL_SCALED"
        }
        if ("FLBBCD_BB_700_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_BB_700_COUNT", "backscatterCount", names(d))
            res@metadata$dataNamesOriginal$payload1$backscatterCount <- "FLBBCD_BB_700_COUNT"
        }
        if ("FLBBCD_BB_700_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_BB_700_SCALED", "backscatter", names(d))
            res@metadata$dataNamesOriginal$payload1$backscatter <- "FLBBCD_BB_700_SCALED"
        }
        if ("FLBBCD_CDOM_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CDOM_COUNT", "cdomCount", names(d))
            res@metadata$dataNamesOriginal$payload1$cdomCount <- "FLBBCD_CDOM_COUNT"
        }
        gliderDebug(debug > 3, 'i=',i,' (position 4) \n')
        if ("FLBBCD_CDOM_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(d))
            res@metadata$dataNamesOriginal$payload1$cdom <- "FLBBCD_CDOM_SCALED"
        }
        if ("PLD_REALTIMECLOCK" %in% names(d)) {
            names(d) <- gsub("PLD_REALTIMECLOCK", "time", names(d))
            # FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
            if (issue40)
                d$time <- as.POSIXct(d$time, format="%d/%m/%Y %H:%M:%OS", tz="UTC")
            else
                d$time <- as.POSIXct(d$time, format="%d/%m/%Y %H:%M:%S", tz="UTC")
            res@metadata$dataNamesOriginal$payload1$time <- "-"
        }
        gliderDebug(debug > 3, 'i=',i,' (position 5) \n')
        pld1[[i]] <- d
        gliderDebug(debug > 3, 'i=',i,' (position 6) \n')
    }
    gliderDebug(debug > 3,  '(position 7) \n')
    df <- do.call(rbind.data.frame, pld1)
    gliderDebug(debug, ' (position 8) \n')
    df[['X']] <- NULL # get rid of the weird last column
    gliderDebug(debug > 3, ' (position 9) \n')
    if (is.logical(progressBar) && progressBar) {
        cat('\n')
        flush.console()
    }
    gliderDebug(debug, 'Finished reading data \n')
    # First remove all duplicated lon/lat
    df$longitude[which(duplicated(df$longitude))] <- NA
    df$latitude[which(duplicated(df$latitude))] <- NA
    # Then remove all lon/lat that aren't from when navState is 116
    trans <- df$navState == 116
    df$longitude[!trans] <- NA
    df$latitude[!trans] <- NA
    # Now interpolate lon/lat
    df$longitude <- approx(df$time, df$longitude, df$time)$y
    df$latitude <- approx(df$time, df$latitude, df$time)$y
    # Trim out any empty rows (no data at all)
    sub <- df[, which(!(names(df) %in% c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber')))]
    naRows <- apply(sub, 1, function(x) sum(is.na(x)))
    ok <- naRows < dim(sub)[2]
    df <- df[ok,]
    if (level == 0) {
        res@data <- list(payload1=df)
        res@processingLog <- processingLogAppend(res@processingLog,
            paste("read.glider.seaexplorer.delayed(directory=", directory, ", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
        return(res)
    } else if (level == 1) {
        if (removeTimeSincePowerOn > 0) {
            starts <- c(1, which(diff(df$time) > 60) + 1)  # FIXME: should 60s be an argument?
            dt <- median(diff(as.numeric(df$time[!is.na(df$temperature)])))
            ok <- rep(TRUE, length(df$time))
            n <- round(removeTimeSincePowerOn/dt)
            for (s in starts) ok[s:(s+n)] <- FALSE
            df <- df[ok,]
        }
        # Interpolate NAs
        ctd <- which(!is.na(df$temperature)) # indices of measure CTD points
        n <- length(names(df)) - length(c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))
        if (is.logical(progressBar) && progressBar) {
            cat('* Interpolating NAs...\n')
            pb <- txtProgressBar(1, n, 1, style=3)
        } else if (is.character(progressBar) && progressBar == "shiny") {
            shiny::setProgress(0, paste("handling NA values for", n, "variables"))
        }
        i <- 1
        for (var in names(df)) {
            if (!(var %in% c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))) {
                if (is.logical(progressBar) && progressBar) {
                    setTxtProgressBar(pb, i)
                } else if (is.character(progressBar) && progressBar == "shiny") {
                    shiny::incProgress(1 / n, paste("handling NA for", var))
                }
                if (!all(is.na(df[[var]]))) # in case the entire field is missing, e.g. oxygenFrequency
                    df[[var]] <- approx(df[['time']], df[[var]], df[['time']])$y
                i <- i + 1
            }
        }
        # We want to interpolate non-CTD fields to the time stamps
        # for which there is actual measured CTD data. Since we've
        # already interpolated to all existing time stamps, we can
        # just remove the ones that were *not* from the CTD
        if (interpolateToCTD) {
            df <- df[ctd, ]
        }
        if (is.logical(progressBar) && progressBar) {
            cat('\n')
            flush.console()
        }
        # Remove duplicated times
        if (is.character(progressBar) && progressBar == "shiny") {
            shiny::setProgress(0, "removing duplicated items")
        }
        df <- df[!duplicated(df), ]
        # Calculate salinity
        if (is.character(progressBar) && progressBar == "shiny") {
            shiny::setProgress(0, "calculating salinity")
        }
        df$salinity <- with(df, swSCTp(conductivity, temperature, pressure, conductivityUnit = 'S/m'))
        df$salinity[df$salinity > 40] <- NA

        res@data <- list(payload1=df)
    }
    # BOOKMARK START assure that this is echoed in read.glider.seaexplorer.realtime()
    # insert units
    if (is.character(progressBar) && progressBar == "shiny") {
        shiny::setProgress(0, "creating units")
    }
    for (stream in names(res@data)) {
        # FIXME: add more units here, if any of them are certain to be known
        res@metadata$units[[stream]] <- list()
        dataNames <- names(res@data[[stream]])
        if ("salinity" %in% dataNames)
            res@metadata$units[[stream]]$salinity <- list(unit=expression(), scale="PSS-78") # FIXME: is this modern?
        if ("temperature" %in% dataNames)
            res@metadata$units[[stream]]$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        if ("pressure" %in% dataNames)
            res@metadata$units[[stream]]$pressure <- list(unit=expression(dbar), scale="")
        if ("longitude" %in% dataNames)
            res@metadata$units[[stream]]$longitude <- list(unit=expression(degree*E), scale="")
        if ("latitude" %in% dataNames)
            res@metadata$units[[stream]]$latitude <- list(unit=expression(degree*N), scale="")
        if ("heading" %in% dataNames)
            res@metadata$units[[stream]]$heading <- list(unit=expression(degree), scale="")
        if ("pitch" %in% dataNames)
            res@metadata$units[[stream]]$pitch <- list(unit=expression(degree), scale="")
        if ("roll" %in% dataNames)
            res@metadata$units[[stream]]$roll <- list(unit=expression(degree), scale="")
        # set up flags to value 2, which means not-checked
        len <- length(res@data[[stream]][[1]]) # all have same length
        for (name in dataNames) {
            res@metadata$flags[[stream]][[name]] <- rep(2, len)
        }
    }
    # BOOKMARK END
    res@processingLog <- processingLogAppend(res@processingLog,
        paste("read.glider.seaexplorer.delayed(directory=\"", directory, "\", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
    gliderDebug(debug, "read.glider.seaexplorer.delayed(\"", directory, "\", ...) {\n", unindent=1)
    res
}
