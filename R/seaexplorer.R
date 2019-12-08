issue40 <- TRUE # read fractional seconds? (https://github.com/dankelley/oceanglider/issues/40)

#' Possible navState values of a glider object
#'
#' This function provides names for the numerical `navState` codes
#' used by various gliders, with the aim of making it easier to
#' write self-explantory code (see \dQuote{Examples}).
#'
#' The numerical values for `seaexplorer` are as follows.
#' \tabular{lll}{
#' **Name** \tab **Value** \tab **Description**\cr
#' `not_navigating`       \tab 105 \tab glider is being set up\cr
#' `surfacing`            \tab 115 \tab nearing the surface\cr
#' `at_surface`           \tab 116 \tab at the surface, acquiring GPS and transmitting data\cr
#' `inflecting_downwards` \tab 110 \tab ballast being adjusted to cause descent\cr
#' `descending`           \tab 100 \tab ballast causing descent\cr
#' `inflecting_upwards`   \tab 118 \tab ballast being adjusted to cause ascent\cr
#' `ascending`            \tab 117 \tab ballast causing ascent\cr
#'}
#' Note that the downward portions of profiles are roughly signalled by several
#' `inflecting_downwards` codes followed by `descending`
#' codes, while the upward portions have `inflecting_upwards` codes
#" followed by `ascending` codes.
#'
#' The numerical values for type `slocum` are as follows. (These
#' are(defined as `m_depth_state` in the `slocum` documentation;
#' see pages 1-24 of reference 1.)
#' \tabular{lll}{
#' **Name**   \tab **Value** \tab **Description**\cr
#' `ignore`   \tab        99 \tab              - \cr
#' `hover`    \tab         3 \tab              - \cr
#' `climbing` \tab         2 \tab              - \cr
#' `diving`   \tab         1 \tab              - \cr
#' `surface`  \tab         0 \tab              - \cr
#' `none`     \tab        -1 \tab              - \cr
#'}
#'
#' @param g Either a character string or glider object. If it is a string,
#' then it is the type of glider, which in the present version of the
#' function must be `"seaexplorer"`. If it is
#' a glider object, then the value of `navStateCodes` in the `metadata`
#' slot of that object is returned, if that exists, or else the `type`
#' item in the `metadata` slot is used to determine the type, as
#' in the case with `g` being a character string.
#'
#' @return A list of integers defining the navigation state, each
#' given a brief name as indicated in the \dQuote{Details} section.
#'
#' @examples
#' ## Use codes to identify upcasts, at least roughly (note the stray points)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceanglider")
#' g <- read.glider.seaexplorer.delayed(directory)
#' ns <- navStateCodes(g)
#' plot(g, which="p")
#' ga <- subset(g, navState == ns$ascending)
#' points(ga[["time"]], ga[["pressure"]], col=3, pch=20)
#' giu <- subset(g, navState == ns$inflecting_upwards)
#' points(giu[["time"]], giu[["pressure"]], col=2, pch=20)
#' mtext(" red=inflecting_upwards; green=ascending", side=3, line=-1, adj=0)
#'
#' @references
#' 1.Teledyne Webb Research. \emph{Slocum G2 Glider Operators Manual}, January 2012.
#' \url{https://gliderfs2.coas.oregonstate.edu/gliderweb/docs/slocum_manuals/Slocum_G2_Glider_Operators_Manual.pdf}.
#'
#' @export
#'
#' @author Dan Kelley
#'
#' @md
navStateCodes <- function(g)
{
    type <- if (is.character(g)) g else g@metadata$type
    if (type == "seaexplorer") {
        list("not_navigating"=105,
             "surfacing"=115,
             "at_surface"=116,
             "inflecting_downwards"=110,
             "descending"=100,
             "inflecting_upwards"=118,
             "ascending"=117)
    } else {
        stop("only g=\"seaexplorer\" data handled so far")
    }
}

#' Read real-time SeaExplorer glider data
#'
#' Reads real-time CSV files produced by a SeaExplorer glider, as
#' detected by the presence of `".sub."` in their names.
#' Such real-time data are decimated before transmission, and thus do not
#' represent the full data collected by the glider sensors.
#' (Use [read.glider.seaexplorer.delayed)] instead of
#' this, to read delayed-mode data, as downloaded from the glider
#' after recovery.)
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
#' @references
#' 1. IOOS. “Manual for Real-Time Oceanographic Data Quality Control Flags,” May 2017.
#' https://cdn.ioos.noaa.gov/media/2017/12/QARTOD-Data-Flags-Manual_Final_version1.1.pdf.
#'
#' @param directory The directory in which the realtime SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If this is not provided, [read.glider.seaexplorer.delayed()]
#'     will read all yo numbers for which files are present in `dir`.
#'
#' @param level Ignored by [read.glider.seaexplorer.realtime] and
#'     only included for similarity with
#'     [read.glider.seaexplorer.delayed()].
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
#'
#' @param missingValue A value that indicates missing data; all
#'     values that match this are set to `NA`.
#'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @examples
#' library(oceanglider)
#' directory <- system.file("extdata/seaexplorer/sub", package="oceanglider")
#' g <- read.glider.seaexplorer.realtime(directory)
#' plot(g, which="navState")
#' plot(g, which="S")
#' plot(g, which="T")
## ctd <- as.ctd(g[['salinity']], g[['temperature']], g[['pressure']],
##               longitude=g[['longitude']], latitude=g[['latitude']])
## plot(ctd)
## ## Isolate the upcast, inferred with oce::ctdTrim().
## plot(ctdTrim(ctd, "upcast"))
## ## Isolate the upcast, using g[["NAV_RESOURCE"]]==117;
## ## note that the downcast has code 100.
## plot(subset(ctd, g[["NAV_RESOURCE"]]==117))
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend initializeFlagScheme
## @importFrom shiny incProgress setProgress
#'
#' @export
#'
#' @author Dan Kelley and Clark Richards
#'
#' @md
read.glider.seaexplorer.realtime <- function(directory, yo, level=1, progressBar=interactive(), missingValue=9999, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(directory))
        stop("must provide 'directory', in which glider files reside")
    if (is.character(progressBar) && progressBar == "shiny") {
        if (!requireNamespace("shiny", quietly=TRUE))
            stop("cannot have progressBar=\"shiny\" unless the \"shiny\" package is installed")
    }
    gliderDebug(debug, "read.glider.seaexplorer.realtime(\"", directory, "\", ...) {\n", sep="", unindent=1)
    yoGiven <- !missing(yo)
    glifiles <- dir(directory, pattern='*gli*', full.names=TRUE)
    pld1files <- dir(directory, pattern='*.pld1.*', full.names=TRUE)
    if (length(glifiles) != length(pld1files)) {
        warning("There is an unequal number of *gli* files (", length(glifiles),
                ") and *pld1* files (", length(pld1files), "), but they ought to be paired. This may indicate a problem in the data directory. Try calling this function with debug=2 to see filenames.")
    }

    if (debug > 1) {
        cat("Originally, gli files:\n")
        print(glifiles)
        cat("\n")
        cat("Originally, pld1 files:\n")
        print(pld1files)
        cat("\n")
    }

    ## If 'yo' was not given, we use all possible values, based on the files
    ## identified so far. This is done with gsub() calls in a step-by-step
    ## process, for simplicity of recoding if the manufacturer changes the
    ## filename pattern.
    if (!yoGiven) {
        yo <- gsub(".*/", "", pld1files) # now just filename
        yo <- gsub("^.*pld1.sub.([0-9]+).*$", "\\1", yo) # now just yo number
        yo <- as.numeric(yo)
        gliderDebug(debug, "yo=", paste(yo, collapase=" "), "\n", sep="")
    }
    ## Narrow glifiles and pld1files, to just those that match the yo pattern
    keepglifiles <- NULL
    for (y in yo) {
        found <- grep(paste("\\.",y,"\\.",sep=""), glifiles)
        if (length(found) == 1)
            keepglifiles <- c(keepglifiles, glifiles[found])
    }
    if (!length(keepglifiles))
        stop("no gli file found for yo=", paste(yo, collapse=" "), sep="")
    glifiles <- keepglifiles
    keeppld1files <- NULL
    for (y in yo) {
        found <- grep(paste("\\.",y,"\\.",sep=""), pld1files)
        if (length(found) == 1)
            keeppld1files <- c(keeppld1files, pld1files[found])
    }
    if (!length(keeppld1files))
        stop("no pld1 file found for yo=", paste(yo, collapse=" "))
    pld1files <- keeppld1files

    if (debug > 1) {
        cat("After trimming to the yo subset, gli files:\n")
        print(glifiles)
        cat("\n")
        cat("After trimming to the yo subset, pld1 files:\n")
        print(pld1files)
        cat("\n")
    }

    ## gli files
    nfiles <- length(glifiles)
    if (is.logical(progressBar) && progressBar) {
        cat('* Reading', nfiles, ifelse(nfiles==1, 'gli file\n', 'gli files...\n'))
        pb <- txtProgressBar(0, nfiles, 0, style=3) # start at 0 to allow for a single yo
    } else if (is.character(progressBar) && progressBar == "shiny") {
        shiny::setProgress(0, paste("reading", nfiles, "files"))
    }
    gli <- list()
    for (i in seq_len(nfiles)) {
        if (is.logical(progressBar) && progressBar) {
            setTxtProgressBar(pb, i)
        } else if (is.character(progressBar) && progressBar == "shiny") {
            shiny::incProgress(1 / nfiles)
        }
        gliderDebug(debug, "reading gli file:  ", glifiles[i], "\n")
        gliData <- utils::read.delim(glifiles[i], sep=";")
        gliData$yoNumberNav <- rep(yo[i], dim(gliData)[1])
        gli[[i]] <- gliData
    }
    gliData <- do.call(rbind.data.frame, gli)
    gliData$X <- NULL
    ## pld1 files
    nfiles <- length(pld1files)
    if (is.logical(progressBar) && progressBar) {
        cat('\n')
        flush.console()
        cat('* Reading', nfiles, ifelse(nfiles==1, 'pld1 file\n', 'pld1 files...\n'))
        pb <- txtProgressBar(0, nfiles, 0, style=3)
    } else if (is.character(progressBar) && progressBar == "shiny") {
        shiny::setProgress(0, paste("reading", nfiles, "files"))
    }
    pld1 <- list()
    for (i in seq_len(nfiles)) {
        if (is.logical(progressBar) && progressBar) {
            setTxtProgressBar(pb, i)
        } else if (is.character(progressBar) && progressBar == "shiny") {
            shiny::incProgress(1 / nfiles)
        }
        gliderDebug(debug, "reading pld1 file: ", pld1files[i], "?\n")
        pld1Data <- utils::read.delim(pld1files[i], sep=";")
        pld1Data$yoNumber <- rep(yo[i], dim(pld1Data)[1])
        pld1[[i]] <- pld1Data
    }
    pld1Data <- do.call(rbind.data.frame, pld1)
    pld1Data$X <- NULL
    if (is.logical(progressBar) && progressBar)
        cat('\n')

    ## change missingValue to NA
    gliData[gliData == missingValue] <- NA
    pld1Data[pld1Data == missingValue] <- NA

    ##> print(names(gliData))
    ## [1] "Timestamp"     "NavState"      "SecurityLevel" "Heading"       "Pitch"         "Roll"          "Depth"
    ## [8] "Temperature"   "Pa"            "Lat"           "Lon"           "DesiredH"      "BallastCmd"    "BallastPos"
    ## [15] "LinCmd"        "LinPos"        "AngCmd"        "AngPos"        "Voltage"       "Altitude"      "X"
    ##
    ##> print(names(pld1Data))
    ## [1] "PLD_REALTIMECLOCK"    "NAV_RESOURCE"         "NAV_LONGITUDE"        "NAV_LATITUDE"         "NAV_DEPTH"
    ## [6] "FLBBCD_CHL_COUNT"     "FLBBCD_CHL_SCALED"    "FLBBCD_BB_700_COUNT"  "FLBBCD_BB_700_SCALED" "FLBBCD_CDOM_COUNT"
    ## [11] "FLBBCD_CDOM_SCALED"   "GPCTD_CONDUCTIVITY"   "GPCTD_TEMPERATURE"    "GPCTD_PRESSURE"       "GPCTD_DOF"
    ## [16] "X"

    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "realtime"
    res <- initializeFlagScheme(res, name="IOOS",
                                mapping=list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9))
    res@metadata$filename <- c(glifiles, pld1files)
    ##44 https://github.com/dankelley/oceanglider/issues/44
    ##44 res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list(glider=list(), payload1=list())
    for (name in names(gliData))
        res@metadata$dataNamesOriginal$glider[[name]] <- name
    for (name in names(pld1Data))
        res@metadata$dataNamesOriginal$payload1[[name]] <- name

    gliderDebug(debug, "about to rename items read from the 'gli' file\n")

    ## Rename items in glider data.
    ## FIXME: add more conversions here, and also to the corresponding
    ## spot in the .delayed() function. When both are added, adjust
    ## ../man-roxygen/seaexplorer_names.R accordingly.
    if ("Timestamp" %in% names(gliData)) {
        ## FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
        if (issue40)
            gliData$Timestamp <- as.POSIXct(gliData$Timestamp, format="%d/%m/%Y %H:%M:%OS", tz="UTC")
        else
            gliData$Timestamp <- as.POSIXct(gliData$Timestamp, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        names(gliData) <- gsub("Timestamp", "time", names(gliData))
        res@metadata$dataNamesOriginal$glider$time <- "Timestamp"
    }
    if ("NavState" %in% names(gliData)) {
        names(gliData) <- gsub("NavState", "navState", names(gliData))
        res@metadata$dataNamesOriginal$glider$navState <- "NavState"
    }
    if ("SecurityLevel" %in% names(gliData)) {
        names(gliData) <- gsub("SecurityLevel", "alarm", names(gliData))
        res@metadata$dataNamesOriginal$glider$alarm <- "SecurityLevel"
    }
    if ("Heading" %in% names(gliData)) {
        names(gliData) <- gsub("Heading", "heading", names(gliData))
        res@metadata$dataNamesOriginal$glider$heading <- "Heading"
    }
    if ("Pitch" %in% names(gliData)) {
        names(gliData) <- gsub("Pitch", "pitch", names(gliData))
        res@metadata$dataNamesOriginal$glider$pitch <- "Pitch"
    }
    if ("Roll" %in% names(gliData)) {
        names(gliData) <- gsub("Roll", "roll", names(gliData))
        res@metadata$dataNamesOriginal$glider$roll <- "Roll"
    }
    if ("Depth" %in% names(gliData)) {
        names(gliData) <- gsub("Depth", "pressureNav", names(gliData))
        res@metadata$dataNamesOriginal$glider$pressureNav <- "Depth"
    }
    if ("Temperature" %in% names(gliData)) {
        names(gliData) <- gsub("Temperature", "temperatureInternal", names(gliData))
        res@metadata$dataNamesOriginal$glider$temperatureInternal <- "Temperature"
    }
    if ("Pa" %in% names(gliData)) {
        names(gliData) <- gsub("Pa", "pressureInternal", names(gliData))
        res@metadata$dataNamesOriginal$glider$pressureInternal <- "Pa"
    }
    if ("Lat" %in% names(gliData)) {
        gliData$Lat <- degreeMinute(gliData$Lat)
        names(gliData) <- gsub("Lat", "latitude", names(gliData))
        res@metadata$dataNamesOriginal$glider$latitude <- "Lat"
    }
    if ("Lon" %in% names(gliData)) {
        gliData$Lon <- degreeMinute(gliData$Lon)
        names(gliData) <- gsub("Lon", "longitude", names(gliData))
        res@metadata$dataNamesOriginal$glider$longitude <- "Lon"
    }
    if ("DesiredH" %in% names(gliData)) {
        names(gliData) <- gsub("DesiredH", "headingDesired", names(gliData))
        res@metadata$dataNamesOriginal$glider$headingDesired <- "DesiredH"
    }
    if ("BallastCmd" %in% names(gliData)) {
        names(gliData) <- gsub("BallastCmd", "ballastCmd", names(gliData))
        res@metadata$dataNamesOriginal$glider$ballastCmd<- "BallastCmd"
    }
    if ("BallastPos" %in% names(gliData)) {
        names(gliData) <- gsub("BallastPos", "ballastPos", names(gliData))
        res@metadata$dataNamesOriginal$glider$ballastPos <- "BallastPos"
    }
    if ("LinCmd" %in% names(gliData)) {
        names(gliData) <- gsub("LinCmd", "linCmd", names(gliData))
        res@metadata$dataNamesOriginal$glider$linCmd <- "LinCmd"
    }
    if ("LinPos" %in% names(gliData)) {
        names(gliData) <- gsub("LinPos", "linPos", names(gliData))
        res@metadata$dataNamesOriginal$glider$linPos <- "LinPos"
    }
    if ("AngCmd" %in% names(gliData)) {
        names(gliData) <- gsub("AngCmd", "angCmd", names(gliData))
        res@metadata$dataNamesOriginal$glider$angCmd <- "AngCmd"
    }
    if ("AngPos" %in% names(gliData)) {
        names(gliData) <- gsub("AngPos", "angPos", names(gliData))
        res@metadata$dataNamesOriginal$glider$angPos <- "AngPos"
    }
    if ("Voltage" %in% names(gliData)) {
        names(gliData) <- gsub("Voltage", "voltage", names(gliData))
        res@metadata$dataNamesOriginal$glider$voltage <- "Voltage"
    }
    if ("Altitude" %in% names(gliData)) {
        names(gliData) <- gsub("Altitude", "altitude", names(gliData))
        res@metadata$dataNamesOriginal$glider$altitude <- "Altitude"
    }

    ## Rename items in payload1 data.
    gliderDebug(debug, "about to rename items read from the 'pld1' file\n")
    if ("PLD_REALTIMECLOCK" %in% names(pld1Data)) {
        ## FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
        if (issue40)
            pld1Data$PLD_REALTIMECLOCK <- as.POSIXct(pld1Data$PLD_REALTIMECLOCK, format="%d/%m/%Y %H:%M:%OS", tz="UTC")
        else
            pld1Data$PLD_REALTIMECLOCK <- as.POSIXct(pld1Data$PLD_REALTIMECLOCK, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        names(pld1Data) <- gsub("PLD_REALTIMECLOCK", "time", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$time <- "PLD_REALTIMECLOCK"
    }
    if ("NAV_RESOURCE" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("NAV_RESOURCE", "navState", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$navState <- "NAV_RESOURCE"
    }
    if ("NAV_LONGITUDE" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("NAV_LONGITUDE", "longitude", names(pld1Data))
        pld1Data$longitude <- degreeMinute(pld1Data$longitude)
        res@metadata$dataNamesOriginal$payload1$longitude <- "NAV_LONGITUDE"
    }
    if ("NAV_LATITUDE" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("NAV_LATITUDE", "latitude", names(pld1Data))
        pld1Data$latitude <- degreeMinute(pld1Data$latitude)
        res@metadata$dataNamesOriginal$payload1$latitude <- "NAV_LATITUDE"
    }
    if ("NAV_DEPTH" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("NAV_DEPTH", "pressureNav", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$pressureNav <- "NAV_DEPTH"
    }
    if ("FLBBCD_CHL_COUNT" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_CHL_COUNT", "chlorophyllCount", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$chlorophyllCount <- "FLBBCD_CHL_COUNT"
    }
    if ("FLBBCD_CHL_SCALED" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_CHL_SCALED", "chlorophyll", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$chlorophyll <- "FLBBCD_CHL_SCALED"
    }
    if ("FLBBCD_BB_700_COUNT" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_BB_700_COUNT", "backscatterCount", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$backscatterCount <- "FLBBCD_BB_700_COUNT"
    }
    if ("FLBBCD_BB_700_SCALED" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_BB_700_SCALED", "backscatter", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$backscatter <- "FLBBCD_BB_700_SCALED"
    }
    if ("FLBBCD_CDOM_COUNT" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_CDOM_COUNT", "cdomCount", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$cdomCount <- "FLBBCD_CDOM_COUNT"
    }
    if ("FLBBCD_CDOM_SCALED" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$cdom <- "FLBBCD_CDOM_SCALED"
    }
    if ("GPCTD_CONDUCTIVITY" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$conductivity <- "GPCTD_CONDUCTIVITY"
    }
    if ("GPCTD_TEMPERATURE" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("GPCTD_TEMPERATURE", "temperature", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$temperature <- "GPCTD_TEMPERATURE"
    }
    if ("GPCTD_PRESSURE" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("GPCTD_PRESSURE", "pressure", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$pressure <- "GPCTD_PRESSURE"
    }
    if (3 == sum(c("conductivity", "temperature", "pressure") %in% names(pld1Data))) {
        pld1Data$salinity <- swSCTp(pld1Data$conductivity/4.2914, pld1Data$temperature, pld1Data$pressure)
        res@metadata$dataNamesOriginal$payload1$salinity <- "-"
    }
    if ("GPCTD_DOF" %in% names(pld1Data)) {
        names(pld1Data) <- gsub("GPCTD_DOF", "oxygenFrequency", names(pld1Data))
        res@metadata$dataNamesOriginal$payload1$oxygenFrequency <- "GPCTD_DOF"
    }

    res@data <- list(glider=gliData, payload1=pld1Data)
    ## BOOKMARK START assure that this is echoed in read.glider.seaexplorer.realtime()
    ## insert units
    for (stream in names(res@data)) {
        ## FIXME: add more units here, if any of them are certain to be known
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
        ## set up flags to value 2, which means not-checked
        len <- length(res@data[[stream]][[1]]) # all have same length
        for (name in dataNames) {
            res@metadata$flags[[stream]][[name]] <- rep(2, len)
        }
    }
    ## BOOKMARK END
    gliderDebug(debug, "read.glider.seaexplorer.delayed(\"", directory, "\", ...) {\n", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.glider.seaexplorer.realtime(directory=\"", directory, "\",",
                                                   "yo=c(", paste(yo, collapse=","), "),",
                                                   "missingValue=", missingValue, ")", sep=""))
    gliderDebug(debug, "} # read.glider.seaexplorer.realtime()\n", unindent=1)
    res
}


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
#' \item NAs for all the sensors are interpolated to a common
#' time. For example, if a Wetlabs FLBBCD sensor sampled, but there is
#' no corresponding GP-CTD sample from the same time, the CTD
#' parameters will be interpolated from the ones before and
#' after. This has the disadvantage of interpolating values that were
#' not measured, but has the advantage of assigning pressures to
#' values measured by sensors not integrated into the CTD
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
#' library(oceanglider)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceanglider")
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
#' @export
#'
#' @author Clark Richards and Dan Kelley
#'
#' @md
read.glider.seaexplorer.delayed <- function(directory, yo, level=1, removeTimeSincePowerOn=0, progressBar=interactive(), debug)
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

    ## Note the removal of .gz at the end of filenames. This is to permit both compressed
    ## and uncompressed files.  (For example, the files stored within inst/extdata/ in the
    ## present package have been gzipped to save space, even though the original files were
    ## not gzipped.)
    yoNumber <- as.numeric(unlist(lapply(strsplit(gsub(".gz$","",pld1files), '.', fixed=TRUE), tail, 1)))
    o <- order(yoNumber)
    yoNumber <- yoNumber[o]
    pld1files <- pld1files[o]

    if (missing(yo))
        yo <- yoNumber

    y <- yoNumber %in% yo
    files <- pld1files[y]
    if (length(files) == 0) {
        stop("no .pld1. files in directory '", directory, "'", sep="")
    }

    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "delayed"
    res <- initializeFlagScheme(res, name="IOOS",
                                mapping=list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9))
    res@metadata$level <- level
    res@metadata$filename <- directory
    ##44 https://github.com/dankelley/oceanglider/issues/44
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
        if (is.logical(progressBar) && progressBar) {
            setTxtProgressBar(pb, i)
        } else if (is.character(progressBar) && progressBar == "shiny") {
            shiny::incProgress(1 / nfiles)
        }

        d <- utils::read.delim(files[i], sep=';', stringsAsFactors=FALSE, row.names=NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        ## Rename items in payload1 data.
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
            ## FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
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

    ## First remove all duplicated lon/lat
    df$longitude[which(duplicated(df$longitude))] <- NA
    df$latitude[which(duplicated(df$latitude))] <- NA

    ## Then remove all lon/lat that aren't from when navState is 116
    trans <- df$navState == 116
    df$longitude[!trans] <- NA
    df$latitude[!trans] <- NA

    ## Now interpolate lon/lat
    df$longitude <- approx(df$time, df$longitude, df$time)$y
    df$latitude <- approx(df$time, df$latitude, df$time)$y

    ## Trim out any empty rows (no data at all)
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

        ## Interpolate NAs for all sensors
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
                df[[var]] <- approx(df[['time']], df[[var]], df[['time']])$y
                i <- i + 1
            }
        }
        if (is.logical(progressBar) && progressBar) {
            cat('\n')
            flush.console()
        }
        ## Remove duplicated times
        if (is.character(progressBar) && progressBar == "shiny") {
            shiny::setProgress(0, "removing duplicated items")
        }
        df <- df[!duplicated(df), ]

        ## Calculate salinity
        if (is.character(progressBar) && progressBar == "shiny") {
            shiny::setProgress(0, "calculating salinity")
        }
        df$salinity <- with(df, swSCTp(conductivity, temperature, pressure, conductivityUnit = 'S/m'))
        df$salinity[df$salinity > 40] <- NA

        res@data <- list(payload1=df)
    }
    ## BOOKMARK START assure that this is echoed in read.glider.seaexplorer.realtime()
    ## insert units
    if (is.character(progressBar) && progressBar == "shiny") {
        shiny::setProgress(0, "creating units")
    }
    for (stream in names(res@data)) {
        ## FIXME: add more units here, if any of them are certain to be known
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
        ## set up flags to value 2, which means not-checked
        len <- length(res@data[[stream]][[1]]) # all have same length
        for (name in dataNames) {
            res@metadata$flags[[stream]][[name]] <- rep(2, len)
        }
    }
    ## BOOKMARK END
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.glider.seaexplorer.delayed(directory=\"", directory, "\", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
    gliderDebug(debug, "read.glider.seaexplorer.delayed(\"", directory, "\", ...) {\n", unindent=1)
    res
}
