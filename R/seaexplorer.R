#### #' Download and Cache a Seaexplorer Glider File [deprecated]
#### #'
#### #' This function assumes some knowledge of the data being sought,
#### #' but if some parameters are set to \code{"?"}, the server
#### #' will be interrogated to try to determine possible choices.
#### #'
#### #' @param url Character value indicating the base URL for the
#### #' server. If this is not given as in the default, it seems
#### #' fairly likely that this function will fail.
#### #' If \code{url} is \code{"?"}, the default is printed.
#### #'
#### #' @param stream Character value indicating the data stream. This
#### #' should probably be either \code{"realData"} or \code{"delayedData"}.
#### #' If \code{stream} is \code{"?"}, the data repository will
#### #' be examined, and a message will be printed about possible values.
#### #'
#### #' @param glider Name Character value indicating the name of the
#### #' glider. This is probably in a form like \code{"SEA021"}.
#### #' If \code{glider} is \code{"?"}, the data repository will
#### #' be examined, and a message will be printed about possible values.
#### #'
#### #' @param mission Character value indicating the name of the
#### #' mission. This is probably in a form like \code{"M32"}.
#### #' If \code{missionName} is \code{"?"}, the data repository will
#### #' be examined, and a message will be printed about possible values.
#### #'
#### #' @param type Character value, either \code{"pld1"} or \code{"gli"}.
#### #'
#### #' @param yo Numerical value indicating the yo number,
#### #' or the character value \code{"?"}.  In the second case,
#### #' the data repository will be examined, and the return
#### #' value will be a vector of \code{yo} numbers that can
#### #' be retrieved by the server.
#### #'
#### #' @param debug Integer indicating the debugging level; 0 for quiet
#### #' action and higher values for more indications of the processing
#### #' steps.
#### #'
#### #' @return Either a character vector of file names or (if \code{yo=="?"})
#### #' a numerical vector of possible \code{yo} values for the indicated
#### #' server.
#### #'
#### #' @author Dan Kelley
#### #'
#### #' @examples
#### #' \dontrun{
#### #' # Download and read a file (default server, mission, etc)
#### #' yo <- 200
#### #' url <- "ftp://SERVER/PATH"
#### #' files <- download.glider(url, paste("\\.", yo, "\\.gz$", sep=""), debug=1)
#### #' yo2 <- read.glider.seaexplorer.realtime(files)
#### #' # Download (or use cache for) a set files
#### #' download.glider.seaexplorer(yo=download.glider.seaexplorer(yo="?"))
#### #' }
#### #'
#### #' @family functions for seaexplorer gliders
#### #' @family functions to download data
#### #' @importFrom RCurl getURL
#### #' @importFrom utils download.file
#### #' @export
#### download.glider.seaexplorer <- function(url="ftp://ftp.dfo-mpo.gc.ca/glider",
####                                         stream="realData",
####                                         glider="SEA024",
####                                         mission="M32",
####                                         type="pld1",
####                                         yo="?",
####                                         debug=0)
#### {
####     ## ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25/
####     gliderDebug(debug, 'download.glider.seaexplorer(url="', url, '"',
####                 ', stream="', stream, '"',
####                 ', glider="', glider, '"',
####                 ', mission="', mission, '"',
####                 ', yo=c(', paste(yo, collapse=","), ')',
####                 ', debug=', debug, ')\n', sep="")
#### 
####     if ("?" == url) {
####         guess <- "ftp://ftp.dfo-mpo.gc.ca/glider"
####         cat("try using url=\"", guess, "\" (or not specifying url, because this is the default)\n", sep="")
####         return(invisible(guess))
####     }
####     if ("?" == stream) {
####         if ("?" == url)
####             stop("must set url= before can use stream=\"?\"")
####         if (substr(url, nchar(url), nchar(url)) != "\"")
####             url <- paste(url, "/", sep="")
####         streams <- strsplit(RCurl::getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE), "\n")[[1]]
####         cat("possible stream values: \"", paste(streams, collapse="\", \""), "\"\n", sep="")
####         return(invisible(streams))
####     }
####     if ("?" == glider) {
####         if ("?" == url)
####             stop("must set url= before can use glider=\"?\"")
####         if ("?" == stream)
####             stop("must set stream= before can use glider=\"?\"")
####         directory <- paste(url, stream, sep="/")
####         if ("/" != substr(directory, nchar(directory), nchar(directory)))
####             directory <- paste(directory, "/", sep="")
####         gliders <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
####         gliders <- strsplit(gliders, "\n")[[1]]
####         gliders <- gliders[grep("^[a-zA-Z].*$", gliders)]
####         gliders <- gliders[grep(".*(.msn)$", gliders, invert=TRUE)]
####         cat("possible glider values: \"", paste(gliders, collapse="\", \""), "\"\n", sep="")
####         return(invisible(gliders))
####     }
####     if ("?" == mission) {
####         if ("?" == url)
####             stop("must set url= before can use mission=\"?\"")
####         if ("?" == stream)
####             stop("must set stream= before can use mission=\"?\"")
####         if ("?" == glider)
####             stop("must set glider= before can use mission=\"?\"")
####         directory <- paste(url, stream, glider, sep="/")
####         if ("/" != substr(directory, nchar(directory), nchar(directory)))
####             directory <- paste(directory, "/", sep="")
####         missions <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
####         missions <- strsplit(missions, "\n")[[1]]
####         missions <- missions[grep("^[a-zA-Z].*$", missions)]
####         missions <- missions[grep(".*(msn)$", missions, invert=TRUE)]
####         missions <- missions[grep(".*(cfg)$", missions, invert=TRUE)]
####         missions <- missions[grep(".*(dat)$", missions, invert=TRUE)]
####         missions <- missions[grep(".*(log)$", missions, invert=TRUE)]
####         cat("possible missionName values: \"", paste(missions, collapse="\", \""), "\"\n", sep="")
####         return(invisible(missions))
####     }
####     if ("?" == yo[1]) {
####         if ("?" == url)
####             stop("must set url= before can use yo=\"?\"")
####         if ("?" == stream)
####             stop("must set stream= before can use yo=\"?\"")
####         if ("?" == glider)
####             stop("must set glider= before can use yo=\"?\"")
####         if ("?" == mission)
####             stop("must set mission= before can use yo=\"?\"")
####         directory <- paste(url, stream, glider, mission, sep="/")
####         gliderDebug(debug, "directory='", directory, "'\n", sep="")
####         if ("/" != substr(directory, nchar(directory), nchar(directory)))
####             directory <- paste(directory, "/", sep="")
####         gliderDebug(debug, "directory='", directory, "' (after ensuring trailing /)\n", sep="")
####         yos <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
####         gliderDebug(debug, "got data\n")
####         yos <- strsplit(yos, "\n")[[1]]
####         gliderDebug(debug, "split data\n")
####         ## Keep sea024.25.pld1.sub.465.gz and sea024.25.gli.sub.465.gz but remove a
####         ## few other files, e.g. ending in csv and kml.
####         yos <- yos[grep("^sea.*(pld)|(gli).*\\.sub.*\\.gz$", yos)]
####         gliderDebug(debug, "isolate so pld and gli .gz files\n")
####         yoNumbers <- sort(as.numeric(gsub(".*\\.([0-9]*)\\.gz", "\\1", yos)))
####         ##. yos2<<-yos
####         return(yoNumbers)
####     }
####     filenames <- NULL
####     for (thisyo in yo) {
####         gliderDebug(debug, "yo=", thisyo, "\n", sep="")
####         filename <- paste(tolower(glider), ".", gsub("M", "", mission), "", type, ".sub.", thisyo, ".gz", sep="")
####         gliderDebug(debug, "filename='", filename, "'\n", sep="")
####         path <- paste(url, stream, glider, mission, sep="/")
####         gliderDebug(debug, "path='", path, "'\n", sep="")
####         source <- paste(path, filename, sep="/")
####         gliderDebug(debug, "source='", source, "'\n", sep="")
####         gliderDebug(debug, "plan: '", source, "' -> '", filename, "'\n", sep="")
####         ##return(list.files(path=path))
####         if (!file.exists(filename))
####             utils::download.file(source, filename)
####         filenames <- c(filenames, filename)
####     }
####     filenames
#### }


#' Read real-time SeaExplorer glider data
#'
#' Reads real-time CSV files produced by a SeaExplorer glider, as
#' detected by the presence of \code{".sub."} in their names.
#' Such real-time data are decimated before transmission, and thus do not
#' represent the full data collected by the glider sensors.
#' (Use \code{\link{read.glider.seaexplorer.delayed}} instead of
#' this, to read delayed-mode data, as downloaded from the glider
#' after recovery.)
#'
#' @param directory The directory in which the realtime SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If this is not provided, \code{read.glider.seaexplorer.delayed}
#'     will read all yo numbers for which files are present in \code{dir}.
#'
#' @param level Ignored by \code{read.glider.seaexplorer.realtime} and
#'     only included for similarity with
#'     \code{\link{read.glider.seaexplorer.delayed}}.
#'
#' @param progressBar A logical indicating whether to show progress
#'     bars while reading the data. Can be useful when reading full
#'     datasets.
#'
#' @param missingValue A value that indicates missing data; all
#'     values that match this are set to \code{NA}.
#'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @author Dan Kelley and Clark Richards
#'
#' @examples
#' library(oceanglider)
#' directory <- system.file("extdata/seaexplorer/sub", package="oceanglider")
#' g <- read.glider.seaexplorer.realtime(directory, yo=1)
#' ctd <- as.ctd(g[['salinity']], g[['temperature']], g[['pressure']],
#'               longitude=g[['longitude']], latitude=g[['latitude']])
#' plot(ctd)
#' ## Isolate the upcast, inferred with oce::ctdTrim().
#' plot(ctdTrim(ctd, "upcast"))
#' ## Isolate the upcast, using g[["NAV_RESOURCE"]]==117;
#' ## note that the downcast has code 100.
#' plot(subset(ctd, g[["NAV_RESOURCE"]]==117))
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @export
read.glider.seaexplorer.realtime <- function(directory, yo, level=1, progressBar=TRUE, missingValue=9999, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    gliderDebug(debug, "read.glider.seaexplorer.realtime() {\n", unindent=1)
    if (missing(directory))
        stop("must provide 'directory', in which glider files reside")
    yoGiven <- !missing(yo)
    glifiles <- dir(directory, pattern='*gli*', full.names=TRUE)
    pld1files <- dir(directory, pattern='*.pld1.*', full.names=TRUE)
    if (length(glifiles) != length(pld1files))
        stop("There is an unequal number of *gli* files (", length(glifiles),
             ") and *pld1* files (", length(pld1files), "), but they ought to be paired")

    if (debug > 1) {
        cat("gli files:\n")
        print(glifiles)
        cat("\n")
        cat("pld1 files:\n")
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
        gliderDebug(debug, "yo=", paste(yo, collapase=" "), "\n")
    }
    nfiles <- length(yo)
    if (progressBar) {
        cat('* Reading', nfiles, ifelse(nfiles==1, 'file pair\n', 'file pairs...\n'))
        pb <- txtProgressBar(0, nfiles, 0, style=3) # start at 0 to allow for a single yo
    }
    gli <- list()
    pld1 <- list()
    for (i in seq_len(nfiles)) {
        if (progressBar) setTxtProgressBar(pb, i)
        gliderDebug(debug, "reading", glifiles[i], "\n")
        gliData <- utils::read.delim(glifiles[i], sep=";")
        ## remove junk file from trailing semicolon in file
        if ("X" %in% names(gliData) && all(is.na(gliData$X)))
            gliData$X <- NULL
        gliderDebug(debug, "reading", pld1files[i], "\n")
        pld1Data <- utils::read.delim(pld1files[i], sep=";")
        ## remove junk file from trailing semicolon in file
        if ("X" %in% names(pld1Data) && all(is.na(pld1Data$X)))
            pld1Data$X <- NULL
        gliData$yoNumberNav <- rep(yo[i], dim(gliData)[1])
        pld1Data$yoNumber <- rep(yo[i], dim(pld1Data)[1])
        gli[[i]] <- gliData
        pld1[[i]] <- pld1Data
    }
    gliData <- do.call(rbind.data.frame, gli)
    gliData[["X"]] <- NULL
    pld1Data <- do.call(rbind.data.frame, pld1)
    pld1Data[["X"]] <- NULL
    if (progressBar) {
        cat('\n')
        flush.console()
    }
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
    res@metadata$filename <- c(glifiles, pld1files)
    res@metadata$yo <- yo
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
#' as detected by the presence of \code{".raw."} in their names.
#' Such delayed-mode data are the full resolution data stored on
#' the glider and downloaded after recovery.
#' (Use \code{\link{read.glider.seaexplorer.realtime}} instead
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
#' is in \code{navState=118} (inflecting up) or \code{navState=110}
#' (inflecting down). The reason for this is that when the glider is
#' set to sample on alternating profiles, when the CTD is powered up
#' the first sample output to the payload computer is the \emph{last}
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
#' and pressure using \code{swSCTp()}.
#'
#' }
#'
#' @param directory The directory in which the delayed-mode SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If this is not provided, \code{read.glider.seaexplorer.delayed}
#'     will read all yo numbers for which files are present in \code{dir}.
#'
#' @param level A numeric value specifying the processing level, 0 or
#'     1. See Details.
#'
#' @param progressBar A logical indicating whether to show progress
#'     bars while reading the data. Can be useful when reading full
#'     datasets.
#'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @author Clark Richards and Dan Kelley
#'
#' @examples
#' \dontrun{
#' library(oceanglider)
#' directory <- '/data/archive/glider/2019/sx/sea021m49/raw'
#' d <- read.glider.seaexplorer.delayed(directory, yo=1:100)
#' plot(d, which=1)
#' }
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @importFrom stats approx
#' @importFrom utils read.delim flush.console head setTxtProgressBar tail txtProgressBar
#' @export
read.glider.seaexplorer.delayed <- function(directory, yo, level=1, progressBar=TRUE, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(directory))
        stop("must provide 'directory', in which glider files reside")
    if (level != 0 & level != 1)
        stop("Level must be either 0 or 1")
    navfiles <- dir(directory, pattern='*gli*', full.names=TRUE) # FIXME: not used
    pld1files <- dir(directory, pattern='*.pld1.*', full.names=TRUE)
    pld2files <- dir(directory, pattern='*.pld2.*', full.names=TRUE)
    if (length(pld2files))
        warning("pld2 files are ignored by this function; contact developers if you need to read them")

    yoNumber <- as.numeric(unlist(lapply(strsplit(pld1files, '.', fixed=TRUE), tail, 1)))
    o <- order(yoNumber)
    yoNumber <- yoNumber[o]
    pld1files <- pld1files[o]

    if (missing(yo))
        yo <- yoNumber

    y <- yoNumber %in% yo
    files <- pld1files[y]

    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "delayed"
    res@metadata$level <- level
    res@metadata$filename <- files
    res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list(glider=list(), payload1=list())

    pld1 <- list()
    if (progressBar) {
        cat('* Reading', length(files), 'files...\n')
        pb <- txtProgressBar(0, length(files), 0, style=3) # start at 0 to allow for a single yo
    }
    for (i in 1:length(files)) {
        if (progressBar) setTxtProgressBar(pb, i)
        d <- utils::read.delim(files[i], sep=';', stringsAsFactors=FALSE, row.names=NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        ## Rename items in payload1 data.
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
        if ("FLBBCD_CDOM_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(d))
            res@metadata$dataNamesOriginal$payload1$cdom <- "FLBBCD_CDOM_SCALED"
        }
        if ("PLD_REALTIMECLOCK" %in% names(d)) {
            names(d) <- gsub("PLD_REALTIMECLOCK", "time", names(d))
            d$time <- as.POSIXct(d$time, format="%d/%m/%Y %H:%M:%S", tz="UTC")
            res@metadata$dataNamesOriginal$payload1$time <- "-"
        }
        pld1[[i]] <- d
    }
    df <- do.call(rbind.data.frame, pld1)
    df[['X']] <- NULL # get rid of the weird last column
    if (progressBar) {
        cat('\n')
        flush.console()
    }

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
        inflectUp <- as.integer(df$navState == 118)
        iuStart <- which(diff(inflectUp) == 1) + 1
        inflectDown <- as.integer(df$navState == 110)
        idStart <- which(diff(inflectDown) == 1) + 1
        if (length(iuStart) > 0 & length(idStart) > 0) {
            ok <- rep(TRUE, dim(df)[1])
            for (i in 0:5) {
                ok[iuStart+i] <- FALSE
                ok[idStart+i] <- FALSE
            }
            df <- df[ok,]
        }

        ## Interpolate NAs for all sensors
        n <- length(names(df)) - length(c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))
        if (progressBar) {
            cat('* Interpolating NAs...\n')
            pb <- txtProgressBar(1, n, 1, style=3)
        }
        i <- 1
        for (var in names(df)) {
            if (!(var %in% c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))) {
                if (progressBar) setTxtProgressBar(pb, i)
                df[[var]] <- approx(df[['time']], df[[var]], df[['time']])$y
                i <- i + 1
            }
        }
        if (progressBar) {
            cat('\n')
            flush.console()
        }

        ## Remove duplicated times
        df <- df[!duplicated(df), ]

        ## Calculate salinity
        df$salinity <- with(df, swSCTp(conductivity, temperature, pressure, conductivityUnit = 'S/m'))
        df$salinity[df$salinity > 40] <- NA

        res@data <- list(payload1=df)
        res@processingLog <- processingLogAppend(res@processingLog,
                                                 paste("read.glider.seaexplorer.delayed(directory=\"", directory, "\", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
        return(res)
    }
}
