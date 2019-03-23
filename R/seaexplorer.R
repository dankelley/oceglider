#' Download and Cache a Seaexplorer Glider File [deprecated]
#'
#' This function assumes some knowledge of the data being sought,
#' but if some parameters are set to \code{"?"}, the server
#' will be interrogated to try to determine possible choices.
#'
#' @param url Character value indicating the base URL for the
#' server. If this is not given as in the default, it seems
#' fairly likely that this function will fail.
#' If \code{url} is \code{"?"}, the default is printed.
#'
#' @param stream Character value indicating the data stream. This
#' should probably be either \code{"realData"} or \code{"delayedData"}.
#' If \code{stream} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param glider Name Character value indicating the name of the
#' glider. This is probably in a form like \code{"SEA024"}.
#' If \code{glider} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param mission Character value indicating the name of the
#' mission. This is probably in a form like \code{"M32"}.
#' If \code{missionName} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param type Character value, either \code{"pld1"} or \code{"gli"}.
#'
#' @param yo Numerical value indicating the yo number,
#' or the character value \code{"?"}.  In the second case,
#' the data repository will be examined, and the return
#' value will be a vector of \code{yo} numbers that can
#' be retrieved by the server.
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @return Either a character vector of file names or (if \code{yo=="?"})
#' a numerical vector of possible \code{yo} values for the indicated
#' server.
#'
#' @author Dan Kelley
#'
#' @examples
#' \dontrun{
#' # Download and read a file (default server, mission, etc)
#' yo <- 200
#' url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
#' files <- download.glider(url, paste("\\.", yo, "\\.gz$", sep=""), debug=1)
#' yo2 <- read.glider.seaexplorer(files)
#' # Download (or use cache for) a set files
#' download.glider.seaexplorer(yo=download.glider.seaexplorer(yo="?"))
#' }
#'
#' @family functions for seaexplorer gliders
#' @family functions to download data
#' @importFrom RCurl getURL
#' @importFrom utils download.file
#' @export
download.glider.seaexplorer <- function(url="ftp://ftp.dfo-mpo.gc.ca/glider",
                                        stream="realData",
                                        glider="SEA024",
                                        mission="M32",
                                        type="pld1",
                                        yo="?",
                                        debug=0)
{
    ## ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25/
    gliderDebug(debug, 'download.glider.seaexplorer(url="', url, '"',
                ', stream="', stream, '"',
                ', glider="', glider, '"',
                ', mission="', mission, '"',
                ', yo=c(', paste(yo, collapse=","), ')',
                ', debug=', debug, ')\n', sep="")

    if ("?" == url) {
        guess <- "ftp://ftp.dfo-mpo.gc.ca/glider"
        cat("try using url=\"", guess, "\" (or not specifying url, because this is the default)\n", sep="")
        return(invisible(guess))
    }
    if ("?" == stream) {
        if ("?" == url)
            stop("must set url= before can use stream=\"?\"")
        if (substr(url, nchar(url), nchar(url)) != "\"")
            url <- paste(url, "/", sep="")
        streams <- strsplit(RCurl::getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE), "\n")[[1]]
        cat("possible stream values: \"", paste(streams, collapse="\", \""), "\"\n", sep="")
        return(invisible(streams))
    }
    if ("?" == glider) {
        if ("?" == url)
            stop("must set url= before can use glider=\"?\"")
        if ("?" == stream)
            stop("must set stream= before can use glider=\"?\"")
        directory <- paste(url, stream, sep="/")
        if ("/" != substr(directory, nchar(directory), nchar(directory)))
            directory <- paste(directory, "/", sep="")
        gliders <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
        gliders <- strsplit(gliders, "\n")[[1]]
        gliders <- gliders[grep("^[a-zA-Z].*$", gliders)]
        gliders <- gliders[grep(".*(.msn)$", gliders, invert=TRUE)]
        cat("possible glider values: \"", paste(gliders, collapse="\", \""), "\"\n", sep="")
        return(invisible(gliders))
    }
    if ("?" == mission) {
        if ("?" == url)
            stop("must set url= before can use mission=\"?\"")
        if ("?" == stream)
            stop("must set stream= before can use mission=\"?\"")
        if ("?" == glider)
            stop("must set glider= before can use mission=\"?\"")
        directory <- paste(url, stream, glider, sep="/")
        if ("/" != substr(directory, nchar(directory), nchar(directory)))
            directory <- paste(directory, "/", sep="")
        missions <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
        missions <- strsplit(missions, "\n")[[1]]
        missions <- missions[grep("^[a-zA-Z].*$", missions)]
        missions <- missions[grep(".*(msn)$", missions, invert=TRUE)]
        missions <- missions[grep(".*(cfg)$", missions, invert=TRUE)]
        missions <- missions[grep(".*(dat)$", missions, invert=TRUE)]
        missions <- missions[grep(".*(log)$", missions, invert=TRUE)]
        cat("possible missionName values: \"", paste(missions, collapse="\", \""), "\"\n", sep="")
        return(invisible(missions))
    }
    if ("?" == yo[1]) {
        if ("?" == url)
            stop("must set url= before can use yo=\"?\"")
        if ("?" == stream)
            stop("must set stream= before can use yo=\"?\"")
        if ("?" == glider)
            stop("must set glider= before can use yo=\"?\"")
        if ("?" == mission)
            stop("must set mission= before can use yo=\"?\"")
        directory <- paste(url, stream, glider, mission, sep="/")
        gliderDebug(debug, "directory='", directory, "'\n", sep="")
        if ("/" != substr(directory, nchar(directory), nchar(directory)))
            directory <- paste(directory, "/", sep="")
        gliderDebug(debug, "directory='", directory, "' (after ensuring trailing /)\n", sep="")
        yos <- RCurl::getURL(directory, ftp.use.epsv=FALSE, dirlistonly=TRUE)
        gliderDebug(debug, "got data\n")
        yos <- strsplit(yos, "\n")[[1]]
        gliderDebug(debug, "split data\n")
        ## Keep sea024.25.pld1.sub.465.gz and sea024.25.gli.sub.465.gz but remove a
        ## few other files, e.g. ending in csv and kml.
        yos <- yos[grep("^sea.*(pld)|(gli).*\\.sub.*\\.gz$", yos)]
        gliderDebug(debug, "isolate so pld and gli .gz files\n")
        yoNumbers <- sort(as.numeric(gsub(".*\\.([0-9]*)\\.gz", "\\1", yos)))
        ##. yos2<<-yos
        return(yoNumbers)
    }
    filenames <- NULL
    for (thisyo in yo) {
        gliderDebug(debug, "yo=", thisyo, "\n", sep="")
        filename <- paste(tolower(glider), ".", gsub("M", "", mission), "", type, ".sub.", thisyo, ".gz", sep="")
        gliderDebug(debug, "filename='", filename, "'\n", sep="")
        path <- paste(url, stream, glider, mission, sep="/")
        gliderDebug(debug, "path='", path, "'\n", sep="")
        source <- paste(path, filename, sep="/")
        gliderDebug(debug, "source='", source, "'\n", sep="")
        gliderDebug(debug, "plan: '", source, "' -> '", filename, "'\n", sep="")
        ##return(list.files(path=path))
        if (!file.exists(filename))
            utils::download.file(source, filename)
        filenames <- c(filenames, filename)
    }
    filenames
}


#' Read real-time SeaExplorer glider data
#'
#' Reads real-time CSV files produced by a SeaExplorer glider. The
#' real-time data are decimated before transmission, and thus do not
#' represent the full data collected by the glider sensors.
#'
#' While the data format is similar to the "raw" SeaExplorer file
#' format, there are some differences and
#' \code{read.glider.explorer.raw()} should be used for the latter.
#'
#' @param files Either a single integer, in which case it specifies
#' a yo number for a local file, or a character value of length 2
#' that provides the names of two local files, one (typically
#' with \code{gli} in its filename) representing
#' measurements made by sensors on the glider,
#' and the other (with \code{pld1} in its filename) representing
#' measurements made by sensors in the glider's payload. For the
#' the case of \code{files} being an integer, all local filenames are
#' found, and name-matching is done to try to find the \code{gli}
#' and \code{pld1} files.
#'
#' @template debug
#'
#' @param missingValue Numerical value for missing data; all such values
#' are set to \code{NA} in the data as interpreted. The default value
#' works for one particular mission that was examined, but it might
#' not apply to other missions. When in doubt as to the correct value,
#' use \code{summary()} on the returned object, and check to see if the
#' data maximum is a peculiar value, e.g. 99, 999 or, as the default, 9999.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @examples
#' library(oceanglider)
#' ## # Download and read a file (default server, mission, etc)
#' ## yo <- 200
#' ## url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
#' ## filenames <- download.glider(url, paste("\\.", yo, "\\.gz$", sep=""), debug=1)
#' files <- system.file("extdata",
#'                      c("sea024.32.gli.sub.200.gz",
#'                        "sea024.32.pld1.sub.200.gz"), package="oceanglider")
#' d <- read.glider.seaexplorer.realtime(files)
#' ctd <- as.ctd(d[['salinity']], d[['temperature']], d[['pressure']],
#'               lon=d[['longitude']], lat=d[['latitude']])
#' plot(ctd)
#' ## Isolate the upcast, inferred with oce::ctdTrim().
#' plot(ctdTrim(ctd, "upcast"))
#' ## Isolate the upcast, using d[["NAV_RESOURCE"]]==117;
#' ## note that the downcast has code 100.
#' plot(subset(ctd, d[["NAV_RESOURCE"]]==117))
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @export
read.glider.seaexplorer.realtime <- function(files, debug, missingValue=9999)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(files))
        stop("must provide `files'")
    nfiles <- length(files)
    if (1 == nfiles) {
        localFiles <- list.files()
        ## e.g. "sea024.32.gli.sub.118.gz"
        pattern <- paste("\\.sub\\.", files, "\\.gz$", sep="")
        w <- grep(pattern, localFiles)
        if (length(w) != 2) {
            stop("files=", files, " (interpreted as a yo number) matches ", length(w), " files, but it must match only 2", sep="")
        }
        files <- localFiles[w]
    } else if (2 == nfiles) {
        if (!is.character(files))
            stop("if files is of character type, it must be of length 2")
    } else {
        stop("files must be an integer (yo number) or a character value of length 2 (names of pld and gli files)")
    }
    wpld <- grep("\\.pld.*\\.sub\\..*gz$", files)
    if (!length(wpld))
        stop("files does not contain a pld filename")
    wgli <- grep("\\.gli.*\\.sub\\..*gz$", files)
    if (!length(wgli))
        stop("files does not contain a gli filename")
    pldFile <- files[wpld]
    gliFile <- files[wgli]
    gliData <- utils::read.delim(gliFile, sep=";")
    pldData <- utils::read.delim(pldFile, sep=";")
    pldData[pldData == missingValue] <- NA
    gliData[gliData == missingValue] <- NA

    ## names(gliData)
    ##  [1] "Timestamp"     "NavState"      "SecurityLevel"
    ##  [4] "Heading"       "Pitch"         "Roll"
    ##  [7] "Depth"         "Temperature"   "Pa"
    ## [10] "Lat"           "Lon"           "DesiredH"
    ## [13] "BallastCmd"    "BallastPos"    "LinCmd"
    ## [16] "LinPos"        "AngCmd"        "AngPos"
    ## [19] "Voltage"       "Altitude"      "X"

    ## names(pldData)
    ##  [1] "PLD_REALTIMECLOCK"    "NAV_RESOURCE"
    ##  [3] "NAV_LONGITUDE"        "NAV_LATITUDE"
    ##  [5] "NAV_DEPTH"            "FLBBCD_CHL_COUNT"
    ##  [7] "FLBBCD_CHL_SCALED"    "FLBBCD_BB_700_COUNT"
    ##  [9] "FLBBCD_BB_700_SCALED" "FLBBCD_CDOM_COUNT"
    ## [11] "FLBBCD_CDOM_SCALED"   "GPCTD_CONDUCTIVITY"
    ## [13] "GPCTD_TEMPERATURE"    "GPCTD_PRESSURE"
    ## [15] "GPCTD_DOF"            "X"

    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$filename <- files
    res@metadata$yo <- as.numeric(gsub(".*\\.([0-9]*)\\.gz", "\\1", files[1]))
    res@metadata$dataNamesOriginal <- list(glider=list(), payload=list())
    for (name in names(gliData))
        res@metadata$dataNamesOriginal$glider[[name]] <- name
    for (name in names(pldData))
        res@metadata$dataNamesOriginal$payload[[name]] <- name
    if ("NAV_LONGITUDE" %in% names(pldData)) {
        names(pldData) <- gsub("NAV_LONGITUDE", "longitude", names(pldData))
        pldData$longitude <- degreeMinute(pldData$longitude)
        res@metadata$dataNamesOriginal$payload$longitude <- "NAV_LONGITUDE"
    }
    if ("NAV_LATITUDE" %in% names(pldData)) {
        names(pldData) <- gsub("NAV_LATITUDE", "latitude", names(pldData))
        pldData$latitude <- degreeMinute(pldData$latitude)
        res@metadata$dataNamesOriginal$payload$latitude <- "NAV_LATITUDE"
    }
    if ("GPCTD_TEMPERATURE" %in% names(pldData)) {
        names(pldData) <- gsub("GPCTD_TEMPERATURE", "temperature", names(pldData))
        res@metadata$dataNamesOriginal$payload$temperature <- "GPCTD_TEMPERATURE"
    }
    if ("GPCTD_PRESSURE" %in% names(pldData)) {
        names(pldData) <- gsub("GPCTD_PRESSURE", "pressure", names(pldData))
        res@metadata$dataNamesOriginal$payload$pressure <- "GPCTD_PRESSURE"
    }
    if ("GPCTD_CONDUCTIVITY" %in% names(pldData)) {
        names(pldData) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(pldData))
        res@metadata$dataNamesOriginal$payload$conductivity <- "GPCTD_CONDUCTIVITY"
    }
    if ("PLD_REALTIMECLOCK" %in% names(pldData)) {
        pldData$time <- as.POSIXct(pldData$PLD_REALTIMECLOCK, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        res@metadata$dataNamesOriginal$payload$time <- "-"
    }
    if (3 == sum(c("conductivity", "temperature", "pressure") %in% names(pldData))) {
        pldData$salinity <- swSCTp(pldData$conductivity/4.2914, pldData$temperature, pldData$pressure)
        res@metadata$dataNamesOriginal$payload$salinity <- "-"
    }
    res@data <- list(glider=gliData, payload=pldData)
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.glider.seaexplorer(c(\"", files[1], "\", \"",
                                                   files[2], "\"), missingValue=", missingValue, ")", sep=""))
    res
}


#' Read raw SeaExplorer glider data
#'
#' Reads raw CSV files produced by a SeaExplorer glider. The raw data
#' are the full resolution data stored on the glider and downloaded
#' after recovery.
#'
#' While the data format is similar to the "real-time" SeaExplorer file
#' format, there are some differences and
#' \code{read.glider.explorer.realtime()} should be used for the latter.
#'
#' This function can output either "Level 0" or "Level 1" type
#' data. Level 0 is simply the raw data as written in the CSV files
#' with no processing done, other than to remove longitude and
#' latitude for samples where the glider wasn't actually communicating
#' with satellites and then interpolate between surface values.
#'
#' Level 1 processing preforms a number of steps to give an "analysis ready" dataset, including
#'
#' \enumerate{
#'
#' \item Interpolation of the surface longitude and latitude to give
#' an esimate of the subsurface positions. This is a crude estimate of
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
#' time. For example, if a Wetlabs FLBBCD senor sampled, but there is
#' no corresponding GP-CTD sample from the same time, the CTD
#' parameters will be interpolated from the ones before and
#' after. This has the disadvantage of interpolating values that were
#' not measured, but has the advantage of assigning pressures to
#' values measured by sensors not integrated into the CTD
#' (e.g. Wetlabs FLBBCD, Rinko O2). Following the interpolation, any
#' rows with duplicated times are removed.
#'
#' \item Calculate Practical salinity from conductiivity, temperature
#' and pressure using \code{swSCTp()}.
#' 
#' }
#' 
#' @param dir The directory in which the raw SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If not provided will read all yo numbers for which files
#'     are present in \code{dir}.
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
#' @author Clark Richards and Dan Kelley
#'
#' @examples
#' \dontrun{
#' library(oceanglider)
#' dir <- '/data/archive/glider/2019/sx/sea021m49/raw'
#' d <- read.glider.seaexplorer.raw(dir, yo=1:100)
#' plot(d, which=1, type='p')
#' }
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @export
read.glider.seaexplorer.raw <- function(dir, yo, level=1, debug, progressBar=TRUE)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    if (missing(dir))
        stop("must provide a directory with files")
    navfiles <- dir(dir, pattern='*gli*', full.names=TRUE)
    pldfiles <- dir(dir, pattern='*pld*', full.names=TRUE)
    
    yoNumber <- as.numeric(unlist(lapply(strsplit(pldfiles, '.', fixed=TRUE), tail, 1)))
    o <- order(yoNumber)
    yoNumber <- yoNumber[o]
    pldfiles <- pldfiles[o]
    
    if (missing(yo))
        yo <- yoNumber
    
    y <- yoNumber %in% yo
    files <- pldfiles[y]
    
    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$filename <- files
    res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list(glider=list(), payload=list())

    pld <- list()
    if (progressBar) {
        cat('* Reading files...\n')
        pb <- txtProgressBar(1, length(files), 1, style=3)
    }
    for (i in 1:length(files)) {
        if (progressBar) setTxtProgressBar(pb, i)
        d <- utils::read.delim(files[i], sep=';', stringsAsFactors=FALSE, row.names=NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        if ("NAV_RESOURCE" %in% names(d)) {
            names(d) <- gsub("NAV_RESOURCE", "navState", names(d))
            res@metadata$dataNamesOriginal$payload$navState <- "NAV_RESOURCE"
        }
        if ("NAV_DEPTH" %in% names(d)) {
            names(d) <- gsub("NAV_DEPTH", "pressureNav", names(d))
            res@metadata$dataNamesOriginal$payload$pressureNav <- "NAV_DEPTH"
        }
        if ("NAV_LONGITUDE" %in% names(d)) {
            names(d) <- gsub("NAV_LONGITUDE", "longitude", names(d))
            d$longitude <- degreeMinute(d$longitude)
            res@metadata$dataNamesOriginal$payload$longitude <- "NAV_LONGITUDE"
        }
        if ("NAV_LATITUDE" %in% names(d)) {
            names(d) <- gsub("NAV_LATITUDE", "latitude", names(d))
            d$latitude <- degreeMinute(d$latitude)
            res@metadata$dataNamesOriginal$payload$latitude <- "NAV_LATITUDE"
        }
        if ("GPCTD_TEMPERATURE" %in% names(d)) {
            names(d) <- gsub("GPCTD_TEMPERATURE", "temperature", names(d))
            res@metadata$dataNamesOriginal$payload$temperature <- "GPCTD_TEMPERATURE"
        }
        if ("GPCTD_PRESSURE" %in% names(d)) {
            names(d) <- gsub("GPCTD_PRESSURE", "pressure", names(d))
            res@metadata$dataNamesOriginal$payload$pressure <- "GPCTD_PRESSURE"
        }
        if ("GPCTD_CONDUCTIVITY" %in% names(d)) {
            names(d) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(d))
            res@metadata$dataNamesOriginal$payload$conductivity <- "GPCTD_CONDUCTIVITY"
        }
        if ("GPCTD_DOF" %in% names(d)) {
            names(d) <- gsub("GPCTD_DOF", "oxygenFrequency", names(d))
            res@metadata$dataNamesOriginal$payload$oxygenFrequency <- "GPCTD_DOF"
        }
        if ("FLBBCD_CHL_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CHL_COUNT", "chlorophylCount", names(d))
            res@metadata$dataNamesOriginal$payload$chlorophylCount <- "FLBBCD_CHL_COUNT"
        }
        if ("FLBBCD_CHL_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CHL_SCALED", "chlorophyl", names(d))
            res@metadata$dataNamesOriginal$payload$chlorophyl <- "FLBBCD_CHL_SCALED"
        }
        if ("FLBBCD_BB_700_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_BB_700_COUNT", "backscatterCount", names(d))
            res@metadata$dataNamesOriginal$payload$backscatterCount <- "FLBBCD_BB_700_COUNT"
        }
        if ("FLBBCD_BB_700_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_BB_700_SCALED", "backscatter", names(d))
            res@metadata$dataNamesOriginal$payload$backscatter <- "FLBBCD_BB_700_SCALED"
        }
        if ("FLBBCD_CDOM_COUNT" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CDOM_COUNT", "cdomCount", names(d))
            res@metadata$dataNamesOriginal$payload$cdomCount <- "FLBBCD_CDOM_COUNT"
        }
        if ("FLBBCD_CDOM_SCALED" %in% names(d)) {
            names(d) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(d))
            res@metadata$dataNamesOriginal$payload$cdom <- "FLBBCD_CDOM_SCALED"
        }
        if ("PLD_REALTIMECLOCK" %in% names(d)) {
            names(d) <- gsub("PLD_REALTIMECLOCK", "time", names(d))
            d$time <- as.POSIXct(d$time, format="%d/%m/%Y %H:%M:%S", tz="UTC")
            res@metadata$dataNamesOriginal$payload$time <- "-"
        }
        pld[[i]] <- d
    }
    df <- do.call(rbind.data.frame, pld)
    df[['X']] <- NULL # get rid of the weird last column
    if (progressBar) cat('\n')
    
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
        res@data <- list(payload=df)
        res@processingLog <- processingLogAppend(res@processingLog,
                                                 paste("read.glider.seaexplorer.raw(dir=", dir, ", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
        return(res)
    } else if (level == 1) {
        inflectUp <- as.integer(df$navState == 118)
        iuStart <- which(diff(inflectUp) == 1) + 1
        inflectDown <- as.integer(df$navState == 110)
        idStart <- which(diff(inflectDown) == 1) + 1
        ok <- rep(TRUE, dim(df)[1])
        for (i in 0:5) {
            ok[iuStart+i] <- FALSE
            ok[idStart+i] <- FALSE
        }
        df <- df[ok,]

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
        if (progressBar) cat('\n')

        ## Remove duplicated times
        df <- df[!duplicated(df), ]

        ## Calculate salinity
        df$salinity <- with(df, swSCTp(conductivity, temperature, pressure, conductivityUnit = 'S/m'))
        df$salinity[df$salinity > 40] <- NA

        res@data <- list(payload=df)
        res@processingLog <- processingLogAppend(res@processingLog,
                                                 paste("read.glider.seaexplorer.raw(dir=", dir, ", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep=""))
        return(res)
    }
}
