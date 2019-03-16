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


#' Read a Seaexplorer File
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
#' @author Dan Kelley
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
#' d <- read.glider.seaexplorer(files)
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
read.glider.seaexplorer <- function(files, debug, missingValue=9999)
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

