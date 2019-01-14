#' Download and Cache a Seaexplorer Glider File
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
#' glider. This is probably in a form like \code{"SEA019"}.
#' If \code{glider} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param mission Character value indicating the name of the
#' mission. This is probably in a form like \code{"M25"}.
#' If \code{missionName} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param yo Numerical value indicating the name of the yo.
#' If \code{yo} is \code{"?"}, the data repository will
#' be examined, and a message will be printed about possible values.
#'
#' @param type Character value indicating the type of file, either
#' \code{"pld1"} (the default) or \code{sub}.
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @author Dan Kelley
#'
#' @examples
#' \dontrun{
#' # Download and read a file (default server, mission, etc)
#' filename <- download.glider.seaexplorer(yo=2)
#' yo2 <- read.glider.seaexplorer(filename)
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
                                        mission="M25",
                                        yo="2",
                                        type="pld1", # or  "gli"
                                        debug=0)
{
    ## ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25/
    gliderDebug(debug, 'download.glider.seaexplorer(url="', url, '"',
                ', stream="', stream, '"',
                ', glider="', glider, '"',
                ', mission="', mission, '"',
                ', yo=c(', paste(yo, collapse=","), ')',
                ', type="', type, '"',
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
        ## EG sea024.25.pld1.sub.465.gz
        ## EG sea024.25.gli.sub.465.gz
        ##. yos0<<-yos
        yos <- yos[grep(type, yos)]
        gliderDebug(debug, "subsetted for type '", type, "'\n", sep="")
        ##. yos1<<-yos
        yos <- sort(as.numeric(gsub(".*\\.([0-9]*)\\.gz", "\\1", yos)))
        gliderDebug(debug, "ordered\n")
        ##. yos2<<-yos
        cat("possible yo values: ", paste(yos, collapse=" "), "\n", sep="")
        return(invisible(yos))
    }
    if (!(type %in% c("pld1", "gli")))
        stop("type must be \"pld1\" or \"gli\"")
    filenames <- NULL
    for (thisyo in yo) {
        gliderDebug(debug, "yo=", thisyo, "\n", sep="")
        filename <- paste(tolower(glider), ".", gsub("M", "", mission), ".", type, ".sub.", thisyo, ".gz", sep="")
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


#' Read a Seaexplorer Glider file
#'
#' These files do not use standard names for variables, but
#' the \code{nameMap} argument facilitates renaming for storage
#' in the returned object. (Renaming simplifies later analysis, e.g.
#' permitting direct use of algorithms in the \code{oce} package,
#' which assume that salinity is named \code{"salinity"}, etc.)
#' The original names of data items are retained in the metadata
#' of the returned object, so that the \code{[[} operator in the \code{oce}
#' package can retrieve the data using either the original name
#' (e.g. \code{x[["sci_water_temp"]]}) or the more standard
#' name (e.g. \code{x[["temperature"]]}). In addition to names
#' given in \code{nameMap}, several other name/value inferences
#' are made, e.g. a character timestamp (named differently in gli
#' and pld1 files) is used to compute \code{time}, and similar actions
#' are done to infer \code{longitude} and \code{latitude},
#' which are in a combined degree+minute format that is
#' decoded by \code{\link{degreeMinute}}.
#'
#' @param file A connection or a character string naming the file to load.
#'
#' @param missingValue Numerical value for missing data; all such values
#' are set to \code{NA} in the data as interpreted. The default value
#' works for one particular mission that was examined, but it might
#' not apply to other missions. When in doubt as to the correct value,
#' use \code{summary()} on the returned object, and check to see if the
#' data maximum is a peculiar value, e.g. 99, 999 or, as the default, 9999.
#'
#' @param nameMap List used to rename data columns. See \dQuote{Details}.
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oceanglider)
#' yo <- 200
#' url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
#' glif <- download.glider(url, paste("gli.sub.", yo, ".gz$", sep=""), debug=1)
#' pldf <- download.glider(url, paste("pld1.sub.", yo, ".gz$", sep=""), debug=1)
#' if (!is.null(pldf) && !is.null(glif)) {
#'     gli <- read.glider.seaexplorer(glif)
#'     pld <- read.glider.seaexplorer(pldf)
#'     ## Plot depth-time (as a way to learn about gli and pld differences)
#'     par(mfrow=c(2, 1))
#'     tlim <- range(c(gli[['time']], pld[['time']]))
#'     oce.plot.ts(gli[['time']], gli[['depth']], type='p', xlim=tlim, ylab="Depth [m]")
#'     points(pld[['time']], pld[['depth']], col=2, pch=3)
#'     legend("topleft", col=c(1,2), pch=c(1,3), legend=c("gli", "pld"))
#'     oce.plot.ts(gli[['time']], gli[['pitch']], type='p', xlim=tlim, ylab="Pitch [deg]")
#'     abline(h=0, col='gray')
#'     legend("topleft", col=1, pch=1, legend="gli")
#'     ## Compute glide ratio (as a way to learn about lon and lat)
#'     S <- function (x) diff(range(x))
#'     rgli <- S(geodDist(gli[['longitude']],gli[['latitude']],alongPath=TRUE))*1e3/S(gli[['depth']])
#'     message("glide ratio: ", rgli, " from gli file")
#'     rpld <- S(geodDist(pld[['longitude']],pld[['latitude']],alongPath=TRUE))*1e3/S(pld[['depth']])
#'     message("glide ratio: ", rpld, " from pld file")
#'     ## Experiments for isolating downcast (FIXME: is that preferred?)
#'     selectNavState <- 117 # 100 is going up
#'     selected <- runmed(gli[["NavState"]], 3) == selectNavState
#'     istart <- which(selected)[1]
#'     tstart <- gli[["time"]][istart]
#'     iend <- rev(which(selected))[1]
#'     tend <- gli[["time"]][iend]
#'     par(mfrow=c(3, 1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#'     plot(gli[['time']], gli[['depth']])
#'     abline(v=gli[['time']][c(istart, iend)], col=2)
#'     plot(gli[['time']], gli[['pitch']])
#'     abline(v=gli[['time']][c(istart, iend)], col=2)
#'     plot(gli[['time']], gli[['NavStat']])
#'     abline(v=gli[['time']][c(istart, iend)], col=2)
#'     ## Examine some details.
#'     ## Isolate science data using start/end inferred from gli[["NavState"]]
#'     ctd <- as.ctd(pld)
#'     focus <- tstart <= pld[["time"]] & pld[["time"]] <= tend
#'     col <- ifelse(focus, "black", "red")
#'     par(mfcol=c(3, 1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#'     plot(ctd[["time"]], ctd[["pressure"]], type="p")
#'     points(ctd[["time"]][focus], ctd[["pressure"]][focus], pch=20, col=2)
#'     plot(ctd[["time"]], ctd[["SA"]], type="p")
#'     points(ctd[["time"]][focus], ctd[["SA"]][focus], pch=20, col=2)
#'     plot(ctd[["time"]], ctd[["CT"]], type="p")
#'     points(ctd[["time"]][focus], ctd[["CT"]][focus], pch=20, col=2)
#'     par(mfcol=c(1, 3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#'     plotProfile(ctd, xtype="salinity", type="p")
#'     points(ctd[["SA"]][focus], ctd[["pressure"]][focus], pch=20, col=2)
#'     plotProfile(ctd, xtype="temperature", type="p")
#'     points(ctd[["CT"]][focus], ctd[["pressure"]][focus], pch=20, col=2)
#'     plotTS(ctd, type="p")
#'     points(ctd[["SA"]][focus], ctd[["CT"]][focus], pch=20, col=2)
#' }
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp
#' @export
read.glider.seaexplorer <- function(file,
                                    missingValue=9999,
                                    nameMap=list(conductivity="GPCTD_CONDUCTIVITY",
                                                 temperature="GPCTD_TEMPERATURE",
                                                 temperature="Temperature",
                                                 pressure="GPCTD_PRESSURE", ## values not right for pressure
                                                 depth="NAV_DEPTH",
                                                 depth="Depth",
                                                 heading="Heading",
                                                 pitch="Pitch",
                                                 roll="Roll"),
                                    debug=0)
{
    if (missing(file))
        stop("must provide `file'")
    filename <- ""
    if (is.character(file)) {
        filename <- normalizePath(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    data <- utils::read.delim(file, sep=";")
    ## Take care of missing-value codes.  This may vary from file to file.
    data[data == missingValue] <- NA
    names <- names(data)
    gliderDebug(debug, 'original data names: "', paste(names, collapse='", "'), '"\n', sep="")
    nameMapNames <- names(nameMap)
    rval <- methods::new("glider")
    rval@metadata$type <- "seaexplorer" # FIXME separate gli and pld1?
    rval@metadata$dataNamesOriginal <- list() # FIXME: work on this
    for (iname in seq_along(names)) {
        if (names[iname] %in% nameMap) {
            newName <- nameMapNames[which(names[iname] == nameMap)]
            rval@metadata$dataNamesOriginal[[newName]] <- names[iname]
            names[iname] <-  newName
        } else {
            rval@metadata$dataNamesOriginal[[names[iname]]] <- names[iname]
        }
    }
    gliderDebug(debug, 'new data names: "', paste(names, collapse='", "'), '"\n', sep="")
    names(data) <- names
    ## Time is named differently in the two (pld1 and gli) files
    if ("Timestamp" %in% names) {      # in gli files
        data$time <- as.POSIXct(data$Timestamp, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        rval@metadata$dataNamesOriginal$time <- "-"
    }
    if ("PLD_REALTIMECLOCK" %in% names) { # in pld1 files
        data$time <- as.POSIXct(data$PLD_REALTIMECLOCK, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        rval@metadata$dataNamesOriginal$time <- "-"
    }

    ## Longitude and latitude are named differently in pld1 and gli files, and each is in centi-degrees
    if ("Lon" %in% names) {            # in gli files
        data$longitude <- degreeMinute(data$Lon)
        rval@metadata$dataNamesOriginal$longitude <- "-"
    }
    if ("Lat" %in% names) {            # in gli files
        data$latitude <- degreeMinute(data$Lat)
        rval@metadata$dataNamesOriginal$latitude <- "-"
    }
    if ("NAV_LONGITUDE" %in% names) {  # in pld1 files
        data$longitude <- degreeMinute(data$NAV_LONGITUDE)
        rval@metadata$dataNamesOriginal$longitude <- "-"
    }
    if ("NAV_LATITUDE" %in% names) {   # in pld1 files
        data$latitude <- degreeMinute(data$NAV_LATITUDE)
        rval@metadata$dataNamesOriginal$latitude <- "-"
    }
    if ("NAV_DEPTH" %in% names) {      # in pld1 files
        data$pressure <- data$NAV_DEPTH
        rval@metadata$dataNamesOriginal$pressure <- "-"
    }
    names <- names(data)
    if ("conductivity" %in% names && "temperature" %in% names && "pressure" %in% names) {
        data$salinity <- with(data,
                              oce::swSCTp(conductivity, temperature, pressure,
                                          conductivityUnit="S/m", eos="unesco"))
        rval@metadata$dataNamesOriginal$salinity <- "-"
    }
    rval@data <- data
    rval@metadata$filename <- filename
    rval
}

