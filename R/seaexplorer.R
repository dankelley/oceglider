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
#' @param file File name.
#'
#' @param debug Integer indicating the debugging level; 0 for quiet
#' action and higher values for more indications of the processing
#' steps.
#'
#' @author Dan Kelley
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#' @importFrom utils read.delim
#' @export
read.glider.seaexplorer <- function(file, debug=0)
{
    utils::read.delim(file, sep=";")
}

