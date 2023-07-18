#' Plot a glider Object
#'
#' This is a limited function that is intended for quick views of a dataset.
#' More serious analysis is best done by extracting data and using whatever
#' graphical methods best suit the task at hand.
#'
#' The form of the plot is set by the `which` argument, as follows.
#'
#'\itemize{
#'
#' \item `which=0` or `which="map"`: plot a map of sampling locations. This
#' can be quite slow with the default plot type, so try e.g.
#' `plot(g, type="l")` to speed things up for a quick look at the data.
#' In many cases, that quick look might be followed by the drawing of
#' a larger view, including a coastline, with functions provided for
#' `coastline` objects in the \CRANpkg{oce} package.
#'
#' \item `which=1` or `which="p"`: time-series plot
#' of pressure versus time. This
#' is done using [oce::oce.plot.ts()],
#' which also makes the other time-series plots listed below.
#'
#'\item `which=2` or `which="T"`: time-series temperature plot
#'
#'\item `which=3` or `which="S"`: time-series salinity plot
#'
#'\item `which=4` or `which="TS"`: temperature-salinity diagram,
#' with dots for data and labels indicating density anomaly; see
#' [oce::plotTS()] for details.
#'
#' \item `which=5` or `which="navState"`: time-series of the
#' navigation state, stored as the `navState` item within
#' the `payload1` element of the `data` slot. The meanings
#' of the `navState` values for `seaexplorer` data
#' are:
#'
#' \itemize{
#'
#' \item `105`: glider is not navigating yet
#'
#' \item `115`: glider is surfacing, with ballast and
#' centre of gravity being adjusted to put antenna out
#' of the water
#'
#' \item `116`: glider is at the surface,
#' acquiring a GPS signal, and communicating
#'
#' \item `110`: ballast and centre of mass are
#' adjusted to cause glider to inflect downward
#'
#' \item `100`: ballast is in diving position; adjustments
#' may be made to adjust pitch and heading
#'
#' \item `118`: target depth or altitude has been achieved,
#' so ballast and centre of mass are adjusted to inflect glider
#' upwards
#'
#' \item `117`: glider is ascending, with controls being
#' adjusted for desired pitch and heading
#'
#'}
#'
#' Lines and notes in the plot border indicate these states, both
#' numerically and with phrases, as inferred by
#' [navStateCodes()].
#'
#'}
#'
#' @param x A `glider` object, i.e. one inheriting from [glider-class].
#'
#' @param which either an integer or character value specifying which style is
#' to be used; see \dQuote{Details}.
#'
#' @template debug
#'
#' @param ... ignored.
#'
#' @importFrom oce oce.plot.ts plotTS resizableLabel
#' @importFrom graphics abline par plot text
#'
#' @examples
#' library(oceGlider)
#'
#' # Examples 1: a single yo of low-resolution real-time data
#' dirRealtime <- system.file("extdata/seaexplorer/sub", package="oceGlider")
#' g <- read.glider.seaexplorer.realtime(dirRealtime, yo=100)
#' plot(g, which="p")
#' plot(g, which="S")
#' plot(g, which="T")
#' plot(g, which="TS") # note odd connections between points
#' plot(g, which="map")
#' plot(g, which="navState")
#'
#' # Example 2: navState and pressure history of some delayed-mode yos,
#' # from a deployment in which sampling was supposed to be
#' # suppressed during the descending phases of motion.
#' dirRaw <- system.file("extdata/seaexplorer/raw", package="oceGlider")
#' g <- read.glider.seaexplorer.delayed(dirRaw)
#' plot(g, which="navState")
#'
#' # Note: colormap and drawPalette are oce functions.
#' cm <- colormap(g[["temperature"]])
#' # Note the setting of mar, here and in th plot.
#' par(mar=c(2, 3.5, 2, 4))
#' drawPalette(colormap=cm)
#' plot(g, which="p", type="p", cex=1/3, col=cm$zcol, mar=c(2, 3.5, 2, 4))
#'
#' @md
#'
#' @aliases plot.glider
#'
#' @export
setMethod(f="plot",
    signature=signature("glider"),
    definition=function(x, which, debug, ...)
    {
        dots <- list(...)
        debug <- if (!missing(debug)) debug else getOption("gliderDebug",0)
        oce::oceDebug(debug, "plot,glider-method {\n", sep="", unindent=1)
        if (which == 0 || which == "map") {
            oce::oceDebug(debug, "map plot\n", sep="")
            latitude <- x[["latitude"]]
            longitude <- x[["longitude"]]
            asp <- 1 / cos(mean(latitude*pi/180))
            if ("type" %in% names(dots)) {
                plot(longitude, latitude, asp=asp,
                    xlab=resizableLabel("longitude"),
                    ylab=resizableLabel("latitude"), ...)
            } else {
                plot(longitude, latitude, asp=asp,
                    xlab=resizableLabel("longitude"),
                    ylab=resizableLabel("latitude"), type="p", ...)
            }
        } else if (which == 1 || which == "p") {
            oce::oceDebug(debug, "pressure time-series plot\n", sep="")
            p <- x[["pressure"]]
            if ("ylim" %in% names(dots))
                oce.plot.ts(x[["time"]], p, ylab=resizableLabel("p"), debug=debug-1, ...)
            else
                oce.plot.ts(x[["time"]], p, ylab=resizableLabel("p"), ylim=rev(range(p, na.rm=TRUE)), debug=debug-1, ...)
        } else if (which == 2 || which == "T") {
            oce::oceDebug(debug, "temperature time-series plot\n", sep="")
            oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), debug=debug-1, ...)
        } else if (which == 3 || which == "S") {
            oce::oceDebug(debug, "salinity time-series plot\n", sep="")
            oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), debug=debug-1, ...)
        } else if (which == 4 || which == "TS") {
            oce::oceDebug(debug, "TS plot\n", sep="")
            plotTS(x, debug=debug-1, ...)
        } else if (which == 5 || which == "navState") {
            oce::oceDebug(debug, "navState plot\n", sep="")
            ns <- navStateCodes(x)
            oce.plot.ts(x[["time"]], x[["navState"]], ylab="navState",
                mar=c(2, 3, 1, 9), ...)
            for (ii in seq_along(ns)) {
                abline(h=ns[[ii]], col="blue")
            }
            # labels in margin, not rotated so we can read them.
            oxpd <- par("xpd")
            par(xpd=NA)
            tmax <- par("usr")[2] + 0.00 * diff(par("usr")[1:2])
            for (ii in seq_along(ns)) {
                text(tmax, ns[[ii]],
                    sprintf(" %d: %s", ns[[ii]],
                        names(ns[ii])),
                    col="blue", cex=0.75, xpd=TRUE, pos=4)
            }
            par(xpd=oxpd)
        } else {
            stop("which=", which, " is not permitted; see ?\"plot,glider-method\"")
        }
        oce::oceDebug(debug, "} # plot,glider-method\n", sep="", unindent=1)
    })
 
