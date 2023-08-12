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
#' can be quite slow with the default plot type (using points), so you
#' may find it helpful to use `plot(g, type="l")` to get a quick
#' plot. If you want to change the view, e.g. expanding it so coastline
#' are visible, start by drawing a coastline using the \CRANpkg{oce} package,
#' and then add dots with `points(g[["longitude"]], g[["latitude"]]`
#' or similar.  This method is more flexible than the present
#' `plot()` function.
#'
#' \item `which=1` or `which="p"`: time-series plot
#' of pressure, produced with [oce::oce.plot.ts()].
#'
#'\item `which=2` or `which="T"`: time-series plot
#' of temperature , produced with [oce::oce.plot.ts()].
#'
#'\item `which=3` or `which="S"`: time-series plot
#' of salinity, produced with [oce::oce.plot.ts()].
#'
#'\item `which=4` or `which="TS"`: temperature-salinity diagram,
#' with dots for data produced with [oce::plotTS()].
#'
#' \item `which=5` or `which="navState"`: ignored except
#' for seaexplorer data, this means to plot a time-series of the
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
#' @param col colour to be used for lines or characters. Note that if
#' `colorby` is provided, then it will be used for point plots, instead
#' of `col`.
#'
#' @param colorby character value, ignored for line plots, that
#' names a data variable to be indicated on the plot through the colourization
#' of individual plotted points (i.e. `type="p"` must be governing the plot for
#' `colorby` to have an effect).
#' For example, a form of a temperature section plot
#' can be created by plotting glider depth versus time, coloured by temperature.
#' For reference, a colour palette (using [oceColorsTurbo()] is displayed
#' to the right of the plot.  See Example 3.
#'
#' @param colorbylim optional value, used only if `colorby` is provided,
#' to set the limits of the colorizing limits.  It does this by being
#' provided as the `zlim` argument to [colormap()].
#'
#' @template debug
#'
#' @param ... ignored.
#'
#' @importFrom oce as.ctd colormap drawPalette oceColorsTurbo oce.plot.ts plotTS resizableLabel
#' @importFrom graphics abline par plot text
#'
#' @examples
#' library(oceglider)
#'
#' # Example 1: various plot types
#' dirRealtime <- system.file("extdata/seaexplorer/sub", package="oceglider")
#' gr <- read.glider.seaexplorer.realtime(dirRealtime, yo=100)
#' plot(gr, which="p")
#' plot(gr, which="S")
#' plot(gr, which="T")
#' plot(gr, which="TS")
#' plot(gr, which="map")
#' plot(gr, which="navState")
#'
#' # Example 2: colour-code p by chlorophyll
#' plot(gr, which="p", type="p", colorby="chlorophyll", pch=20, cex=2)
#'
#' # Example 3: navState and pressure history of some delayed-mode yos,
#' # from a deployment in which sampling was supposed to be
#' # suppressed during the descending phases of motion.
#' dirRaw <- system.file("extdata/seaexplorer/raw", package="oceglider")
#' gd <- read.glider.seaexplorer.delayed(dirRaw)
#' plot(gd, which="navState")
#'
#' # Example 4: colourizing by temperature, with fine-grained control.
#' cm <- colormap(gd[["temperature"]], col=oceColorsTurbo)
#' par(mar=c(2, 3.5, 2, 4))
#' drawPalette(colormap=cm)
#' plot(gd, which="p", type="p", col=cm$zcol, mar=c(2, 3.5, 2, 4), pch=20)
#'
#' @md
#'
#' @aliases plot.glider
#'
#' @export
setMethod(f="plot",
    signature=signature("glider"),
    definition=function(x, which, col=1, colorby=NULL, colorbylim, debug, ...) # plot,glider-method
    {
        debug <- if (!missing(debug)) debug else getOption("gliderDebug",0)
        gliderDebug(debug, "plot,glider-method {\n", sep="", unindent=1)
        dots <- list(...)
        dotsNames <- names(dots)
        gliderDebug(debug, oce::vectorShow(dots))
        cm <- NULL
        if (!is.null(colorby)) {
            z <- x[[colorby]]
            if (is.null(z)) {
                warning("In plot,glider-method() : there is no \"", colorby, "\" field, so ignoring 'colorby'", call.=FALSE)
                colorby <- NULL
            } else {
                cm <- if (missing(colorbylim)) {
                    oce::colormap(x[[colorby]], col=oce::oceColorsTurbo, debug=debug-1)
                } else {
                    oce::colormap(x[[colorby]], col=oce::oceColorsTurbo, zlim=colorbylim, debug=debug-1)
                }
                gliderDebug(debug, "set col to indicate values of \"", colorby, "\"\n", sep="")
            }
        }
        if (which == 0 || which == "map") {
            gliderDebug(debug, "map plot\n", sep="")
            longitude <- x[["longitude"]]
            latitude <- x[["latitude"]]
            args <- list(
                x=longitude,
                y=latitude,
                asp=1.0/cos(mean(range(latitude, na.rm=TRUE)*pi/180)),
                xlab=resizableLabel("longitude"),
                ylab=resizableLabel("latitude"))
            if (!"type" %in% dotsNames)
                dots$type <- "p"
            args <- c(args, dots)
            omar <- par("mar")
            if (!is.null(colorby)) {
                if (length(cm$zcol) != length(longitude)) {
                    stop("cannot colour-code location by ", colorby, "because there are", length(latitude), "locations, but ", length(cm$zcol), "values for", colorby)
                } else {
                    oce::drawPalette(colormap=cm)
                    args$col <- cm$zcol
                    par(mar=omar)
                }
            }
            mar <- omar
            mar[4] <- mar[4] + 2
            par(mar=mar)
            do.call("plot", args)
            par(mar=omar)
        } else if (which == 1 || which == "p") {
            gliderDebug(debug, "pressure time-series plot\n", sep="")
            t <- x[["time"]]
            p <- x[["pressure"]]
            args <- list(x=t, y=p, xlab="", ylab="Pressure [dbar]", col=col)
            if (!"ylim" %in% dotsNames)
                dots$ylim <- rev(range(p, na.rm=TRUE))
            omar <- par("mar")
            if (!is.null(colorby)) { # we know 'col' cannot be in dots, from earlier tests
                oce::drawPalette(colormap=cm)
                args$col <- cm$zcol
                args$marginsAsImage <- TRUE
            }
            args <- c(args, dots)
            par(mar=omar)
            do.call("oce.plot.ts", args)
            par(mar=omar)
        } else if (which == 2 || which == "T") {
            gliderDebug(debug, "temperature time-series plot\n", sep="")
            t <- x[["time"]]
            T <- x[["temperature"]]
            args <- list(x=t, y=T, xlab="", ylab=expression("Temperature ["*degree*"C]"))
            omar <- par("mar")
            if (!is.null(colorby)) { # we know 'col' cannot be in dots, from earlier tests
                oce::drawPalette(colormap=cm)
                args$col <- cm$zcol
                args$marginsAsImage <- TRUE
            }
            args <- c(args, dots)
            par(mar=omar)
            do.call("oce.plot.ts", args)
            par(mar=omar)
        } else if (which == 3 || which == "S") {
            gliderDebug(debug, "salinity time-series plot\n", sep="")
            t <- x[["time"]]
            S <- x[["salinity"]]
            args <- list(x=t, y=S, xlab="", ylab="Practical Salinity")
            omar <- par("mar")
            if (!is.null(colorby)) { # we know 'col' cannot be in dots, from earlier tests
                oce::drawPalette(colormap=cm)
                args$col <- cm$zcol
                args$marginsAsImage <- TRUE
            }
            args <- c(args, dots)
            par(mar=omar)
            do.call("oce.plot.ts", args)
            par(mar=omar)
        } else if (which == 4 || which == "TS") {
            gliderDebug(debug, "TS plot\n", sep="")
            gliderDebug(debug, "salinity time-series plot\n", sep="")
            S <- x[["salinity"]]
            T <- x[["temperature"]]
            p <- x[["pressure"]]
            longitude <- x[["longitude"]]
            latitude <- x[["latitude"]]
            ctd <- oce::as.ctd(S, T, p, longitude=longitude, latitude=latitude)
            args <- list(x=ctd)
            omar <- par("mar")
            if (!is.null(colorby)) { # we know 'col' cannot be in dots, from earlier tests
                oce::drawPalette(colormap=cm)
                args$col <- cm$zcol
                args$mar <- omar + c(0, 0, 0, 2)
                par(mar=omar)
            }
            args <- c(args, dots)
            do.call("plotTS", args)
            par(mar=omar)
        } else if (which == 5 || which == "navState") {
            gliderDebug(debug, "navState plot\n", sep="")
            ns <- navStateCodes(x)
            oce.plot.ts(x[["time"]], x[["navState"]], ylab="navState",
                mar=c(2, 3, 1, 9), ...)
            for (ii in seq_along(ns))
                abline(h=ns[[ii]], col="blue")
            oxpd <- par("xpd")
            par(xpd=NA)
            tmax <- par("usr")[2] + 0.00 * diff(par("usr")[1:2])
            for (ii in seq_along(ns)) {
                text(tmax, ns[[ii]],
                    sprintf(" %d: %s", ns[[ii]], names(ns[ii])),
                    col="blue", cex=0.75, xpd=TRUE, pos=4)
            }
            par(xpd=oxpd)
        } else {
            stop("which=", which, " is not permitted; see ?\"plot,glider-method\"")
        }
        gliderDebug(debug, "} # plot,glider-method\n", sep="", unindent=1)
    })
 
