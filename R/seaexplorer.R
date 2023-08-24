#' Possible navState values of a glider object
#'
#' This function provides names for the numerical `navState` codes
#' used by various gliders, with the aim of making it easier to
#' write self-explanatory code (see \dQuote{Examples}).
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
#' # Use codes to identify upcasts, at least roughly (note the stray points)
#' directory <- system.file("extdata/seaexplorer/raw", package="oceglider")
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
#' @author Dan Kelley
#'
#' @md
#'
#' @export
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
        stop("only type \"seaexplorer\" data handled so far")
    }
}

