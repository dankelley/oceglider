#' @param debug an integer specifying whether debugging information is
#' to be printed during processing. If this is not provided, then
#' the value of \code{\link{getOption}("gliderDebug",0)} is used.
#' The printing is done by a call to \code{\link{gliderDebug}}.
#' Setting \code{debug=0} turns off this form of debugging, while
#' higher values may yield more information, depending on the
#' function. If one \code{glider} function calls another, it
#' passes the value of \code{debug} but decreased by 1, which means
#' that the value of \code{debug} controls not just the breadth
#' of debugging, but also the depth.

