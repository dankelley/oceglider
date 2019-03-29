#' @section Renaming of data in seaexplorer files:
#'
#' FIXME: are the original names the same in both "raw" and "sub" datasets?
#'
#' Data in the \code{gli} files are stored in the \code{glider} item within
#' the \code{data} slot of the returned object, renamed as follows.
#' (Note that there is also an empty column in the seaexplorer data files,
#' caused by a semicolon at the ends of the lines. This is read by R, but
#' then discarded and not stored in the glider object.)
#'
#' Data in the \code{gli} files are stored in the \code{glider} item within
#' the \code{data} slot of the returned object, renamed as follows. (If the
#' new name is listed as \code{.} that means that the old name is retained,
#' but bear in mind that a new name will likely be assigned at some later
#' point in the development of this package.)
#' Note that \code{read.glider.seaexplorer.raw} does not (presently) read
#' glider data.
#'
#' \tabular{lll}{
#' \strong{OriginalName}       \tab \strong{New name}      \tab \strong{Notes}\cr
#' \code{Timestamp}            \tab \code{time}            \tab Converted to POSIXt time.\cr
#' \code{NavState}             \tab \code{navState}        \tab - \cr
#' \code{SecurityLevel}        \tab \code{.}               \tab - \cr
#' \code{Heading}              \tab \code{heading}         \tab - \cr
#' \code{Pitch}                \tab \code{pitch}           \tab - \cr
#' \code{Roll}                 \tab \code{roll}            \tab - \cr
#' \code{Depth}                \tab \code{depth}           \tab - \cr
#' \code{Temperature}          \tab \code{.}               \tab - \cr
#' \code{Pa}                   \tab \code{.}               \tab - \cr
#' \code{Lat}                  \tab \code{latitude}        \tab Converted to decimal degrees.\cr
#' \code{Lon}                  \tab \code{longitude}       \tab Converted to decimal degrees.\cr
#' \code{DesiredH}             \tab \code{.}               \tab - \cr
#' \code{BallastCmd}           \tab \code{.}               \tab - \cr
#' \code{BallastPos}           \tab \code{.}               \tab - \cr
#' \code{LinCmd}               \tab \code{.}               \tab - \cr
#' \code{LinPos}               \tab \code{.}               \tab - \cr
#' \code{AngCmd}               \tab \code{.}               \tab - \cr
#' \code{AngPos}               \tab \code{.}               \tab - \cr
#' \code{Voltage}              \tab \code{voltage}         \tab - \cr
#' \code{Altitude}             \tab \code{.}               \tab - \cr
#'}
#'
#' Data in the \code{pld1} files are stored in the \code{payload1} item within
#' the \code{data} slot of the returned object, renamed as follows.
#'
#' \tabular{lll}{
#' \strong{OriginalName}       \tab \strong{New name}      \tab \strong{Notes}\cr
#' \code{PLD_REALTIMECLOCK}    \tab \code{time}            \tab Converted to POSIXt time.\cr
#' \code{NAV_RESOURCE}         \tab \code{navResource}     \tab - \cr
#' \code{NAV_LONGITUDE}        \tab \code{longitude}       \tab Converted to decimal degrees.\cr
#' \code{NAV_LATITUDE}         \tab \code{latitude}        \tab Converted to decimal degrees.\cr
#' \code{NAV_DEPTH}            \tab \code{pressureNav}     \tab - \cr
#' \code{FLBBCD_CHL_COUNT}     \tab \code{chlorophylCount} \tab - \cr
#' \code{FLBBCD_CHL_SCALED}    \tab \code{chlorophyl}      \tab - \cr
#' \code{FLBBCD_BB_700_COUNT}  \tab \code{backscatterCount}\tab - \cr
#' \code{FLBBCD_BB_700_SCALED} \tab \code{backscatter}     \tab - \cr
#' \code{FLBBCD_CDOM_COUNT}    \tab \code{cdomCount}       \tab - \cr
#' \code{FLBBCD_CDOM_SCALED}   \tab \code{cdom}            \tab - \cr
#' \code{GPCTD_CONDUCTIVITY}   \tab \code{conductivity}    \tab - \cr
#' \code{GPCTD_TEMPERATURE}    \tab \code{temperature}     \tab - \cr
#' \code{GPCTD_PRESSURE}       \tab \code{pressure}        \tab - \cr
#' \code{GPCTD_DOF}            \tab \code{oxygenFrequency} \tab - \cr
#'}
#'
#' Data in the \code{pld2} files (or others for additional payloads)
#' are ignored in this version of the package. Please contact the
#' authors, if you need to handle such files.

