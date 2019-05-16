## vim:textwidth=300

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
#'
#' \tabular{lll}{
#' \strong{OriginalName}       \tab \strong{Name}              \tab \strong{Notes}\cr
#' \code{Timestamp}            \tab \code{time}                \tab Converted to POSIXt time.\cr
#' \code{NavState}             \tab \code{navState}            \tab - \cr
#' \code{SecurityLevel}        \tab \code{alarm}               \tab - \cr
#' \code{Heading}              \tab \code{heading}             \tab - \cr
#' \code{Pitch}                \tab \code{pitch}               \tab - \cr
#' \code{Roll}                 \tab \code{roll}                \tab - \cr
#' \code{Depth}                \tab \code{pressureNav}         \tab - \cr
#' \code{Temperature}          \tab \code{temperatureInternal} \tab - \cr
#' \code{Pa}                   \tab \code{pressureInternal}    \tab - \cr
#' \code{Lat}                  \tab \code{latitude}            \tab Converted to decimal degrees.\cr
#' \code{Lon}                  \tab \code{longitude}           \tab Converted to decimal degrees.\cr
#' \code{DesiredH}             \tab \code{headingDesired}      \tab - \cr
#' \code{BallastCmd}           \tab \code{ballastCmd}          \tab - \cr
#' \code{BallastPos}           \tab \code{ballastPos}          \tab - \cr
#' \code{LinCmd}               \tab \code{linCmd}              \tab - \cr
#' \code{LinPos}               \tab \code{linPos}              \tab - \cr
#' \code{AngCmd}               \tab \code{angCmd}              \tab - \cr
#' \code{AngPos}               \tab \code{angPos}              \tab - \cr
#' \code{Voltage}              \tab \code{voltage}             \tab - \cr
#' \code{Altitude}             \tab \code{altitude}            \tab - \cr
#'}
#'
#' Data in the \code{pld1} files are stored in the \code{payload1} item within
#' the \code{data} slot of the returned object, renamed as follows. Where possible,
#' the corresponding IOOS NetCDF file variable names are also listed [see
#' Integrated Ocean Observing System (U.S.). “NGDAC NetCDF File Format Version
#' 2,” March 27, 2019.
#' https://github.com/ioos/ioosngdac/wiki/NGDAC-NetCDF-File-Format-Version-2].
#'
#' \tabular{llll}{
#' \strong{OriginalName}       \tab \strong{Name}              \tab \strong{IOOS name} \tab \strong{Notes}\cr
#' \code{PLD_REALTIMECLOCK}    \tab \code{time}                \tab \code{time}        \tab Converted to POSIXt time.\cr
#' \code{NAV_RESOURCE}         \tab \code{navState}            \tab -                  \tab - \cr
#' \code{NAV_LONGITUDE}        \tab \code{longitude}           \tab \code{lon}         \tab Converted to decimal degrees.\cr
#' \code{NAV_LATITUDE}         \tab \code{latitude}            \tab \code{lat}         \tab Converted to decimal degrees.\cr
#' \code{NAV_DEPTH}            \tab \code{pressureNav}         \tab \code{depth}       \tab - \cr
#' \code{FLBBCD_CHL_COUNT}     \tab \code{chlorophyllCount}    \tab -                  \tab - \cr
#' \code{FLBBCD_CHL_SCALED}    \tab \code{chlorophyll}         \tab -                  \tab - \cr
#' \code{FLBBCD_BB_700_COUNT}  \tab \code{backscatterCount}    \tab -                  \tab - \cr
#' \code{FLBBCD_BB_700_SCALED} \tab \code{backscatter}         \tab -                  \tab - \cr
#' \code{FLBBCD_CDOM_COUNT}    \tab \code{cdomCount}           \tab -                  \tab - \cr
#' \code{FLBBCD_CDOM_SCALED}   \tab \code{cdom}                \tab -                  \tab - \cr
#' \code{GPCTD_CONDUCTIVITY}   \tab \code{conductivity}        \tab -                  \tab - \cr
#' \code{GPCTD_TEMPERATURE}    \tab \code{temperature}         \tab \code{temperature} \tab degC \cr
#' \code{GPCTD_PRESSURE}       \tab \code{pressure}            \tab \code{pressure}    \tab dbar \cr
#' \code{GPCTD_DOF}            \tab \code{oxygenFrequency}     \tab -                  \tab - \cr
#' -                           \tab \code{salinity}            \tab \code{salinity}    \tab Practical Salinity (computed)\cr
#'}
#'
#' Data in the \code{pld2} files (or others for additional payloads)
#' are ignored in this version of the package. Please contact the
#' authors, if you need to handle such files.

