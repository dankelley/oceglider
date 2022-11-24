#' Read a glider file in netcdf IOOS format
#'
#' \strong{This is a provisional function, written to handle provisional
#' netcdf files created from the python `pyglider` processing chain
#' as well as files downloaded from the Glider IOOS ERDDAP site.}
#'
#' The data are copied directly from the file, except that `time`
#' is converted from an integer to a POSIX time. Variable names containing
#' underscores are renamed as e.g. `profile_direction`
#' to `profileDirection`, although the \code{\link{[[,glider-method}}
#' mechanism works with either name, e.g. if `g` is a glider object, then
#' `g[["profileDirection"]]` and
#' `g[["profile_direction"]]` give the same result.
#'
#' @param file Name of a netcdf file.
#'
#' @template debug
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Chantelle Layton and Dan Kelley
#'
#' @family functions to read glider data
#' @importFrom ncdf4 nc_open ncatt_get ncvar_get nc_close
#' @importFrom utils capture.output
#' @export
#'
#' @md
read.glider.netcdf.ioos <- function(file, debug)
{
    if (missing(debug))
        debug <- getOption("gliderDebug", default=0)
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {", unindent=1, sep="")
    if (missing(file))
        stop("must provide `file'")
    if (length(file) != 1)
        stop("file must have length 1")
    f <- ncdf4::nc_open(file)
    res <- new("glider")

    # get all global attributes aka metadata
    gliderDebug(debug, "reading and renaming attributes (aka metadata)\n")
    attributes <- ncdf4::ncatt_get(f, 0)
    # change attribute names from snake-case to camel-case
    attributeNames <- names(attributes)
    attributeNamesCamel <- unlist(lapply(attributeNames, toCamelCase))
    names(attributes) <- attributeNamesCamel
    # assign attributes to metadata
    res@metadata <- attributes
    # make space for units and flags in metadata
    res@metadata$units <- list()
    res@metadata$flags <- list()

    # get all data, including dimensions
    data <- list()
    # check length of variables and dimensions
    # dimNames <- names(f$dim)
    # dimlen <- lapply(dimNames, function(k) ncdf4::ncatt_get(f, k)$len)
    # dimlen <- lapply(f$dim, '[[', 'len')
    # varNames <- names(f$var)
    # varlen <- lapply(varNames, function(k) ncdf4::ncatt_get(f, k)$dim)
    # varlen <- lapply(f$var, function(k) k[['dim']][[1]][['len']]) # a bit strange
    # find the most dominant one, denote this as the primary length
    # if the length of the dim/var isn't the same, we'll do something different with it below
    # tlen <- table(c(unlist(dimlen), unlist(varlen)))
    # primarylen <- as.numeric(names(tlen)[which.max(tlen)])
    dataNames <- c(names(f$var),
                   names(f$dim)
                   )
    dataType <- c(rep('var', length(f$var)),
                  rep('dim', length(f$dim))
                  )
    # NOTE : nc file downloaded from IOOS glider ERDDAP has multiple time variables
    #        and actually, the dimension variables are very strange
    # FIXME : we'll need to decide on a hierarchy for which 'time' variable should be denoted as 'time'
    #         for plotting purposes
    # FIXME : incorporate "qc" and "flag" variables nicely (similar to oce-ctd objects)
    # TO-DO : look into dimensions of variables
    dataNamesOriginal <- list()
    gliderDebug(debug, "reading and renaming data\n")
    fixVector <- function(x)
    {
        nan <- is.nan(x)
        x[nan] <- NA
        as.vector(x)
    }
    for (i in seq_along(dataNames))  {
        newName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[newName]] <- dataNames[i]
        # get data length
        # if(dataType[i] == 'var'){
        #   ok <- which(names(f$var) == dataNames[i])
        #   f$var[[ok]][['dim']][['len']]
        # }
        capture.output(d <- try(ncdf4::ncvar_get(f, dataNames[i]), silent = TRUE))
        gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" try-error logical is \"", inherits(d, "try-error"), "\"\n", sep="")

        # check if it's a time variable,
        #   IOOS glider ERDDAP has 'flag' and 'qc' variables for time, so don't convert those to POSIX
        #   the qc check might be fragile, watch out for it. Debug statements will help in future
        if(!inherits(d, "try-error")){
          isTime <- grepl(".*[t,T]ime.*", newName) & !grepl(".*[q,Q]c.*", newName) & !grepl(".*[f,F]lag.*", newName)
          if (isTime) {
              data[[newName]] <- numberAsPOSIXct(fixVector(d))
              gliderDebug(debug, "i=", i, " ... time name \"", dataNames[i], "\" converted to \"", newName, "\" converted from integer to POSIXct\n", sep="")
          } else {
              data[[newName]] <- fixVector(d)
              gliderDebug(debug, "i=", i, " ... length of \"", dataNames[i], "\" is \"", length(data[[newName]]), "\"\n", sep="")
              gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" converted to \"", newName, "\"\n", sep="")
              # Handle units. Note that ncatt_get() prints a message for things that
              # lack attributes, and its 'quiet' argument does not silence them, so
              # we discard the output.
              capture.output(unit <- try(ncdf4::ncatt_get(f, dataNames[i])$units, silent=TRUE))
              if (!inherits(unit, "try-error") && !is.null(unit)) {
                if(unit != "1"){
                  newUnit <- switch(unit,
                                    "Celcius" = list(unit = expression(degree*C), scale="ITS-90"),
                                    "kg m-3" = list(unit = expression(kg/m^3), scale=""),
                                    "ug l-1" = list(unit=expression(mu*g/l), scale=""),
                                    "S m-1" = list(unit=expression(S/m), scale=""),
                                    "degrees_north" = list(unit=expression(degree*N), scale=""),
                                    "degrees_east" = list(unit=expression(degree*E), scale=""),
                                    "m" = list(unit=expression(m), scale=""),
                                    "m-1" = list(unit=expression(m^-1), scale=""), # or should it be 1/m?
                                    "degrees" = list(unit=expression(degree), scale=""),
                                    "m s-1" = list(unit=expression(m/s), scale=""),
                                    "dbar" = list(unit=expression(dbar), scale=""),
                                    "nm" = list(unit=expression(nm), scale=""),
                                    "umol kg-1" = list(unit=expression(mu*mol/kg), scale=""),
                                    "percent" = list(unit=expression("%"), scale=""),
                                    "rad" = list(unit=expression(rad), scale=""),
                                    "mg m-3" = list(unit=expression(mg/m^3), scale=""),
                                    "ppb" = list(unit=expression(ppb), scale="")
                                  )
                } else {
                  # for unit == 1
                  newUnit <- switch(newName,
                                    'salinity' = list(unit="", scale="PSS-78") # need to check on scale
                                    )
                }
                if(is.null(newUnit)){
                  message("FIXME: write code for newName=\"", newName, "\" ... unit=\"", unit, "\"")
                }
                res@metadata$units[[newName]] <- newUnits
              }
          }
        } else {
          #data[[newName]] <- NULL # not sure if I need this
          message("Could not read \"", newName, "\", proceeding to next variable\"")
        }
    }
    #res@data <- data # try leaving it as a list ?
    # remove any data that is NULL
    #isNull <- unlist(lapply(data, is.null))
    inData <- names(dataNamesOriginal) %in% names(data)
    res@data <- as.data.frame(data)
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- dataNamesOriginal[inData]
    gliderDebug(debug, "} # read.glider.netcdf.ioos", unindent=1, sep="")
    ncdf4::nc_close(f)
    res@metadata$type <- "ioos"
    res
}

