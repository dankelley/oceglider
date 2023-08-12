# function pulled from oceanglider.R line 1317, removed formatting for easy testing
library(ocelider) # need various functions
library(ncdf4)
path <- './sandbox/cl/01downloadFromIOOSERDDAP/'
file <- list.files(path = path, pattern = '.*\\.nc$', full.names = TRUE)
debug <- 3

# read.glider.netcdf.ioos <- function(file, debug)
# {
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

# get all data
data <- list()
dataNames <- names(f$var)
# FIXME : nc file downloaded from IOOS glider ERDDAP has multiple time variables
#         and actually, the dimension variables are very strange
dataNamesOriginal <- list()
gliderDebug(debug, "reading and renaming data\n")
for (i in seq_along(dataNames))  {
  newName <- toCamelCase(dataNames[i])
  dataNamesOriginal[[newName]] <- dataNames[i]
  # check if it's a time variable,
  #   IOOS glider ERDDAP has 'flag' and 'qc' variables for time, so don't convert those to POSIX
  #   the qc check might be fragile, watch out for it. Debug statements will help in future
  isTime <- grepl(".*[t,T]ime.*", newName) & !grepl(".*[q,Q]c.*", newName) & !grepl(".*[f,F]lag.*", newName)
  if (isTime) {
    data[[newName]] <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, dataNames[i])))
    gliderDebug(debug, "i=", i, " ... time name \"", dataNames[i], "\" converted to \"", newName, "\" converted from integer to POSIXct\n", sep="")
    dataNames[i] <- newName
  } else {
    data[[newName]] <- as.vector(ncdf4::ncvar_get(f, dataNames[i]))
    gliderDebug(debug, "i=", i, " ... data name \"", dataNames[i], "\" converted to \"", newName, "\"\n", sep="")
    dataNames[i] <- newName
  }
}
# FIXME it was res@metadata$payload1
#res@data$payload1 <- as.data.frame(data)
res@data <- as.data.frame(data)
res@metadata$filename <- file
# FIXME it was list(payload1 = dataNamesOriginal) is the 'payload1' needed
#       for both res@data and @metadata$dataNamesOriginal so they can 'hook-up' to eachother ?
#res@metadata$dataNamesOriginal <- list(payload1 = dataNamesOriginal)
res@metadata$dataNamesOriginal <- list(dataNamesOriginal)
gliderDebug(debug, "} # read.glider.netcdf.ioos", unindent=1, sep="")
ncdf4::nc_close(f)
res
