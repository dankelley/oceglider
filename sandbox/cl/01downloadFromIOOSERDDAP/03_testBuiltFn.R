library(oceanglider)
path <- './sandbox/cl/01downloadFromIOOSERDDAP/'
file <- list.files(path = path, pattern = '.*\\.nc$', full.names = TRUE)
d <- read.glider.netcdf.ioos(file, debug = 3)
plotGlider(d, which = 2) # temperature
