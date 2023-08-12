library(oceglider)
source('R/ioos.R')
path <- './sandbox/cl/01downloadFromIOOSERDDAP/'
file <- list.files(path = path, pattern = '.*\\.nc$', full.names = TRUE)
d <- read.glider.netcdf.ioos(file)
plotGlider(d, which = 3) # temperature
