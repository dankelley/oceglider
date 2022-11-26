library(oceanglider)
source('R/ioos.R')
path <- './sandbox/cl/02testDFO/'
file <- list.files(path = path, pattern = '.*\\.nc$', full.names = TRUE)
d <- read.glider.netcdf.ioos(file, debug = 3)
plotGlider(d, which = 3) # temperature
