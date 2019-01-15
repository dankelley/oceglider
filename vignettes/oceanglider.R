## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(oceanglider)
url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
filenames <- download.glider(url, "\\.200\\.gz$")
g <- read.glider.seaexplorer(filenames)

## ------------------------------------------------------------------------
urlBIO <- function(glider="024", mission=32)
    paste("ftp://ftp.dfo-mpo.gc.ca/glider/realData/", glider, "/M", mission, sep="")
patternBIO <- function(yo=2)
    paste("\\.", yo, "\\.gz$", sep="")

## ------------------------------------------------------------------------
filenames <- download.glider(urlBIO("SEA024", 32), patternBIO(200), debug=1)
g <- read.glider.seaexplorer(filenames)

## ------------------------------------------------------------------------
head(g[["glider"]], 3)
head(g[["payload"]], 3)

## ------------------------------------------------------------------------
ctd <- as.ctd(g[["salinity"]],g[["temperature"]],g[["pressure"]],lon=g[["longitude"]],lat=g[["latitude"]])
summary(ctd)
plot(ctd)

## ------------------------------------------------------------------------
plotScan(ctd, type="o")

## ------------------------------------------------------------------------
plot(ctdTrim(ctd, "upcast"))

