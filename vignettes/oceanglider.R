## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(oceanglider)
url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25"
yo2pldf <- download.glider(url, "pld1.sub.2.gz$", debug=1)
yo2pld <- read.glider.seaexplorer(yo2pldf)

## ------------------------------------------------------------------------
urlBIO <- function(glider="024", mission=25)
    paste("ftp://ftp.dfo-mpo.gc.ca/glider/realData/", glider, "/M", mission, sep="")
patternBIO <- function(type="pld1", yo=2)
    paste(type, ".sub.", yo, ".gz$", sep="")

## ------------------------------------------------------------------------
yo2glif <- download.glider(urlBIO("SEA024", 25), patternBIO("gli", 2), debug=1)
yo2gli <- read.glider.seaexplorer(yo2glif)

