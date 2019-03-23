## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(oceanglider)
## url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M32"
## filenames <- download.glider(url, "\\.200\\.gz$")
files <- system.file("extdata/seaexplorer/realtime",
                     c("sea024.32.gli.sub.200.gz",
                       "sea024.32.pld1.sub.200.gz"), package="oceanglider")
g <- read.glider.seaexplorer.realtime(files)

## ------------------------------------------------------------------------
urlBIO <- function(glider="024", mission=32)
    paste("ftp://ftp.dfo-mpo.gc.ca/glider/realData/", glider, "/M", mission, sep="")
patternBIO <- function(yo=2)
    paste("\\.", yo, "\\.gz$", sep="")

## ----eval=FALSE----------------------------------------------------------
#  filenames <- download.glider(urlBIO("SEA024", 32), patternBIO(200), debug=1)
#  g <- read.glider.seaexplorer.realtime(filenames)

## ------------------------------------------------------------------------
head(g[["glider"]], 3)
head(g[["payload"]], 3)

