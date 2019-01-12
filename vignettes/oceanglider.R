## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(oceanglider)
url <- "ftp://ftp.dfo-mpo.gc.ca/glider/realData/SEA024/M25"
yo2file <- download.glider(url, "pld1.sub.2.gz$", debug=1)
yo2 <- read.glider.seaexplorer(yo2file)

