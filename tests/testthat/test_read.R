## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("read seaexplorer")

test_that("read.glider.seaexplorer.realtime", {
          directory <- system.file("extdata/seaexplorer/sub", package="oceanglider")
          expect_silent(g <- read.glider.seaexplorer.realtime(directory=directory, yo=100, progressBar=FALSE))
          expect_output(summary(g), "Input files:")
          expect_equal(c("glider", "payload1"), names(g[["data"]]))
          ## dimensionality and names in glider stream
          expect_equal(dim(g[["glider"]]), c(71, 21))
          gliderNamesExpected <- c("time", "navState", "alarm", "heading",
                                   "pitch", "roll", "pressureNav",
                                   "temperatureInternal", "pressureInternal",
                                   "latitude", "longitude", "headingDesired",
                                   "ballastCmd", "ballastPos", "linCmd",
                                   "linPos", "angCmd", "angPos", "voltage",
                                   "altitude", "yoNumberNav")
          expect_equal(names(g[["glider"]]), gliderNamesExpected)
          ## dimensionality and names in payload1 stream (and payload nickname)
          expect_equal(dim(g[["payload1"]]), c(22, 17))
          expect_equal(dim(g[["payload"]]), c(22, 17))
          payloadNamesExpected <- c("time", "navState", "longitude", "latitude",
                                    "pressureNav", "chlorophyllCount",
                                    "chlorophyll", "backscatterCount",
                                    "backscatter", "cdomCount", "cdom",
                                    "conductivity", "temperature", "pressure",
                                    "oxygenFrequency", "yoNumber", "salinity")
          expect_equal(names(g[["payload1"]]), payloadNamesExpected)
          expect_equal(names(g[["payload"]]), payloadNamesExpected)
})

## test_that("read.glider.seaexplorer.raw", {
##           dir <- system.file("extdata/seaexplorer/raw", package="oceanglider")
##           expect_silent(g <- read.glider.seaexplorer.raw(dir))
##           summary(g)
## })

