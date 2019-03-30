## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("read seaexplorer")

test_that("read.glider.seaexplorer.sub", {
          files <- system.file("extdata/seaexplorer/sub",
                               c("sea021.49.gli.sub.100.gz",
                                 "sea021.49.pld1.sub.100.gz"), package="oceanglider")
          expect_silent(g <- read.glider.seaexplorer.sub(files))
          summary(g)
          expect_equal(c("glider", "payload1"), names(g[["data"]]))
          ## dimensionality and names in glider stream
          expect_equal(dim(g[["glider"]]), c(71, 20))
          gliderNamesExpected <- c("time", "navState", "alarm", "heading",
                                   "pitch", "roll", "pressureNav",
                                   "temperatureInternal", "pressureInternal",
                                   "latitude", "longitude", "headingDesired",
                                   "BallastCmd", "BallastPos", "LinCmd",
                                   "LinPos", "AngCmd", "AngPos", "voltage",
                                   "Altitude")
          expect_equal(names(g[["glider"]]), gliderNamesExpected)
          ## dimensionality and names in payload1 stream (and payload nickname)
          expect_equal(dim(g[["payload1"]]), c(22, 16))
          expect_equal(dim(g[["payload"]]), c(22, 16))
          payloadNamesExpected <- c("time", "navState", "longitude", "latitude",
                                    "pressureNav", "chlorophyllCount",
                                    "chlorophyll", "backscatterCount",
                                    "backscatter", "cdomCount", "cdom",
                                    "conductivity", "temperature", "pressure",
                                    "oxygenFrequency", "salinity")
          expect_equal(names(g[["payload1"]]), payloadNamesExpected)
          expect_equal(names(g[["payload"]]), payloadNamesExpected)
})

test_that("read.glider.seaexplorer.raw", {
          dir <- system.file("extdata/seaexplorer/raw", package="oceanglider")
          ## Next fails (https://github.com/dankelley/oceanglider/issues/24)
          expect_silent(g <- read.glider.seaexplorer.raw(dir))
          summary(g)
})
