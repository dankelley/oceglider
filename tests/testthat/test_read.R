# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceGlider)

test_that("read.glider.seaexplorer.realtime", {
    directory <- system.file("extdata/seaexplorer/sub", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory=directory, yo=101, progressBar=FALSE))
    expect_output(summary(g), "Input file:")
    expect_equal(c("glider", "payload1"), names(g[["data"]]))
    # dimensionality and names in glider stream
    expect_equal(dim(g[["glider"]]), c(119, 21)) # the first number works for this particular file
    gliderNamesExpected <- c("time", "navState", "alarm", "heading",
        "pitch", "roll", "pressureNav",
        "temperatureInternal", "pressureInternal",
        "latitude", "longitude", "headingDesired",
        "ballastCmd", "ballastPos", "linCmd",
        "linPos", "angCmd", "angPos", "voltage",
        "altitude", "yoNumberNav")
    expect_equal(names(g[["glider"]]), gliderNamesExpected)
    # dimensionality and names in payload1 stream (and payload nickname)
    expect_equal(dim(g[["payload1"]]), c(36, 17)) # the first number works for this particular file
    expect_equal(dim(g[["payload"]]), c(36, 17))
    payloadNamesExpected <- c("time", "navState", "longitude", "latitude",
        "pressureNav", "chlorophyllCount",
        "chlorophyll", "backscatterCount",
        "backscatter", "cdomCount", "cdom",
        "conductivity", "temperature", "pressure",
        "oxygenFrequency", "yoNumber", "salinity")
    expect_equal(names(g[["payload1"]]), payloadNamesExpected)
    expect_equal(names(g[["payload"]]), payloadNamesExpected)
})

test_that("read.glider.seaexplorer.delayed", {
    directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory=directory, progressBar=FALSE))
    # Note that this data structure does not have a "glider" component.
    # The purpose in testing for this is to ensure that if that component
    # gets added, a developer will notice the change and invent new tests
    # for that component.
    expect_equal(names(g@data), "payload1")
    payloadNamesExpected<-c("time", "navState", "longitude", "latitude",
        "pressureNav", "chlorophyllCount",
        "chlorophyll", "backscatterCount",
        "backscatter", "cdomCount", "cdom",
        "conductivity", "temperature", "pressure",
        "oxygenFrequency", "yoNumber", "salinity")
    expect_equal(names(g@data$payload1), payloadNamesExpected)
    expect_equal(names(g[["payload1"]]), payloadNamesExpected)
    expect_equal(names(g[["payload"]]), payloadNamesExpected)
})

