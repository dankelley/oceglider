# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceGlider)

test_that("degreeMinute() works as expected", {
    x <- 1234.56
    expect_equal(12+34.56/60, degreeMinute(x))
    x <- -x
    expect_equal(-(12+34.56/60), degreeMinute(x))
})

test_that("subset seaexplorer by 'ascending' and descending", {
    directory <- system.file("extdata/seaexplorer/sub", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory, yo=101, progressBar=FALSE))
    expect_silent(ga <- subset(g, "ascending"))
    expect_silent(gd <- subset(g, "descending"))
})

test_that("subset seaexplorer by pressure", {
    directory <- system.file("extdata/seaexplorer/sub", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory, yo=101, progressBar=FALSE))
    deep <- g[["pressure"]] > 20
    deep[is.na(deep)] <- FALSE
    expect_silent(gdeep <- subset(g, pressure > 20))
    expect_equal(gdeep[["payload1"]], g[["payload1"]][deep,])
})

#context("subset")
# test_that("ascending subset", {
#           files <- system.file("extdata/seaexplorer/sub",
#                                c("sea021.49.gli.sub.100.gz",
#                                  "sea021.49.pld1.sub.100.gz"), package="oceGlider")
#           g <- read.glider.seaexplorer.sub(files)
#           print(class(g))
#           ### ## capture_output(ascending <- subset(g, "ascending"), print=TRUE)
#           ### capture_output(ascending <- subset(g, navState==117), print=TRUE)
#           ### expect_true(all(ascending[["NAV_RESOURCE"]] == 117))
#           ### expect_true(all(ascending[["NAV_RESOURCE", "payload"]] == 117))
#           ### expect_true(all(ascending[["navState"]] == 117))
#           ### expect_true(all(ascending[["navState", "glider"]] == 117))
#           ### expect_true(all(ascending[["payload"]]$NAV_RESOURCE == 117))
#           ### expect_true(all(ascending[["glider"]]$navState == 117))
# })
# 
# test_that("descending subset", {
#           files <- system.file("extdata/seaexplorer/sub",
#                                c("sea024.32.gli.sub.200.gz",
#                                  "sea024.32.pld1.sub.200.gz"), package="oceGlider")
#           g <- read.glider.seaexplorer.sub(files)
#           capture_output(print(class(g)), print=TRUE)
#           ### ## capture_output(descending <- subset(g, "descending"), print=TRUE)
#           ### capture_output(descending <- subset(g, navState == 100), print=TRUE)
#           ### expect_true(all(descending[["NAV_RESOURCE"]] == 100))
#           ### expect_true(all(descending[["NAV_RESOURCE", "payload"]] == 100))
#           ### expect_true(all(descending[["navState"]] == 100))
#           ### expect_true(all(descending[["navState", "glider"]] == 100))
#           ### expect_true(all(descending[["payload"]]$NAV_RESOURCE == 100))
#           ### expect_true(all(descending[["glider"]]$navState == 100))
# })
# 
# ## FIXME: add a test on length, once we get a longer test dataset
# 
