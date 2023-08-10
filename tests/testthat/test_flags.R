# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceGlider)

test_that("read.glider.seaexplorer.realtime flag names", {
    directory <- system.file("extdata/seaexplorer/sub", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory=directory, yo=101, progressBar=FALSE))
    expect_equal(g@metadata$flagScheme,
        list(name="IOOS",
            mapping=list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9),
            default=NULL))
})

test_that("read.glider.seaexplorer.delayed flag names", {
    directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory=directory, progressBar=FALSE))
    expect_equal(g@metadata$flagScheme,
        list(name="IOOS",
            mapping=list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9),
            default=NULL))
})

test_that("read.glider.seaexplorer.realtime flag setting and handling", {
    # This is based on the example given by ?"handleFlags,glider-method"
    directory <- system.file("extdata/seaexplorer/sub", package="oceGlider")
    g <- read.glider.seaexplorer.realtime(directory)
    # NOTE: this test was more hard-wired before issue40, e.g. it
    # demanded that the number of data read be 2784, but that number
    # became 3435 when read.glider.seaexplorer.delayed() was changed to
    # address issue40.
    n <- length(g[["salinity"]])
    expect_equal(n, sum(g[["salinityFlag"]]==2))
    lowSalinity <- which(g[["salinity"]] < 31)
    g2 <- setFlags(g, "salinity", g[["salinity"]] < 31, 3)
    expect_true(all(g2[["salinityFlag"]][lowSalinity] == 3))
    g3 <- handleFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
    expect_true(all(g3[["salinityFlag"]][lowSalinity] == 3))
    expect_true(all(is.na(g3[["salinity"]][lowSalinity])))
})

test_that("read.glider.seaexplorer.delayed flag setting and handling", {
    # This is based on the example given by ?"handleFlags,glider-method"
    directory <- system.file("extdata/seaexplorer/raw", package="oceGlider")
    g <- read.glider.seaexplorer.delayed(directory)
    # NOTE: this test was more hard-wired before issue40, e.g. it
    # demanded that the number of data read be 2784, but that number
    # became 3435 when read.glider.seaexplorer.delayed() was changed to
    # address issue40.
    n <- length(g[["salinity"]])
    expect_equal(n, sum(g[["salinityFlag"]]==2))
    lowSalinity <- which(g[["salinity"]] < 31)
    g2 <- setFlags(g, "salinity", g[["salinity"]] < 31, 3)
    expect_true(all(g2[["salinityFlag"]][lowSalinity] == 3))
    g3 <- handleFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
    expect_true(all(g3[["salinityFlag"]][lowSalinity] == 3))
    expect_true(all(is.na(g3[["salinity"]][lowSalinity])))
})

