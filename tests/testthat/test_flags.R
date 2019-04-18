## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("read seaexplorer")

test_that("read.glider.seaexplorer.realtime flag names", {
        directory <- system.file("extdata/seaexplorer/sub", package="oceanglider")
        expect_silent(g <- read.glider.seaexplorer.realtime(directory=directory, yo=101, progressBar=FALSE))
        expect_equal(g@metadata$flagScheme,
        structure(list(name="IOOS",
                 mapping=structure(list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9),
                                     .Names=c("pass", "not_evaluated", "suspect", "fail", "missing"))),
            .Names=c("name", "mapping")))
})

test_that("read.glider.seaexplorer.delayed flag nams", {
  directory <- system.file("extdata/seaexplorer/sub", package="oceanglider")
  expect_silent(g <- read.glider.seaexplorer.delayed(directory=directory, yo=101, progressBar=FALSE))
  expect_equal(g@metadata$flagScheme,
               structure(list(name="IOOS",
                              mapping=structure(list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9),
                                                .Names=c("pass", "not_evaluated", "suspect", "fail", "missing"))),
                         .Names=c("name", "mapping")))
})

test_that("read.glider.seaexplorer.delayed flag setting and handling", {
          ## This is based on the example given by ?"handleFlags,glider-method"
          ## The test requires THIS PARTICULAR dataset, because it demands
          ## that a certain number of salinity data will have flag '2' and
          ## that another number have flag '3'
          directory <- system.file("extdata/seaexplorer/raw", package="oceanglider")
          g <- read.glider.seaexplorer.delayed(directory)
          expect_equal(2784, sum(g[["salinityFlag"]] == 2))
          g2 <- setFlags(g, "salinity", g[["salinity"]]<31, 3)
          expect_equal(2763, sum(g2[["salinityFlag"]] == 2))
          expect_equal(21, sum(g2[["salinityFlag"]] == 3))
          g3 <- handleFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
          expect_equal(2763, sum(g3[["salinityFlag"]] == 2))
          expect_equal(21, sum(g3[["salinityFlag"]] == 3))
})

