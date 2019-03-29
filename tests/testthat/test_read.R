## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("read seaexplorer")

test_that("read.glider.seaexplorer.sub", {
          files <- system.file("extdata/seaexplorer/sub",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          expect_silent(g <- read.glider.seaexplorer.sub(files))
})

test_that("read.glider.seaexplorer.raw", {
          dir <- system.file("extdata/seaexplorer/raw", package="oceanglider")
          ##expect_silent(g <- read.glider.seaexplorer.raw(dir))
          g <- read.glider.seaexplorer.raw(dir)
})
