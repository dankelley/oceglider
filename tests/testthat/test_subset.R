## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("subset")

test_that("ascending subset", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          ascending <- subset(g, "ascending")
          expect_true(all(ascending[["NAV_RESOURCE"]] == 117))
          expect_true(all(ascending[["NAV_RESOURCE", "payload"]] == 117))
          expect_true(all(ascending[["navState"]] == 117))
          expect_true(all(ascending[["navState", "glider"]] == 117))
          expect_true(all(ascending[["payload"]]$NAV_RESOURCE == 117))
          expect_true(all(ascending[["glider"]]$navState == 117))
})

test_that("descending subset", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          descending <- subset(g, "descending")
          expect_true(all(descending[["NAV_RESOURCE"]] == 100))
          expect_true(all(descending[["NAV_RESOURCE", "payload"]] == 100))
          expect_true(all(descending[["navState"]] == 100))
          expect_true(all(descending[["navState", "glider"]] == 100))
          expect_true(all(descending[["payload"]]$NAV_RESOURCE == 100))
          expect_true(all(descending[["glider"]]$navState == 100))
})

## FIXME: add a test on length, once we get a longer test dataset

