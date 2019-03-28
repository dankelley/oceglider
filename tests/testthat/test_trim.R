## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("oceanglider")

test_that("ascending subset", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          ## ascending <- subset(g, "ascending") # why does this fail?
          ascending <- subset(g, navState==117)
          expect_true(all(ascending[["NAV_RESOURCE"]] == 117))
          expect_true(all(ascending[["NAV_RESOURCE", "payload"]] == 117))
          expect_true(all(ascending[["NavState"]] == 117))
          expect_true(all(ascending[["NavState", "glider"]] == 117))
          expect_true(all(ascending[["payload"]]$NAV_RESOURCE == 117))
          expect_true(all(ascending[["glider"]]$NavState == 117))
})

test_that("descending subset", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          ## decending <- subset(g, "descending") # why does this fail?
          decending <- subset(g, navState==100)
          expect_true(all(descending[["NAV_RESOURCE"]] == 100))
          expect_true(all(descending[["NAV_RESOURCE", "payload"]] == 100))
          expect_true(all(descending[["NavState"]] == 100))
          expect_true(all(descending[["NavState", "glider"]] == 100))
          expect_true(all(descending[["payload"]]$NAV_RESOURCE == 100))
          expect_true(all(descending[["glider"]]$NavState == 100))
})

