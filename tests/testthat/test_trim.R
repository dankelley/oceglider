## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oceanglider)

context("oceanglider")

test_that("gliderTrim(g, ascending)", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          expect_error(gliderTrim(g, "junk"),
                       'method "junk" is not permitted; see \\?gliderTrim for choices')
          ascending <- gliderTrim(g, "ascending")
          expect_true(all(ascending[["NAV_RESOURCE"]] == 117))
          expect_true(all(ascending[["NAV_RESOURCE", "payload"]] == 117))
          expect_true(all(ascending[["NavState"]] == 117))
          expect_true(all(ascending[["NavState", "glider"]] == 117))
          expect_true(all(ascending[["payload"]]$NAV_RESOURCE == 117))
          expect_true(all(ascending[["glider"]]$NavState == 117))
})

test_that("gliderTrim(g, descending)", {
          files <- system.file("extdata/seaexplorer/realtime",
                               c("sea024.32.gli.sub.200.gz",
                                 "sea024.32.pld1.sub.200.gz"), package="oceanglider")
          g <- read.glider.seaexplorer.realtime(files)
          expect_error(gliderTrim(g, "junk"),
                       'method "junk" is not permitted; see \\?gliderTrim for choices')
          descending <- gliderTrim(g, "descending")
          expect_true(all(descending[["NAV_RESOURCE"]] == 100))
          expect_true(all(descending[["NAV_RESOURCE", "payload"]] == 100))
          expect_true(all(descending[["NavState"]] == 100))
          expect_true(all(descending[["NavState", "glider"]] == 100))
          expect_true(all(descending[["payload"]]$NAV_RESOURCE == 100))
          expect_true(all(descending[["glider"]]$NavState == 100))
})

## FIXME: add a test on length, once we get a longer test dataset
