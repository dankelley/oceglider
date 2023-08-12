library(oceglider)
if (!exists("g0")) {
    file <- "~/Dropbox/sea021m49/netcdf/GLI2018_SEA021_049DM_L1.nc"
    ## file <- "/data/archive/glider/2019/sx/sea021m49/netcdf/GLI2018_SEA021_049DM_L1.nc"
    g0 <- read.glider(file)
}

head(g0[["time"]])
expect_true(all(!is.na(g0[["time"]])))
## The above shows that we have a few (4) points from 2018-07-12 at the start,
## followed by data on 2019-02-22, so we subset for just the later data.
start <- as.POSIXct("2019-02-21")
g1 <- subset(g0, time > start)
head(g1[["time"]])
g2 <- subset(g1, salinity < 40)
head(g2[["time"]])
head(g0[["salinity"]])


