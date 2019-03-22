## Want to be able to read the raw SeaExplorer CSV files. Need to read
## both the `gli` (navigation computer) and `pld` (payload) files.

## Nav state 116 is when the glider is transmitting -- this is when we
## should extract the lon/lat, and otherwise interpolate between those
## values.
## Code Navigation State
## 100 Going down
## 105 Initializating
## 110 Inflecting down
## 115 Surfacing
## 116 Transmitting
## 117 Going up
## 118 Inflecting up

## To sensibly interpolate the lon/lat for each point, we have to know
## the lon/lat just when it started diving and then the lon/lat when
## it just surfaced.

## It looks like when the lon/lat start being recorded immediately
## after surfacing, even though the navState is in 116, they are still
## the same values as the last ones recorded before the last yo
## cycle. Likely this is because the GPS has not yet obtained a fix,
## and it is still repeating the last values

rm(list=ls())
library(oce)
library(oceanglider)

dir <- '/data/archive/glider/2019/sx/sea021m49/raw'
navfiles <- dir(dir, pattern='*gli*', full.names=TRUE)
pldfiles <- dir(dir, pattern='*pld*', full.names=TRUE)

yonumber <- as.numeric(unlist(lapply(strsplit(pldfiles, '.', fixed=TRUE), tail, 1)))
o <- order(yonumber)
yonumber <- yonumber[o]
pldfiles <- pldfiles[o]

res <- new("glider")
res@metadata$type <- "seaexplorer"
res@metadata$filename <- pldfiles
res@metadata$yo <- yonumber

## We'll read each file into a list of data frames, since it's faster
## than reading into one large data frame
pld <- list()
pb <- txtProgressBar(1, length(pldfiles), 1, style=3)
for (i in 1:length(pldfiles)) {
## for (i in 1:200) {
    setTxtProgressBar(pb, i)
    d <- read.delim(pldfiles[i], sep=';', stringsAsFactors=FALSE, row.names=NULL)
    d$yoNumber <- rep(yonumber[i], dim(d)[1])
    if ("NAV_RESOURCE" %in% names(d)) {
        names(d) <- gsub("NAV_RESOURCE", "navState", names(d))
        res@metadata$dataNamesOriginal$payload$navState <- "NAV_RESOURCE"
    }
    if ("NAV_DEPTH" %in% names(d)) {
        names(d) <- gsub("NAV_DEPTH", "pressureNav", names(d))
        res@metadata$dataNamesOriginal$payload$pressureNav <- "NAV_DEPTH"
    }
    if ("NAV_LONGITUDE" %in% names(d)) {
        names(d) <- gsub("NAV_LONGITUDE", "longitude", names(d))
        d$longitude <- degreeMinute(d$longitude)
        res@metadata$dataNamesOriginal$payload$longitude <- "NAV_LONGITUDE"
    }
    if ("NAV_LATITUDE" %in% names(d)) {
        names(d) <- gsub("NAV_LATITUDE", "latitude", names(d))
        d$latitude <- degreeMinute(d$latitude)
        res@metadata$dataNamesOriginal$payload$latitude <- "NAV_LATITUDE"
    }
    if ("GPCTD_TEMPERATURE" %in% names(d)) {
        names(d) <- gsub("GPCTD_TEMPERATURE", "temperature", names(d))
        res@metadata$dataNamesOriginal$payload$temperature <- "GPCTD_TEMPERATURE"
    }
    if ("GPCTD_PRESSURE" %in% names(d)) {
        names(d) <- gsub("GPCTD_PRESSURE", "pressure", names(d))
        res@metadata$dataNamesOriginal$payload$pressure <- "GPCTD_PRESSURE"
    }
    if ("GPCTD_CONDUCTIVITY" %in% names(d)) {
        names(d) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(d))
        res@metadata$dataNamesOriginal$payload$conductivity <- "GPCTD_CONDUCTIVITY"
    }
    if ("GPCTD_DOF" %in% names(d)) {
        names(d) <- gsub("GPCTD_DOF", "oxygenFrequency", names(d))
        res@metadata$dataNamesOriginal$payload$oxygenFrequency <- "GPCTD_DOF"
    }
    if ("FLBBCD_CHL_COUNT" %in% names(d)) {
        names(d) <- gsub("FLBBCD_CHL_COUNT", "chlorophylCount", names(d))
        res@metadata$dataNamesOriginal$payload$chlorophylCount <- "FLBBCD_CHL_COUNT"
    }
    if ("FLBBCD_CHL_SCALED" %in% names(d)) {
        names(d) <- gsub("FLBBCD_CHL_SCALED", "chlorophyl", names(d))
        res@metadata$dataNamesOriginal$payload$chlorophyl <- "FLBBCD_CHL_SCALED"
    }
    if ("FLBBCD_BB_700_COUNT" %in% names(d)) {
        names(d) <- gsub("FLBBCD_BB_700_COUNT", "backscatterCount", names(d))
        res@metadata$dataNamesOriginal$payload$backscatterCount <- "FLBBCD_BB_700_COUNT"
    }
    if ("FLBBCD_BB_700_SCALED" %in% names(d)) {
        names(d) <- gsub("FLBBCD_BB_700_SCALED", "backscatter", names(d))
        res@metadata$dataNamesOriginal$payload$backscatter <- "FLBBCD_BB_700_SCALED"
    }
    if ("FLBBCD_CDOM_COUNT" %in% names(d)) {
        names(d) <- gsub("FLBBCD_CDOM_COUNT", "cdomCount", names(d))
        res@metadata$dataNamesOriginal$payload$cdomCount <- "FLBBCD_CDOM_COUNT"
    }
    if ("FLBBCD_CDOM_SCALED" %in% names(d)) {
        names(d) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(d))
        res@metadata$dataNamesOriginal$payload$cdom <- "FLBBCD_CDOM_SCALED"
    }
    if ("PLD_REALTIMECLOCK" %in% names(d)) {
        names(d) <- gsub("PLD_REALTIMECLOCK", "time", names(d))
        d$time <- as.POSIXct(d$time, format="%d/%m/%Y %H:%M:%S", tz="UTC")
        res@metadata$dataNamesOriginal$payload$time <- "-"
    }
    pld[[i]] <- d
}
cat('\n')

cat('* Convert list to data frame ...')
## convert the list to a data frame
df <- do.call(rbind.data.frame, pld)
df[['X']] <- NULL # get rid of the weird last column
cat('done\n')

## First remove all duplicated lon/lat
df$longitude[which(duplicated(df$longitude))] <- NA
df$latitude[which(duplicated(df$latitude))] <- NA

## Then remove all lon/lat that aren't from when navState is 116
trans <- df$navState == 116
df$longitude[!trans] <- NA
df$latitude[!trans] <- NA

## Now interpolate
df$longitude <- approx(df$time, df$longitude, df$time)$y
df$latitude <- approx(df$time, df$latitude, df$time)$y

## The only pressure measurements come from the GP-CTD, but the FLBBCD
## samples on it's own schedule that isn't synced with the CTD. So, to
## "fill out" the data we should interpolate the pressures to the
## FLBBCD times.
##   * Should we also interpolate the FLBBCD values to the GP-CTD times?
##   * What about interpolating the GP-CTD (i.e. C and T) to the measured FLBBCD times?

## However, we only want to interpolate to times when something was actually being sampled
##   * maybe need to trim out all times where there is a complete row of NAs first?

cat('* trimming empty rows ...')
sub <- df[, which(!(names(df) %in% c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber')))]
naRows <- apply(sub, 1, function(x) sum(is.na(x)))
ok <- naRows < dim(sub)[2]
dft <- df[ok,]
cat('done\n')

## We need to remove the first point (or two) from the beginning of
## each "inflecting" state (110 and 118), because the CTD may repeat
## the last measured value when it is first turned on
## Have to remove at least a few of the first points in the
## "inflecting" stage because they don't always have GP-CTD samples:
system.time({
cat('* Removing bad points from inflection ...')
inflectUp <- as.integer(dft$navState == 118)
iuStart <- which(diff(inflectUp) == 1) + 1
inflectDown <- as.integer(dft$navState == 110)
idStart <- which(diff(inflectDown) == 1) + 1
ok <- rep(TRUE, dim(dft)[1])
for (i in 0:5) {
    ok[iuStart+i] <- FALSE
    ok[idStart+i] <- FALSE
}
dft <- dft[ok,]
})

cat('* Interpolating NAs:\n')
n <- length(names(dft)) - length(c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))
pb <- txtProgressBar(1, n, 1, style=3)
i <- 1
for (var in names(dft)) {
    if (!(var %in% c('time', 'navState', 'longitude', 'latitude', 'pressureNav', 'yoNumber'))) {
        setTxtProgressBar(pb, i)
        dft[[var]] <- approx(dft[['time']], dft[[var]], dft[['time']])$y
        i <- i + 1
    }
}
cat('done\n')

cat('* Remove duplicate times ...')
dft <- dft[!duplicated(dft), ]
cat('done\n')

dft$salinity <- with(dft, swSCTp(conductivity, temperature, pressure, conductivityUnit = 'S/m'))
dft$salinity[dft$salinity > 40] <- NA

res@data <- list(payload=dft)

## plot(res, which=1, xlim=focus, type='p')
plot(res, which=1, type='p', pch='.', cex=3, col=colormap(res[['chlorophyl']], breaks=31)$zcol,
     ylim=c(250, 0))
