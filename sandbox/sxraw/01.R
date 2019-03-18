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

pb <- txtProgressBar(1, length(pldfiles), 1, style=3)
for (i in 1:length(pldfiles)) {
    ## for (i in 1:50) {
    setTxtProgressBar(pb, i)
    d <- read.delim(pldfiles[i], sep=';', stringsAsFactors=FALSE)
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
    ii <- ifelse(i == 1, 1, dim(pld)[1])
    iii  <- ii + dim(d)[1] - 1
    if (i == 1) {
        pld <- d
    } else {
        pld[ii:iii, ] <- d
    }
}

## First remove all duplicated lon/lat
pld$longitude[which(duplicated(pld$longitude))] <- NA
pld$latitude[which(duplicated(pld$latitude))] <- NA

## Then remove all lon/lat that aren't from when navState is 116
trans <- pld$navState == 116
pld$longitude[!trans] <- NA
pld$latitude[!trans] <- NA

## Now interpolate
pld$longitude <- approx(pld$time, pld$longitude, pld$time)$y
pld$latitude <- approx(pld$time, pld$latitude, pld$time)$y

res@data <- list(payload=pld)

