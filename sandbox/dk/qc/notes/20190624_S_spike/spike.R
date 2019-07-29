library(oceanglider)

N2criterion <- 0.005

## Cache data
if (file.exists("odd_yos.rda")) {
    load("odd_yos.rda")
} else {
    load("../../sea021m51_20190729_1125.rda") # read and saved (i.e. all data)
    yo364 <- subset(g, yoNumber==364)
    yo711 <- subset(g, yoNumber==711)
    yo804 <- subset(g, yoNumber==804)
    yo820 <- subset(g, yoNumber==820)
    save(yo364, yo711, yo804, yo820, file="odd_yos.rda")
}

N2function <- function(pressure, sigma0, g=9.8) {
    rho0 <- 1000 + mean(sigma0, na.rm=TRUE)
    z <- swZ(pressure)
    rval <- -g / rho0 * diff(sigma0)/diff(z)
    rval <- c(rval[1], rval)
    rval[!is.finite(rval)] <- NA
    rval
}

spikeDetection <- function(p, x, sep=50, criterion=FALSE) {
    n <- length(p)
    message("n=",n)
    inspike <- rep(NA, n)
    look <- diff(p) < 0
    look <- 1:n
    xx <- x[look]
    pp <- p[look]
    n <- length(pp)
    message("n=",n)
    if (is.logical(criterion) && criterion) {
        for (i in seq(sep+1, n-sep-1))
            inspike[i] <- abs(xx[i] - 0.5*(xx[i-sep]+xx[i+sep])) > criterion
    } else {
        for (i in seq(sep+1, n-sep-1))
            inspike[i] <- (xx[i] - 0.5*(xx[i-sep]+xx[i+sep]))
    }
    inspike
}

#spikeDetection(p,SA)

## par(mfrow=c(1, 2), mar=c(3,3,1,1),mgp=c(2,0.7,0))
## plot(SA, p, ylim=rev(range(p)), type='p', pch=20, cex=1/2)
## sep <- 30
## inspike <- spikeDetection(p, SA, sep=sep)
## plot(inspike, p, ylim=rev(range(p)), type='p', pch=20, cex=1/2)
## 
## stop()


if (!interactive()) png("spike_%d.png")


## oce::oce.plot.ts(yo364[["time"]], yo364[["SA"]], type="o", pch=20)
## t <- yo364[["time"]]
## p <- yo364[["pressure"]]
## CT <- yo364[["CT"]]
## SA <- yo364[["SA"]]
## tt <- oce::numberAsPOSIXct(1554474120) - 10
## dt <- 15
## tt <- c(tt, tt + dt)
## par(mfrow=c(2, 2))
## plot(SA, CT, type='o', cex=0.5)
## oce.plot.ts(t, p, type='o', cex=0.5)
## abline(v=tt, col=2)
## mtext(paste(dt,"s event"), adj=1, col=2)

for (yo in c(yo364, yo711, yo804, yo820)) {
    t <- yo[["time"]]
    p <- yo[["pressure"]]
    CT <- yo[["CT"]]
    SA <- yo[["SA"]]
    z <- swZ(p)
    g <- 9.8
    sigma0 <- yo[["sigma0"]]
    spice0 <- yo[["spice0"]]
    N2 <- N2function(p, sigma0)
    mean(N2,na.rm=TRUE)
    sd(N2,na.rm=TRUE)
    ##oce.plot.ts(t, SA, type='o', cex=0.5)
    ##plot(sigma0, p, ylim=rev(range(p)), type='o')
    par(mfrow=c(1, 3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
    #ylim <- c(65,55)
    ylim <- rev(range(p, na.rm=TRUE))
    plot(sigma0, p, type='o', ylim=ylim, pch=20, cex=0.5)
    mtext(sprintf("yo=%d, mean N2=%.2e", yo[["yoNumber"]][1], mean(N2,na.rm=TRUE)), side=3)
    N2high <- N2 >= N2criterion
    N2low <- N2 <= -N2criterion
    plot(N2, p, type='o', ylim=ylim, pch=20, cex=0.5)
    points(N2[N2high], p[N2high], col="forestgreen", pch=20)
    points(N2[N2low], p[N2low], col="red", pch=20)
    abline(v=mean(N2, na.rm=TRUE)+c(-3,3)*sd(N2, na.rm=TRUE),
           col=c("red","forestgreen"), lty='dashed')
    abline(v=N2criterion*c(-1,1), col=c("red","forestgreen"))
    mtext("dash=yo-based N2 criterion; solid=fixed", adj=0, cex=0.8)
    sepTime <- 20 # seconds
    sep <- sepTime / median(diff(as.numeric(t)))
    inspike <- spikeDetection(p, SA, sep=sep)
    plot(inspike, p, ylim=rev(range(p)), type='p', pch=20, cex=1/2)
}

## 
## spiciness0 <- yo364[["spiciness0"]]
## oce.plot.ts(t, spiciness0, type="p")
## oce.plot.ts(t[c(-1,-2)], diff(diff(SA)), type="o", cex=1/2)
## sigma0 <- yo364[["sigma0"]]
## oce.plot.ts(t, sigma0, type="o")
## plot(sigma0, p, type="o", ylim=rev(range(p)))
## oce.plot.ts(t[c(-1,-2)], diff(diff(SA)), type="o", cex=1/2)
## 
## 
## oce.plot.ts(t, CT, type='o', cex=0.5)
## abline(v=tt, col=2)
## mtext(paste(dt,"s event"), adj=1, col=2)

if (!interactive()) dev.off()
