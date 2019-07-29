library(oceanglider)
if (file.exists("odd_yos.rda")) {
    load("odd_yos.rda")
} else {
    load("../../sea021m51_20190729_1125.rda")
    yo364 <- subset(g, yoNumber==364)
    yo711 <- subset(g, yoNumber==711)
    yo804 <- subset(g, yoNumber==804)
    yo820 <- subset(g, yoNumber==820)
    save(yo364, yo711, yo804, yo820, file="odd_yos.rda")
}
oce::oce.plot.ts(yo364[["time"]], yo364[["SA"]], type="o", pch=20)
t <- yo364[["time"]]
p <- yo364[["pressure"]]
CT <- yo364[["CT"]]
SA <- yo364[["SA"]]
tt <- oce::numberAsPOSIXct(1554474120) - 10
dt <- 15
tt <- c(tt, tt + dt)
par(mfrow=c(2, 2))
plot(SA, CT, type='o', cex=0.5)
oce.plot.ts(t, p, type='o', cex=0.5)
abline(v=tt, col=2)
mtext(paste(dt,"s event"), adj=1, col=2)


oce.plot.ts(t, SA, type='o', cex=0.5)
abline(v=tt, col=2)
mtext(paste(dt,"s event"), adj=1, col=2)
sigmaSA <- sd(SA, na.rm=TRUE)
k <- 4 * round(dt / mean(diff(as.numeric(t))))
if (2 * floor(k/2) == k)
    k <- k + 1
SAsmooth <- runmed(SA, k=k)
lines(t, SAsmooth, col=3)
lines(t, smooth(SA), col=4, lwd=3)
ss <- smooth.spline(t, SA, df=30)
lines(ss, col="gray", lwd=3)
sss <- supsmu(t, SA, base=4)
lines(sss, col="pink", lwd=3)
spiciness0 <- yo364[["spiciness0"]]
oce.plot.ts(t, spiciness0, type="p")
oce.plot.ts(t[c(-1,-2)], diff(diff(SA)), type="o", cex=1/2)
sigma0 <- yo364[["sigma0"]]
oce.plot.ts(t, sigma0, type="o")
plot(sigma0, p, type="o", ylim=rev(range(p)))
oce.plot.ts(t[c(-1,-2)], diff(diff(SA)), type="o", cex=1/2)


oce.plot.ts(t, CT, type='o', cex=0.5)
abline(v=tt, col=2)
mtext(paste(dt,"s event"), adj=1, col=2)
