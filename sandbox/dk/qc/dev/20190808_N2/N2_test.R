library(oceanglider)
options(oceEOS="gsw") # A *lot* of code is hard-wired for this, so we don't let user choose.

N2profile <- function(pressure, sigma0, g=9.8, L=2, rho0=1025)
{
    z <- swZ(pressure)
    o <- order(z)
    oo <- order(o)
    if (L > 0) {
        m <- runlm(z[o], sigma0[o], L=L)
        rval <- -g / rho0 * m$dydx
    } else {
        rval <- -g / rho0 * diff(sigma0)/diff(z)
        rval <- c(rval[1], rval)
    }
    rval[!is.finite(rval)] <- NA
    rval[oo]
}

if (!exists("g")) {
    load("../../sea021m51_unedited.rda")
}

max <- max(g[["yoNumber"]])

pdf("N2_test.pdf")

par(mfrow=c(1, 2), mar=c(3, 3, 1.5, 1), mgp=c(2, 0.7, 0))
for (y in 1:max) {
    message("y=", y)
    yo <- subset(g, yoNumber==y)
    sigma0 <- yo[["sigma0"]]
    p <- yo[["pressure"]]
    o <- order(p)
    sigma0 <- sigma0[o]
    p <- p[o]
    bad <- is.na(p) | is.na(sigma0) | p > 300
    p <- p[!bad]
    sigma0 <- sigma0[!bad]
    cex <- 0.5
    pch <- 20
    plot(sigma0, p, ylim=rev(range(p)), pch=pch, cex=cex)
    N2 <- N2profile(p, sigma0)
    unstable <- N2 < (-mean(N2[N2>0]))
    plot(N2, p, ylim=rev(range(p)), pch=pch, cex=cex, col=ifelse(unstable, "red", "gray"))
    mtext(sprintf("yo=%d", y), side=3)
}

dev.off()

