library(oceglider)
options(oceEOS="gsw") # A *lot* of code is hard-wired for this, so we don't let user choose.

N2unstable <- function(pressure, sigma0, g=9.8, L=2, rho0=1025)
{
    i <- seq_along(pressure) # for checking on reordering scheme
    z <- swZ(pressure)
    o <- order(z)
    oo <- order(o)
    i <- i[o]
    if (L > 0) {
        m <- runlm(z[o], sigma0[o], L=L)
        N2 <- -g / rho0 * m$dydx
    } else {
        N2 <- -g / rho0 * diff(sigma0)/diff(z)
        N2 <- c(rval[1], rval)
    }
    N2[!is.finite(N2)] <- NA
    N2 <- N2[oo] # return to original order
    unstable <- N2 < (-mean(N2[N2>0], na.rm=TRUE))
    io <- i[oo]
    if (any(1 != diff(io)))
        stop("reordering is broken: please contact author")
    list(N2=N2, unstable=unstable)
}

file <- "../../sea021m51_unedited.rda"

if (!exists("g")) {
    load(file)
}

max <- max(g[["yoNumber"]])

createPDF <- TRUE

if (createPDF) pdf("N2_test.pdf")

par(mfrow=c(1, 2), mar=c(3, 3, 1.5, 1), mgp=c(2, 0.7, 0))
for (y in 1:max) {
    message("y=", y)
    yo <- subset(g, yoNumber==y)
    sigma0 <- yo[["sigma0"]]
    p <- yo[["pressure"]]
    N2u <- N2unstable(p, sigma0)
    N2 <- N2u$N2
    unstable <- N2u$unstable
    col <- ifelse(unstable, "red", "gray")
    cex <- ifelse(unstable, 1, 0.5)
    pch <- ifelse(unstable, 20, 20)
    if (any(is.finite(p)) && any(is.finite(sigma0)) && any(is.finite(N2))) {
        ylim <- rev(range(p, na.rm=TRUE))
        plot(sigma0, p, ylim=ylim, pch=pch, cex=cex, col=col, xlab=expression(sigma[0]))
        mtext(file, side=3)
        plot(N2, p, ylim=ylim, pch=pch, cex=cex, col=col, xlab=expression(N^2))
        abline(v=0, col='pink')
        mtext(paste("yo", y), side=3)
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "No valid (p, sigma0, N2) data")
        box()
        mtext(file, side=3)
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "No valid (p, sigma0, N2) data")
        box()
        mtext(file, side=3)
    }
}

if (createPDF) dev.off()

