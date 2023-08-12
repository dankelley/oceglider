# See if bad pressures align with bad conductivities. The output must be
# viewed in a wide terminal or small font to fit on one line!

library(oceglider)
f <- "sea021m51.rda"
if (file.exists(f)) {
    load(f) # defines g
} else {
    g <- read.glider.seaexplorer.delayed("~/Dropbox/data/glider/delayedData/SEA021/Data/M51/pld/logs")
    save(g, file=f)
}
pressureBad <- which(g[["pressure"]] > 500)
options(width = 500) # to get all on one line
for (i in pressureBad) {
    print(g[["payload1"]][i+seq(-5,5), ], digits=3)
    cat("\n\n")
}

