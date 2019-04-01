## This finds a sequence of files that starts and ends with a yo containing
## navState==116, which means that the glider is in transmit mode, do that
## read.glider.seaexplorer.delayed() can infer locations.
library(oceanglider)
dir <- "~/git/glider/data/sea021/m49/raw"
if (!exists("g"))
    g <- read.glider.seaexplorer.delayed(dir)

gli <- list.files(dir, ".gli.", full.names=TRUE)
all <- list.files(dir, full.names=TRUE)
all[1:2]
read.glider.seaexplorer.realtime(gli[1])
sink("a");gli[1];sink()

plot(g, which="navState")
navState <- g[["navState"]]
length(navState)
length(g[["time"]])
look <- g[["navState"]] == 116
oce.plot.ts(g[["time"]], look)
head(g@data$payload1)

dim(g@data$payload1)
gg <- subset(g, navState==116)
head(unique(gg[["yoNumber"]]))
plot(gg[["yoNumber"]])


