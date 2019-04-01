library(oce)
library(oceanglider)

dir  <- "/data/glider/2019/seaexplorer/sea019/m54/raw/"
g <- read.glider.seaexplorer.raw(dir)

head(g@data$payload[g@data$payload$yoNumber==886,])
system(paste0("head ", dir, "sea019.54.pld1.raw.886"))

head(g@data$payload[g@data$payload$yoNumber==887,])
system(paste0("head ", dir, "sea019.54.pld1.raw.887"))

head(g@data$payload[g@data$payload$yoNumber==888,])
system(paste0("head ", dir, "sea019.54.pld1.raw.888"))

head(g@data$payload[g@data$payload$yoNumber==889,])
system(paste0("head ", dir, "sea019.54.pld1.raw.889"))

head(g@data$payload[g@data$payload$yoNumber==890,])
system(paste0("head ", dir, "sea019.54.pld1.raw.890"))


