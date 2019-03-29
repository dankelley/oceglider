library(oce)
library(oceanglider)

dir  <- "../data/sea019/m54/all_data/"
dir  <- "/data/glider/2019/seaexplorer/sea019/m54/raw/"
g <- read.glider.seaexplorer.raw(dir)

head(g@data$payload[g@data$payload$yoNumber==888,])
system(paste0("head ", dir, "sea019.54.pld1.raw.888"))

head(g@data$payload[g@data$payload$yoNumber==889,])
system("head ../data/sea019/m54/all_data/sea019.54.pld1.raw.889")

head(g@data$payload[g@data$payload$yoNumber==890,])
system("head ../data/sea019/m54/all_data/sea019.54.pld1.raw.890")


