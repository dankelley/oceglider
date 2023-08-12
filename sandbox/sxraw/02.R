# Want to test out the new read.glider.seaexplorer.raw() function

library(oceglider)

dir <- '/data/archive/glider/2019/sx/sea021m49/raw'

d <- read.glider.seaexplorer.raw(dir, yo=1:100, level=0)
plot(d, which=1)

d <- read.glider.seaexplorer.raw(dir)
plot(d, which=1)

plot(d, which=1, type='p', pch='.', ylim=c(250, 0),
     col=colormap(d[['temperature']])$zcol)

plot(d, which=1, type='p', pch='.', ylim=c(250, 0),
     col=colormap(d[['chlorophyl']], col=oceColorsChlorophyll, zlim=c(0, 4))$zcol)
