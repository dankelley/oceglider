## realtime ("sub") data.
cp -f /data/glider/2019/seaexplorer/sea019/m54/sub/sea019.54.gli.sub.[1-5].gz ../inst/extdata/seaexplorer/sub
cp -f /data/glider/2019/seaexplorer/sea019/m54/sub/sea019.54.pld1.sub.[1-5].gz ../inst/extdata/seaexplorer/sub
## delayed ("raw") data
cp -f /data/glider/2019/seaexplorer/sea019/m49/raw/sea019.54.gli.sub.[1-5].gz ../inst/extdata/seaexplorer/raw
cp -f /data/glider/2019/seaexplorer/sea019/m49/raw/sea019.54.pld1.sub.[1-5].gz ../inst/extdata/seaexplorer/raw
## fix permissions, just in case
chmod -x ../inst/extdata/seaexplorer/sub/*
chmod -x ../inst/extdata/seaexplorer/raw/*

