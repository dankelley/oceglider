dir=/Users/kelley/git/glider/data
## realtime ("sub") data.
cp -f ${dir}/sea019/m54/nav/logs/sea019.54.gli.sub.10[1-6].gz ../inst/extdata/seaexplorer/sub
cp -f ${dir}/sea019/m54/nav/logs/sea019.54.pld1.sub.10[1-6].gz ../inst/extdata/seaexplorer/sub
## delayed ("raw") data
cp -f ${dir}/sea019/m54/all_data/sea019.54.gli.sub.10[1-6] ../inst/extdata/seaexplorer/raw
cp -f ${dir}/sea019/m54/all_data/sea019.54.pld1.raw.10[1-6] ../inst/extdata/seaexplorer/raw
gzip ../inst/extdata/seaexplorer/raw/*
## fix permissions, just in case
chmod -x ../inst/extdata/seaexplorer/sub/*
chmod -x ../inst/extdata/seaexplorer/raw/*

