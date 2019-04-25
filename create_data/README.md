---
title: Identifying a sequence of files for inclusion in package
author: Dan Kelley
date: 2019 April 2
---

# Introduction

For `read.glider.seaexplorer.delayed()` to work, a sequence of glider yos must
start and end with yos that have some `navState` values equal to 116, which
means that the glide was at the surface and communicating.

I used system tools to find yo sequences. Somewhat arbitrarily, I started at yo
100.

# Realtime data

## gli files
```
zgrep -l ";[ ]*116[ ]*;" /Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.gli.sub.10[0-9].gz
```
yields
```
/Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.gli.sub.101.gz
/Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.gli.sub.106.gz
```

## pld1 files

Using
```
zgrep -l ";[ ]*116[ ]*;" /Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.pld1.sub.10[0-9].gz
```
yields
```
/Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.pld1.sub.100.gz
/Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.pld1.sub.101.gz
/Users/kelley/git/glider/data/sea019/m54/nav/logs/sea019.54.pld1.sub.106.gz
```

Conclusion: 101-106 is a sequence bounded by navState=116 conditions, i.e. conditions where a GPS reading could have been obtained.

# Delayed-mode data

## gli files

Using
```
grep -l ";[ ]*116[ ]*;" /Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.gli.sub.10[0-9]
```
yields
```
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.gli.sub.101
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.gli.sub.106
```

## pld1 files

Using
```
grep -l ";[ ]*116[ ]*;" /Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.10[0-9]
```
yields
```
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.100
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.101
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.102
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.104
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.106
/Users/kelley/git/glider/data/sea019/m54/all_data/sea019.54.pld1.raw.108
```

Conclusion: again, 101-106 is a sequence bounded by GPS-containing gli files.

# Conclusion

A sequence of the yos from 101 to 106 should provide a test case.

