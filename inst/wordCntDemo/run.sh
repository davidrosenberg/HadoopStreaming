#! /usr/bin/env sh
#export R_LIBS="pathToLocalRLibraryIfNeede"
cat anna.txt | ./hsWordCnt.R -m | sort | ./hsWordCnt.R -r


## do map reduce with a single reducer using GNU parallel:
ls * | parallel './hsWordCnt.R -m < {}' |sort | ./hsWordCnt.R -r
