#!/bin/sh
#$ -e /home/icb/harsha.kumar/pois_err.out
#$ -o /home/icb/harsha.kumar/pois.out
#$ -hard
#$ -l job_mem=8G
#$ -t 1-1000
WORKDIR=/home/icb/harsha.kumar/genes/storage
# INFILE=$WORKDIR/data.$SGE_TASK_ID
# OUTFILE=$WORKDIR/data.$SGE_TASK_ID.out
# See comment below about paths to R
PATHTOR=/usr/bin
# if [ -e $OUTFILE ]
# then
# rm -f $OUTFILE
# fi
# Below, the phrase "EOF" marks the beginning and end of the HERE document.
# Basically, what’s going on is that we’re running R, and suppressing all of
# it’s output to STDOUT, and then redirecting whatever’s between the EOF words
# as an R script, and using variable substitution to act on the desired files.
$PATHTOR/R --quiet --no-save > /dev/null <<EOF
d <- readRDS("$WORKDIR/ge.rds")
outputFile <- "$WORKDIR/pois_fits.rds"
if(file.exists(outputFile)) {
  pois_fits <- readRDS(outputFile)
  all_ids <- setdiff(rownames(d), names(pois_fits))
} else {
  all_ids <- rownames(d)
}
n <- length(all_ids)
block_size <- n %/% 1000 + 1
for(i in 1:block_size) {
  if(1000*(i-1)+$SGE_TASK_ID <= n) {
    id <- all_ids[1000*(i-1)+$SGE_TASK_ID]
    set.seed(1000*(i-1)+$SGE_TASK_ID)
    f <- scModels::fit_params(d[id,], "pois")
    saveRDS(f, file=paste0("$WORKDIR/pois_fits/",id,".rds"))
  }
}
EOF
