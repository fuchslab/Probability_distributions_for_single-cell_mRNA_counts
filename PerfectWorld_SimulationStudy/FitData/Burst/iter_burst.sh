#!/bin/sh

for index in $(seq 1 1000);
    do
        :
        qsub -cwd \
        -e /storage/groups/biostat01/workspace/Lisa_ProbDistr/PerfectWorld/FitData/Burst/output/\
        -o /storage/groups/biostat01/workspace/Lisa_ProbDistr/PerfectWorld/FitData/Burst/output/ \
         -hard -l job_mem=8G -q long_fed25 \
         /storage/groups/biostat01/workspace/Lisa_ProbDistr/PerfectWorld/FitData/Burst/burst.sh $index

done
