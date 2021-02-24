#!/bin/bash

WORKDIR=/storage/groups/biostat01/projects/Lisa_ProbDistr/
cd $WORKDIR

i=${SLURM_ARRAY_TASK_ID}

Rscript PerfectWorld/FitData/Burst/FitData_GOF_Burst_final.R ${i}