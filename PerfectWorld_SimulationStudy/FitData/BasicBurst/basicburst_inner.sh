#!/bin/bash

WORKDIR=/storage/groups/biostat01/projects/Lisa_ProbDistr/
cd $WORKDIR

i=${SLURM_ARRAY_TASK_ID}

Rscript PerfectWorld/FitData/BasicBurst/FitData_GOF_BasicBurst_final.R ${i}