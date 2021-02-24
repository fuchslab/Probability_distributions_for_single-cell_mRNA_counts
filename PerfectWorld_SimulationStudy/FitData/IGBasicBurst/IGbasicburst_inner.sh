#!/bin/bash

WORKDIR=/storage/groups/biostat01/projects/Lisa_ProbDistr/
cd $WORKDIR

i=${SLURM_ARRAY_TASK_ID}

Rscript PerfectWorld/FitData/IGBasicBurst/FitData_GOF_IGBasicBurst_final.R ${i}