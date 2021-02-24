
#!/bin/bash

WORKDIR=/storage/groups/biostat01/projects/Lisa_ProbDistr/
cd $WORKDIR

i=${SLURM_ARRAY_TASK_ID}

Rscript PerfectWorld/FitData/Basic/FitData_GOF_Basic_final.R ${i}

