#!/bin/sh
echo It is now: $(date)
echo Running on machine: $(hostname)
echo Operating system: $(uname -r)
echo UGE Job ID: $SGE_TASK_ID
echo 
echo $R_LIBS
cd /storage/groups/biostat01/workspace/Lisa_ProbDistr/PerfectWorld/FitData/Basic
echo
echo "The current working directory: $PWD"
echo

echo input parameters:
echo $@

 Rscript FitData_GOF_Basic.R $@
#echo $@ >> test_combinations.txt
echo
echo Now it is: $(date)
echo "Fit and GOF are finished"

kill -9 `jobs -p`
echo and Im finished
echo
