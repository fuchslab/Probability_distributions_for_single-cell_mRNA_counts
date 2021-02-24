#!/bin/bash
#SBATCH --output=/storage/groups/biostat01/projects/Lisa_ProbDistr/PerfectWorld/FitData/Burst/output/Fit_arrayJob_%A_%a.out
#SBATCH --error=/storage/groups/biostat01/projects/Lisa_ProbDistr/PerfectWorld/FitData/Burst/output/Fit_arrayJob_%A_%a.err
#SBATCH -J Burst_Fit_GOF
#SBATCH -p icb_cpu
#SBATCH -x icb-neu-00[1-3] 
#SBATCH --cpus-per-task=1
#SBATCH --mem=500MB
#SBATCH -t 2-00:00:00
#SBATCH --nice=10000
#SBATCH --array=1-1000


# This bash script can be submitted to SLURM with 
# `sbatch burst.sh`
# and needs the file burst_inner.sh

echo job name: $SLURM_JOB_NAME
echo
echo job id: $SLURM_JOB_ID
echo
echo node name: $SLURMD_NODENAME

# create a local folder for your own user after check if it already exists
# -p: don't report error if it already exists
if [ ! -d "/localscratch/${USER}/r3.5.2packages_scmodels/" ]  
then 
   mkdir -p /localscratch/${USER}/tmp
 
 # extract the container image from a tarball
 ch-tar2dir /home/icb/lisa.amrhein/Charliecloudcontainer/r3.5.2packages_scmodels.tar.gz   /localscratch/${USER} 
 else
 # wait for 5 minutes, if jobs were submitted together, the other job needs time to extract image
 sleep 300
fi

# start a charliecloud container;
# the user home directory seems to be mounted automatically; however,
# note that the path to the "user home" folder does not contain "/icb" 
ch-run -b /storage/groups/:/storage/groups/  -b /storage/groups/:/storage/groups  /localscratch/${USER}/r3.5.2packages_scmodels/ -- /bin/bash /storage/groups/biostat01/projects/Lisa_ProbDistr/PerfectWorld/FitData/Burst/burst_inner.sh

# once done, remove directory again
# but I will leave it as I will work with it more often
# rm -rf /localscratch/${USER}/

