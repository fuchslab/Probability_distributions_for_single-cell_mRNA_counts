#!/bin/bash
#SBATCH --output=/home/hpc/t1172/ga87fil3/pc.out
#SBATCH --error=/home/hpc/t1172/ga87fil3/pc.err
#SBATCH -J package_comparison
#SBATCH --get-user-env 
#SBATCH --clusters=serial 
#SBATCH --partition=serial_mpp2
#SBATCH --mem=1200
#SBATCH --export=NONE 
#SBATCH --time=96:00:00 
#SBATCH --array=1-4
WORKDIR=$WORK/../gu32hob2/packages/package_comparison

cd $WORKDIR
module load r/3.5.0-X11-mkl

/usr/bin/env R --quiet --no-save > /dev/null <<EOF
load("sim_data.Rda")
load("all_results.Rda")
i <- $SLURM_ARRAY_TASK_ID
for(k in 1:8) {
  all_results[[i]][k,5] <- scModels::nlogL_pb(dataset$SLURM_ARRAY_TASK_ID, all_results[[i]][k,1:3])
}
plot_data$SLURM_ARRAY_TASK_ID <- list()
for(r in rownames(all_results[[i]])) {
  plot_data$SLURM_ARRAY_TASK_ID[[r]] <- scModels::dpb(dataset$SLURM_ARRAY_TASK_ID, all_results[[i]][r,1], all_results[[i]][r,2], all_results[[i]][r,3])
}
save(plot_data$SLURM_ARRAY_TASK_ID, file="plot_data$SLURM_ARRAY_TASK_ID.Rda")
EOF
