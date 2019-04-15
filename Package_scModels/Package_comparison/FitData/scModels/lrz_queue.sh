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
s <- as.numeric(Sys.time())
set.seed(s)
t_estim <- system.time(estim <- scModels:::estimate_pb_optim_init(dataset$SLURM_ARRAY_TASK_ID))
fit <- scModels::fit_params(x = dataset$SLURM_ARRAY_TASK_ID, type = "pb", optim_control = list(maxit=1000))
save(s, t_estim, estim, fit, file = "pb_results$SLURM_ARRAY_TASK_ID.Rda")
EOF
