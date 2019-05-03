#!/bin/sh
#$ -e /home/icb/harsha.kumar/scModels.err
#$ -o /home/icb/harsha.kumar/scModels.out
#$ -hard
#$ -l job_mem=1G
#$ -t 1-4

WORKDIR=/home/icb/harsha.kumar/package_comparison
PATHTOR=/usr/bin

cd $WORKDIR

$PATHTOR/R --quiet --no-save <<EOF
load("sim_data.Rda")
s <- as.numeric(Sys.time())
set.seed(s)
t_estim <- system.time(estim <- scModels:::estimate_pb_optim_init(dataset$SGE_TASK_ID))
fit <- scModels::fit_params(x = dataset$SGE_TASK_ID, type = "pb", optim_control = list(maxit=1000))
save(s, t_estim, estim, fit, file = "pb_results$SGE_TASK_ID.Rda")
EOF
