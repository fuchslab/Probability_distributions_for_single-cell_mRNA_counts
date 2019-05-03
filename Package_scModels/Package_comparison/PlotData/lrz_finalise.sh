#!/bin/sh
#$ -e /home/icb/harsha.kumar/finalise.err
#$ -o /home/icb/harsha.kumar/finalise.out
#$ -hard
#$ -l job_mem=1G
#$ -t 1-4

WORKDIR=/home/icb/harsha.kumar/package_comparison
PATHTOR=/usr/bin

cd $WORKDIR

$PATHTOR/R --quiet --no-save <<EOF
load("sim_data.Rda")
load("all_results.Rda")
i <- $SGE_TASK_ID
for(k in 1:8) {
  all_results[[i]][k,5] <- scModels::nlogL_pb(dataset$SGE_TASK_ID, all_results[[i]][k,1:3])
}
results$SGE_TASK_ID <- all_results[[i]]
plot_data$SGE_TASK_ID <- list()
for(r in rownames(all_results[[i]])) {
  plot_data$SGE_TASK_ID[[r]] <- scModels::dpb(0:max(dataset$SGE_TASK_ID), all_results[[i]][r,1], all_results[[i]][r,2], all_results[[i]][r,3])
}
save(results$SGE_TASK_ID, plot_data$SGE_TASK_ID, file="plot_data$SGE_TASK_ID.Rda")
EOF
