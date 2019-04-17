#!/bin/bash
#SBATCH --output=/home/hpc/t1172/gu32hob2/genes/storage/log_mm10/arrayJob_%A_%a.out
#SBATCH --error=/home/hpc/t1172/gu32hob2/genes/storage/log_mm10/arrayJob_%A_%a.err
#SBATCH -J zipois
#SBATCH --get-user-env 
#SBATCH --clusters=serial 
#SBATCH --partition=serial_mpp2
#SBATCH --mail-user=lisa.amrhein@helmholtz-muenchen.de
#SBATCH --mem=1200
#SBATCH --export=NONE 
#SBATCH --time=96:00:00 
#SBATCH --array=1-1000
WORKDIR=$WORK/storage/mm10

module load r/3.5.0-X11-mkl

/usr/bin/env R --quiet --no-save > /dev/null <<EOF
d <- readRDS("$WORKDIR/ge.rds")
outputFile <- "$WORKDIR/zipois_fits.rds"

if(file.exists(outputFile)) {
  zipois_fits <- readRDS(outputFile)
  all_ids <- setdiff(rownames(d), names(zipois_fits))
} else {
  all_ids <- rownames(d)
}
n <- length(all_ids)
block_size <- n %/% 1000 + 1

for(i in 1:block_size) {
  if(1000*(i-1)+$SLURM_ARRAY_TASK_ID <= n) {
    id <- all_ids[1000*(i-1)+$SLURM_ARRAY_TASK_ID]
    t <- as.numeric(Sys.time())
    set.seed(t)
    f <- scModels::fit_params(d[id,], "zipois", optim_control=list(maxit=1000, reltol=1e-4))
    f['seed'] <- t
    saveRDS(f, file=paste0("$WORKDIR/zipois_fits/",id,".rds"))
}}
EOF
