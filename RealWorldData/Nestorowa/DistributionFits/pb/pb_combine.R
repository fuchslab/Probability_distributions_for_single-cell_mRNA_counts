#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine pb results into a single file
pb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/pb/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    pb_fits[[id]] <- f
    rm(f)
  }
}


saveRDS(pb_fits, file="DistributionFits/pb/pb_fits.rds")

print(paste0("Combined ", length(pb_fits), " gene files.."))
