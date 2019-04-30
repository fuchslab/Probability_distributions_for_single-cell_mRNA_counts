#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine zipois2 results into a single file
zipois2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/zipois2/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipois2_fits[[id]] <- f
    rm(f)
  }
}



saveRDS(zipois2_fits, file="DistributionFits/zipois2/zipois2_fits.rds")

print(paste0("Combined ", length(zipois2_fits), " gene files.."))
