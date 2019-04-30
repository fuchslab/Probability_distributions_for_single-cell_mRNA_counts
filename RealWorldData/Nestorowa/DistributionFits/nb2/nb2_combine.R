#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine nb2 results into a single file
nb2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/nb2/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    nb2_fits[[id]] <- f
    rm(f)
  }
}


saveRDS(nb2_fits, file="DistributionFits/nb2/nb2_fits.rds")

print(paste0("Combined ", length(nb2_fits), " gene files.."))
