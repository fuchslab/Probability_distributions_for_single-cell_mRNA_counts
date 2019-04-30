#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine zipb results into a single file
zipb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/zipb/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipb_fits[[id]] <- f
    rm(f)
  }
}

saveRDS(zipb_fits, file="DistributionFits/zipb/zipb_fits.rds")

print(paste0("Combined ", length(zipb_fits), " gene files.."))
