#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine zinb results into a single file
zinb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/zinb/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zinb_fits[[id]] <- f
    rm(f)
  }
}



saveRDS(zinb_fits, file="DistributionFits/zinb/zinb_fits.rds")

print(paste0("Combined ", length(zinb_fits), " gene files.."))
