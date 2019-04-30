#!/usr/bin/env Rscript
d <- readRDS("Nestorowa.rds")

## function to combine nb results into a single file
nb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("DistributionFits/nb/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    nb_fits[[id]] <- f
    rm(f)
  }
}

saveRDS(nb_fits, file="DistributionFits/nb/nb_fits.rds")

print(paste0("Combined ", length(nb_fits), " gene files.."))
