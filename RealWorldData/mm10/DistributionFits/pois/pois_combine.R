#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine pois results into a single file
pois_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/pois_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    pois_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(pois_fits, file="storage/mm10/pois_fits.rds")

print(paste0("Combined ", length(pois_fits), " gene files.."))
