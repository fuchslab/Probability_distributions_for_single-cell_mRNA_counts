#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine pois2 results into a single file
pois2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/pois2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    pois2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(pois2_fits, file="storage/mm10/pois2_fits.rds")

print(paste0("Combined ", length(pois2_fits), " gene files.."))
