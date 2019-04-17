#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine zipois results into a single file
zipois_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/zipois_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipois_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(zipois_fits, file="storage/mm10/zipois_fits.rds")

print(paste0("Combined ", length(zipois_fits), " gene files.."))
