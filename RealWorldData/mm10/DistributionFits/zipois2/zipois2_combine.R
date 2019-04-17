#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine zipois2 results into a single file
zipois2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/zipois2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipois2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(zipois2_fits, file="storage/mm10/zipois2_fits.rds")

print(paste0("Combined ", length(zipois2_fits), " gene files.."))
