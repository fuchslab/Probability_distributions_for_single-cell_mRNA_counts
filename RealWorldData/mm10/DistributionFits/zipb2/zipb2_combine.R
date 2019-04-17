#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine zipb2 results into a single file
zipb2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/zipb2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipb2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(zipb2_fits, file="storage/mm10/zipb2_fits.rds")

print(paste0("Combined ", length(zipb2_fits), " gene files.."))
