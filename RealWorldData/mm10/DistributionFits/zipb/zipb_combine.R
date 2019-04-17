#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine zipb results into a single file
zipb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/zipb_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zipb_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(zipb_fits, file="storage/mm10/zipb_fits.rds")

print(paste0("Combined ", length(zipb_fits), " gene files.."))
