#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine nb2 results into a single file
nb2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/nb2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    nb2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(nb2_fits, file="storage/mm10/nb2_fits.rds")

print(paste0("Combined ", length(nb2_fits), " gene files.."))
