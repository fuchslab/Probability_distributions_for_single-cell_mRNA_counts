#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine pb2 results into a single file
pb2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/pb2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    pb2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(pb2_fits, file="storage/mm10/pb2_fits.rds")

print(paste0("Combined ", length(pb2_fits), " gene files.."))
