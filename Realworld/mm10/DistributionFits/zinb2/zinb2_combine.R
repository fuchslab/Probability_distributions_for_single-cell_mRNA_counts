#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine zinb2 results into a single file
zinb2_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/zinb2_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    zinb2_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(zinb2_fits, file="storage/mm10/zinb2_fits.rds")

print(paste0("Combined ", length(zinb2_fits), " gene files.."))
