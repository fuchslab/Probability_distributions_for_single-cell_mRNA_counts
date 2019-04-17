#!/usr/bin/env Rscript
d <- readRDS("storage/mm10/ge.rds")
## function to combine nb results into a single file
nb_fits <- list()
for (id in rownames(d)) {
  fileName <- paste0("storage/mm10/nb_fits/", id, ".rds")
  if(file.exists(fileName)){
    f <- readRDS(fileName)
    nb_fits[[id]] <- f
    rm(f)
  }
}
saveRDS(nb_fits, file="storage/mm10/nb_fits.rds")

print(paste0("Combined ", length(nb_fits), " gene files.."))
