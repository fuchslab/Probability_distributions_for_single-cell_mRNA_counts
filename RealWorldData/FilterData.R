######################################################################
# Filter Nestorowa Data
Cell_Names <- read.table(file="cell_names_nestorowa_data.txt")  #1646
Gene_Names <- read.table(file="gene_names_nestorowa_data.txt")   #4448
raw_count_data <- read.table(file="raw_count_data.txt",sep=" ")

RD <- as.matrix(raw_count_data )

dim(RD)
RS <- rowSums(RD)

# Select only genes that are in mean expressed in every cell
filter1 <- rowMeans(RD) > 1

# Select only genes which have at least 20 entries higher that 5 reads
filter2 <- apply(RD, 1,function(x) sum(x>5)>=20)
filter <- filter1*filter2

ge <- RD[which(filter == 1),]
dim(ge)

saveRDS(ge, "Nestorowa.rds")

############################################################################
# Filter mm10:10x data:
t <- Matrix::readMM("matrix.mtx")
b <- read.table("barcodes.tsv")
g <- read.table("genes.tsv")

ge <- as.matrix(t)
rownames(ge) <- as.vector(g$V1)

# select only genes that are expressed
ge <- ge[which(rowSums(ge) > 0),]

# select only cells that express ore than 1500 genes
ge <- ge[,which(colSums(ge > 0) > 1500)]

# select only genes which have at least 10 entries higher that 3 UMIs
ge <- ge[which(rowSums(ge > 3) > 10),]


saveRDS(ge, "mm10.rds")
