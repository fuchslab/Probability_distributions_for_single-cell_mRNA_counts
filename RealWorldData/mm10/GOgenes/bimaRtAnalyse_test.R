# biomaRt http://bioconductor.org/packages/release/bioc/html/biomaRt.html

library(biomaRt)
mart <- useMart(biomart ="ensembl", dataset="mmusculus_gene_ensembl")
mart2 <- useMart(biomart ="ensembl", dataset="mmusculus_gene_ensembl",host = "www.ensembl.org")


load(file="Fits_mm10_all.rda")

Pois_prev <-colnames(Fits_mm10_all_DataFrame)[Fits_mm10_all_DataFrame["BIC_smallest",]=="pois"]
Pois <- substr(Pois_prev, 6,23 )

Pois_prev_gof <- colnames(Fits_mm10_all_DataFrame)[Fits_mm10_all_DataFrame["BIC_smallest",]=="pois" & Fits_mm10_all_DataFrame["x2_adj",]==TRUE]
Pois_gof <- substr(Pois_prev_gof, 6,23 )

Non_Pois_prev <- colnames(Fits_mm10_all_DataFrame)[Fits_mm10_all_DataFrame["BIC_smallest",]!="pois"]
Non_Pois <- substr(Non_Pois_prev, 6,23 )

Non_Pois_prev_gof <- colnames(Fits_mm10_all_DataFrame)[Fits_mm10_all_DataFrame["BIC_smallest",]!="pois"& Fits_mm10_all_DataFrame["x2_adj",]==TRUE]
Non_Pois_gof <- substr(Non_Pois_prev_gof, 6,23 )

# Get all the GO Terms a Gene is present in
GO_terms <- list()
for(i in 1:length(Pois)){
    GO_terms[[i]] <- getBM(attributes = c("go_id","name_1006","definition_1006","namespace_1003"), filters="ensembl_gene_id", values = Pois[i], mart = mart)
    names(GO_terms)[i] <- Pois[i]
}


GO_terms_gof <- list()
for(i in Pois_gof){
    GO_terms_gof[[i]] <- GO_terms[[i]]
}

# Count in how many GO Terms the Gene is present
GO_terms_Nr <- c()
for(i in Pois){
    GO_terms_Nr[[i]]<-length(GO_terms[[i]]$go_id)
}

GO_terms_Nr_gof <- c()
for(i in Pois_gof){
    GO_terms_Nr_gof[[i]] <- GO_terms_Nr[[i]]
}

### Get the same information for all Non-Poisson Genes.

#Sometimes the mapping didn_t work
# For those we don't asked for the definition_1006 826 2544 2584 3694
# Those had to be started manually again because of biomaRT server error:
# 3705 3842  3843 3865 3868 3873 3880 3903 3909 3923(einfach nochmal ausführen geht)
GO_terms_NP <- list()
for(i in 1:length(Non_Pois)){
    GO_terms_NP[[i]] <- getBM(attributes = c("go_id","name_1006","definition_1006","namespace_1003"), filters="ensembl_gene_id", values = Non_Pois[i], mart = mart)
    names(GO_terms_NP)[i] <- Non_Pois[i]
}

GO_terms_NP_gof <- list()
for(i in Non_Pois_gof){
    GO_terms_NP_gof[[i]]<-GO_terms_NP[[i]]
}

# Again get the number of GO Terms
GO_terms_NP_Nr <- c()
for(i in Non_Pois){
    GO_terms_NP_Nr[[i]]<-length(GO_terms_NP[[i]]$go_id)
}

GO_terms_NP_Nr_gof <- c()
for(i in Non_Pois_gof){
    GO_terms_NP_Nr_gof[[i]]<-GO_terms_NP_Nr[[i]]
}

save(GO_terms, GO_terms_NP, GO_terms_gof, GO_terms_NP_gof,file="GO_Terms_Pois_Non_Pois.rda")


# First Analysis of GO Terms of Biological Process
# Names of 30 children GO processes:

GO_BP30 <- c("GO:0022414","GO:0032501","GO:0009987","GO:0008283","GO:0007610","GO:0032502","GO:0008152","GO:0002376","GO:0051704","GO:0050896","GO:0040007","GO:0051179","GO:0099531","GO:0040011","GO:0023052","GO:0098754","GO:0000003","GO:0044848","GO:0043473","GO:0048511","GO:0022610","GO:0065007","GO:0001906","GO:0098743","GO:0071840","GO:0019740","GO:0009758","GO:0015976","GO:0006794","GO:0006791")
GO_name_BP30 <- c("reproductive process","multicellular organismal process","cellular process","cell proliferation","behavior","developmental process","metabolic process","immune system process","multi-organism process","response to stimulus","growth","localization","presynaptic process involved in chemical synaptic transmission","locomotion","signaling","detoxification","reproduction","biological phase","pigmentation","rhythmic process","biological adhesion","biological regulation","cell killing","cell aggregation","cellular component organization or biogenesis","nitrogen utilization","carbohydrate utilization","carbon utilization","phosphorus utilization","sulfur utilization")

# Next Analysis of GO Terms of Molecular Function
# Names of 15 children GO processes:
GO_MF15 <- c("GO:0005488,","GO:0005198","GO:0003824","GO:0004871","GO:0038024","GO:0016209","GO:0140104","GO:0005215","GO:0045182","GO:0140110","GO:0098772","GO:0104005","GO:0045735","GO:0031386","GO:0090729")
GO_name_MF15 <- c("binding","structural molecule activity","catalytic activity","","cargo receptor activity","antioxidant activity","molecular carrier activity","transporter activity","translation regulator activity","transcription regulator activity","molecular function regulator","hijacked molecular function","nutrient reservoir activity","protein tag","toxin activity")


# install: GOfuncR  http://bioconductor.org/packages/release/bioc/html/GOfuncR.html to get the parents of a GO Terms
# here we are only interessted in the highest hierarchy (directly under biological process and molecular dunction respectively)

Parents <- list()
Which_BP30 <- list()
Which_MF15 <- list()
for(i in Pois){
Parents[[i]] <- try(GOfuncR::get_parent_nodes(GO_terms[[i]]$go_id, term_df = NULL, graph_path_df = NULL, godir = NULL))
Which_BP30[[i]] <- try(GO_BP30[GO_BP30 %in% Parents[[i]]$parent_go_id])
Which_MF15[[i]] <- try(GO_MF15[GO_MF15 %in% Parents[[i]]$parent_go_id])
}

Parents_NP <- list()
Which_BP30_NP <- list()
Which_MF15_NP <- list()
for(i in Non_Pois){
    Parents_NP[[i]] <- try(GOfuncR::get_parent_nodes(GO_terms_NP[[i]]$go_id, term_df = NULL, graph_path_df = NULL, godir = NULL))
    Which_BP30_NP[[i]] <- try(GO_BP30[GO_BP30 %in% Parents_NP[[i]]$parent_go_id])
    Which_MF15_NP[[i]] <- try(GO_MF15[GO_MF15 %in% Parents_NP[[i]]$parent_go_id])
}

#Again the same for genes that passed GOF

Parents_gof <- list()
Which_BP30_gof <- list()
Which_MF15_gof <- list()
for(i in Pois_gof){
Parents_gof[[i]] <- Parents[[i]]
Which_BP30_gof[[i]] <- Which_BP30[[i]]
Which_MF15_gof[[i]] <- Which_MF15[[i]]
}

Parents_NP_gof <- list()
Which_BP30_NP_gof <- list()
Which_MF15_NP_gof <- list()
for(i in Non_Pois_gof){
    Parents_NP_gof[[i]] <- Parents_NP[[i]]
    Which_BP30_NP_gof[[i]] <- Which_BP30_NP[[i]]
    Which_MF15_NP_gof[[i]] <- Which_MF15_NP[[i]]
}

save(Parents,Parents_NP,Which_BP30,Which_BP30_NP,Which_MF15,Which_MF15_NP,Parents_gof,Parents_NP_gof,Which_BP30_gof,Which_BP30_NP_gof,Which_MF15_gof,Which_MF15_NP_gof, file="BP30_MF15_Parents_Which_NP.rda")


# In how many "highest BPs" and "highest MF" is each gene contained?
BP30_Nr <- c()
MF15_Nr <- c()
for(i in Pois){
BP30_Nr[[i]] <- try(length(Which_BP30[[i]]))
MF15_Nr[[i]] <- try(length(Which_MF15[[i]]))
}

BP30_Nr_NP <- c()
MF15_Nr_NP <- c()
for(i in Non_Pois){
    BP30_Nr_NP[[i]] <- try(length(Which_BP30_NP[[i]]))
    MF15_Nr_NP[[i]] <- try(length(Which_MF15_NP[[i]]))
}

# same for after GOF
BP30_Nr_gof <- c()
MF15_Nr_gof <- c()
for(i in Pois){
    BP30_Nr_gof[[i]] <- try(length(Which_BP30_gof[[i]]))
    MF15_Nr_gof[[i]] <- try(length(Which_MF15_gof[[i]]))
}


BP30_Nr_NP_gof <- c()
MF15_Nr_NP_gof <- c()
for(i in Non_Pois){
    BP30_Nr_NP_gof[[i]] <- try(length(Which_BP30_NP_gof[[i]]))
    MF15_Nr_NP_gof[[i]] <- try(length(Which_MF15_NP_gof[[i]]))
}

save(BP30_Nr,BP30_Nr_NP,MF15_Nr,MF15_Nr_NP,BP30_Nr_gof,BP30_Nr_NP_gof,MF15_Nr_gof,MF15_Nr_NP_gof, file="BP30_MF15_Nr_NP.rda")

table(BP30_Nr)
#BP30_Nr
# 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 17
#41 22 46 51 59 27 30 26 14 19 10  7  8  1  3  2  2

table(MF15_Nr)
# MF15_Nr
#   0   1   2   3
# 182 160  23   3

table(BP30_Nr_gof)
# BP30_Nr_gof
#  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 17
# 43 22 46 51 58 27 30 26 14 18 10  7  8  1  3  2  2

table(MF15_Nr_gof)
# MF15_Nr_gof
#   0   1   2   3
# 184 158  23   3

### same for Non Poisson Genes
table(BP30_Nr_NP)
# BP30_Nr_NP
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
# 343 225 535 587 415 351 300 239 257 182 130 116  91  69  28  24  20  11   6

table(MF15_Nr_NP)
# MF15_Nr_NP
#    0    1    2    3    4    5
# 1675 1943  286   23    1    1

table(BP30_Nr_NP_gof)
# BP30_Nr_NP_gof
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
# 434 223 511 566 406 345 290 234 256 179 128 113  89  66  28  24  20  11   6

table(MF15_Nr_NP_gof)
# MF15_Nr_NP_gof
#    0    1    2    3    4    5
# 1749 1882  273   23    1    1

# GO of higest hierarchy and number of contained genes

table(unlist(Which_BP30))
table(unlist(Which_MF15))

table(unlist(Which_BP30_NP))
table(unlist(Which_MF15_NP))


100/368 *3929
50/368 *3929
30/368 *3929
1/368 *3929
round(table(unlist(Which_BP30))/368*100,2)
round(table(unlist(Which_MF15))/368*100,2)

round(table(unlist(Which_BP30_NP))/3929*100,2)
round(table(unlist(Which_MF15_NP))/3929*100,2)

#same after GOF
table(unlist(Which_BP30_gof))
table(unlist(Which_MF15_gof))

table(unlist(Which_BP30_NP_gof))
table(unlist(Which_MF15_NP_gof))


100/366 *3837
50/366 *3837
30/366 *3837
1/366 *3837
round(table(unlist(Which_BP30))/366*100,2)
round(table(unlist(Which_MF15))/366*100,2)

round(table(unlist(Which_BP30_NP))/3837*100,2)
round(table(unlist(Which_MF15_NP))/3837*100,2)


# Barplots of higest hierarchy
A <- factor(BP30_Nr, levels = 0:18)
svg(paste0( "Barplot_Pois_BP30.svg"),  width = 7, height = 8)
barplot(table(A))
dev.off()

A_NP <- factor(BP30_Nr_NP, levels = 0:18)
svg(paste0( "Barplot_NonPois_BP30.svg"),  width = 7, height = 8)
barplot(table(A_NP))
dev.off()

B <- factor(MF15_Nr, levels= 0:5)
svg(paste0( "Barplot_Pois_MF15.svg"),  width = 7, height = 8)
barplot(table(B))
dev.off()

B_NP <- factor(MF15_Nr_NP, levels= 0:5)
svg(paste0( "Barplot_NonPois_MF15.svg"),  width = 7, height = 8)
barplot(table(B_NP))
dev.off()

# after GOF
A_gof <- factor(BP30_Nr_gof, levels = 0:18)
svg(paste0( "Barplot_Pois_BP30_gof.svg"),  width = 7, height = 8)
barplot(table(A_gof))
dev.off()

A_NP_gof <- factor(BP30_Nr_NP_gof, levels = 0:18)
svg(paste0( "Barplot_NonPois_BP30_gof.svg"),  width = 7, height = 8)
barplot(table(A_NP_gof))
dev.off()

B_gof <- factor(MF15_Nr_gof, levels= 0:5)
svg(paste0( "Barplot_Pois_MF15_gof.svg"),  width = 7, height = 8)
barplot(table(B_gof))
dev.off()

B_NP_gof<- factor(MF15_Nr_NP_gof, levels= 0:5)
svg(paste0( "Barplot_NonPois_MF15_gof.svg"),  width = 7, height = 8)
barplot(table(B_NP_gof))
dev.off()


##







### In how many original GO terms where the genes contained?
svg(paste0( "Barplot_Pois_allGO.svg"),  width = 7, height = 8)
barplot(table(factor(GO_terms_Nr, levels=0:269)))
dev.off()

svg(paste0( "Barplot_NonPois_allGO.svg"),  width = 7, height = 8)
barplot(table(factor(GO_terms_NP_Nr, levels=0:269)) )
dev.off()

plot(density(GO_terms_Nr, bw = 2))
lines(density(GO_terms_NP_Nr, bw=2), col ="red")

#same for GOF

### In how many original GO terms where the genes contained?
svg(paste0( "Barplot_Pois_allGO_gof.svg"),  width = 7, height = 8)
barplot(table(factor(GO_terms_Nr_gof, levels=0:269)))
dev.off()

svg(paste0( "Barplot_NonPois_allGO_gof.svg"),  width = 7, height = 8)
barplot(table(factor(GO_terms_NP_Nr_gof, levels=0:269)) )
dev.off()

plot(density(GO_terms_Nr_gof, bw = 2))
lines(density(GO_terms_NP_Nr_gof, bw=2), col ="red")



#### Add on Hub Genes Plot Gene importance:

Connections_075<-read.table("network_075.tsv", sep ="\t")

Pois_Connections_075 <- rep(0, length(Pois))
for(i in 1:length(Pois)){
    Pois_Connections_075[i]<- sum(Connections_075 == Pois[i])
}

Non_Pois_Connections_075 <- rep(0, length(Non_Pois))
for(i in 1:length(Non_Pois)){
    Non_Pois_Connections_075[i]<- sum(Connections_075 == Non_Pois[i])
}


svg(paste0( "Hist_Pois_Importance_gof.svg"),  width = 7, height = 8)
hist(Pois_Connections_075,breaks = 500, col = rgb(131,139,139, max=255 ),  freq = TRUE)
dev.off()

svg(paste0( "Hist_Non_Pois_Importance_gof.svg"),  width = 7, height = 8)
hist(Non_Pois_Connections_075,breaks = 400, freq = TRUE,col = rgb(131,139,139, max=255 ))
dev.off()

