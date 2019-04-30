library("scModels")

ge <- readRDS("Nestorowa.rds")
load(file="DistributionFits/GOF/Result_Nestorowa_BIC_GOF.rda")
load(file="DistributionFits/GOF/GOF_input.rds")

# Selected Marker Genes
ids <- c("Csf1r","Ccl5", "Prss34","H2-Aa", "Gfi1b")


tab_res_BIC <- matrix(NA, ncol = 5, nrow = 17)

colnames(tab_res_BIC) <-ids
rownames(tab_res_BIC) <- c("BIC_Pois","BIC_ZIPois","BIC_Pois2","BIC_ZIPois2","BIC_NB","BIC_ZINB","BIC_NB2","BIC_ZINB2","BIC_PB","BIC_ZIPB","BIC_PB2","BIC_ZIPB2","selected model","GOF x2 test" , "Fraction of zeros", "Fraction of ones", "Fraction of counts > 1")

for (j in ids){
    load(paste0( "ExampleGenes/",j, "_res.rda"))
    x <- seq(0, max(ge[j,]))
    win <-  which(BIC==min(BIC))
    tab_res_BIC[1:12, j] <- BIC
    tab_res_BIC[13, j] <-  rownames(BIC)[win]
    tab_res_BIC[14, j] <-  Nestorowa_GOF[j,"unlist(GOF_x2)"]
    tab_res_BIC[15, j]<- round(sum(ge[j,]==0)/length(ge[j,]),3)
    tab_res_BIC[16, j]<- round(sum(ge[j,]==1)/length(ge[j,]),3)
    tab_res_BIC[17, j]<- round(sum(ge[j,]>=2)/length(ge[j,]),3)
}


tab_res_BIC
