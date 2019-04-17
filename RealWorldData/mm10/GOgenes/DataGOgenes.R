# Results Fits for GO Analyse

pois_fits<-readRDS("pois_fits.rds")
nb_fits<-readRDS("nb_fits.rds")
pb_fits<-readRDS("pb_fits.rds")
zipois_fits<-readRDS("zipois_fits.rds")
zinb_fits<-readRDS("zinb_fits.rds")
zipb_fits<-readRDS("zipb_fits.rds")
pois2_fits<-readRDS("pois2_fits.rds")
nb2_fits<-readRDS("nb2_fits.rds")
pb2_fits<-readRDS("pb2_fits.rds")
zipois2_fits<-readRDS("zipois2_fits.rds")
zinb2_fits<-readRDS("zinb2_fits.rds")
zipb2_fits<-readRDS("zipb2_fits.rds")

load(file="Result_mm10_BIC_GOF.rda")

Fits_mm10_all <- list()
for(id in names(Fits_mm10)){
    Fits_mm10_all[[id]]<- c(BIC_pois_all = min(as.numeric(c(Fits_mm10[[id]]["BIC_pois"],Fits_mm10[[id]]["BIC_zipois"], Fits_mm10[[id]]["BIC_pois2"], Fits_mm10[[id]]["BIC_zipois2"]))),
                            BIC_nb_all = min(as.numeric(c(Fits_mm10[[id]]["BIC_nb"],Fits_mm10[[id]]["BIC_zinb"], Fits_mm10[[id]]["BIC_nb2"], Fits_mm10[[id]]["BIC_zinb2"]))),
                            BIC_pb_all = min(as.numeric(c(Fits_mm10[[id]]["BIC_pb"],Fits_mm10[[id]]["BIC_zipb"], Fits_mm10[[id]]["BIC_pb2"], Fits_mm10[[id]]["BIC_zipb2"]))),
                            BIC_smallest = if(grepl("pois",Fits_mm10[[id]]["BIC_smallest"])) "pois" else if(grepl("nb",Fits_mm10[[id]]["BIC_smallest"])) "nb" else if(grepl("pb",Fits_mm10[[id]]["BIC_smallest"])) "pb",
                            x2_p = as.numeric(Fits_mm10[[id]]["x2_p"]) ,
                            Fits_mm10[[id]]["x2_adj"]
    )
}

Fits_mm10_all_DataFrame <- as.data.frame(Fits_mm10_all)

save(Fits_mm10_all, Fits_mm10_all_DataFrame, file="Fits_mm10_all.rda")
