# Results Fits for GO Analyse

pois_fits<-readRDS("pois_fits.rds")
nb_fits<-readRDS("nb_fits.rds")
pb_fits<-readRDS("pb_fits.rds")
pig_fits<-readRDS("pig_fits.rds")
del_fits<-readRDS("del_fits.rds")
zipois_fits<-readRDS("zipois_fits.rds")
zinb_fits<-readRDS("zinb_fits.rds")
zipb_fits<-readRDS("zipb_fits.rds")
zipig_fits<-readRDS("zipig_fits.rds")
zidel_fits<-readRDS("zidel_fits.rds")
pois2_fits<-readRDS("pois2_fits.rds")
nb2_fits<-readRDS("nb2_fits.rds")
pb2_fits<-readRDS("pb2_fits.rds")
pig2_fits<-readRDS("pig2_fits.rds")
del2_fits<-readRDS("del2_fits.rds")
zipois2_fits<-readRDS("zipois2_fits.rds")
zinb2_fits<-readRDS("zinb2_fits.rds")
zipb2_fits<-readRDS("zipb2_fits.rds")
zipig2_fits<-readRDS("zipig2_fits.rds")
zidel2_fits<-readRDS("zidel2_fits.rds")



pois_fits<-readRDS("pois/pois_fits.rds")
nb_fits<-readRDS("nb/nb_fits.rds")
pb_fits<-readRDS("pb/pb_fits.rds")
pig_fits<-readRDS("pig/pig_fits.rds")
del_fits<-readRDS("del/del_fits.rds")
zipois_fits<-readRDS("zipois/zipois_fits.rds")
zinb_fits<-readRDS("zinb/zinb_fits.rds")
zipb_fits<-readRDS("zipb/zipb_fits.rds")
zipig_fits<-readRDS("zipig/zipig_fits.rds")
zidel_fits<-readRDS("zidel/zidel_fits.rds")
pois2_fits<-readRDS("pois2/pois2_fits.rds")
nb2_fits<-readRDS("nb2/nb2_fits.rds")
pb2_fits<-readRDS("pb2/pb2_fits.rds")
pig2_fits<-readRDS("pig2/pig2_fits.rds")
del2_fits<-readRDS("del2/del2_fits.rds")
zipois2_fits<-readRDS("zipois2/zipois2_fits.rds")
zinb2_fits<-readRDS("zinb2/zinb2_fits.rds")
zipb2_fits<-readRDS("zipb2/zipb2_fits.rds")
zipig2_fits<-readRDS("zipig2/zipig2_fits.rds")
zidel2_fits<-readRDS("zidel2/zidel2_fits.rds")

load(file="GOF/Result_mm10_BIC_GOF.rda")
load(fle="DistributionFits/Overview_mm10.rds")


id <-  rownames(Overview_mm10)[1]
Fits_mm10_all <- data.frame(BIC_pois_all = min(as.numeric(c(Overview_mm10[id, ]["Pois_BIC"],Overview_mm10[id, ]["ZIPois_BIC"], Overview_mm10[id, ]["Pois2_BIC"], Overview_mm10[id, ]["ZIPois2_BIC"]))),
                            BIC_nb_all = min(as.numeric(c(Overview_mm10[id, ]["NB_BIC"],Overview_mm10[id, ]["ZINB_BIC"], Overview_mm10[id, ]["NB2_BIC"], Overview_mm10[id, ]["ZINB2_BIC"]))),
                            BIC_pb_all = min(as.numeric(c(Overview_mm10[id, ]["PB_BIC"],Overview_mm10[id, ]["ZIPB_BIC"], Overview_mm10[id, ]["PB2_BIC"], Overview_mm10[id, ]["ZIPB2_BIC"]))),
                            BIC_pig_all = min(as.numeric(c(Overview_mm10[id, ]["PIG_BIC"],Overview_mm10[id, ]["ZIPIG_BIC"], Overview_mm10[id, ]["PIG2_BIC"], Overview_mm10[id, ]["ZIPIG2_BIC"]))),
                            BIC_del_all = min(as.numeric(c(Overview_mm10[id, ]["DEL_BIC"],Overview_mm10[id, ]["ZIDEL_BIC"], Overview_mm10[id, ]["DEL2_BIC"], Overview_mm10[id, ]["ZIDEL2_BIC"]))),
                            BIC_smallest = if(grepl("Pois",Overview_mm10[id, ]["modelname"])) "Pois" else if(grepl("nb",Overview_mm10[id, ]["modelname"])) "nb" else if(grepl("pig",Overview_mm10[id, ]["modelname"])) "pig" else if(grepl("del",Overview_mm10[id, ]["modelname"])) "del"else if(grepl("pb",Overview_mm10[id, ]["modelname"])) "pb",
                            x2P = mm10_GOF[id,"unlist(GOF_x2)"] ,
                            x2_adj = mm10_GOF[id, ]["x2_adj"]
                            )



for(id in rownames(Overview_mm10)[-1]){
  Fits_mm10_new <- data.frame(BIC_pois_all = min(as.numeric(c(Overview_mm10[id, ]["Pois_BIC"],Overview_mm10[id, ]["ZIPois_BIC"], Overview_mm10[id, ]["Pois2_BIC"], Overview_mm10[id, ]["ZIPois2_BIC"]))),
                          BIC_nb_all = min(as.numeric(c(Overview_mm10[id, ]["NB_BIC"],Overview_mm10[id, ]["ZINB_BIC"], Overview_mm10[id, ]["NB2_BIC"], Overview_mm10[id, ]["ZINB2_BIC"]))),
                          BIC_pb_all = min(as.numeric(c(Overview_mm10[id, ]["PB_BIC"],Overview_mm10[id, ]["ZIPB_BIC"], Overview_mm10[id, ]["PB2_BIC"], Overview_mm10[id, ]["ZIPB2_BIC"]))),
                          BIC_pig_all = min(as.numeric(c(Overview_mm10[id, ]["PIG_BIC"],Overview_mm10[id, ]["ZIPIG_BIC"], Overview_mm10[id, ]["PIG2_BIC"], Overview_mm10[id, ]["ZIPIG2_BIC"]))),
                          BIC_del_all = min(as.numeric(c(Overview_mm10[id, ]["DEL_BIC"],Overview_mm10[id, ]["ZIDEL_BIC"], Overview_mm10[id, ]["DEL2_BIC"], Overview_mm10[id, ]["ZIDEL2_BIC"]))),
                          BIC_smallest = if(grepl("ois",Overview_mm10[id, ]["modelname"])) "Pois" else if(grepl("nb",Overview_mm10[id, ]["modelname"])) "nb" else if(grepl("pig",Overview_mm10[id, ]["modelname"])) "pig" else if(grepl("del",Overview_mm10[id, ]["modelname"])) "del"else if(grepl("pb",Overview_mm10[id, ]["modelname"])) "pb",
                          x2P = mm10_GOF[id,"unlist(GOF_x2)"] ,
                          x2_adj = mm10_GOF[id, ]["x2_adj"]
  )
  Fits_mm10_all <- rbind(Fits_mm10_all, Fits_mm10_new)
}


save(Fits_mm10_all, Fits_mm10_all_DataFrame, file="Fits_mm10_all.rda")
