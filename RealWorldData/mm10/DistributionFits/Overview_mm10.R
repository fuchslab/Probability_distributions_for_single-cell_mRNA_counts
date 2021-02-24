#goodness of fit tests mm10


main_data <- readRDS("mm10.rds")

setwd("DistributionFits")

pois_fits <- readRDS("pois/pois_fits.rds")
nb_fits <- readRDS("nb/nb_fits.rds")
pb_fits <- readRDS("pb/pb_fits.rds")
pig_fits <- readRDS("pig/pig_fits.rds")
del_fits <- readRDS("del/del_fits.rds")
pois2_fits <- readRDS("pois2/pois2_fits.rds")
nb2_fits <- readRDS("nb2/nb2_fits.rds")
pb2_fits <- readRDS("pb2/pb2_fits.rds")
pig2_fits <- readRDS("pig2/pig2_fits.rds")
del2_fits <- readRDS("del2/del2_fits.rds")
zipois_fits <- readRDS("zipois/zipois_fits.rds")
zinb_fits <- readRDS("zinb/zinb_fits.rds")
zipb_fits <- readRDS("zipb/zipb_fits.rds")
zipig_fits <- readRDS("zipig/zipig_fits.rds")
zidel_fits <- readRDS("zidel/zidel_fits.rds")
zipois2_fits <- readRDS("zipois2/zipois2_fits.rds")
zinb2_fits <- readRDS("zinb2/zinb2_fits.rds")
zipb2_fits <- readRDS("zipb2/zipb2_fits.rds")
zipig2_fits <- readRDS("zipig2/zipig2_fits.rds")
zidel2_fits <- readRDS("zidel2/zidel2_fits.rds")


#get lowest BIC per gene
getPar <- function(fits_list, id_check) {
    if(id_check %in% names(fits_list))
        return(fits_list[[id_check]]$par)
    else
        return(99999999)
}

getValue <- function(fits_list, id_check) {
    if(id_check %in% names(fits_list))
        return(fits_list[[id_check]]$value)
    else
        return(99999999)
}
getConv <- function(fits_list, id_check) {
    if(id_check %in% names(fits_list))
        return(fits_list[[id_check]]$convergence)
    else
        return(99999999)
}

getBIC <- function(fits_list, id_check) {
    if(id_check %in% names(fits_list))
        return(fits_list[[id_check]]$BIC)
    else
        return(99999999)
}


## compare pois-nb-pb
pois_fits <- Filter(function(x) x$convergence == 0, pois_fits)
nb_fits <- Filter(function(x) x$convergence == 0, nb_fits)
pb_fits <- Filter(function(x) x$convergence == 0, pb_fits)
pig_fits <- Filter(function(x) x$convergence == 0, pig_fits)
del_fits <- Filter(function(x) x$convergence == 0, del_fits)
list_base <- union(union(union(union(names(pois_fits), names(nb_fits)), names(pb_fits)), names(pig_fits)), names(del_fits))

zipois_fits <- Filter(function(x) x$convergence == 0, zipois_fits)
zinb_fits <- Filter(function(x) x$convergence == 0, zinb_fits)
zipb_fits <- Filter(function(x) x$convergence == 0, zipb_fits)
zipig_fits <- Filter(function(x) x$convergence == 0, zipig_fits)
zidel_fits <- Filter(function(x) x$convergence == 0, zidel_fits)
list_zi <- union(union(union(union(names(zipois_fits), names(zinb_fits)), names(zipb_fits)), names(zipig_fits)), names(zidel_fits))

pois2_fits <- Filter(function(x) x$convergence == 0, pois2_fits)
nb2_fits <- Filter(function(x) x$convergence == 0, nb2_fits)
pb2_fits <- Filter(function(x) x$convergence == 0, pb2_fits)
pig2_fits <- Filter(function(x) x$convergence == 0, pig2_fits)
del2_fits <- Filter(function(x) x$convergence == 0, del2_fits)
list_mix <- union(union(union(union(names(pois2_fits), names(nb2_fits)), names(pb2_fits)), names(pig2_fits)), names(del2_fits))

zipois2_fits <- Filter(function(x) x$convergence == 0, zipois2_fits)
zinb2_fits <- Filter(function(x) x$convergence == 0, zinb2_fits)
zipb2_fits <- Filter(function(x) x$convergence == 0, zipb2_fits)
zipig2_fits <- Filter(function(x) x$convergence == 0, zipig2_fits)
zidel2_fits <- Filter(function(x) x$convergence == 0, zidel2_fits)
list_zimix <- union(union(union(union(names(zipois2_fits), names(zinb2_fits)), names(zipb2_fits)), names(zipig2_fits)), names(zidel2_fits))


list_all <- union(union(union(list_base, list_zi), list_mix),list_zimix)


comparison_all <- c()

for (id in list_all) {
    t <- which.min(c(
        "pois" = getBIC(pois_fits, id),
        "zipois" = getBIC(zipois_fits, id),
        "pois2" = getBIC(pois2_fits, id),
        "zipois2" = getBIC(zipois2_fits, id),
        "nb" = getBIC(nb_fits, id),
        "zinb" = getBIC(zinb_fits, id),
        "nb2" = getBIC(nb2_fits, id),
        "zinb2" = getBIC(zinb2_fits, id),
        "pig" = getBIC(pig_fits, id),
        "zipig" = getBIC(zipig_fits, id),
        "pig2" = getBIC(pig2_fits, id),
        "zipig2" = getBIC(zipig2_fits, id),
        "del" = getBIC(del_fits, id),
        "zidel" = getBIC(zidel_fits, id),
        "del2" = getBIC(del2_fits, id),
        "zidel2" = getBIC(zidel2_fits, id),
        "pb" = getBIC(pb_fits, id),
        "zipb" = getBIC(zipb_fits, id),
        "pb2" = getBIC(pb2_fits, id),
        "zipb2" = getBIC(zipb2_fits, id)
    ))
    comparison_all[id] <- t
}

comparison_all_name <- c()

for (id in list_all) {
    t <- names(which.min(c(
        "pois" = getBIC(pois_fits, id),
        "zipois" = getBIC(zipois_fits, id),
        "pois2" = getBIC(pois2_fits, id),
        "zipois2" = getBIC(zipois2_fits, id),
        "nb" = getBIC(nb_fits, id),
        "zinb" = getBIC(zinb_fits, id),
        "nb2" = getBIC(nb2_fits, id),
        "zinb2" = getBIC(zinb2_fits, id),
        "pig" = getBIC(pig_fits, id),
        "zipig" = getBIC(zipig_fits, id),
        "pig2" = getBIC(pig2_fits, id),
        "zipig2" = getBIC(zipig2_fits, id),
        "del" = getBIC(del_fits, id),
        "zidel" = getBIC(zidel_fits, id),
        "del2" = getBIC(del2_fits, id),
        "zidel2" = getBIC(zidel2_fits, id),
        "pb" = getBIC(pb_fits, id),
        "zipb" = getBIC(zipb_fits, id),
        "pb2" = getBIC(pb2_fits, id),
        "zipb2" = getBIC(zipb2_fits, id)
    )))
    comparison_all_name[id] <- t
}




Pois_lambda <- c()
Pois_value <- c()
Pois_conv <- c()
Pois_BIC <- c()
NB_par <- matrix(NA,ncol=2,nrow=length(list_all))
rownames(NB_par) <- list_all
colnames(NB_par) <- c("size", "mu")
NB_value <- c()
NB_conv <- c()
NB_BIC <- c()
PIG_par <- matrix(NA,ncol=2,nrow=length(list_all))
rownames(PIG_par) <- list_all
colnames(PIG_par) <- c("mu", "sigma")
PIG_value <- c()
PIG_conv <- c()
PIG_BIC <- c()
DEL_par <- matrix(NA,ncol=3,nrow=length(list_all))
rownames(DEL_par) <- list_all
colnames(DEL_par) <- c("mu", "sigma","nu")
DEL_value <- c()
DEL_conv <- c()
DEL_BIC <- c()
PB_par <- matrix(NA,ncol=3,nrow=length(list_all))
rownames(PB_par) <- list_all
colnames(PB_par) <- c("alpha", "beta","c")
PB_value <- c()
PB_conv <- c()
PB_BIC <- c()


for (id in list_all){
    Pois_lambda[[id]] <- getPar(pois_fits, id)
    Pois_value[[id]] <- getValue(pois_fits, id)
    Pois_conv[[id]]<- getConv(pois_fits, id)
    Pois_BIC[[id]]<- getBIC(pois_fits, id)
}
for (id in list_all){
    NB_par[id,] <- getPar(nb_fits, id)
    NB_value[[id]] <- getValue(nb_fits, id)
    NB_conv[[id]]<- getConv(nb_fits, id)
    NB_BIC[[id]]<- getBIC(nb_fits, id)
}
for (id in list_all){
    PIG_par[id,] <- getPar(pig_fits, id)
    PIG_value[[id]] <- getValue(pig_fits, id)
    PIG_conv[[id]]<- getConv(pig_fits, id)
    PIG_BIC[[id]]<- getBIC(pig_fits, id)
}
for (id in list_all){
    PB_par[id,] <- getPar(pb_fits, id)
    PB_value[[id]] <- getValue(pb_fits, id)
    PB_conv[[id]]<- getConv(pb_fits, id)
    PB_BIC[[id]]<- getBIC(pb_fits, id)
}
for (id in list_all){
    DEL_par[id,] <- getPar(del_fits, id)
    DEL_value[[id]] <- getValue(del_fits, id)
    DEL_conv[[id]]<- getConv(del_fits, id)
    DEL_BIC[[id]]<- getBIC(del_fits, id)
}
###ZI
ZIPois_par <- matrix(NA,ncol=2,nrow=length(list_all))
rownames(ZIPois_par) <- list_all
colnames(ZIPois_par) <- c("p","lambda")
ZIPois_value <- c()
ZIPois_conv <- c()
ZIPois_BIC <- c()
ZINB_par <- matrix(NA,ncol=3,nrow=length(list_all))
rownames(ZINB_par) <- list_all
colnames(ZINB_par) <- c("p","size", "mu")
ZINB_value <- c()
ZINB_conv <- c()
ZINB_BIC <- c()
ZIPIG_par <- matrix(NA,ncol=3,nrow=length(list_all))
rownames(ZIPIG_par) <- list_all
colnames(ZIPIG_par) <- c("p","mu", "sigma")
ZIPIG_value <- c()
ZIPIG_conv <- c()
ZIPIG_BIC <- c()
ZIPB_par <- matrix(NA,ncol=4,nrow=length(list_all))
rownames(ZIPB_par) <- list_all
colnames(ZIPB_par) <- c("p","alpha", "beta","c")
ZIPB_value <- c()
ZIPB_conv <- c()
ZIPB_BIC <- c()
ZIDEL_par <- matrix(NA,ncol=4,nrow=length(list_all))
rownames(ZIDEL_par) <- list_all
colnames(ZIDEL_par) <- c("p","mu", "sigma","nu")
ZIDEL_value <- c()
ZIDEL_conv <- c()
ZIDEL_BIC <- c()

for (id in list_all){
    ZIPois_par[id,] <- getPar(zipois_fits, id)
    ZIPois_value[[id]] <- getValue(zipois_fits, id)
    ZIPois_conv[[id]]<- getConv(zipois_fits, id)
    ZIPois_BIC[[id]]<- getBIC(zipois_fits, id)
}
for (id in list_all){
    ZINB_par[id,] <- getPar(zinb_fits, id)
    ZINB_value[[id]] <- getValue(zinb_fits, id)
    ZINB_conv[[id]]<- getConv(zinb_fits, id)
    ZINB_BIC[[id]]<- getBIC(zinb_fits, id)
}
for (id in list_all){
    ZIPIG_par[id,] <- getPar(zipig_fits, id)
    ZIPIG_value[[id]] <- getValue(zipig_fits, id)
    ZIPIG_conv[[id]]<- getConv(zipig_fits, id)
    ZIPIG_BIC[[id]]<- getBIC(zipig_fits, id)
}
for (id in list_all){
    ZIPB_par[id,] <- getPar(zipb_fits, id)
    ZIPB_value[[id]] <- getValue(zipb_fits, id)
    ZIPB_conv[[id]]<- getConv(zipb_fits, id)
    ZIPB_BIC[[id]]<- getBIC(zipb_fits, id)
}
for (id in list_all){
    ZIDEL_par[id,] <- getPar(zidel_fits, id)
    ZIDEL_value[[id]] <- getValue(zidel_fits, id)
    ZIDEL_conv[[id]]<- getConv(zidel_fits, id)
    ZIDEL_BIC[[id]]<- getBIC(zidel_fits, id)
}

###2Pop
Pois2_par <- matrix(NA,ncol=3,nrow=length(list_all))
rownames(Pois2_par) <- list_all
colnames(Pois2_par) <- c("p","lambda_1","lambda_2")
Pois2_value <- c()
Pois2_conv <- c()
Pois2_BIC <- c()
NB2_par <- matrix(NA,ncol=5,nrow=length(list_all))
rownames(NB2_par) <- list_all
colnames(NB2_par) <- c("p","size_1", "mu_1","size_2", "mu_2")
NB2_value <- c()
NB2_conv <- c()
NB2_BIC <- c()
PIG2_par <- matrix(NA,ncol=5,nrow=length(list_all))
rownames(PIG2_par) <- list_all
colnames(PIG2_par) <- c("p","mu_1", "sigma_1","mu_2", "sigma_2")
PIG2_value <- c()
PIG2_conv <- c()
PIG2_BIC <- c()
PB2_par <- matrix(NA,ncol=7,nrow=length(list_all))
rownames(PB2_par) <- list_all
colnames(PB2_par) <- c("p","alpha_1", "beta_1","c_1","alpha_2", "beta_2","c_2")
PB2_value <- c()
PB2_conv <- c()
PB2_BIC <- c()
DEL2_par <- matrix(NA,ncol=7,nrow=length(list_all))
rownames(DEL2_par) <- list_all
colnames(DEL2_par) <- c("p","mu_1", "sigma_1","nu_1","mu_2", "sigma_2","nu_2")
DEL2_value <- c()
DEL2_conv <- c()
DEL2_BIC <- c()

for (id in list_all){
    Pois2_par[id,] <- getPar(pois2_fits, id)
    Pois2_value[[id]] <- getValue(pois2_fits, id)
    Pois2_conv[[id]]<- getConv(pois2_fits, id)
    Pois2_BIC[[id]]<- getBIC(pois2_fits, id)
}
for (id in list_all){
    NB2_par[id,] <- getPar(nb2_fits, id)
    NB2_value[[id]] <- getValue(nb2_fits, id)
    NB2_conv[[id]]<- getConv(nb2_fits, id)
    NB2_BIC[[id]]<- getBIC(nb2_fits, id)
}
for (id in list_all){
    PIG2_par[id,] <- getPar(pig2_fits, id)
    PIG2_value[[id]] <- getValue(pig2_fits, id)
    PIG2_conv[[id]]<- getConv(pig2_fits, id)
    PIG2_BIC[[id]]<- getBIC(pig2_fits, id)
}
for (id in list_all){
    PB2_par[id,] <- getPar(pb2_fits, id)
    PB2_value[[id]] <- getValue(pb2_fits, id)
    PB2_conv[[id]]<- getConv(pb2_fits, id)
    PB2_BIC[[id]]<- getBIC(pb2_fits, id)
}
for (id in list_all){
    DEL2_par[id,] <- getPar(del2_fits, id)
    DEL2_value[[id]] <- getValue(del2_fits, id)
    DEL2_conv[[id]]<- getConv(del2_fits, id)
    DEL2_BIC[[id]]<- getBIC(del2_fits, id)
}

###ZI2Pop
ZIPois2_par <- matrix(NA,ncol=4,nrow=length(list_all))
rownames(ZIPois2_par) <- list_all
colnames(ZIPois2_par) <- c("p_1","p_2","lambda_1","lambda_2")
ZIPois2_value <- c()
ZIPois2_conv <- c()
ZIPois2_BIC <- c()
ZINB2_par <- matrix(NA,ncol=6,nrow=length(list_all))
rownames(ZINB2_par) <- list_all
colnames(ZINB2_par) <- c("p_1","p_2","size_1", "mu_1","size_2", "mu_2")
ZINB2_value <- c()
ZINB2_conv <- c()
ZINB2_BIC <- c()
ZIPIG2_par <- matrix(NA,ncol=6,nrow=length(list_all))
rownames(ZIPIG2_par) <- list_all
colnames(ZIPIG2_par) <- c("p_1","p_2","mu_1", "sigma_1","mu_2", "sigma_2")
ZIPIG2_value <- c()
ZIPIG2_conv <- c()
ZIPIG2_BIC <- c()
ZIPB2_par <- matrix(NA,ncol=8,nrow=length(list_all))
rownames(ZIPB2_par) <- list_all
colnames(ZIPB2_par) <- c("p_1","p_2","alpha_1", "beta_1","c_1","alpha_2", "beta_2","c_2")
ZIPB2_value <- c()
ZIPB2_conv <- c()
ZIPB2_BIC <- c()
ZIDEL2_par <- matrix(NA,ncol=8,nrow=length(list_all))
rownames(ZIDEL2_par) <- list_all
colnames(ZIDEL2_par) <- c("p_1","p_2","mu_1", "sigma_1","nu_1","mu_2", "sigma_2","nu_2")
ZIDEL2_value <- c()
ZIDEL2_conv <- c()
ZIDEL2_BIC <- c()


for (id in list_all){
    ZIPois2_par[id,] <- getPar(zipois2_fits, id)
    ZIPois2_value[[id]] <- getValue(zipois2_fits, id)
    ZIPois2_conv[[id]]<- getConv(zipois2_fits, id)
    ZIPois2_BIC[[id]]<- getBIC(zipois2_fits, id)
}
for (id in list_all){
    ZINB2_par[id,] <- getPar(zinb2_fits, id)
    ZINB2_value[[id]] <- getValue(zinb2_fits, id)
    ZINB2_conv[[id]]<- getConv(zinb2_fits, id)
    ZINB2_BIC[[id]]<- getBIC(zinb2_fits, id)
}
for (id in list_all){
    ZIPIG2_par[id,] <- getPar(zipig2_fits, id)
    ZIPIG2_value[[id]] <- getValue(zipig2_fits, id)
    ZIPIG2_conv[[id]]<- getConv(zipig2_fits, id)
    ZIPIG2_BIC[[id]]<- getBIC(zipig2_fits, id)
}
for (id in list_all){
    ZIPB2_par[id,] <- getPar(zipb2_fits, id)
    ZIPB2_value[[id]] <- getValue(zipb2_fits, id)
    ZIPB2_conv[[id]]<- getConv(zipb2_fits, id)
    ZIPB2_BIC[[id]]<- getBIC(zipb2_fits, id)
}

for (id in list_all){
    ZIDEL2_par[id,] <- getPar(zidel2_fits, id)
    ZIDEL2_value[[id]] <- getValue(zidel2_fits, id)
    ZIDEL2_conv[[id]]<- getConv(zidel2_fits, id)
    ZIDEL2_BIC[[id]]<- getBIC(zidel2_fits, id)
}


### Generate Dataframe with all inforamtions


Overview_mm10 <- data.frame( row.names=list_all )

Overview_mm10$Pois_lambda <- Pois_lambda
Overview_mm10$Pois_value <- Pois_value
Overview_mm10$Pois_conv <- Pois_conv
Overview_mm10$Pois_BIC <- Pois_BIC

Overview_mm10$NB_size <- NB_par[,"size"]
Overview_mm10$NB_mu <-  NB_par[,"mu"]
Overview_mm10$NB_value <- NB_value
Overview_mm10$NB_conv <- NB_conv
Overview_mm10$NB_BIC <- NB_BIC

Overview_mm10$PIG_mu <- PIG_par[,"mu"]
Overview_mm10$PIG_sigma <-  PIG_par[,"sigma"]
Overview_mm10$PIG_value <- PIG_value
Overview_mm10$PIG_conv <- PIG_conv
Overview_mm10$PIG_BIC <- PIG_BIC

Overview_mm10$PB_alpha <- PB_par[,"alpha"]
Overview_mm10$PB_beta <-  PB_par[,"beta"]
Overview_mm10$PB_c <-  PB_par[,"c"]
Overview_mm10$PB_value <- PB_value
Overview_mm10$PB_conv <- PB_conv
Overview_mm10$PB_BIC <- PB_BIC

Overview_mm10$DEL_mu <- DEL_par[,"mu"]
Overview_mm10$DEL_sigma <-  DEL_par[,"sigma"]
Overview_mm10$DEL_nu <-  DEL_par[,"nu"]
Overview_mm10$DEL_value <- DEL_value
Overview_mm10$DEL_conv <- DEL_conv
Overview_mm10$DEL_BIC <- DEL_BIC

Overview_mm10$ZIPois_p <- ZIPois_par[,"p"]
Overview_mm10$ZIPois_lambda <- ZIPois_par[,"lambda"]
Overview_mm10$ZIPois_value <- ZIPois_value
Overview_mm10$ZIPois_conv <- ZIPois_conv
Overview_mm10$ZIPois_BIC <- ZIPois_BIC

Overview_mm10$ZINB_p <- ZINB_par[,"p"]
Overview_mm10$ZINB_size <- ZINB_par[,"size"]
Overview_mm10$ZINB_mu <-  ZINB_par[,"mu"]
Overview_mm10$ZINB_value <- ZINB_value
Overview_mm10$ZINB_conv <- ZINB_conv
Overview_mm10$ZINB_BIC <- ZINB_BIC

Overview_mm10$ZIPIG_p <- ZIPIG_par[,"p"]
Overview_mm10$ZIPIG_mu <- ZIPIG_par[,"mu"]
Overview_mm10$ZIPIG_sigma <-  ZIPIG_par[,"sigma"]
Overview_mm10$ZIPIG_value <- ZIPIG_value
Overview_mm10$ZIPIG_conv <- ZIPIG_conv
Overview_mm10$ZIPIG_BIC <- ZIPIG_BIC

Overview_mm10$ZIPB_p <- ZIPB_par[,"p"]
Overview_mm10$ZIPB_alpha <- ZIPB_par[,"alpha"]
Overview_mm10$ZIPB_beta <-  ZIPB_par[,"beta"]
Overview_mm10$ZIPB_c <-  ZIPB_par[,"c"]
Overview_mm10$ZIPB_value <- ZIPB_value
Overview_mm10$ZIPB_conv <- ZIPB_conv
Overview_mm10$ZIPB_BIC <- ZIPB_BIC

Overview_mm10$ZIDEL_p <- ZIDEL_par[,"p"]
Overview_mm10$ZIDEL_mu <- ZIDEL_par[,"mu"]
Overview_mm10$ZIDEL_sigma <-  ZIDEL_par[,"sigma"]
Overview_mm10$ZIDEL_nu <-  ZIDEL_par[,"nu"]
Overview_mm10$ZIDEL_value <- ZIDEL_value
Overview_mm10$ZIDEL_conv <- ZIDEL_conv
Overview_mm10$ZIDEL_BIC <- ZIDEL_BIC

Overview_mm10$Pois2_p <- Pois2_par[,"p"]
Overview_mm10$Pois2_lambda1 <- Pois2_par[,"lambda_1"]
Overview_mm10$Pois2_lambda2 <- Pois2_par[,"lambda_2"]
Overview_mm10$Pois2_value <- Pois2_value
Overview_mm10$Pois2_conv <- Pois2_conv
Overview_mm10$Pois2_BIC <- Pois2_BIC

Overview_mm10$NB2_p <- NB2_par[,"p"]
Overview_mm10$NB2_size1 <- NB2_par[,"size_1"]
Overview_mm10$NB2_mu1 <-  NB2_par[,"mu_1"]
Overview_mm10$NB2_size2 <- NB2_par[,"size_2"]
Overview_mm10$NB2_mu2 <-  NB2_par[,"mu_2"]
Overview_mm10$NB2_value <- NB2_value
Overview_mm10$NB2_conv <- NB2_conv
Overview_mm10$NB2_BIC <- NB2_BIC

Overview_mm10$PIG2_p <- PIG2_par[,"p"]
Overview_mm10$PIG2_mu1 <- PIG2_par[,"mu_1"]
Overview_mm10$PIG2_sigma1 <-  PIG2_par[,"sigma_1"]
Overview_mm10$PIG2_mu2 <- PIG2_par[,"mu_2"]
Overview_mm10$PIG2_sigma2 <-  PIG2_par[,"sigma_2"]
Overview_mm10$PIG2_value <- PIG2_value
Overview_mm10$PIG2_conv <- PIG2_conv
Overview_mm10$PIG2_BIC <- PIG2_BIC

Overview_mm10$PB2_p <- PB2_par[,"p"]
Overview_mm10$PB2_alpha1 <- PB2_par[,"alpha_1"]
Overview_mm10$PB2_beta1 <-  PB2_par[,"beta_1"]
Overview_mm10$PB2_c1 <-  PB2_par[,"c_1"]
Overview_mm10$PB2_alpha2 <- PB2_par[,"alpha_2"]
Overview_mm10$PB2_beta2 <-  PB2_par[,"beta_2"]
Overview_mm10$PB2_c2 <-  PB2_par[,"c_2"]
Overview_mm10$PB2_value <- PB2_value
Overview_mm10$PB2_conv <- PB2_conv
Overview_mm10$PB2_BIC <- PB2_BIC

Overview_mm10$DEL2_p <- DEL2_par[,"p"]
Overview_mm10$DEL2_mu1 <- DEL2_par[,"mu_1"]
Overview_mm10$DEL2_sigma1 <-  DEL2_par[,"sigma_1"]
Overview_mm10$DEL2_nu1 <- DEL2_par[,"nu_1"]
Overview_mm10$DEL2_mu2 <- DEL2_par[,"mu_2"]
Overview_mm10$DEL2_sigma2 <-  DEL2_par[,"sigma_2"]
Overview_mm10$DEL2_nu2 <- DEL2_par[,"nu_2"]
Overview_mm10$DEL2_value <- DEL2_value
Overview_mm10$DEL2_conv <- DEL2_conv
Overview_mm10$DEL2_BIC <- DEL2_BIC

Overview_mm10$ZIPois2_p1 <- ZIPois2_par[,"p_1"]
Overview_mm10$ZIPois2_p2 <- ZIPois2_par[,"p_2"]
Overview_mm10$ZIPois2_lambda1 <- ZIPois2_par[,"lambda_1"]
Overview_mm10$ZIPois2_lambda2 <- ZIPois2_par[,"lambda_2"]
Overview_mm10$ZIPois2_value <- ZIPois2_value
Overview_mm10$ZIPois2_conv <- ZIPois2_conv
Overview_mm10$ZIPois2_BIC <- ZIPois2_BIC

Overview_mm10$ZINB2_p1 <- ZINB2_par[,"p_1"]
Overview_mm10$ZINB2_p2 <- ZINB2_par[,"p_2"]
Overview_mm10$ZINB2_size1 <- ZINB2_par[,"size_1"]
Overview_mm10$ZINB2_mu1 <-  ZINB2_par[,"mu_1"]
Overview_mm10$ZINB2_size2 <- ZINB2_par[,"size_2"]
Overview_mm10$ZINB2_mu2 <-  ZINB2_par[,"mu_2"]
Overview_mm10$ZINB2_value <- ZINB2_value
Overview_mm10$ZINB2_conv <- ZINB2_conv
Overview_mm10$ZINB2_BIC <- ZINB2_BIC

Overview_mm10$ZIPIG2_p1 <- ZIPIG2_par[,"p_1"]
Overview_mm10$ZIPIG2_p2 <- ZIPIG2_par[,"p_2"]
Overview_mm10$ZIPIG2_mu1 <- ZIPIG2_par[,"mu_1"]
Overview_mm10$ZIPIG2_sigma1 <-  ZIPIG2_par[,"sigma_1"]
Overview_mm10$ZIPIG2_mu2 <- ZIPIG2_par[,"mu_2"]
Overview_mm10$ZIPIG2_sigma2 <-  ZIPIG2_par[,"sigma_2"]
Overview_mm10$ZIPIG2_value <- ZIPIG2_value
Overview_mm10$ZIPIG2_conv <- ZIPIG2_conv
Overview_mm10$ZIPIG2_BIC <- ZIPIG2_BIC

Overview_mm10$ZIPB2_p1 <- ZIPB2_par[,"p_1"]
Overview_mm10$ZIPB2_p2 <- ZIPB2_par[,"p_2"]
Overview_mm10$ZIPB2_alpha1 <- ZIPB2_par[,"alpha_1"]
Overview_mm10$ZIPB2_beta1 <-  ZIPB2_par[,"beta_1"]
Overview_mm10$ZIPB2_c1 <-  ZIPB2_par[,"c_1"]
Overview_mm10$ZIPB2_alpha2 <- ZIPB2_par[,"alpha_2"]
Overview_mm10$ZIPB2_beta2 <-  ZIPB2_par[,"beta_2"]
Overview_mm10$ZIPB2_c2 <-  ZIPB2_par[,"c_2"]
Overview_mm10$ZIPB2_value <- ZIPB2_value
Overview_mm10$ZIPB2_conv <- ZIPB2_conv
Overview_mm10$ZIPB2_BIC <- ZIPB2_BIC

Overview_mm10$ZIDEL2_p1 <- ZIDEL2_par[,"p_1"]
Overview_mm10$ZIDEL2_p2 <- ZIDEL2_par[,"p_2"]
Overview_mm10$ZIDEL2_mu1 <- ZIDEL2_par[,"mu_1"]
Overview_mm10$ZIDEL2_sigma1 <-  ZIDEL2_par[,"sigma_1"]
Overview_mm10$ZIDEL2_nu1 <- ZIDEL2_par[,"nu_1"]
Overview_mm10$ZIDEL2_mu2 <- ZIDEL2_par[,"mu_2"]
Overview_mm10$ZIDEL2_sigma2 <-  ZIDEL2_par[,"sigma_2"]
Overview_mm10$ZIDEL2_nu2 <- ZIDEL2_par[,"nu_2"]
Overview_mm10$ZIDEL2_value <- ZIDEL2_value
Overview_mm10$ZIDEL2_conv <- ZIDEL2_conv
Overview_mm10$ZIDEL2_BIC <- ZIDEL2_BIC


Overview_mm10$model <- comparison_all
Overview_mm10$modelname <- comparison_all_name

save(Overview_mm10,file="Overview_mm10.rds")




Par <- list()
#NB
for (id in names(which(comparison_all_name=="pois"))) {
    Par[[id]] <- getPar(pois_fits, id)
}
for (id in names(which(comparison_all_name=="nb"))) {
    Par[[id]] <- getPar(nb_fits, id)
}
for (id in names(which(comparison_all_name=="pig"))) {
    Par[[id]] <- getPar(nb_fits, id)
}
for (id in names(which(comparison_all_name=="pb"))) {
    Par[[id]] <- getPar(pb_fits, id)
}
for (id in names(which(comparison_all_name=="del"))) {
    Par[[id]] <- getPar(del_fits, id)
}
for (id in names(which(comparison_all_name=="zipois"))) {
    Par[[id]] <- getPar(zipois_fits, id)
}
for (id in names(which(comparison_all_name=="zinb"))) {
    Par[[id]] <- getPar(zinb_fits, id)
}
for (id in names(which(comparison_all_name=="zipig"))) {
    Par[[id]] <- getPar(zinb_fits, id)
}
for (id in names(which(comparison_all_name=="zipb"))) {
    Par[[id]] <- getPar(zipb_fits, id)
}
for (id in names(which(comparison_all_name=="zidel"))) {
    Par[[id]] <- getPar(zidel_fits, id)
}
for (id in names(which(comparison_all_name=="pois2"))) {
    Par[[id]] <- getPar(pois2_fits, id)
}
for (id in names(which(comparison_all_name=="nb2"))) {
    Par[[id]] <- getPar(nb2_fits, id)
}
for (id in names(which(comparison_all_name=="pig2"))) {
    Par[[id]] <- getPar(nb2_fits, id)
}
for (id in names(which(comparison_all_name=="pb2"))) {
    Par[[id]] <- getPar(pb2_fits, id)
}
for (id in names(which(comparison_all_name=="del2"))) {
    Par[[id]] <- getPar(del2_fits, id)
}
for (id in names(which(comparison_all_name=="zipois2"))) {
    Par[[id]] <- getPar(zipois2_fits, id)
}
for (id in names(which(comparison_all_name=="zinb2"))) {
    Par[[id]] <- getPar(zinb2_fits, id)
}
for (id in names(which(comparison_all_name=="zipig2"))) {
    Par[[id]] <- getPar(zinb2_fits, id)
}
for (id in names(which(comparison_all_name=="zipb2"))) {
    Par[[id]] <- getPar(zipb2_fits, id)
}
for (id in names(which(comparison_all_name=="zidel2"))) {
    Par[[id]] <- getPar(zidel2_fits, id)
}

setwd("GOF")
save(Par,comparison_all_name, file="GOF_input.rds")







