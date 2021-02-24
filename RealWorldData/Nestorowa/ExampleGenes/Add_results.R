setwd("/mnt/znas/icb_zstore01/groups/biostat01/projects/Lisa_ProbDistr/RealWorld/nestorowa")
library("scModels")
library(gamlss.dist)

ge <- readRDS("Nestorowa.rds")


ge <- readRDS("ge.rds")

ids <- c("Csf1r","Ccl5", "Prss34","H2-Aa", "Gfi1b")

for(j in ids){
load(file= paste0( "ExampleGenes/old/",j, "_res.rda"))
  x <- seq(0, max(ge[j,]))
  
for(i in c("pig","zipig","pig2","zipig2","del","zidel","del2","zidel2")) {

  t <- tryCatch({readRDS(paste0(i, "_fits/", j, ".rds"))}, error = function(err) return(list('value'=0, 'time'=c('elapsed'=0))))
  if(i == "pig"){
    y <-  dPIG(x = x, mu = t$par[1], sigma = t$par[2], log = FALSE)
    #lines(x,y,col ="green")
  }else if(i == "zipig"){
    y <- t$par[1]*c(1,rep(0, length(x)-1))+ (1-t$par[1])*   dPIG(x = x, mu = t$par[2], sigma = t$par[3], log = FALSE)
    #lines(x,y,col="green", lty = 2)
  }else if(i == "pig2"){
    y <- t$par[1]* dPIG(x = x, mu = t$par[2], sigma = t$par[3], log = FALSE)+ (1-t$par[1])*   dPIG(x = x, mu = t$par[4], sigma = t$par[5], log = FALSE)
    #lines(x,y,col="green", lty=3)
  }else if(i == "zipig2"){
    y <- t$par[1]*c(1,rep(0, length(x)-1)) + t$par[2]* dPIG(x = x, mu = t$par[3], sigma = t$par[4], log = FALSE)+ (1-t$par[1]-t$par[2])*   dPIG(x = x, mu = t$par[5], sigma = t$par[6], log = FALSE)
    #lines(x,y,col="green", lty = 4)
  }else if(i == "del"){
    y <-  dDEL(x = x, mu = t$par[1], sigma = t$par[2], nu = t$par[3], log = FALSE)
    #lines(x,y,col ="green")
  }else if(i == "zidel"){
    y <- t$par[1]*c(1,rep(0, length(x)-1))+ (1-t$par[1])*   dDEL(x = x, mu = t$par[2], sigma = t$par[3], nu = t$par[4], log = FALSE)
    #lines(x,y,col="green", lty = 2)
  }else if(i == "del2"){
    y <- t$par[1]* dDEL(x = x, mu = t$par[2], sigma = t$par[3], nu = t$par[4], log = FALSE)+ (1-t$par[1])*   dDEL(x = x, mu = t$par[5], sigma = t$par[6], nu = t$par[7], log = FALSE)
    #lines(x,y,col="green", lty=3)
  }else if(i == "zidel2"){
    y <- t$par[1]*c(1,rep(0, length(x)-1)) + t$par[2]* dDEL(x = x, mu = t$par[3], sigma = t$par[4], nu = t$par[5], log = FALSE)+ (1-t$par[1]-t$par[2]) * dDEL(x = x, mu = t$par[6], sigma = t$par[7], nu = t$par[8],  log = FALSE)
    #lines(x,y,col="green", lty = 4)
  }
  
  Dens_fits <- rbind(Dens_fits, y)
  as.matrix(t$BIC )
  BIC <- rbind(BIC, t$BIC)
  nloglik <- rbind(nloglik, t$value)
  
}
  
  rownames(BIC[])

  save(Dens_fits, BIC, nloglik, file= paste0( "ExampleGenes/",j, "_res.rda"))
  

}



for(j in ids){
 load(file= paste0( "ExampleGenes/",j, "_res.rda"))
 length(rownames(BIC))
 rownames(BIC)[(length(rownames(BIC))-7):(length(rownames(BIC)))] <- c("pig","zipig","pig2","zipig2","del","zidel","del2","zidel2")
 rownames(nloglik)[(length(rownames(nloglik))-7):(length(rownames(nloglik)))] <- c("pig","zipig","pig2","zipig2","del","zidel","del2","zidel2")
 rownames(Dens_fits)[(length(rownames(Dens_fits))-7):(length(rownames(Dens_fits)))] <- c("pig","zipig","pig2","zipig2","del","zidel","del2","zidel2")
 
 save(Dens_fits, BIC, nloglik, file= paste0( "ExampleGenes/",j, "_res.rda"))
 
 
}

