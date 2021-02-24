# plot Gene expression and their fits
library("scModels")
library(gamlss.dist)

ge <- readRDS("Nestorowa.rds")
ids <- c("Csf1r","Ccl5", "Prss34","H2-Aa", "Gfi1b")


for(j in ids) {
  x <- seq(0, max(ge[j,]))
  Dens_fits <- matrix(0,nrow = 12, ncol = length(x))
  BIC<- matrix(0,nrow=12 , ncol =1)
  nloglik<- matrix(0,nrow=12 , ncol =1)
  rownames(Dens_fits) <-c( "pois","zipois","pois2","zipois2","nb","zinb","nb2","zinb2","pig","zipig","pig2","zipig2","del","zidel","del2","zidel2","pb","zipb","pb2","zipb2")
  rownames(BIC) <- c( "pois","zipois","pois2","zipois2","nb","zinb","nb2","zinb2","pig","zipig","pig2","zipig2","del","zidel","del2","zidel2","pb","zipb","pb2","zipb2")
  rownames(nloglik) <-c( "pois","zipois","pois2","zipois2","nb","zinb","nb2","zinb2","pig","zipig","pig2","zipig2","del","zidel","del2","zidel2","pb","zipb","pb2","zipb2")
  for(i in c( "pois","zipois","pois2","zipois2","nb","zinb","nb2","zinb2","pig","zipig","pig2","zipig2","del","zidel","del2","zidel2","pb","zipb","pb2","zipb2")) {
    t <- tryCatch({readRDS(paste0(i, "_fits/", j, ".rds"))}, error = function(err) return(list('value'=0, 'time'=c('elapsed'=0))))
    if(i == "pois"){
      y <- dpois(x = x, lambda = t$par, log = FALSE)
      #lines(x,y,col ="red")
    }else if(i == "zipois"){
      y <- t$par[1]*c(1,rep(0, length(x)-1))+ (1-t$par[1])*  dpois(x = x, lambda = t$par[-1], log = FALSE)
        #lines(x,y,col="red", lty = 2)
    }else if(i == "pois2"){
      y <- t$par[1]*dpois(x = x, lambda = t$par[2], log = FALSE)+ (1-t$par[1])*  dpois(x = x, lambda = t$par[3], log = FALSE)
        #lines(x,y,col="red", lty=3)
    }else if(i == "zipois2"){
        y <- t$par[1]*c(1,rep(0, length(x)-1)) + t$par[2]*dpois(x = x, lambda = t$par[3], log = FALSE)+ (1-t$par[1]-t$par[2])*  dpois(x = x, lambda = t$par[4], log = FALSE)
        #lines(x,y,col="red", lty = 4)
    }else if(i == "nb"){
      y <-  dnbinom(x = x, size = t$par[1], mu = t$par[2], log = FALSE)
      #lines(x,y,col ="green")
    }else if(i == "zinb"){
      y <- t$par[1]*c(1,rep(0, length(x)-1))+ (1-t$par[1])*   dnbinom(x = x, size = t$par[2], mu = t$par[3], log = FALSE)
      #lines(x,y,col="green", lty = 2)
    }else if(i == "nb2"){
      y <- t$par[1]* dnbinom(x = x, size = t$par[2], mu = t$par[3], log = FALSE)+ (1-t$par[1])*   dnbinom(x = x, size = t$par[4], mu = t$par[5], log = FALSE)
      #lines(x,y,col="green", lty=3)
    }else if(i == "zinb2"){
      y <- t$par[1]*c(1,rep(0, length(x)-1)) + t$par[2]* dnbinom(x = x, size = t$par[3], mu = t$par[4], log = FALSE)+ (1-t$par[1]-t$par[2])*   dnbinom(x = x, size = t$par[5], mu = t$par[6], log = FALSE)
      #lines(x,y,col="green", lty = 4)
    }else if(i == "pig"){
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
    }else if(i == "pb"){
     y <-  dpb(x = x, alpha = t$par[1], beta = t$par[2], c = t$par[3], log = FALSE)
     #lines(x,y,col ="blue")
    }else if(i == "zipb"){
      y <- t$par[1]*c(1,rep(0, length(x)-1))+ (1-t$par[1])*   dpb(x = x, alpha = t$par[2], beta = t$par[3], c = t$par[4], log = FALSE)
      #lines(x,y,col="blue", lty = 2)
    }else if(i == "pb2"){
      #y <- t$par[1]* dpb(x = x, alpha = t$par[2], beta = t$par[3], c = t$par[4], log = FALSE)+ (1-t$par[1])*   dpb(x = x, alpha = t$par[5], beta = t$par[6], c = t$par[7], log = FALSE)
      #lines(x,y,col="blue", lty=3)
    }else if(i == "zipb2"){
      #y <- t$par[1]*c(1,rep(0, length(x)-1)) + t$par[2]* dpb(x = x, alpha = t$par[3], beta = t$par[4], c = t$par[5], log = FALSE)+ (1-t$par[1]-t$par[2])*   dpb(x = x, alpha = t$par[6], beta = t$par[7], c = t$par[8], log = FALSE)
      #lines(x,y,col="blue", lty = 4)
    }
Dens_fits[i,]<- y
BIC[i,1]<- t$BIC
nloglik[i,1] <- t$value

  }
  save(Dens_fits, BIC, nloglik, file= paste0( "ExampleGenes/",j, "_res.rda"))

    }



