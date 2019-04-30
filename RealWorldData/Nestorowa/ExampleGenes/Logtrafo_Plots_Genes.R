# plot Gene expression and their fits
library("scModels")


ge <- readRDS("Nestorowa.rds")
ids <- c("Csf1r","Ccl5", "Prss34","H2-Aa", "Gfi1b")

DensFits_Final5_logscale <- matrix(0, nrow = 4,ncol = 2000 )

for(j in rownames(ids_final)) {
    z <- seq(0, log(max(ge[j,])+1), length.out = 2000)
    x <- exp(z)-1
    k <- 1
for(i in c("nb","zinb","nb2","zinb2")){
    t <-readRDS(paste0(i, "_fits/", j, ".rds"))
    if(i == "nb"){
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
    }
    DensFits_Final5_logscale[k,]<- y*(x+1)
    k <- k+1
    }

        save(DensFits_Final5_logscale, file= paste0( "ExampleGenes/",j, "_logscale_res.rda"))
}


par(mfrow=c(1,1))
k <- 1
for(j in rownames(ids_final)) {
    x <- seq(0, max(ge[j,]))
    load(file=paste0( j, "_res.rda"))
    svg(paste0(  "ExampleGenes/",j, "_plot_hist_dens_NBfits_trafo.svg"),width = 10, height = 7, pointsize = 12)
    C <- hist(log(ge[j,]+1), plot = FALSE, breaks = seq(-0.25, max(log(ge[j,]+1)+1),by = 0.5))
    B <- density(log(ge[j,]+1),bw = 0.3, from=0)
    plot(C, freq = FALSE, main = bquote(.(j)), xlab = "log(x+1) counts", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    for(i in 5:8){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))[i-4])
    }
    lines(B$x, B$y )
    legend("topright", legend=c("data", "NB","ZINB", "NB2", "ZINB2"),lty = 1,bty="n",col = c(rep("black",1),c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))))
    dev.off()
    k <- k+1
}



