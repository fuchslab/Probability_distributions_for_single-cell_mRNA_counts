# plot Gene expression and their fits

library(viridis)
plotColor <- magma(12,begin = 0)
plotColor2 <- cividis(12,begin = 0)
plotColor3 <- viridis(12,begin = 0)
library(grDevices)

ge <- readRDS("Nestorowa.rds")

ge <- readRDS("ge.rds")
ids <- c("Csf1r","Ccl5", "Prss34","H2-Aa", "Gfi1b")






svg( filename= "all_plot_hist_dens_NBfits_trafo.svg", width = 10, height = 12, pointsize = 12)
par(mfrow=c(5,3))
k <- 1
Label <- c("A","B","C","D","E")
for(j in ids) {
    x <- seq(0, max(ge[j,]))
    load(file=paste0( j, "_res.rda"))
    #
    C <- hist(log(ge[j,]+1), plot = FALSE, breaks = seq(-0.25, max(log(ge[j,]+1)+1),by = 0.5))
    #B <- density(log(ge[j,]+1),bw = 0.3, from=0)
    plot(C, freq = FALSE, border = "white", col ="lightgrey", main = "", xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    fig_label(Label[k], cex = 2) 
    #lines(B$x, B$y , lwd = 2)
    for(i in 5:8){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = plotColor3[i], lwd = 2)
        #m = m+1
        }

    legend("topright", legend=c("NB","ZINB", "NB2", "ZINB2"),lty = 1,bty="n",col = c(plotColor3[5:8]), lwd = 2 )
    

    plot(C, freq = FALSE, border = "white", col ="lightgrey", main = bquote(.(j)), xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    #lines(B$x, B$y , lwd = 2)
    m = 5
    for(i in 13:16){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = plotColor[m], lwd = 2)
        m = m+1
        }

    legend("topright", legend=c("PIG","ZIPIG", "PIG2", "ZIPIG2"),lty = 1,bty="n",col = c(plotColor[5:8]), lwd = 2)
    
 
    plot(C, freq = FALSE, border = "white", col ="lightgrey", main = "", xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    #lines(B$x, B$y , lwd = 2)
    for(i in 17:20){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = plotColor2[m], lwd = 2)
        m= m+1
        }
    
    legend("topright", legend=c("DEL","ZIDEL", "DEL2", "ZIDEL2"),lty = 1,bty="n",col = c(plotColor2[9:12]), lwd = 2)
    
    
    
    #
    k <- k+1
}
dev.off()





par(mfrow=c(3,1))
k <- 1
for(j in ids) {
    x <- seq(0, max(ge[j,]))
    load(file=paste0( j, "_res.rda"))
    #svg(paste0(  "ExampleGenes/",j, "_plot_hist_dens_NBfits_trafo.svg"),width = 10, height = 7, pointsize = 12)
    C <- hist(log(ge[j,]+1), plot = FALSE, breaks = seq(-0.25, max(log(ge[j,]+1)+1),by = 0.5))
    B <- density(log(ge[j,]+1),bw = 0.3, from=0)
    plot(C, freq = FALSE, main = bquote(.(j)), xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    m = 1
    for(i in 5:8){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))[m])
        m = m+1
    }
    lines(B$x, B$y )
    legend("topright", legend=c("data", "NB","ZINB", "NB2", "ZINB2"),lty = 1,bty="n",col = c(rep("black",1),c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))))
    
    
    plot(C, freq = FALSE, main = bquote(.(j)), xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    m = 1
    for(i in 13:16){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))[m])
        m = m+1
    }
    lines(B$x, B$y )
    legend("topright", legend=c("data", "PIG","ZIPIG", "PIG2", "ZIPIG2"),lty = 1,bty="n",col = c(rep("black",1),c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))))
    
    
    plot(C, freq = FALSE, main = bquote(.(j)), xlab = "log(Counts + 1)", ylab = "PMF", ylim = c(0, max(B$y,C$density,(Dens_fits[7,]*(x+1)))))
    
    m = 1
    for(i in 17:20){
        lines(log(x+1),Dens_fits[i,]*(x+1),col = c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))[m])
        m= m+1
    }
    lines(B$x, B$y )
    legend("topright", legend=c("data", "DEL","ZIDEL", "DEL2", "ZIDEL2"),lty = 1,bty="n",col = c(rep("black",1),c(rep("red",1),rep("green",1),rep("blue",1),rep("orange",1))))
    
    
    
    #dev.off()
    k <- k+1
}



###############################################################################################################################################################################
# function found at: https://logfc.wordpress.com/2017/03/15/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
    
    region <- match.arg(region, c("figure", "plot", "device"))
    pos <- match.arg(pos, c("topleft", "top", "topright", 
                            "left", "center", "right", 
                            "bottomleft", "bottom", "bottomright"))
    
    if(region %in% c("figure", "device")) {
        ds <- dev.size("in")
        # xy coordinates of device corners in user coordinates
        x <- grconvertX(c(0, ds[1]), from="in", to="user")
        y <- grconvertY(c(0, ds[2]), from="in", to="user")
        
        # fragment of the device we use to plot
        if(region == "figure") {
            # account for the fragment of the device that 
            # the figure is using
            fig <- par("fig")
            dx <- (x[2] - x[1])
            dy <- (y[2] - y[1])
            x <- x[1] + dx * fig[1:2]
            y <- y[1] + dy * fig[3:4]
        } 
    }
    
    # much simpler if in plotting region
    if(region == "plot") {
        u <- par("usr")
        x <- u[1:2]
        y <- u[3:4]
    }
    
    sw <- strwidth(text, cex=cex) * 60/100
    sh <- strheight(text, cex=cex) * 60/100
    
    x1 <- switch(pos,
                 topleft     =x[1] + sw, 
                 left        =x[1] + sw,
                 bottomleft  =x[1] + sw,
                 top         =(x[1] + x[2])/2,
                 center      =(x[1] + x[2])/2,
                 bottom      =(x[1] + x[2])/2,
                 topright    =x[2] - sw,
                 right       =x[2] - sw,
                 bottomright =x[2] - sw)
    
    y1 <- switch(pos,
                 topleft     =y[2] - sh,
                 top         =y[2] - sh,
                 topright    =y[2] - sh,
                 left        =(y[1] + y[2])/2,
                 center      =(y[1] + y[2])/2,
                 right       =(y[1] + y[2])/2,
                 bottomleft  =y[1] + sh,
                 bottom      =y[1] + sh,
                 bottomright =y[1] + sh)
    
    old.par <- par(xpd=NA)
    on.exit(par(old.par))
    
    text(x1, y1, text, cex=cex, ...)
    return(invisible(c(x,y)))
}

