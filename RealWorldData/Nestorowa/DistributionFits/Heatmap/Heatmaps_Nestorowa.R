setwd("Nestorowa/DistributionFits/Heatmap")

#Heatmap of Nestorowa Fits
Nestorowa_fit <- matrix(c(0, 2167, 217, 0, 4452,2,1,8659,277,1,581,7) ,ncol = 3, byrow=T)

Nestorowa <- Nestorowa_fit
Nestorowa <- cbind(Nestorowa, rowSums(Nestorowa))
Nestorowa <- rbind(Nestorowa, colSums(Nestorowa))
rownames(Nestorowa) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(Nestorowa) <- c("Pois", "NB", "PB", "Sum")






#Heatmap of Nestorowa Fits

my_palette2 <- colorRampPalette(c( rgb(229,249,251, 1, maxColorValue = 255),rgb(142,229,238, 1, maxColorValue = 255),rgb(21,122,133, 1, maxColorValue = 255) ))(n = 200)

#pdf(paste0( "Nestorowa_heatmap_Fit.pdf"),  width = 15, height = 7)
svg(paste0( "Nestorowa_heatmap_Fit_new.svg"),  width = 7, height = 8)
library(gplots)
heatmap.2(log10(Nestorowa+1),
          main = "Overview fitted distributions: Nestorowa",
          col = my_palette2,
          trace="none",
          dendrogram ="none",
          Colv="NA",
          Rowv="NA",
          sepcolor="black",
          colsep = 1:3,
          rowsep = 1:4,
          sepwidth = c(0.001,0.001),
          srtCol = 0,
          cellnote=format(Nestorowa, big.mark =",") ,
          notecol= "black",
          notecex = 1,
          margins = c(6,6),
          cexRow = 1.5,
          cexCol=1.5,
          key.title = NA,
          keysize = 1
)

dev.off()




############################################################################
#After GOF

Nestorowa_gof <- matrix(c(0, 1251, 43, 0, 1043,2,1,7248,159,1,427,0) ,ncol = 3, byrow=T)


Nestorowa <- Nestorowa_gof
Nestorowa <- cbind(Nestorowa, rowSums(Nestorowa))
Nestorowa <- rbind(Nestorowa, colSums(Nestorowa))
rownames(Nestorowa) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(Nestorowa) <- c("Pois", "NB", "PB", "Sum")

my_palette2 <- colorRampPalette(c( rgb(229,249,251, 1, maxColorValue = 255),rgb(142,229,238, 1, maxColorValue = 255),rgb(21,122,133, 1, maxColorValue = 255) ))(n = 200)

#pdf(paste0( "Nestorowa_heatmap_GOF_new.pdf"),  width = 7, height = 8)
svg(paste0( "Nestorowa_heatmap_GOF_new.svg"),  width = 7, height = 8)
library(gplots)
heatmap.2(log10(Nestorowa+1),
          main = "Overview fitted distributions: Nestorowa:10x, after GOF",
          col = my_palette2,
          trace="none",
          dendrogram ="none",
          Colv="NA",
          Rowv="NA",
          sepcolor="black",
          colsep = 1:3,
          rowsep = 1:4,
          sepwidth = c(0.001,0.001),
          srtCol = 0,
          cellnote=format(Nestorowa, big.mark =",") ,
          notecol= "black",
          notecex = 1,
          margins = c(6,6),
          cexRow = 1.5,
          cexCol=1.5,
          key.title = NA,
          keysize = 1
)

dev.off()


