
#Heatmap of mm10 Fits
mm10_fit <- matrix(c(43, 1932, 28, 1726, 22, 115, 147, 4, 91, 23, 24, 16, 25, 69, 0, 2, 2, 0, 14, 14) ,ncol = 5, byrow=T)

mm10 <- mm10_fit
mm10 <- cbind(mm10, rowSums(mm10))
mm10 <- rbind(mm10, colSums(mm10))
rownames(mm10) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(mm10) <- c("Pois", "NB", "PB" ,"PIG", "DEL", "Sum")

my_palette2 <- colorRampPalette(c( rgb(229,249,251, 1, maxColorValue = 255),rgb(142,229,238, 1, maxColorValue = 255),rgb(21,122,133, 1, maxColorValue = 255) ))(n = 200)



svg(paste0( "mm10_heatmap_Fit_new5.svg"),  width = 9, height = 8)
library(gplots)
heatmap.2(log10(mm10+1),
          main = "Overview fitted distributions: mm10:10x",
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
          cellnote=format(mm10, big.mark =",") ,
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
mm10_gof <- matrix(c(42, 1929, 25, 1724, 21, 115, 137, 3, 91, 22, 24, 16, 23, 55,0, 2, 2, 0, 14, 12) ,ncol = 5, byrow=T)

mm10 <- mm10_gof
mm10 <- cbind(mm10, rowSums(mm10))
mm10 <- rbind(mm10, colSums(mm10))
rownames(mm10) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(mm10) <- c("Pois", "NB", "PB" ,"PIG", "DEL", "Sum")



svg(paste0( "mm10_heatmap_GOF_new5.svg"),  width = 7, height = 8)
library(gplots)
heatmap.2(log10(mm10+1),
          main = "Overview fitted distributions: mm10:10x, after GOF",
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
          cellnote=format(mm10, big.mark =",") ,
          notecol= "black",
          notecex = 1,
          margins = c(6,6),
          cexRow = 1.5,
          cexCol=1.5,
          key.title = NA,
          keysize = 1
)

dev.off()

#########################################################################################################################################################
# ALT ALT ALT ALT
#Heatmap of mm10 Fits
mm10_fit <- matrix(c(52,3400, 49,193,287,6,111,104,74,12,9,0) ,ncol = 3, byrow=T)

mm10 <- mm10_fit
mm10 <- cbind(mm10, rowSums(mm10))
mm10 <- rbind(mm10, colSums(mm10))
rownames(mm10) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(mm10) <- c("Pois", "NB", "PB", "Sum")

my_palette2 <- colorRampPalette(c( rgb(229,249,251, 1, maxColorValue = 255),rgb(142,229,238, 1, maxColorValue = 255),rgb(21,122,133, 1, maxColorValue = 255) ))(n = 200)



svg(paste0( "mm10_heatmap_Fit_new.svg"),  width = 7, height = 8)
library(gplots)
heatmap.2(log10(mm10+1),
          main = "Overview fitted distributions: mm10:10x",
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
          cellnote=format(mm10, big.mark =",") ,
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
mm10_gof <- matrix(c(50,3374, 30,193,266,3,111,98,58,12,8,0) ,ncol = 3, byrow=T)

mm10 <- mm10_gof
mm10 <- cbind(mm10, rowSums(mm10))
mm10 <- rbind(mm10, colSums(mm10))
rownames(mm10) <- c("1-Pop", "ZI", "2-Pop",  "ZI-2-Pop", "Sum")
colnames(mm10) <- c("Pois", "NB", "PB", "Sum")



svg(paste0( "mm10_heatmap_GOF_new.svg"),  width = 7, height = 8)
library(gplots)
heatmap.2(log10(mm10+1),
          main = "Overview fitted distributions: mm10:10x, after GOF",
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
          cellnote=format(mm10, big.mark =",") ,
          notecol= "black",
          notecex = 1,
          margins = c(6,6),
          cexRow = 1.5,
          cexCol=1.5,
          key.title = NA,
          keysize = 1
)

dev.off()


