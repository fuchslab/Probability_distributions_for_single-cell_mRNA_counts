# Plot fits
library(viridis)
plotColor <- magma(12,begin = 0)
plotColor2 <- cividis(12,begin = 0)
plotColor3 <- viridis(12,begin = 0)

# Aus Distribution Comparison Plot
#"NB" plotColor3[5]
#"PIG" plotColor[5]
#"PB: c = 10 E" plotColor3[8]
#"PB: c = 1000 E" plotColor3[10]
#"DEL: nu = 0.1" plotColor2[9]
#"DEL: nu = 0.9"plotColor2[12]

# von vorher:
# Pois: rgb(003e6eff) rgb( 0, 62, 110, maxColorValue= 255)
# NB: rgb( 116, 149, 152, maxColorValue= 255)

col_def <- c( rgb( 0, 62, 110, maxColorValue= 255), rgb( 116, 149, 152, maxColorValue= 255), plotColor[5],  plotColor2[9] , plotColor3[10])
# reihenfolge: pois, nb, pig, del, pb

pch_comb <- c(19, 17,3,4)
lwd_comb <- c(0,0,2,2)

# Symbols and thickness:
#(aber hier andere symbole, je nach komplexit?t des models)
symb_def <- c( 19,  17, 4,  15, 3)
#symb_def <- c("pois"= 19, "nb"= 17, "pig" = 4,  "del" = 15, "pb"=  3)

def_lwd = c(0, 0, 2, 0, 2)
#def_lwd = c("pois"=0, "nb"=0,"pig" = 2,"del" =0,"pb"= 2)


load(file="Overview_mm10.rds")
Overview_mm10$model<-as.numeric(Overview_mm10$model)


Overview_mm10$BIC_col <- "NA"
Overview_mm10$BIC_pch <- "NA"

for (id in rownames(Overview_mm10)) {
    if(Overview_mm10[id,]$model >=1 && Overview_mm10[id,]$model<5) Overview_mm10[id,]$BIC_col <- 1
    if(Overview_mm10[id,]$model >=5 && Overview_mm10[id,]$model<9) Overview_mm10[id,]$BIC_col <- 2
    if(Overview_mm10[id,]$model >=9 && Overview_mm10[id,]$model<13) Overview_mm10[id,]$BIC_col <- 3
    if(Overview_mm10[id,]$model >=12 && Overview_mm10[id,]$model<17) Overview_mm10[id,]$BIC_col <- 4
    if(Overview_mm10[id,]$model >=15 && Overview_mm10[id,]$model<21) Overview_mm10[id,]$BIC_col <- 5
    if(Overview_mm10[id,]$model ==1 || Overview_mm10[id,]$model==5 || Overview_mm10[id,]$model==9|| Overview_mm10[id,]$model==13|| Overview_mm10[id,]$model==17) Overview_mm10[id,]$BIC_pch <- 1
    if(Overview_mm10[id,]$model ==2 || Overview_mm10[id,]$model==6 || Overview_mm10[id,]$model==10|| Overview_mm10[id,]$model==14|| Overview_mm10[id,]$model==18) Overview_mm10[id,]$BIC_pch <- 2
    if(Overview_mm10[id,]$model ==3 || Overview_mm10[id,]$model==7 || Overview_mm10[id,]$model==11|| Overview_mm10[id,]$model==15|| Overview_mm10[id,]$model==19) Overview_mm10[id,]$BIC_pch <- 3
    if(Overview_mm10[id,]$model ==4 || Overview_mm10[id,]$model==8 || Overview_mm10[id,]$model==12|| Overview_mm10[id,]$model==16|| Overview_mm10[id,]$model==20) Overview_mm10[id,]$BIC_pch <- 4

    }

Overview_mm10$BIC_col<-as.numeric(Overview_mm10$BIC_col)
Overview_mm10$BIC_pch<-as.numeric(Overview_mm10$BIC_pch)

Overview_mm10$NB_prob <- Overview_mm10$NB_size/(Overview_mm10$NB_size+Overview_mm10$NB_mu)


Overview_mm10_all <- Overview_mm10

# Keep only those IDs that passed the GOF!

load("Result_mm10_BIC_GOF.rda")

Overview_mm10 <- Overview_mm10[rownames(mm10_GOF)[mm10_GOF$x2_adj==TRUE],]


#plot Pois
pdf("Pois_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda,rep(0,length(Overview_mm10$Pois_lambda)),xlim =c(0,250), bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col] , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#plot NB
pdf("NB1_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu,Overview_mm10$PIG_lambda,xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col], xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob,Overview_mm10$PIG_mu, bty ="n",xlim =c(0,1), ylim =c(0,18),type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col] , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()




#plot PB
pdf("PB1_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha,Overview_mm10$PB_beta,xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col] , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c,Overview_mm10$PB_beta, xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col] , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha,Overview_mm10$PB_c, xlim =c(0,72),ylim=c(0,11500),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col] , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()



#Highlight distribution in one plot
HL_Pois <- Overview_mm10$BIC_col
HL_Pois[HL_Pois != 1] <- 0

HL_NB <- Overview_mm10$BIC_col
HL_NB[HL_NB != 2 ] <- 0

HL_PB <- Overview_mm10$BIC_col
HL_PB[HL_PB != 5 ] <- 0

HL_PIG <- Overview_mm10$BIC_col
HL_PIG[HL_PIG != 3 ] <- 0

HL_DEL <- Overview_mm10$BIC_col
HL_DEL[HL_DEL != 4 ] <- 0

###Poisson distribution
#Highlight Pois pch = pch_comb[Overview_mm10$BIC_pch], lwd = lwd_comb[Overview_mm10$BIC_pch], cex = 1.2, col = col_def[Overview_mm10$BIC_col]
pdf("Pois_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_Pois == 0],rep(0,length(Overview_mm10$Pois_lambda[HL_Pois == 0])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_Pois != 0],rep(0,length(Overview_mm10$Pois_lambda[HL_Pois != 0])), col = col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight NB
pdf("Pois_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_NB == 0],rep(0,length(Overview_mm10$Pois_lambda[HL_NB == 0])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) , cex = 1.2,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_NB != 0],rep(0,length(Overview_mm10$Pois_lambda[HL_NB != 0])), col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight PB
pdf("Pois_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_PB == 0],rep(0,length(Overview_mm10$Pois_lambda[HL_PB == 0])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)), cex = 1.2 ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_PB != 0],rep(0,length(Overview_mm10$Pois_lambda[HL_PB != 0])), col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()


#Highlight PIG
pdf("Pois_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_PIG == 0],rep(0,length(Overview_mm10$Pois_lambda[HL_PIG == 0])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)), cex = 1.2 ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_PIG != 0],rep(0,length(Overview_mm10$Pois_lambda[HL_PIG != 0])), col = col_def[3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight DEL
pdf("Pois_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_DEL == 0],rep(0,length(Overview_mm10$Pois_lambda[HL_DEL == 0])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)), cex = 1.2 ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_DEL != 0],rep(0,length(Overview_mm10$Pois_lambda[HL_DEL != 0])), col = col_def[4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()


####################################################################################################################################################################################################
### NB distribution
#Highligh PB
pdf("NB1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_Pois == 0],Overview_mm10$NB_mu[HL_Pois == 0], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8 , xlab = "parameter of negative binomial: size", cex = 1.2, ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_Pois != 0],Overview_mm10$NB_mu[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_Pois == 0],Overview_mm10$NB_size[HL_Pois == 0], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_Pois != 0],Overview_mm10$NB_size[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight NB
pdf("NB1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_NB == 0],Overview_mm10$NB_mu[HL_NB == 0], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_NB != 0],Overview_mm10$NB_mu[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_NB == 0],Overview_mm10$NB_size[HL_NB == 0],xlim =c(0,1), ylim =c(0,18), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], col = 8 , cex = 1.2, xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_NB != 0],Overview_mm10$NB_size[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight PB
pdf("NB1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_PB == 0],Overview_mm10$NB_mu[HL_PB == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_PB != 0],Overview_mm10$NB_mu[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_PB == 0],Overview_mm10$NB_size[HL_PB == 0], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_PB != 0],Overview_mm10$NB_size[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight PIG
pdf("NB1_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_PIG == 0],Overview_mm10$NB_mu[HL_PIG == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_PIG != 0],Overview_mm10$NB_mu[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_PIG == 0],Overview_mm10$NB_size[HL_PIG == 0], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_PIG != 0],Overview_mm10$NB_size[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

#Highlight DEL
pdf("NB1_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_DEL == 0],Overview_mm10$NB_mu[HL_DEL == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_DEL != 0],Overview_mm10$NB_mu[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("NB2_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_DEL == 0],Overview_mm10$NB_size[HL_DEL == 0], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_DEL != 0],Overview_mm10$NB_size[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

###############################################################################################################################################################################################################
#### PB distribution
# Highlight Pois
pdf("PB1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 0],Overview_mm10$PB_beta[HL_Pois == 0],xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8, cex = 1.2 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_Pois != 0],Overview_mm10$PB_beta[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_Pois == 0],Overview_mm10$PB_beta[HL_Pois == 0],xlim =c(0,12000),ylim=c(0,1300), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_Pois != 0],Overview_mm10$PB_beta[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 0],Overview_mm10$PB_c[HL_Pois == 0], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_Pois != 0],Overview_mm10$PB_c[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight NB
pdf("PB1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 0],Overview_mm10$PB_beta[HL_NB == 0], xlim =c(0,80),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_NB != 0],Overview_mm10$PB_beta[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_NB == 0],Overview_mm10$PB_beta[HL_NB == 0], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_NB != 0],Overview_mm10$PB_beta[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 0],Overview_mm10$PB_c[HL_NB == 0], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_NB != 0],Overview_mm10$PB_c[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight PB
pdf("PB1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 0],Overview_mm10$PB_beta[HL_PB == 0],xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.2, xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PB != 0],Overview_mm10$PB_beta[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_PB == 0],Overview_mm10$PB_beta[HL_PB == 0], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.2, xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_PB != 0],Overview_mm10$PB_beta[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 0],Overview_mm10$PB_c[HL_PB == 0], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.28 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PB != 0],Overview_mm10$PB_c[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight PIG
pdf("PB1_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PIG == 0],Overview_mm10$PB_beta[HL_PIG == 0], xlim =c(0,80),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PIG != 0],Overview_mm10$PB_beta[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_PIG == 0],Overview_mm10$PB_beta[HL_PIG == 0], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_PIG != 0],Overview_mm10$PB_beta[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PIG == 0],Overview_mm10$PB_c[HL_PIG == 0], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PIG != 0],Overview_mm10$PB_c[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight DEL
pdf("PB1_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_DEL == 0],Overview_mm10$PB_beta[HL_DEL == 0], xlim =c(0,80),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_DEL != 0],Overview_mm10$PB_beta[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB2_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_DEL == 0],Overview_mm10$PB_beta[HL_DEL == 0], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_DEL != 0],Overview_mm10$PB_beta[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("PB3_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_DEL == 0],Overview_mm10$PB_c[HL_DEL == 0], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_DEL != 0],Overview_mm10$PB_c[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

####################################################################################################################################################################################################
### PIG distribution
#Highligh Pois
pdf("PIG1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_Pois == 0],Overview_mm10$PIG_sigma[HL_Pois == 0],bty ="n",type = "p", xlim=c(0,150), ylim=c(0,60), pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8 , xlab = bquote(paste("1st Parameter of PIG: ",mu)), cex = 1.2, ylab = bquote(paste("2nd Parameter of PIG: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PIG_mu[HL_Pois != 0],Overview_mm10$PIG_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()


#Highlight NB
pdf("PIG1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_NB == 0],Overview_mm10$PIG_sigma[HL_NB == 0], xlim=c(0,150), ylim=c(0,60),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of PIG: ",mu)), ylab = bquote(paste("2nd Parameter of PIG: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PIG_mu[HL_NB != 0],Overview_mm10$PIG_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()



#Highlight PB
pdf("PIG1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_PB == 0],Overview_mm10$PIG_sigma[HL_PB == 0], xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("1st Parameter of PIG: ",mu)), ylab =  bquote(paste("2nd Parameter of PIG: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PIG_mu[HL_PB != 0],Overview_mm10$PIG_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()


#Highlight PIG
pdf("PIG1_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_PIG == 0],Overview_mm10$PIG_sigma[HL_PIG == 0], xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of PIG: ",mu)), ylab = bquote(paste("2nd Parameter of PIG: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PIG_mu[HL_PIG != 0],Overview_mm10$PIG_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()



#Highlight DEL
pdf("PIG1_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_DEL == 0],Overview_mm10$PIG_sigma[HL_DEL == 0], xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of PIG: ",mu)), ylab = bquote(paste("2nd Parameter of PIG: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PIG_mu[HL_DEL != 0],Overview_mm10$PIG_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

###############################################################################################################################################################################################################
#### DEL distribution
# Highlight Pois
pdf("DEL1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_Pois == 0],Overview_mm10$DEL_sigma[HL_Pois == 0], xlim=c(0,150), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8, cex = 1.2 , xlab = bquote(paste("1st Parameter of DEL: ",mu)) , ylab = bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_Pois != 0],Overview_mm10$DEL_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL2_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_Pois == 0],Overview_mm10$DEL_sigma[HL_Pois == 0], xlim=c(0,1), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of DEL: ", nu)), ylab =  bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_nu[HL_Pois != 0],Overview_mm10$DEL_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL3_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_Pois == 0],Overview_mm10$DEL_nu[HL_Pois == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)), ylab =bquote(paste("3rd Parameter of DEL: ", nu)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_Pois != 0],Overview_mm10$DEL_nu[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight NB
pdf("DEL1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_NB == 0],Overview_mm10$DEL_sigma[HL_NB == 0], xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)) , ylab = bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_NB != 0],Overview_mm10$DEL_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL2_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_NB == 0],Overview_mm10$DEL_sigma[HL_NB == 0], xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of DEL: ", nu)), ylab =  bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_nu[HL_NB != 0],Overview_mm10$DEL_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL3_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_NB == 0],Overview_mm10$DEL_nu[HL_NB == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)), ylab =bquote(paste("3rd Parameter of DEL: ", nu)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_NB != 0],Overview_mm10$DEL_nu[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight PB
pdf("DEL1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PB == 0],Overview_mm10$DEL_sigma[HL_PB == 0], xlim=c(0,150), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.2, xlab = bquote(paste("1st Parameter of DEL: ",mu)) , ylab = bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_PB != 0],Overview_mm10$DEL_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL2_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_PB == 0],Overview_mm10$DEL_sigma[HL_PB == 0], xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.2, xlab =  bquote(paste("3rd Parameter of DEL: ", nu)), ylab =  bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_nu[HL_PB != 0],Overview_mm10$DEL_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL3_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PB == 0],Overview_mm10$DEL_nu[HL_PB == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.28 , xlab = bquote(paste("1st Parameter of DEL: ",mu)), ylab =bquote(paste("3rd Parameter of DEL: ", nu)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_PB != 0],Overview_mm10$DEL_nu[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight PIG
pdf("DEL1_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PIG == 0],Overview_mm10$DEL_sigma[HL_PIG == 0], xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)) , ylab = bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_PIG != 0],Overview_mm10$DEL_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL2_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_PIG == 0],Overview_mm10$DEL_sigma[HL_PIG == 0], xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of DEL: ", nu)), ylab =  bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_nu[HL_PIG != 0],Overview_mm10$DEL_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL3_mm10_HL_PIG.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PIG == 0],Overview_mm10$DEL_nu[HL_PIG == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)), ylab =bquote(paste("3rd Parameter of DEL: ", nu)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_PIG != 0],Overview_mm10$DEL_nu[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

# Highlight DEL
pdf("DEL1_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_DEL == 0],Overview_mm10$DEL_sigma[HL_DEL == 0], xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)) , ylab = bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_DEL != 0],Overview_mm10$DEL_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL2_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_DEL == 0],Overview_mm10$DEL_sigma[HL_DEL == 0], xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab =  bquote(paste("3rd Parameter of DEL: ", nu)), ylab =  bquote(paste("2nd Parameter of DEL: ",sigma)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_nu[HL_DEL != 0],Overview_mm10$DEL_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()

pdf("DEL3_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_DEL == 0],Overview_mm10$DEL_nu[HL_DEL == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.2, col = 8 , xlab = bquote(paste("1st Parameter of DEL: ",mu)), ylab =bquote(paste("3rd Parameter of DEL: ", nu)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$DEL_mu[HL_DEL != 0],Overview_mm10$DEL_nu[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.2)
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.2)
dev.off()


####################################################################################################################################################################################################
####################################################################################################################################################################################################
####################################################################################################################################################################################################
####################################################################################################################################################################################################
####################################################################################################################################################################################################


pdf("ALL.pdf", width = 13, height = 17)
par(mfrow=c(9,5), mar=c(3.9,0.5, 2, 0.3),mgp=c(2.4, .5, 0), oma = c(0, 4, 0, 0), xpd = FALSE)

par(xpd = NA)
#pdf("Pois_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_Pois == 0],rep(5,length(Overview_mm10$Pois_lambda[HL_Pois == 0])),xlim =c(0,250),bty ="n", ylim=c(-0.5,5.5), yaxt = "n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",lambda," of Poisson distribution")) ,ylab ="")
points(Overview_mm10$Pois_lambda[HL_Pois != 0],rep(5,length(Overview_mm10$Pois_lambda[HL_Pois != 0])), col = col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight NB
#pdf("Pois_mm10_HL_NB.#pdf", width = 10, height = 4)
points(Overview_mm10$Pois_lambda[HL_NB == 0],rep(4,length(Overview_mm10$Pois_lambda[HL_NB == 0])),xlim =c(0,250),bty ="n", yaxt = "n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], col = 8 , cex = 1.0,ylab ="")
points(Overview_mm10$Pois_lambda[HL_NB != 0],rep(4,length(Overview_mm10$Pois_lambda[HL_NB != 0])), col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#Highlight PB
#pdf("Pois_mm10_HL_PB.#pdf", width = 10, height = 4)
points(Overview_mm10$Pois_lambda[HL_PB == 0],rep(3,length(Overview_mm10$Pois_lambda[HL_PB == 0])),xlim =c(0,250),bty ="n", yaxt = "n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8, cex = 1.0,ylab ="" )
points(Overview_mm10$Pois_lambda[HL_PB != 0],rep(3,length(Overview_mm10$Pois_lambda[HL_PB != 0])), col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()


#Highlight PIG
#pdf("Pois_mm10_HL_PIG.#pdf", width = 10, height = 4)
points(Overview_mm10$Pois_lambda[HL_PIG == 0],rep(2,length(Overview_mm10$Pois_lambda[HL_PIG == 0])),xlim =c(0,250),bty ="n", yaxt = "n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], col = 8 , cex = 1.0 ,ylab ="")
points(Overview_mm10$Pois_lambda[HL_PIG != 0],rep(2,length(Overview_mm10$Pois_lambda[HL_PIG != 0])), col = col_def[3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#Highlight DEL
#pdf("Pois_mm10_HL_DEL.#pdf", width = 10, height = 4)
points(Overview_mm10$Pois_lambda[HL_DEL == 0],rep(1,length(Overview_mm10$Pois_lambda[HL_DEL == 0])),xlim =c(0,250),bty ="n", yaxt = "n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], col = 8 , cex = 1.0 ,ylab ="")
points(Overview_mm10$Pois_lambda[HL_DEL != 0],rep(1,length(Overview_mm10$Pois_lambda[HL_DEL != 0])), col = col_def[4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()
fig_label("A: Estimated distribution: Poisson (one population)", cex = 2)

par(xpd = FALSE)
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
legend("topright", legend = c("Pois", "NB","PB","PIG", "DEL"), lty = 0, col = c(col_def[c(1,2,5,3,4)]), pch = c(16,16,16,16,16), bty ="n", lwd = c(0,0,0,0,0), cex = 1.2)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
legend("topleft", legend = c("1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = 1, pch = c(pch_comb), bty ="n", lwd = c(0,0,2,2), cex = 1.2)


####################################################################################################################################################################################################
### NB distribution
#Highligh PB
#pdf("NB1_mm10_HL_Pois.#pdf", width = 10, height = 4)
#plot(Overview_mm10$NB_size[HL_Pois == 0],Overview_mm10$NB_mu[HL_Pois == 0], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8 , xlab = "parameter of negative binomial: size", cex = 1.0, ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
#points(Overview_mm10$NB_size[HL_Pois != 0],Overview_mm10$NB_mu[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

par(xpd = NA)
#pdf("NB2_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_Pois == 0],Overview_mm10$NB_size[HL_Pois == 0], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",p," of NB distribution")), ylab = bquote(paste( " Parameter ",r," of NB distribution")), las = 1)
points(Overview_mm10$NB_prob[HL_Pois != 0],Overview_mm10$NB_size[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()
fig_label("B: Estimated distribution: NB (one population)", cex = 2)
par(xpd = FALSE)

#Highlight NB
#pdf("NB1_mm10_HL_NB.#pdf", width = 10, height = 4)
#plot(Overview_mm10$NB_size[HL_NB == 0],Overview_mm10$NB_mu[HL_NB == 0], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
#points(Overview_mm10$NB_size[HL_NB != 0],Overview_mm10$NB_mu[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("NB2_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_NB == 0],Overview_mm10$NB_size[HL_NB == 0], yaxt = 'n',xlim =c(0,1), ylim =c(0,18), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], col = 8 , cex = 1.0, xlab = bquote(paste("Parameter ",p," of NB distribution")),ylab ="")
points(Overview_mm10$NB_prob[HL_NB != 0],Overview_mm10$NB_size[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#Highlight PB
#pdf("NB1_mm10_HL_PB.#pdf", width = 10, height = 4)
#plot(Overview_mm10$NB_size[HL_PB == 0],Overview_mm10$NB_mu[HL_PB == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.0, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
#points(Overview_mm10$NB_size[HL_PB != 0],Overview_mm10$NB_mu[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("NB2_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_PB == 0],Overview_mm10$NB_size[HL_PB == 0], xlim =c(0,1), yaxt = 'n', bty = 'n', ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",p," of NB distribution")),ylab ="")
points(Overview_mm10$NB_prob[HL_PB != 0],Overview_mm10$NB_size[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#Highlight PIG
#pdf("NB1_mm10_HL_PIG.#pdf", width = 10, height = 4)
#plot(Overview_mm10$NB_size[HL_PIG == 0],Overview_mm10$NB_mu[HL_PIG == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
#points(Overview_mm10$NB_size[HL_PIG != 0],Overview_mm10$NB_mu[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("NB2_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_PIG == 0],Overview_mm10$NB_size[HL_PIG == 0], xlim =c(0,1), yaxt = 'n', bty = 'n', ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",p," of NB distribution")), ylab ="")
points(Overview_mm10$NB_prob[HL_PIG != 0],Overview_mm10$NB_size[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#Highlight DEL
#pdf("NB1_mm10_HL_DEL.#pdf", width = 10, height = 4)
#plot(Overview_mm10$NB_size[HL_DEL == 0],Overview_mm10$NB_mu[HL_DEL == 0],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
#points(Overview_mm10$NB_size[HL_DEL != 0],Overview_mm10$NB_mu[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("NB2_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_DEL == 0],Overview_mm10$NB_size[HL_DEL == 0], xlim =c(0,1), yaxt = 'n', bty = 'n', ylim =c(0,18),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",p," of NB distribution")), ylab ="")
points(Overview_mm10$NB_prob[HL_DEL != 0],Overview_mm10$NB_size[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

###############################################################################################################################################################################################################
#### PB distribution
# Highlight Pois
par(xpd = NA)
#pdf("PB1_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 0],Overview_mm10$PB_beta[HL_Pois == 0],xlim =c(0,40),ylim=c(0,600), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8, cex = 1.0 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")), ylab = bquote(paste("Parameter ", beta, " of PB distribution")), las = 1)
points(Overview_mm10$PB_alpha[HL_Pois != 0],Overview_mm10$PB_beta[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()
fig_label("C: Estimated distribution: PB (one population)", cex = 2)
par(xpd = FALSE)

# Highlight NB
#pdf("PB1_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 0],Overview_mm10$PB_beta[HL_NB == 0], yaxt = 'n',xlim =c(0,40),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")) ,ylab ="")
points(Overview_mm10$PB_alpha[HL_NB != 0],Overview_mm10$PB_beta[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight PB
#pdf("PB1_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 0],Overview_mm10$PB_beta[HL_PB == 0], yaxt = 'n',xlim =c(0,40),ylim=c(0,600), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.0, xlab = bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="")
points(Overview_mm10$PB_alpha[HL_PB != 0],Overview_mm10$PB_beta[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight PIG
#pdf("PB1_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PIG == 0],Overview_mm10$PB_beta[HL_PIG == 0], yaxt = 'n',xlim =c(0,40),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")) ,ylab ="")
points(Overview_mm10$PB_alpha[HL_PIG != 0],Overview_mm10$PB_beta[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight DEL
#pdf("PB1_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_DEL == 0],Overview_mm10$PB_beta[HL_DEL == 0], yaxt = 'n',xlim =c(0,40),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab =bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="" )
points(Overview_mm10$PB_alpha[HL_DEL != 0],Overview_mm10$PB_beta[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()




#pdf("PB2_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_Pois == 0],Overview_mm10$PB_beta[HL_Pois == 0],xlim =c(0,4000),ylim=c(0,600), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",c," of PB distribution")), ylab =  bquote(paste("Parameter ",beta," of PB distribution")), las = 1)
points(Overview_mm10$PB_c[HL_Pois != 0],Overview_mm10$PB_beta[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB2_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_NB == 0],Overview_mm10$PB_beta[HL_NB == 0], yaxt = 'n',xlim =c(0,4000),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",c," of PB distribution")), ylab ="")
points(Overview_mm10$PB_c[HL_NB != 0],Overview_mm10$PB_beta[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB2_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_PB == 0],Overview_mm10$PB_beta[HL_PB == 0], yaxt = 'n',xlim =c(0,4000),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.0, xlab =  bquote(paste("Parameter ",c," of PB distribution")), ylab ="")
points(Overview_mm10$PB_c[HL_PB != 0],Overview_mm10$PB_beta[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB2_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_PIG == 0],Overview_mm10$PB_beta[HL_PIG == 0], yaxt = 'n',xlim =c(0,4000),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",c," of PB distribution")), ylab ="")
points(Overview_mm10$PB_c[HL_PIG != 0],Overview_mm10$PB_beta[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB2_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_DEL == 0],Overview_mm10$PB_beta[HL_DEL == 0], yaxt = 'n',xlim =c(0,4000),ylim=c(0,600),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",c," of PB distribution")), ylab ="")
points(Overview_mm10$PB_c[HL_DEL != 0],Overview_mm10$PB_beta[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()



#pdf("PB3_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 0],Overview_mm10$PB_c[HL_Pois == 0], xlim =c(0,40),ylim=c(0,4000), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")), ylab = bquote(paste("Parameter ",c," of PB distribution")), las = 1)
points(Overview_mm10$PB_alpha[HL_Pois != 0],Overview_mm10$PB_c[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB3_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 0],Overview_mm10$PB_c[HL_NB == 0], yaxt = 'n', xlim =c(0,40),ylim=c(0,4000), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="")
points(Overview_mm10$PB_alpha[HL_NB != 0],Overview_mm10$PB_c[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB3_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 0],Overview_mm10$PB_c[HL_PB == 0], yaxt = 'n', xlim =c(0,40),ylim=c(0,4000), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.08 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="")
points(Overview_mm10$PB_alpha[HL_PB != 0],Overview_mm10$PB_c[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()


#pdf("PB3_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PIG == 0],Overview_mm10$PB_c[HL_PIG == 0], yaxt = 'n', bty = 'n', xlim =c(0,40),ylim=c(0,4000), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="")
points(Overview_mm10$PB_alpha[HL_PIG != 0],Overview_mm10$PB_c[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("PB3_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_DEL == 0],Overview_mm10$PB_c[HL_DEL == 0], yaxt = 'n', bty = 'n', xlim =c(0,40),ylim=c(0,4000), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",alpha," of PB distribution")),ylab ="")
points(Overview_mm10$PB_alpha[HL_DEL != 0],Overview_mm10$PB_c[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

####################################################################################################################################################################################################
### PIG distribution
#Highligh Pois
par(xpd = NA)
#pdf("PIG1_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_Pois == 0],Overview_mm10$PIG_sigma[HL_Pois == 0],bty ="n",type = "p", xlim=c(0,150), ylim=c(0,60), pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8 , xlab = bquote(paste("Parameter ",mu," of PIG distribution")), cex = 1.0, ylab =  bquote(paste("Parameter ",sigma," of PIG distribution")), las = 1)
points(Overview_mm10$PIG_mu[HL_Pois != 0],Overview_mm10$PIG_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()
fig_label("D: Estimated distribution: PIG (one population)", cex = 2)
par(xpd = FALSE)

#Highlight NB
#pdf("PIG1_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_NB == 0],Overview_mm10$PIG_sigma[HL_NB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,60),bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",mu," of PIG distribution")),ylab ="")
points(Overview_mm10$PIG_mu[HL_NB != 0],Overview_mm10$PIG_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()



#Highlight PB
#pdf("PIG1_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_PB == 0],Overview_mm10$PIG_sigma[HL_PB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], cex = 1.0, col = 8 , xlab =   bquote(paste("Parameter ",mu," of PIG distribution")),ylab ="")
points(Overview_mm10$PIG_mu[HL_PB != 0],Overview_mm10$PIG_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()


#Highlight PIG
#pdf("PIG1_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_PIG == 0],Overview_mm10$PIG_sigma[HL_PIG == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",mu," of PIG distribution")),ylab ="")
points(Overview_mm10$PIG_mu[HL_PIG != 0],Overview_mm10$PIG_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()



#Highlight DEL
#pdf("PIG1_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$PIG_mu[HL_DEL == 0],Overview_mm10$PIG_sigma[HL_DEL == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,60), bty ="n",type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",mu," of PIG distribution")),ylab ="")
points(Overview_mm10$PIG_mu[HL_DEL != 0],Overview_mm10$PIG_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

###############################################################################################################################################################################################################
#### DEL distribution
# Highlight Pois
par(xpd = NA)
#pdf("DEL1_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_Pois == 0],Overview_mm10$DEL_sigma[HL_Pois == 0], xlim=c(0,150), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], col = 8, cex = 1.0 , xlab =  bquote(paste("Parameter ",mu," of DEL distribution")) , ylab =  bquote(paste("Parameter ",sigma," of DEL distribution")), las = 1)
points(Overview_mm10$DEL_mu[HL_Pois != 0],Overview_mm10$DEL_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()
fig_label("E: Estimated distribution: DEL (one population)", cex = 2)
par(xpd = FALSE)

# Highlight NB
#pdf("DEL1_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_NB == 0],Overview_mm10$DEL_sigma[HL_NB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")),ylab ="" )
points(Overview_mm10$DEL_mu[HL_NB != 0],Overview_mm10$DEL_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight PB
#pdf("DEL1_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PB == 0],Overview_mm10$DEL_sigma[HL_PB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.0, xlab = bquote(paste("Parameter ",mu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_mu[HL_PB != 0],Overview_mm10$DEL_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight PIG
#pdf("DEL1_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PIG == 0],Overview_mm10$DEL_sigma[HL_PIG == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_mu[HL_PIG != 0],Overview_mm10$DEL_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

# Highlight DEL
#pdf("DEL1_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_DEL == 0],Overview_mm10$DEL_sigma[HL_DEL == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")),ylab ="" )
points(Overview_mm10$DEL_mu[HL_DEL != 0],Overview_mm10$DEL_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()



#pdf("DEL2_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_Pois == 0],Overview_mm10$DEL_sigma[HL_Pois == 0], xlim=c(0,1), ylim=c(0,25), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",nu," of DEL distribution")), ylab =  bquote(paste("Parameter ",sigma," of DEL distribution")), las = 1)
points(Overview_mm10$DEL_nu[HL_Pois != 0],Overview_mm10$DEL_sigma[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL2_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_NB == 0],Overview_mm10$DEL_sigma[HL_NB == 0], yaxt = 'n', xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",nu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_nu[HL_NB != 0],Overview_mm10$DEL_sigma[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL2_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_PB == 0],Overview_mm10$DEL_sigma[HL_PB == 0], yaxt = 'n', xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.0, xlab =  bquote(paste("Parameter ",nu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_nu[HL_PB != 0],Overview_mm10$DEL_sigma[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL2_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_PIG == 0],Overview_mm10$DEL_sigma[HL_PIG == 0], yaxt = 'n', xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",nu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_nu[HL_PIG != 0],Overview_mm10$DEL_sigma[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL2_mm10_HL_DEL.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_nu[HL_DEL == 0],Overview_mm10$DEL_sigma[HL_DEL == 0], yaxt = 'n', xlim=c(0,1), ylim=c(0,25),bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab =  bquote(paste("Parameter ",nu," of DEL distribution")),ylab ="")
points(Overview_mm10$DEL_nu[HL_DEL != 0],Overview_mm10$DEL_sigma[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()


#pdf("DEL3_mm10_HL_Pois.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_Pois == 0],Overview_mm10$DEL_nu[HL_Pois == 0], xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")), ylab =bquote(paste("Parameter ",nu," of DEL distribution")), las = 1)
points(Overview_mm10$DEL_mu[HL_Pois != 0],Overview_mm10$DEL_nu[HL_Pois != 0], col=col_def[1], pch = pch_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_Pois != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL3_mm10_HL_NB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_NB == 0],Overview_mm10$DEL_nu[HL_NB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_NB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")), ylab ="")
points(Overview_mm10$DEL_mu[HL_NB != 0],Overview_mm10$DEL_nu[HL_NB != 0], col = col_def[ 2], pch = pch_comb[Overview_mm10$BIC_pch[HL_NB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_NB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL3_mm10_HL_PB.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PB == 0],Overview_mm10$DEL_nu[HL_PB == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PB == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB == 0]], col = 8 , cex = 1.08 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")), ylab ="")
points(Overview_mm10$DEL_mu[HL_PB != 0],Overview_mm10$DEL_nu[HL_PB != 0], col = col_def[ 5], pch = pch_comb[Overview_mm10$BIC_pch[HL_PB != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PB != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL3_mm10_HL_PIG.#pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_PIG == 0],Overview_mm10$DEL_nu[HL_PIG == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")), ylab ="")
points(Overview_mm10$DEL_mu[HL_PIG != 0],Overview_mm10$DEL_nu[HL_PIG != 0], col = col_def[ 3], pch = pch_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_PIG != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

#pdf("DEL3_mm10_HL_DEL.pdf", width = 10, height = 4)
plot(Overview_mm10$DEL_mu[HL_DEL == 0],Overview_mm10$DEL_nu[HL_DEL == 0], yaxt = 'n', xlim=c(0,150), ylim=c(0,1), bty ="n", type = "p", pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL == 0]], cex = 1.0, col = 8 , xlab = bquote(paste("Parameter ",mu," of DEL distribution")), ylab ="")
points(Overview_mm10$DEL_mu[HL_DEL != 0],Overview_mm10$DEL_nu[HL_DEL != 0], col = col_def[ 4], pch = pch_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], lwd = lwd_comb[Overview_mm10$BIC_pch[HL_DEL != 0]], cex = 1.0)
#legend("topright", #legend = c("Pois", "NB","PB","PIG", "DEL","1-Pop","ZI","2-Pop","ZI-2-Pop"), lty = 0, col = c(col_def[c(1,2,5,3,4)],1,1,1,1), pch = c(16,16,16,16,16,pch_comb), bty ="n", lwd = c(0,0,0,0,0,0,0,2,2), cex = 1.0)
#dev.off()

dev.off()













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



