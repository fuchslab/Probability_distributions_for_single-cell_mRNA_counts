# Plot fits


load(file="Overview_mm10.rds")
Overview_mm10$model<-as.numeric(Overview_mm10$model)


Overview_mm10$BIC_col <- "NA"
Overview_mm10$BIC_pch <- "NA"

for (id in rownames(Overview_mm10)) {
    if(Overview_mm10[id,]$model >=1 && Overview_mm10[id,]$model<5) Overview_mm10[id,]$BIC_col <- 2
    if(Overview_mm10[id,]$model >=5 && Overview_mm10[id,]$model<9) Overview_mm10[id,]$BIC_col <- 3
    if(Overview_mm10[id,]$model >=9 && Overview_mm10[id,]$model<12) Overview_mm10[id,]$BIC_col <- 4
    if(Overview_mm10[id,]$model ==1 || Overview_mm10[id,]$model==5 || Overview_mm10[id,]$model==9) Overview_mm10[id,]$BIC_pch <- 1
    if(Overview_mm10[id,]$model ==2 || Overview_mm10[id,]$model==6 || Overview_mm10[id,]$model==10) Overview_mm10[id,]$BIC_pch <- 2
    if(Overview_mm10[id,]$model ==3 || Overview_mm10[id,]$model==7 || Overview_mm10[id,]$model==11) Overview_mm10[id,]$BIC_pch <- 3
    if(Overview_mm10[id,]$model ==4 || Overview_mm10[id,]$model==8 || Overview_mm10[id,]$model==12) Overview_mm10[id,]$BIC_pch <- 4

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
plot(Overview_mm10$Pois_lambda,rep(0,length(Overview_mm10$Pois_lambda)),xlim =c(0,250), bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#plot NB
pdf("NB1_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size,Overview_mm10$NB_mu,xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("NB2_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob,Overview_mm10$NB_size, bty ="n",xlim =c(0,1), ylim =c(0,18),type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()




#plot PB
pdf("PB1_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha,Overview_mm10$PB_beta,xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB2_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c,Overview_mm10$PB_beta, xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB3_mm10.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha,Overview_mm10$PB_c, xlim =c(0,72),ylim=c(0,11500),bty ="n", type = "p", pch = Overview_mm10$BIC_pch, col = Overview_mm10$BIC_col , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#Highlight distribution in one plot
HL_Pois <- Overview_mm10$BIC_col
HL_Pois[HL_Pois != 2 ] <- 1

HL_NB <- Overview_mm10$BIC_col
HL_NB[HL_NB != 3 ] <- 1

HL_PB <- Overview_mm10$BIC_col
HL_PB[HL_PB != 4 ] <- 1

###Poisson distribution
#Highlight Pois
pdf("Pois_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_Pois == 1],rep(0,length(Overview_mm10$Pois_lambda[HL_Pois == 1])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_Pois != 1],rep(0,length(Overview_mm10$Pois_lambda[HL_Pois != 1])), col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

# Highlight NB
pdf("Pois_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_NB == 1],rep(0,length(Overview_mm10$Pois_lambda[HL_NB == 1])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_NB != 1],rep(0,length(Overview_mm10$Pois_lambda[HL_NB != 1])), col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#Highlight PB
pdf("Pois_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$Pois_lambda[HL_PB == 1],rep(0,length(Overview_mm10$Pois_lambda[HL_PB == 1])),xlim =c(0,250),bty ="n", yaxt = "n",ylim = c(-1,1),type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab = bquote(paste("Parameter of Poisson distribution: ",lambda)) ,ylab ="", main = "Poisson: Overview fitted models via highest BIC")
points(Overview_mm10$Pois_lambda[HL_PB != 1],rep(0,length(Overview_mm10$Pois_lambda[HL_PB != 1])), col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

### NB distribution
#Highligh PB
pdf("NB1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_Pois == 1],Overview_mm10$NB_mu[HL_Pois == 1], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_Pois != 1],Overview_mm10$NB_mu[HL_Pois != 1], col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("NB2_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10_1Pop$NB_prob[HL_Pois == 1],Overview_mm10$NB_size[HL_Pois == 1], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_Pois != 1],Overview_mm10$NB_size[HL_Pois != 1], col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#Highlight NB
pdf("NB1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_NB == 1],Overview_mm10$NB_mu[HL_NB == 1], xlim=c(0,18), ylim=c(0,300),bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_NB != 1],Overview_mm10$NB_mu[HL_NB != 1], col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("NB2_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_NB == 1],Overview_mm10$NB_size[HL_NB == 1],xlim =c(0,1), ylim =c(0,18), bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_NB != 1],Overview_mm10$NB_size[HL_NB != 1], col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#Highlight PB
pdf("NB1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_size[HL_PB == 1],Overview_mm10$NB_mu[HL_PB == 1],xlim=c(0,18), ylim=c(0,300), bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab = "parameter of negative binomial: size", ylab = "parameter of negative binomial: mu", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_size[HL_PB != 1],Overview_mm10$NB_mu[HL_PB != 1], col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("NB2_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$NB_prob[HL_PB == 1],Overview_mm10$NB_size[HL_PB == 1], xlim =c(0,1), ylim =c(0,18),bty ="n",type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab = "parameter of negative binomial: prob", ylab = "parameter of negative binomial: size", main = "Overview fitted models via highest BIC")
points(Overview_mm10$NB_prob[HL_PB != 1],Overview_mm10$NB_size[HL_PB != 1], col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

#### PB distribution
# Highlight Pois
pdf("PB1_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 1],Overview_mm10$PB_beta[HL_Pois == 1],xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_Pois != 1],Overview_mm10$PB_beta[HL_Pois != 1], col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB2_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_Pois == 1],Overview_mm10$PB_beta[HL_Pois == 1],xlim =c(0,12000),ylim=c(0,1300), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_Pois != 1],Overview_mm10$PB_beta[HL_Pois != 1], col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB3_mm10_HL_Pois.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_Pois == 1],Overview_mm10$PB_c[HL_Pois == 1], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_Pois == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_Pois != 1],Overview_mm10$PB_c[HL_Pois != 1], col = 2, pch = Overview_mm10$BIC_pch[HL_Pois != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

# Highlight NB
pdf("PB1_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 1],Overview_mm10$PB_beta[HL_NB == 1], xlim =c(0,80),ylim=c(0,1300),bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_NB != 1],Overview_mm10$PB_beta[HL_NB != 1], col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB2_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_NB == 1],Overview_mm10$PB_beta[HL_NB == 1], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_NB != 1],Overview_mm10$PB_beta[HL_NB != 1], col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB3_mm10_HL_NB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_NB == 1],Overview_mm10$PB_c[HL_NB == 1], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_NB == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_NB != 1],Overview_mm10$PB_c[HL_NB != 1], col = 3, pch = Overview_mm10$BIC_pch[HL_NB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

# Highlight PB
pdf("PB1_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 1],Overview_mm10$PB_beta[HL_PB == 1],xlim =c(0,80),ylim=c(0,1300), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)) , ylab = bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PB != 1],Overview_mm10$PB_beta[HL_PB != 1], col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB2_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_c[HL_PB == 1],Overview_mm10$PB_beta[HL_PB == 1], xlim =c(0,12000),ylim=c(0,1300),bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab =  bquote(paste("3rd Parameter of Poisson-beta: c")), ylab =  bquote(paste("2nd Parameter of Poisson-beta: ",beta)), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_c[HL_PB != 1],Overview_mm10$PB_beta[HL_PB != 1], col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()

pdf("PB3_mm10_HL_PB.pdf", width = 10, height = 4)
plot(Overview_mm10$PB_alpha[HL_PB == 1],Overview_mm10$PB_c[HL_PB == 1], xlim =c(0,72),ylim=c(0,11500), bty ="n", type = "p", pch = Overview_mm10$BIC_pch[HL_PB == 1], col = 8 , xlab = bquote(paste("1st Parameter of Poisson-beta: ",alpha)), ylab =bquote(paste("3rd Parameter of Poisson-beta: c")), main = "Overview fitted models via highest BIC")
points(Overview_mm10$PB_alpha[HL_PB != 1],Overview_mm10$PB_c[HL_PB != 1], col = 4, pch = Overview_mm10$BIC_pch[HL_PB != 1])
legend("topright", legend = c("Pois", "NB","PB","1-Pop","ZI","2-Pop","ZI-2-Pop"), col = c(2:4,1,1,1,1), pch = c(19,19,19,1:4), bty ="n")
dev.off()




