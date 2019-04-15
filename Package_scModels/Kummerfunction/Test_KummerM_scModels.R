# Set parameters

a <- 4
a_1 <- seq(1 , 6)
a_8 <-  seq(8 , 48)/8
a_3 <- seq(3 , 18)/3
a_10 <-  seq(10 , 60)/10

b <- 7
b_1 <- seq(5 , 50)
b_8 <- seq(40 , 400)/ 8
b_3 <- seq(15 , 150)/ 3
b_10 <- seq(50 , 500)/ 10


z <- -75
z_1 <- seq(-150 , 150)
z_8 <-  seq(-1200 , 1200)/ 8
z_3 <- seq(-450 , 450)/ 3
z_10 <-  seq(-1500 , 1500)/ 10

z_pos<- -z

# 1) Function depending on a (different steps)

Kummer_a_1 <- rep(0,length(a_1))
Kummer_a_1_pos <- rep(0,length(a_1))
Kummer_a_1_Harsha_2 <- rep(0,length(a_1))
Kummer_a_1_pos_Harsha_2 <- rep(0,length(a_1))

for(i in 1 : length(a_1)){
    Kummer_a_1[i] <- Re(fAsianOptions::kummerM(z, a_1[i], b, lnchf = 1))
    Kummer_a_1_Harsha_2[i] <- scModels::chf_1F1(z, a_1[i], b)
    Kummer_a_1_pos[i] <- Re(fAsianOptions::kummerM(z_pos, a_1[i], b, lnchf = 1))
    Kummer_a_1_pos_Harsha_2[i] <- scModels::chf_1F1(z_pos, a_1[i], b)
}

Kummer_a_8 <- rep(0, length(a_8))
Kummer_a_8_pos <- rep(0, length(a_8))
Kummer_a_8_Harsha_2 <- rep(0, length(a_8))
Kummer_a_8_pos_Harsha_2 <- rep(0, length(a_8))

for(i in 1 : length(a_8)){
    Kummer_a_8[i] <- Re(fAsianOptions::kummerM(z, a_8[i],  b, lnchf = 1))
    Kummer_a_8_Harsha_2[i] <- scModels::chf_1F1(z, a_8[i],  b)
    Kummer_a_8_pos[i] <- Re(fAsianOptions::kummerM(z_pos, a_8[i] ,  b, lnchf = 1))
    Kummer_a_8_pos_Harsha_2[i] <- scModels::chf_1F1(z_pos, a_8[i] ,  b)
}


Kummer_a_3 <- rep(0, length(a_3))
Kummer_a_3_pos <- rep(0, length(a_3))
Kummer_a_3_Harsha_2 <- rep(0, length(a_3))
Kummer_a_3_pos_Harsha_2 <- rep(0, length(a_3))

for(i in 1:length(a_3)){
    Kummer_a_3[i] <- Re(fAsianOptions::kummerM(z, a_3[i] ,  b, lnchf = 1))
    Kummer_a_3_Harsha_2[i] <- scModels::chf_1F1(z, a_3[i] ,  b)
    Kummer_a_3_pos[i] <- Re(fAsianOptions::kummerM(z_pos, a_3[i] ,  b, lnchf = 1))
    Kummer_a_3_pos_Harsha_2[i] <- scModels::chf_1F1(z_pos, a_3[i] ,  b)
}


Kummer_a_10 <- rep(0, length(a_10))
Kummer_a_10_pos <- rep(0, length(a_10))
Kummer_a_10_Harsha_2 <- rep(0, length(a_10))
Kummer_a_10_pos_Harsha_2 <- rep(0, length(a_10))

for(i in 1 : length(a_10)){
    Kummer_a_10[i] <- Re(fAsianOptions::kummerM(z, a_10[i] ,  b, lnchf = 1))
    Kummer_a_10_Harsha_2[i] <- scModels::chf_1F1(z, a_10[i] ,  b)
    Kummer_a_10_pos[i] <- Re(fAsianOptions::kummerM(z_pos, a_10[i] ,  b, lnchf = 1))
    Kummer_a_10_pos_Harsha_2[i] <- scModels::chf_1F1(z_pos, a_10[i] ,  b)

}



dat<-"1_Kummer_a.pdf"
pdf(file=dat,width=12, height=8)
par(mfrow=c(2,2), mar=c(3,3,6,0.5), oma=c(1,1,2,1) , pin=c(4,2))

plot(a_1,Kummer_a_1,type="l",main=expression(paste(a," step size: ", 1)),ylim=c(-20,20),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_1,Kummer_a_1_Harsha_2,col="blue")
plot(a_8,Kummer_a_8,type="l",main=expression(paste(a," step size: ", frac(1,8))),ylim=c(-20,20),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_8,Kummer_a_8_Harsha_2,col="blue")
plot(a_3,Kummer_a_3,type="l",main=expression(paste(a," step size: ", frac(1,3))),ylim=c(-20,20),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_3,Kummer_a_3_Harsha_2,col="blue")
plot(a_10,Kummer_a_10,type="l",main=expression(paste(a," step size: ", frac(1,10))),ylim=c(-20,20),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_10,Kummer_a_10_Harsha_2,col="blue")
mtext(expression(paste("Function",phantom(0)[1], F[1]," depending on ", a," with ", b == 7," and ", z == -75)), outer = TRUE, cex = 1.5)
dev.off()



dat<-"1_Kummer_a_pos.pdf"
pdf(file=dat,width=12, height=8)
par(mfrow=c(2,2), mar=c(3,3,6,0.5), oma=c(1,1,2,1) , pin=c(4,2))
plot(a_1,Kummer_a_1_pos,type="l",main=expression(paste(a," step size: ", 1)),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_1,Kummer_a_1_pos_Harsha_2,col="blue")
plot(a_8,Kummer_a_8_pos,type="l",main=expression(paste(a," step size: ", frac(1,8))),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_8,Kummer_a_8_pos_Harsha_2,col="blue")
plot(a_3,Kummer_a_3_pos,type="l",main=expression(paste(a," step size: ", frac(1,3))),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_3,Kummer_a_3_pos_Harsha_2,col="blue")
plot(a_10,Kummer_a_10_pos,type="l",main=expression(paste(a," step size: ", frac(1,10))),xlab=expression(a),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(a_10,Kummer_a_10_pos_Harsha_2,col="blue")
mtext(expression(paste("Function",phantom(0)[1], F[1]," depending on ", a," with ", b == 7," and ", z == 75)), outer = TRUE, cex = 1.5)
dev.off()


# 2) Function depending on beta in different steps: (integers, half, third, tenth)

Kummer_b_1<-rep(0,length(b_1))
Kummer_b_1_pos<-rep(0,length(b_1))
Kummer_b_1_Harsha_2<-rep(0,length(b_1))
Kummer_b_1_pos_Harsha_2<-rep(0,length(b_1))

for(i in 1:length(b_1)){
    Kummer_b_1[i]<-Re(fAsianOptions::kummerM(z, a,  b_1[i] , lnchf = 1))
    Kummer_b_1_Harsha_2[i]<-scModels::chf_1F1(z, a,  b_1[i])
    Kummer_b_1_pos[i]<-Re(fAsianOptions::kummerM(z_pos, a,  b_1[i] , lnchf = 1))
    Kummer_b_1_pos_Harsha_2[i]<-scModels::chf_1F1(z_pos, a,  b_1[i] )

}

Kummer_b_8<-rep(0,length(b_8))
Kummer_b_8_pos<-rep(0,length(b_8))
Kummer_b_8_Harsha_2<-rep(0,length(b_8))
Kummer_b_8_pos_Harsha_2<-rep(0,length(b_8))

for(i in 1:length(b_8)){
    Kummer_b_8[i]<- Re(fAsianOptions::kummerM(z, a,  b_8[i] , lnchf = 1))
    Kummer_b_8_Harsha_2[i]<- scModels::chf_1F1(z, a,  b_8[i]  )
    Kummer_b_8_pos[i]<- Re(fAsianOptions::kummerM(z_pos, a,  b_8[i] , lnchf = 1))
    Kummer_b_8_pos_Harsha_2[i]<- scModels::chf_1F1(z_pos, a,  b_8[i] )
}


Kummer_b_3<-rep(0,length(b_3))
Kummer_b_3_pos<-rep(0,length(b_3))
Kummer_b_3_Harsha_2<-rep(0,length(b_3))
Kummer_b_3_pos_Harsha_2<-rep(0,length(b_3))

for(i in 1:length(b_3)){
    Kummer_b_3[i]<- Re(fAsianOptions::kummerM(z, a,  b_3[i] , lnchf = 1))
    Kummer_b_3_Harsha_2[i]<- scModels::chf_1F1(z, a,  b_3[i] )
    Kummer_b_3_pos[i]<- Re(fAsianOptions::kummerM(z_pos, a,  b_3[i] , lnchf = 1))
    Kummer_b_3_pos_Harsha_2[i]<- scModels::chf_1F1(z_pos, a,  b_3[i] )
}


Kummer_b_10<-rep(0,length(b_10))
Kummer_b_10_pos<-rep(0,length(b_10))
Kummer_b_10_Harsha_2<-rep(0,length(b_10))
Kummer_b_10_pos_Harsha_2<-rep(0,length(b_10))

for(i in 1:length(b_10)){
    Kummer_b_10[i]<- Re(fAsianOptions::kummerM(z, a,  b_10[i] , lnchf = 1))
    Kummer_b_10_Harsha_2[i]<- scModels::chf_1F1(z, a,  b_10[i] )
    Kummer_b_10_pos[i]<- Re(fAsianOptions::kummerM(z_pos, a,  b_10[i] , lnchf = 1))
    Kummer_b_10_pos_Harsha_2[i]<- scModels::chf_1F1(z_pos, a,  b_10[i] )
}


dat<-"1_Kummer_b.pdf"
pdf(file=dat,width=12, height=8)
par(mfrow=c(2,2), mar=c(3,3,6,0.5), oma=c(1,1,2,1) , pin=c(4,2))
plot(b_1,Kummer_b_1,type="l",main=expression(paste(b," step size: ", 1)),ylim=c(-15,15),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_1,Kummer_b_1_Harsha_2,col="blue")
plot(b_8,Kummer_b_8,type="l",main=expression(paste(b," step size: ", frac(1,8))),ylim=c(-15,15),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_8,Kummer_b_8_Harsha_2,col="blue")
plot(b_3,Kummer_b_3,type="l",main=expression(paste(b," step size: ", frac(1,3))),ylim=c(-15,15),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_3,Kummer_b_3_Harsha_2,col="blue")
plot(b_10,Kummer_b_10,type="l",main=expression(paste(b," step size: ", frac(1,10))),ylim=c(-15,15),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_10,Kummer_b_10_Harsha_2,col="blue")
mtext(expression(paste("Function",phantom(0)[1], F[1]," depending on ", b," with ", a == 4," and ", z == -75)), outer = TRUE, cex = 1.5)
dev.off()


dat<-"1_Kummer_b_pos.pdf"
pdf(file=dat,width=12, height=8)
par(mfrow=c(2,2), mar=c(3,3,6,0.5), oma=c(1,1,2,1) , pin=c(4,2))
plot(b_1,Kummer_b_1_pos,type="l",main=expression(paste(b," step size: ", 1)),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_1,Kummer_b_1_pos_Harsha_2,col="blue")
plot(b_8,Kummer_b_8_pos,type="l",main=expression(paste(b," step size: ", frac(1,8))),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_8,Kummer_b_8_pos_Harsha_2,col="blue")
plot(b_3,Kummer_b_3_pos,type="l",main=expression(paste(b," step size: ", frac(1,3))),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_3,Kummer_b_3_pos_Harsha_2,col="blue")
plot(b_10,Kummer_b_10_pos,type="l",main=expression(paste(b," step size: ", frac(1,10))),xlab=expression(b),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(b_10,Kummer_b_10_pos_Harsha_2,col="blue")
mtext(expression(paste("Function",phantom(0)[1], F[1]," depending on ", b," with ", a == 4," and ", z == 75)), outer = TRUE, cex = 1.5)
dev.off()


# 3) Function depending on c in different steps: (integers, half, third, tenth)

Kummer_z_1<-rep(0,length(z_1))
Kummer_z_1_Harsha_2<-rep(0,length(z_1))


Kummer_z_1<-Re(fAsianOptions::kummerM(z_1, a, b, lnchf = 1))
Kummer_z_1_Harsha_2<-scModels::chf_1F1(z_1, a, b)


Kummer_z_8<-rep(0,length(z_8))
Kummer_z_8_Harsha_2<-rep(0,length(z_8))

Kummer_z_8<-Re(fAsianOptions::kummerM(z_8, a, b, lnchf = 1))
Kummer_z_8_Harsha_2<-scModels::chf_1F1(z_8, a, b)


Kummer_z_3<-rep(0,length(z_3))
Kummer_z_3_Harsha_2<-rep(0,length(z_3))

Kummer_z_3 <- Re(fAsianOptions::kummerM(z_3, a, b, lnchf = 1))
Kummer_z_3_Harsha_2 <- scModels::chf_1F1(z_3, a, b)


Kummer_z_10<-rep(0,length(z_10))
Kummer_z_10_Harsha_2<-rep(0,length(z_10))


Kummer_z_10<-Re(fAsianOptions::kummerM(z_10, a, b, lnchf = 1))
Kummer_z_10_Harsha_2<-scModels::chf_1F1(z_10, a, b)



dat<-"1_Kummer_z.pdf"
pdf(file=dat,width=12, height=8)
par(mfrow=c(2,2), mar=c(3,3,6,0.5), oma=c(1,1,2,1) , pin=c(4,2))
plot(z_1,Kummer_z_1,type="l",main=expression(paste(z," step size: ", 1)),xlab=expression(z),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(z_1,Kummer_z_1_Harsha_2,col="blue")
plot(z_8,Kummer_z_8,type="l",main=expression(paste(z," step size: ", frac(1,8))),xlab=expression(z),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(z_8,Kummer_z_8_Harsha_2,col="blue")
plot(z_3,Kummer_z_3,type="l",main=expression(paste(z," step size: ", frac(1,3))),xlab=expression(z),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(z_3,Kummer_z_3_Harsha_2,col="blue")
plot(z_10,Kummer_z_10,type="l",main=expression(paste(z," step size: ", frac(1,10))),xlab=expression(z),ylab=expression(paste(phantom(0)[1], F[1],"(z,a,b)")))
lines(z_10,Kummer_z_10_Harsha_2,col="blue")
mtext(expression(paste("Function",phantom(0)[1], F[1]," depending on ", z," with ", a == 4," and ", b == 7)), outer = TRUE, cex = 1.5)
dev.off()



