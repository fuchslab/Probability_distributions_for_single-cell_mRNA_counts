
#setwd("C:/Users/lisa.amrhein/Documents/Thesis/Notizen/2018_NB_Overleaf/Nestorowa/Daten")

main_data <- readRDS("Nestorowa.rds")


load(file="DistributionFits/GOF/GOF_input.rds")

#setwd("C:/Users/lisa.amrhein/Documents/Thesis/Notizen/2018_NB_Overleaf/Nestorowa/GOF2019")
setwd("GOF")
load(file="GOF_input.rds")
p_ks <-list()
p_x2 <- list()
# Poisson distr
##Perform goodness of fit test
#################################################################################
#either ks.test or X^2-test
library(scModels)
#PB2
ppb2 <- function(q, p, alpha1, beta1, c1, alpha2, beta2, c2){
    return(p * ppb(q,alpha= alpha1, beta=beta1,c= c1)+(1-p)*ppb(q,alpha= alpha2,beta= beta2, c=c2))
}
dpb2 <- function(x, p, alpha1, beta1, c1, alpha2, beta2, c2){
    return(p * dpb(x,alpha= alpha1,beta= beta1, c=c1)+(1-p)*dpb(x,alpha= alpha2, beta=beta2, c=c2))
}

for(id in names(which(comparison_all_name=="pb2")) ){

    #generate new table with all values (also the ones whcih are 0 in the data)
    tab <- rep(0, max(main_data[id,])+1)
    names(tab) <- 0:max(main_data[id,])
    for(i in names(table(main_data[id,]))){
        tab[[i]] <- table(main_data[id,])[[i]]
    }

    #generate the corresponding distribution vector
    p1 <- dpb2(0:max(main_data[id,]),p=Par[[id]][1],alpha1=Par[[id]][2], beta1= Par[[id]][3],c1=Par[[id]][4], alpha2= Par[[id]][5], beta2= Par[[id]][6], c2= Par[[id]][7])

    #put all the counts in one bin after a threshold: as soon as the expected frequency is less then 1,
    #for both first the data and then the sum up the corresponding p
    N <- sum((p1 * length((main_data[id,]))) > 1)
    if(N == 0){
        N2 <- sum(cumsum(p1 * length((main_data[id,])))<1)
        p2 <- c()
        tab2 <- c()
        for (l in 1:round(max(main_data[id,])/(N2+1))){
            p2[l] <- sum(p1[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
            tab2[l] <- sum(tab[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
            names(tab2)[l]<- names(tab)[((l-1)*(N2+1)+1)]
        }
        p1 <- p2
        tab1 <- tab2
        N <- sum((p1 * length((main_data[id,]))) > 1)
    }


    if(N<length(tab)){
        tab[N+1] <- sum(tab[(N+1):length(tab)])
        tab <- tab[0:N+1]
        p_x2[[id]] <- chisq.test(tab,p=c(p1[1:N],1-sum(p1[1:N])), rescale.p = FALSE)
    }else p_x2[[id]] <- chisq.test(c(tab,0),p=c(p1[1:N],1-sum(p1[1:N])), rescale.p = FALSE)

    #Calculate both gof test: ks & X^2 test
    p_ks[[id]] <- ks.test(main_data[id,],"ppb2",p=Par[[id]][1],alpha1=Par[[id]][2], beta1= Par[[id]][3],c1=Par[[id]][4], alpha2= Par[[id]][5], beta2= Par[[id]][6], c2= Par[[id]][7])

save(p_ks,p_x2, file="DistributionFits/GOF/p_gof_pb2.rda")
}

