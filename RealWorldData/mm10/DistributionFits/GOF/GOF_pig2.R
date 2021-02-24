
main_data <- readRDS("mm10.rds")

load(file="GOF/GOF_input.rds")
library(gamlss.dist)
p_ks <-list()
p_x2 <- list()
# Poisson distr
##Perform goodness of fit test
#################################################################################
#either ks.test or X^2-test
#################################################################################
#NB2
pPIG2 <- function(q, p, mu1, sigma1, mu2, sigma2){
    return(p * pPIG(q,mu = mu1, sigma =  sigma1)+(1-p)*pPIG(q, mu = mu2, sigma = sigma2))
}
dPIG2 <- function(x, p, mu1, sigma1, mu2, sigma2){
    return(p * dPIG(x,mu = mu1, sigma =  sigma1)+(1-p)*dPIG(x, mu = mu2, sigma = sigma2))
}

for(id in names(which(comparison_all_name=="pig2")) ){

    #generate new table with all values (also the ones whcih are 0 in the data)
    tab <- rep(0, max(main_data[id,])+1)
    names(tab) <- 0:max(main_data[id,])
    for(i in names(table(main_data[id,]))){
        tab[[i]] <- table(main_data[id,])[[i]]
    }

    #generate the corresponding distribution vector
    p1 <- dPIG2(0:max(main_data[id,]),p=Par[[id]][1],mu1=Par[[id]][2], sigma1= Par[[id]][3],mu2=Par[[id]][4], sigma2= Par[[id]][5])

    #put all the counts in one bin after a threshold: as soon as the expected frequency is less then 1,
    #for both first the data and then the sum up the corresponding p
    N <- sum((p1 * length((main_data[id,]))) > 1)
    
    MEAN <- (p1 * length((main_data[id,])))
    
    kl1 <- 1
    gr1 <- 1
    
    if(sum(MEAN < 1)>0) kl1 <- which(MEAN < 1)
    if(sum(MEAN > 1)>0) gr1 <- which(MEAN > 1)
    
    
    if((max(gr1) + 1) < max(kl1)){
        tab1 <- tab
        tab1[(max(gr1) + 1)] <- sum(tab1[(max(gr1)+1):(max(kl1))])
        tab1 <- tab1[0:(max(gr1) + 1)]
        p1_cut1 <- c(p1[1:max(gr1)],1-sum(p1[1:max(gr1)]))
        
    }
    
    if(min(kl1) < (min(gr1)-1)){
        tab1[(min(gr1)-1)] <- sum(tab1[(min(kl1)):(min(gr1)-1)])
        tab1 <- c(tab1[(min(gr1)-1):length(tab1)],0)
        p1_cut1 <- c(p1_cut1[(min(gr1)-1):length(p1_cut1)])
        p1_cut1 <- c(p1_cut1, 1-sum(p1_cut1))
        
    }
    
    if(N == 0){
        N2 <- sum(cumsum(p1 * length((main_data[id,])))<1)
        p2 <- c()
        tab2 <- c()
        for (l in 1:round(max(main_data[id,])/(N2+1))){
            p2[l] <- sum(p1[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
            tab2[l] <- sum(tab[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
            names(tab2)[l]<- names(tab)[((l-1)*(N2+1)+1)]
        }
        p1_cut1 <- c(p2,1-sum(p2))
        tab1 <- c(tab2,0)
    }
    
    
    if(min(gr1) < 3 & max(gr1) > (length(tab)-2)){
        p_x2[[id]]  <- chisq.test(c(tab,0),p=c(p1,1-sum(p1)), rescale.p = FALSE)
    } else{
        p_x2[[id]]  <- chisq.test(tab1,p=p1_cut1, rescale.p = FALSE)}
 
    
        #Calculate both gof test: ks & X^2 test
    p_ks[[id]] <- ks.test(main_data[id,],"pPIG2",p=Par[[id]][1],mu1=Par[[id]][2], sigma1= Par[[id]][3],mu2=Par[[id]][4], sigma2= Par[[id]][5])


}
save(p_ks,p_x2, file="GOF/p_gof_pig2.rda")

