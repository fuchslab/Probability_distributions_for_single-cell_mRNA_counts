

main_data <- readRDS("Nestorowa.rds")


load(file="GOF/GOF_input.rds")

p_ks <-list()
p_x2 <- list()
# Poisson distr
##Perform goodness of fit test
#################################################################################
#either ks.test or X^2-test
#################################################################################
#ZINB2
pzinb2 <- function(q, p_1,p_2, size1, mu1, size2, mu2){
    return(p_1 * (q>=0) + p_2 * pnbinom(q,size = size1,mu =  mu1)+(1-p_1-p_2)*pnbinom(q, size = size2, mu = mu2))
}
dzinb2 <- function(x, p_1,p_2, size1, mu1, size2, mu2){
    return(p_1 * (x==0) + p_2 * dnbinom(x,size = size1,mu =  mu1)+(1-p_1-p_2)*dnbinom(x, size = size2, mu = mu2))
}

for(id in names(which(comparison_all_name=="zinb2")) ){

    #generate new table with all values (also the ones whcih are 0 in the data)
    tab <- rep(0, max(main_data[id,])+1)
    names(tab) <- 0:max(main_data[id,])
    for(i in names(table(main_data[id,]))){
        tab[[i]] <- table(main_data[id,])[[i]]
    }

    #generate the corresponding distribution vector
    p1 <- dzinb2(0:max(main_data[id,]),p_1=Par[[id]][1],p_2=Par[[id]][2],size1=Par[[id]][3], mu1= Par[[id]][4],size2=Par[[id]][5], mu2= Par[[id]][6])

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
    p_ks[[id]] <- ks.test(main_data[id,],"pzinb2",p_1=Par[[id]][1],p_2=Par[[id]][2],size1=Par[[id]][3], mu1= Par[[id]][4],size2=Par[[id]][5], mu2= Par[[id]][6])


}
save(p_ks,p_x2, file="GOF/p_gof_zinb2.rda")

