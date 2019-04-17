
main_data <- readRDS("mm10.rds")

load(file="GOF_input.rds")

p_ks <-list()
p_x2 <- list()
# Poisson distr
##Perform goodness of fit test
#################################################################################
#either ks.test or X^2-test
#################################################################################
#NB2
pnb2 <- function(q, p, size1, mu1, size2, mu2){
    return(p * pnbinom(q,size = size1,mu =  mu1)+(1-p)*pnbinom(q, size = size2, mu = mu2))
}
dnb2 <- function(x, p, size1, mu1, size2, mu2){
    return(p * dnbinom(x,size = size1,mu =  mu1)+(1-p)*dnbinom(x, size = size2, mu = mu2))
}

for(id in names(which(comparison_all_name=="nb2")) ){

    #generate new table with all values (also the ones whcih are 0 in the data)
    tab <- rep(0, max(main_data[id,])+1)
    names(tab) <- 0:max(main_data[id,])
    for(i in names(table(main_data[id,]))){
        tab[[i]] <- table(main_data[id,])[[i]]
    }

    #generate the corresponding distribution vector
    p1 <- dnb2(0:max(main_data[id,]),p=Par[[id]][1],size1=Par[[id]][2], mu1= Par[[id]][3],size2=Par[[id]][4], mu2= Par[[id]][5])

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
    p_ks[[id]] <- ks.test(main_data[id,],"pnb2",p=Par[[id]][1],size1=Par[[id]][2], mu1= Par[[id]][3],size2=Par[[id]][4], mu2= Par[[id]][5])


}
save(p_ks,p_x2, file="p_gof_nb2.rda")

