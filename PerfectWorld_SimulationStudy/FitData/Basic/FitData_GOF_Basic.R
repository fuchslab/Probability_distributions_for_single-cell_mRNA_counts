# Fit Data and then perform gof

library("scModels")
setwd("PerfectWorld")

#Basic Fit

load(file="GenData/Data_basic.Rda")
i <- as.numeric(commandArgs(TRUE))

set.seed(i*5+5)
    #name of listentry
    listname <- paste(paste("Sample ",bquote(.(i))))
    main_data <- Data_basic[[listname]]
    #fit the three different distributions
    f_pois <- fit_params(main_data, "pois")
    f_nb <- fit_params(main_data, "nb")
    f_pb <- fit_params(main_data, "pb")

    #and save them
    save(f_pois, f_nb, f_pb, file=paste0("FitData/Basic/basic_",i,".rda"))

#searche wich one has min BIC
minBIC <- which.min(c(
                "pois" = f_pois$BIC,
                "nb" = f_nb$BIC,
                "pb" = f_pb$BIC
            ))

#generate count table for all counts:

tab <- rep(0, max(main_data)+1)
names(tab) <- 0:max(main_data)
for(j in names(table(main_data))){
    tab[[j]] <- table(main_data)[[j]]
}


#Sepending on which one has the smalles BIC, calculate the density functions
if(names(minBIC) == "pois"){
    Par <- f_pois$par
    p1 <- dpois(0:max(main_data),lambda=Par)
}
if(names(minBIC) == "nb"){
    Par <- f_nb$par
    p1 <- dnbinom(0:max(main_data),size=Par[1],mu=Par[2])
}
if(names(minBIC) =="pb"){
    Par <- f_pb$par
    p1 <- dpb(0:max(main_data),alpha=Par[1],beta=Par[2], c=Par[3], log = FALSE)
}



N <- sum((p1 * length((main_data))) > 1)
if(N == 0){
    N2 <- sum(cumsum(p1 * length((main_data)))<1)
    p2 <- c()
    tab2 <- c()
    for (l in 1:round(max(main_data)/(N2+1))){
        p2[l] <- sum(p1[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
        tab2[l] <- sum(tab[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
        names(tab2)[l]<- names(tab)[((l-1)*(N2+1)+1)]
    }
    p1 <- p2
    tab1 <- tab2
    N <- sum((p1 * length((main_data))) > 1)
}


if(N<length(tab)){
    tab[N+1] <- sum(tab[(N+1):length(tab)])
    tab <- tab[0:N+1]
        p_x2 <- chisq.test(tab,p=c(p1[1:N],1-sum(p1[1:N])), rescale.p = FALSE)
}else  p_x2 <- chisq.test(c(tab,0),p=c(p1[1:N],1-sum(p1[1:N])), rescale.p = FALSE)



save(minBIC,p_x2, file=paste0("GOFData/Basic/p_gof_basic_",i,".rda"))

