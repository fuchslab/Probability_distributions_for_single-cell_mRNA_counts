# Fit del and pig to Data 

library("scModels")
setwd("PerfectWorld")

library("gamlss.dist")

perform_GOF <- function(){

if(!exists("f_pb")){
minBIC <- which.min(c(
  "pois" = f_pois$BIC,
  "nb" = f_nb$BIC,
  "del" = f_del$BIC,
  "pig" = f_pig$BIC
))}else{
#searche wich one has min BIC
minBIC <- which.min(c(
  "pois" = f_pois$BIC,
  "nb" = f_nb$BIC,
  "pb" = f_pb$BIC,
  "del" = f_del$BIC,
  "pig" = f_pig$BIC
))
}

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
if(names(minBIC) == "del"){
  Par <- f_del$par
  p1 <- dDEL(0:max(main_data),mu=Par[1],sigma=Par[2], nu = Par[3])
}
if(names(minBIC) =="pig"){
  Par <- f_pig$par
  p1 <- dPIG(0:max(main_data),mu=Par[1],sigma=Par[2])
}



N <- sum((p1 * length((main_data))) > 1)

MEAN <- (p1 * length((main_data)))

kl1 <- 1
gr1 <- 1

if(sum(MEAN < 1)>0) kl1 <- which(MEAN < 1)
if(sum(MEAN > 1)>0) gr1 <- which(MEAN > 1)

tab1 <- tab
p1_cut1 <- p1

if((max(gr1) + 1) < max(kl1)){
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
  N2 <- sum(cumsum(p1 * length((main_data)))<1)
  p2 <- c()
  tab2 <- c()
  for (l in 1:round(max(main_data)/(N2+1))){
    p2[l] <- sum(p1[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
    tab2[l] <- sum(tab[((l-1)*(N2+1)+1):min(((l-1)*(N2+1)+(N2+1)),length(p1))])
    names(tab2)[l]<- names(tab)[((l-1)*(N2+1)+1)]
  }
  p1_cut1 <- c(p2,1-sum(p2))
  tab1 <- c(tab2,0)
}


if(min(gr1) < 3 & max(gr1) > (length(tab)-2)){
  p_x2 <- chisq.test(c(tab,0),p=c(p1,1-sum(p1)), rescale.p = FALSE)
} else{
  p_x2 <- chisq.test(tab1,p=p1_cut1, rescale.p = FALSE)}



save(minBIC,p_x2, file=paste0("GOFData/Basic/p_gof_basic_",i,".rda"))}

#Basic Fit

load(file="GenData/Data_basic.Rda")
i <- as.numeric(commandArgs(TRUE))

filename <- paste0("FitData/Basic/basic_",i,".rda")
if(!file.exists(filename)){
# do complete fit
set.seed(i*5+5)
#name of listentry
listname <- paste(paste("Sample ",bquote(.(i))))
main_data <- Data_basic[[listname]]
#fit the three different distributions
f_pois <- fit_params(main_data, "pois")
f_nb <- fit_params(main_data, "nb")
f_del <- fit_params(main_data, "del")
f_pig <- fit_params(main_data, "pig")
save(f_pois, f_nb, f_del, f_pig, file=filename)
perform_GOF ()
f_pb <- fit_params(main_data, "pb")
save(f_pois, f_nb, f_pb, f_del, f_pig, file=filename)
perform_GOF ()
} else{
  load(file=filename)
  listname <- paste(paste("Sample ",bquote(.(i))))
  main_data <- Data_basic[[listname]]
  if(!exists("f_del")){
    set.seed(i*7+7)
    f_del <- fit_params(main_data, "del")
  }
  if(!exists("f_pig")){
    set.seed(i*6+6)
    f_pig <- fit_params(main_data, "pig")
  }
  if(!exists("f_pb")){
    save(f_pois, f_nb, f_del, f_pig, file=filename)
	perform_GOF ()
    set.seed(i*8+8)
    f_pb <- fit_params(main_data, "pb")
    save(f_pois, f_nb, f_pb, f_del, f_pig, file=filename)
  }
  save(f_pois, f_nb, f_pb, f_del, f_pig, file=filename)
  perform_GOF ()
}



