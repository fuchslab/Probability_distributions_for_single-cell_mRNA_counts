load(file="Parameter_BasicSwitchBurst.rda")
library("scModels")

#Generate Basic Data
Data_basic <- list()

for(i in 1:1000){
    set.seed(1111*i+i+1111)
    listname <- paste(paste("Sample ",bquote(.(i))))
    Data_basic[[listname]] <- gmRNA_basic(1000,Parameter_Basic$trans[i], Parameter_Basic$degr[i])
    save(Data_basic, file = "Data_basic.Rda")
}

#Generate Switch Data

Data_switch <- list()

for(i in 1:1000){
    set.seed(2222*i+i+2222)
    listname <- paste(paste("Sample ",bquote(.(i))))
    Data_switch[[listname]] <- gmRNA_switch(1000,Parameter_Switch$act[i],Parameter_Switch$deact[i], Parameter_Switch$on[i],Parameter_Switch$degr[i])
    save(Data_switch, file = "Data_switch.Rda")
}

#Gnerate Burst Data

Data_burst <- list()

for(i in 1:1000){
    set.seed(3333*i+i+3333)
    listname <- paste(paste("Sample ",bquote(.(i))))
    Data_burst[[listname]] <- gmRNA_burst(1000,Parameter_Burst$burst[i],Parameter_Burst$size[i], Parameter_Burst$degr[i])
    save(Data_burst, file = "Data_burst.Rda")
}


load(file="Parameter_BasicBurst_BasicBurst_IG.rda")

#Gnerate Basic Burst Data

Data_basic_burst <- list()

for(i in 1:1000){
    set.seed(4444*i+i+4444)
    listname <- paste(paste("Sample ",bquote(.(i))))
    Data_basic_burst[[listname]] <- gmRNA_basic_burst(1000,Parameter_Basic_Burst$trans[i],Parameter_Basic_Burst$burst[i],Parameter_Basic_Burst$size[i], Parameter_Basic_Burst$degr[i])
    save(Data_basic_burst, file = "Data_basic_burst.Rda")
}


#Gnerate Basic Burst Data

Data_IGbasic_burst <- list()

for(i in 1:1000){
    set.seed(5555*i+i+5555)
    listname <- paste(paste("Sample ",bquote(.(i))))
    Data_IGbasic_burst[[listname]] <- gmRNA_IGbasic_burst(1000,Parameter_IGBasic_Burst$mu[i],Parameter_IGBasic_Burst$burst[i], Parameter_IGBasic_Burst$degr[i])
    save(Data_IGbasic_burst, file = "Data_IGbasic_burst.Rda")
}
