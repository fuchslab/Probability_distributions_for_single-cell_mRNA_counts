# Perfect World Simulation Study

# Range of parameters for the switching taken from Suter et al
#
act <- seq(0.005, 0.06, by = 0.005)
deact <- c(0.01,seq(0.05, 0.6, by = 0.05))
on <- seq(0.5,2.5, by = 0.25)
degr <- seq(0.001, 0.05, by = 0.001)

# all possible combinations
Para_Switch<-expand.grid(on,act,deact,degr)
names(Para_Switch)<- c("on","act", "deact", "degr")


# Range of Parameters for burst model
# mean burst sizes on/deact
burst_size <- unique(Para_Switch$on/Para_Switch$deact)
# burst rate
burst <- act

Para_Burst <-expand.grid(burst, burst_size, degr)
names(Para_Burst)<- c("burst","size", "degr")

# Range for Parameters of the basic model

trans <- unique(c(on,act))
Para_Basic <- expand.grid(trans, degr)
names(Para_Basic) <- c("trans", "degr")

# Range for Parameters of the basic burst model

Para_Basic_Burst <- expand.grid(trans, burst, burst_size, degr)
names(Para_Basic_Burst) <- c("trans","burst","size", "degr")

# Range for Parameters of the IG basic burst model

mu <- trans
burst2 <-  unique(trans, seq(0.5,3, by = 0.25))
Para_IGBasic_Burst <- expand.grid(mu, burst2, degr)
names(Para_IGBasic_Burst) <- c("mu","burst", "degr")


# Sample 1000 Parametersets

set.seed(40)
n_Basic_Burst <- sample(dim(Para_Basic_Burst)[1], 1000)
Parameter_Basic_Burst<- Para_Basic_Burst[n_Basic_Burst,]

set.seed(50)
n_IGBasic_Burst <- sample(dim(Para_IGBasic_Burst)[1], 1000)
Parameter_IGBasic_Burst<- Para_IGBasic_Burst[n_IGBasic_Burst,]


save(Parameter_Basic_Burst,Parameter_IGBasic_Burst, file="Parameter_BasicBurst_BasicBurst_IG.rda")




