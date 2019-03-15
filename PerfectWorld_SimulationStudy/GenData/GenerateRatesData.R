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



# Sample 1000 Parametersets

set.seed(10)
n_Switch <- sample(dim(Para_Switch)[1], 1000)
Parameter_Switch <- Para_Switch[n_Switch,]

set.seed(20)
n_Burst <- sample(dim(Para_Burst)[1], 1000)
Parameter_Burst <- Para_Burst[n_Burst,]

set.seed(30)
n_Basic <- sample(dim(Para_Basic)[1], 1000)
Parameter_Basic<- Para_Basic[n_Basic,]

save(Parameter_Basic, Parameter_Switch, Parameter_Burst, file="Parameter_BasicSwitchBurst.rda")




