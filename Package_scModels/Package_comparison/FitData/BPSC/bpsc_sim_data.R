load("sim_data.Rda")
load("init.Rda")

bpsc_init_t1 <- system.time(init1 <- BPSC::getInitParam(dataset1, para.num = 3))
bpsc_init_t2 <- system.time(init2 <- BPSC::getInitParam(dataset2, para.num = 3))
bpsc_init_t3 <- system.time(init3 <- BPSC::getInitParam(dataset3, para.num = 3))
bpsc_init_t4 <- system.time(init4 <- BPSC::getInitParam(dataset4, para.num = 3))

bpsc_est_t1 <- system.time(bp1 <- BPSC::estimateBP(x = dataset1, para.num = 3))
bpsc_est_t2 <- system.time(bp2 <- BPSC::estimateBP(x = dataset2, para.num = 3))
bpsc_est_t3 <- system.time(bp3 <- BPSC::estimateBP(x = dataset3, para.num = 3))
bpsc_est_t4 <- system.time(bp4 <- BPSC::estimateBP(x = dataset4, para.num = 3))

all_results[["dataset1"]]["BPSC-init",1:3] <- init1
all_results[["dataset2"]]["BPSC-init",1:3] <- init2
all_results[["dataset3"]]["BPSC-init",1:3] <- init3
all_results[["dataset4"]]["BPSC-init",1:3] <- init4
all_results[["dataset1"]]["BPSC-init",4] <- sum(bpsc_init_t1)
all_results[["dataset2"]]["BPSC-init",4] <- sum(bpsc_init_t2)
all_results[["dataset3"]]["BPSC-init",4] <- sum(bpsc_init_t3)
all_results[["dataset4"]]["BPSC-init",4] <- sum(bpsc_init_t4)

all_results[["dataset1"]]["BPSC-estimates",1:3] <- bp1$par
all_results[["dataset2"]]["BPSC-estimates",1:3] <- bp2$par
all_results[["dataset3"]]["BPSC-estimates",1:3] <- bp3$par
all_results[["dataset4"]]["BPSC-estimates",1:3] <- bp4$par
all_results[["dataset1"]]["BPSC-estimates",4] <- sum(bpsc_est_t1)
all_results[["dataset2"]]["BPSC-estimates",4] <- sum(bpsc_est_t2)
all_results[["dataset3"]]["BPSC-estimates",4] <- sum(bpsc_est_t3)
all_results[["dataset4"]]["BPSC-estimates",4] <- sum(bpsc_est_t4)

save(all_results, file="all_results.Rda")