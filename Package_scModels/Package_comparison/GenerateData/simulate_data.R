lambda <- c(50, 50, 50, 50)
gamma <- c(200, 200, 20, 20)
r_on <- c(4000, 500, 100, 10)
mu <- c(1, 1, 1, 1)
n <- 1000

alpha <- lambda/mu
beta <- gamma/mu
c <- r_on/mu

set.seed(23757)
dataset1 <- scModels::gmRNA_switch(n, lambda[1], gamma[1], r_on[1], mu[1])
set.seed(639363)
dataset2 <- scModels::gmRNA_switch(n, lambda[2], gamma[2], r_on[2], mu[2])
set.seed(746393)
dataset3 <- scModels::gmRNA_switch(n, lambda[3], gamma[3], r_on[3], mu[3])
set.seed(293941)
dataset4 <- scModels::gmRNA_switch(n, lambda[4], gamma[4], r_on[4], mu[4])

save(lambda, gamma, r_on, mu, n, dataset1, dataset2, dataset3, dataset4,  file="sim_data.Rda")
