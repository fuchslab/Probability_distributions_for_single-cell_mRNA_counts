load("sim_data.Rda")
alpha <- lambda/mu
beta <- gamma/mu
c <- r_on/mu


all_datasets = list()
all_datasets[["dataset1"]] <- dataset1
all_datasets[["dataset2"]] <- dataset2
all_datasets[["dataset3"]] <- dataset3
all_datasets[["dataset4"]] <- dataset4
all_results <- list()

for(i in 1:4) {
  all_results[[paste0("dataset", i)]] <- matrix(rep(0, 9*5), nrow = 9)
  colnames(all_results[[paste0("dataset", i)]]) <- c("alpha", "beta", "c", "time", "n-LogLikelihood")
  rownames(all_results[[paste0("dataset", i)]]) <- c("Original", "BPSC-init", "BPSC-estimates", "D3E-#1", "D3E-#2-Moments", "D3E-#2-Bayesian", "D3E-#3", "pb-init", "scModels")
  all_results[[paste0("dataset", i)]]["Original",1:3] <- c(alpha[i], beta[i], c[i])
}

save(all_datasets, all_results, file = "init.Rda")
