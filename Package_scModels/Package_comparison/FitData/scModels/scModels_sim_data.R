load("all_results.Rda")

for(i in 1:4) {
  load(paste0("pb_results", i, ".Rda"))
  all_results[[paste0("dataset", i)]]["pb-init",1:4] <- c(estim, t_estim["elapsed"])
  all_results[[paste0("dataset", i)]]["scModels",] <- c(fit$par, fit$time["elapsed"], fit$value)
}

save(all_results, file="all_results.Rda")
