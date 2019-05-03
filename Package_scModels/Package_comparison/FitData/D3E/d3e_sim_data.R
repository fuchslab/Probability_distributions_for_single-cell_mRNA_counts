# D3E
load("sim_data.Rda")
load("all_results.Rda")
python_location <- "/storage/groups/biostat01/code/package_comparison/env/bin/python2.7 "
seed_int <- c(52538352, 96846493, 5649396, 5847393)

## approach 1

df <- data.frame(cbind(c("SimGene1", "SimGene2", "SimGene3", "SimGene4"), rbind(dataset1, dataset2, dataset3, dataset4)))
colnames(df) <- c("GeneID", rep("G1", n))
for(i in 1:4){
    write.table(df[i,], file = paste0("sim_data", i, ".out"), sep = "\t", row.names = FALSE, quote = FALSE)
    cmd1 <- paste0("D3E/D3EMakeContol.py sim_data", i, ".out sim_data_control", i, ".out G1")
    cmd2 <- paste0("D3E/D3ECmd.py sim_data_control", i, ".out results", i, ".out G1_1 G1_2 -m 1 -t 0 -z 0 -n 1 -v")
    t1 <- system.time(system(paste0(python_location, " ", cmd1)))
    t2 <- system.time(system(paste0(python_location, " ", cmd2)))
    results_d3e1 <- read.delim(paste0("results", i, ".out"))
    all_results[[paste0("dataset",i)]]["D3E-#1",1:3] <- unlist(results_d3e1[c("a1", "b1", "g1")])
    all_results[[paste0("dataset",i)]]["D3E-#1",4] <- t1["elapsed"] + t2["elapsed"]
}

## approach 2

library(reticulate)
use_virtualenv("/home/icb/harsha.kumar/package_comparison/env")
source_python("d3e_sim_data2.py")
for (i in 1:4) {
    all_results[[paste0("dataset",i)]]["D3E-#2-Moments",1:4] <- unlist(d3e_2_moments[i,])
    all_results[[paste0("dataset",i)]]["D3E-#2-Bayesian",1:4] <- unlist(d3e_2_bayesian[i,])
}
#detach("package:reticulate", unload = TRUE)

## approach 3

df <- data.frame(cbind(c("SimGene1", "SimGene2", "SimGene3", "SimGene4"), rbind(dataset1, dataset2, dataset3, dataset4), matrix(rep(0, 4*n), nrow = 4)))
colnames(df) <- c("GeneID", rep("G1_1", n), rep("G1_2", n))
for(i in 1:4) {
    write.table(df[i,], file = paste0("sim_data3_control", i, ".out"), sep = "\t", row.names = FALSE, quote = FALSE)
    cmd3 <- paste0("D3E/D3ECmd.py sim_data3_control", i, ".out results3_", i, ".out G1_1 G1_2 -m 1 -t 0 -z 0 -n 1 -v")
    t3 <- system.time(system(paste0(python_location, " ", cmd3)))
    results_d3e3 <- read.delim(paste0("results3_", i, ".out"))
    all_results[[paste0("dataset",i)]]["D3E-#3",1:3] <- unlist(results_d3e3[c("a1", "b1", "g1")])
    all_results[[paste0("dataset",i)]]["D3E-#3",4] <- t3["elapsed"]
}

save(all_results, file="all_results.Rda")
