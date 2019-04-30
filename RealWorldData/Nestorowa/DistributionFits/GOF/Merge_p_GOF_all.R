

load(file="DistributionFits/GOF/p_gof_pois.rda")
p_x2_pois <- p_x2
p_ks_pois <- p_ks
load(file="DistributionFits/GOF/p_gof_nb.rda")
p_x2_nb <- p_x2
p_ks_nb <- p_ks
load(file="DistributionFits/GOF/p_gof_pb.rda")
p_x2_pb <- p_x2
p_ks_pb <- p_ks
load(file="DistributionFits/GOF/p_gof_zipois.rda")
p_x2_zipois <- p_x2
p_ks_zipois <- p_ks
load(file="DistributionFits/GOF/p_gof_zinb.rda")
p_x2_zinb <- p_x2
p_ks_zinb <- p_ks
load(file="DistributionFits/GOF/p_gof_zipb.rda")
p_x2_zipb <- p_x2
p_ks_zipb <- p_ks
load(file="DistributionFits/GOF/p_gof_pois2.rda")
p_x2_pois2 <- p_x2
p_ks_pois2 <- p_ks
load(file="DistributionFits/GOF/p_gof_nb2.rda")
p_x2_nb2 <- p_x2
p_ks_nb2 <- p_ks
load(file="DistributionFits/GOF/p_gof_pb2.rda")
p_x2_pb2 <- p_x2
p_ks_pb2 <- p_ks
load(file="DistributionFits/GOF/p_gof_zipois2.rda")
p_x2_zipois2 <- p_x2
p_ks_zipois2 <- p_ks
load(file="DistributionFits/GOF/p_gof_zinb2.rda")
p_x2_zinb2 <- p_x2
p_ks_zinb2 <- p_ks
load(file="DistributionFits/GOF/p_gof_zipb2.rda")

library(rlist)

p_x2 <- c(p_x2,p_x2_pois,p_x2_nb,p_x2_pb,p_x2_zipois,p_x2_zinb,p_x2_zipb,p_x2_pois2,p_x2_nb2,p_x2_pb2,p_x2_zipois2,p_x2_zinb2)
p_ks <- c(p_ks,p_ks_pois,p_ks_nb,p_ks_pb,p_ks_zipois,p_ks_zinb,p_ks_zipb,p_ks_pois2,p_ks_nb2,p_ks_pb2,p_ks_zipois2,p_ks_zinb2)

save(p_ks,p_x2,file="DistributionFits/GOF/p_gof_all.rda")

################################################

load(file="DistributionFits/GOF/GOF_input.rds")

GOF_ks <- list()
GOF_x2 <- list()
for(id in names(p_ks)){
    GOF_ks[[id]] <- p_ks[[id]]$p.value
    GOF_x2[[id]] <- p_x2[[id]]$p.value
}


H1<-merge(as.data.frame(unlist(GOF_ks), row.names=names(GOF_ks)),as.data.frame(unlist(GOF_x2), row.names=names(GOF_x2)), by = 0, all = TRUE)

H2<-merge(as.data.frame(comparison_all_name, row.names=names(comparison_all_name)),as.data.frame(H1[,2:3], row.names = H1[,1]), by = 0, all = TRUE)

Nestorowa_GOF <- as.data.frame(H2[2:4], row.names = H2[,1])

#calculate if p-values are lower than adjusted alpha.
Nestorowa_GOF$x2_adj <- Nestorowa_GOF$`unlist(GOF_x2)`> 0.05/length(row.names(Nestorowa_GOF))

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pois"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="pois"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="nb"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="nb"])


sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pb"][!is.na(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pb"])])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="pb"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pois2"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="pois2"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="nb2"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="nb2"])


sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pb2"][!is.na(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="pb2"])])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="pb2"])


sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zipois"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zipois"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zinb"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zinb"])


sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zipb"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zipb"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zipois2"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zipois2"])

sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zinb2"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zinb2"])


sum(Nestorowa_GOF$x2_adj[Nestorowa_GOF$comparison_all=="zipb2"])
plot(Nestorowa_GOF$`unlist(GOF_x2)`[Nestorowa_GOF$comparison_all=="zipb2"])


save(Nestorowa_GOF, file="DistributionFits/GOF/Result_Nestorowa_BIC_GOF.rda")






