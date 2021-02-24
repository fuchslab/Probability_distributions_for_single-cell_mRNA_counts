

load(file="p_gof_pois.rda")
p_x2_pois <- p_x2
p_ks_pois <- p_ks
load(file="p_gof_nb.rda")
p_x2_nb <- p_x2
p_ks_nb <- p_ks
load(file="p_gof_pb.rda")
p_x2_pb <- p_x2
p_ks_pb <- p_ks
load(file="p_gof_pig.rda")
p_x2_pig <- p_x2
p_ks_pig <- p_ks
load(file="p_gof_del.rda")
p_x2_del <- p_x2
p_ks_del <- p_ks
load(file="p_gof_zipois.rda")
p_x2_zipois <- p_x2
p_ks_zipois <- p_ks
load(file="p_gof_zinb.rda")
p_x2_zinb <- p_x2
p_ks_zinb <- p_ks
load(file="p_gof_zipig.rda")
p_x2_zipig <- p_x2
p_ks_zipig <- p_ks
load(file="p_gof_zidel.rda")
p_x2_zidel <- p_x2
p_ks_zidel <- p_ks
load(file="p_gof_zipb.rda")
p_x2_zipb <- p_x2
p_ks_zipb <- p_ks
load(file="p_gof_pois2.rda")
p_x2_pois2 <- p_x2
p_ks_pois2 <- p_ks
load(file="p_gof_nb2.rda")
p_x2_nb2 <- p_x2
p_ks_nb2 <- p_ks
load(file="p_gof_pig2.rda")
p_x2_pig2 <- p_x2
p_ks_pig2 <- p_ks
load(file="p_gof_del2.rda")
p_x2_del2 <- p_x2
p_ks_del2 <- p_ks
load(file="p_gof_pb2.rda")
p_x2_pb2 <- p_x2
p_ks_pb2 <- p_ks
load(file="p_gof_zipois2.rda")
p_x2_zipois2 <- p_x2
p_ks_zipois2 <- p_ks
load(file="p_gof_zinb2.rda")
p_x2_zinb2 <- p_x2
p_ks_zinb2 <- p_ks
load(file="p_gof_zipig2.rda")
p_x2_zipig2 <- p_x2
p_ks_zipig2 <- p_ks
load(file="p_gof_zidel2.rda")
p_x2_zidel2 <- p_x2
p_ks_zidel2 <- p_ks
load(file="p_gof_zipb2.rda")
p_x2_zipb2 <- p_x2
p_ks_zipb2 <- p_ks

library(rlist)

load(file="GOF_input.rds")

p_x2 <- c(p_x2[names(which(comparison_all_name=="zipb2"))],p_x2_pois[names(which(comparison_all_name=="pois"))],p_x2_nb[names(which(comparison_all_name=="nb"))],p_x2_pig[names(which(comparison_all_name=="pig"))],p_x2_del[names(which(comparison_all_name=="del"))],p_x2_pb[names(which(comparison_all_name=="pb"))],p_x2_zipois[names(which(comparison_all_name=="zipois"))],p_x2_zinb[names(which(comparison_all_name=="zinb"))],p_x2_zipig[names(which(comparison_all_name=="zipig"))],p_x2_zidel[names(which(comparison_all_name=="zidel"))],p_x2_zipb[names(which(comparison_all_name=="zipb"))],p_x2_pois2[names(which(comparison_all_name=="pois2"))],p_x2_nb2[names(which(comparison_all_name=="nb2"))],p_x2_pig2[names(which(comparison_all_name=="pig2"))],p_x2_pig2[names(which(comparison_all_name=="del2"))],p_x2_pb2[names(which(comparison_all_name=="pb2"))],p_x2_zipois2[names(which(comparison_all_name=="zipois2"))],p_x2_zinb2[names(which(comparison_all_name=="zinb2"))],p_x2_zipig2[names(which(comparison_all_name=="zipig2"))],p_x2_zidel2[names(which(comparison_all_name=="zidel2"))])
p_ks <- c(p_ks[names(which(comparison_all_name=="zipb2"))],p_ks_pois[names(which(comparison_all_name=="pois"))],p_ks_nb[names(which(comparison_all_name=="nb"))],p_ks_del[names(which(comparison_all_name=="del"))],p_ks_pb[names(which(comparison_all_name=="pb"))],p_ks_pig[names(which(comparison_all_name=="pig"))],p_ks_zipois[names(which(comparison_all_name=="zipois"))],p_ks_zinb[names(which(comparison_all_name=="zinb"))],p_ks_zipig[names(which(comparison_all_name=="zipig"))],p_ks_zidel[names(which(comparison_all_name=="zidel"))],p_ks_zipb[names(which(comparison_all_name=="zipb"))],p_ks_pois2[names(which(comparison_all_name=="pois2"))],p_ks_nb2[names(which(comparison_all_name=="nb2"))],p_ks_pig2[names(which(comparison_all_name=="pig2"))],p_ks_del2[names(which(comparison_all_name=="del2"))],p_ks_pb2[names(which(comparison_all_name=="pb2"))],p_ks_zipois2[names(which(comparison_all_name=="zipois2"))],p_ks_zinb2[names(which(comparison_all_name=="zinb2"))],p_ks_zipig2[names(which(comparison_all_name=="zipig2"))],p_ks_zidel2[names(which(comparison_all_name=="zidel2"))])

save(p_ks,p_x2,file="p_gof_all.rda")

################################################

GOF_ks <- list()
GOF_x2 <- list()
for(id in names(p_ks)){
    GOF_ks[[id]] <- p_ks[[id]]$p.value
    GOF_x2[[id]] <- p_x2[[id]]$p.value
}


H1<-merge(as.data.frame(unlist(GOF_ks), row.names=names(GOF_ks)),as.data.frame(unlist(GOF_x2), row.names=names(GOF_x2)), by = 0, all = TRUE)

H2<-merge(as.data.frame(comparison_all_name, row.names=names(comparison_all_name)),as.data.frame(H1[,2:3], row.names = H1[,1]), by = 0, all = TRUE)

mm10_GOF <- as.data.frame(H2[2:4], row.names = H2[,1])

#calculate if p-values are lower than adjusted alpha.
mm10_GOF$x2_adj <- mm10_GOF$`unlist(GOF_x2)`> 0.05/length(row.names(mm10_GOF))

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pois"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pois"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="nb"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="nb"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pig"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pig"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="del"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="del"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pb"][!is.na(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pb"])])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pb"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pois2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pois2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="nb2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="nb2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pig2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pig2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="del2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="del2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pb2"][!is.na(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="pb2"])])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="pb2"])


sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipois"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipois"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zinb"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zinb"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipig"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipig"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zidel"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zidel"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipb"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipb"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipois2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipois2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zinb2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zinb2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipig2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipig2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zidel2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zidel2"])

sum(mm10_GOF$x2_adj[mm10_GOF$comparison_all_name=="zipb2"])
plot(mm10_GOF$`unlist(GOF_x2)`[mm10_GOF$comparison_all_name=="zipb2"])


save(mm10_GOF, file="Result_mm10_BIC_GOF.rda")






