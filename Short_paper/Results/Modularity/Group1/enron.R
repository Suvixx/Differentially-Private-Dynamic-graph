library(reticulate)
library(RcppCNPy)
library(igraph)
library(aricode)

setwd("/media/spaul/My Passport/short_paper_ccia/")

np <- import("numpy")
enron_d <- np$load("Enron120weeks.npy")

enron_dgr1 <- enron_d[,,1:30]
enron_dgr2 <- enron_d[,,31:60]
enron_dgr3 <- enron_d[,,61:90]
enron_dgr4 <- enron_d[,,91:120]


#alpha, beta
p0 <- seq(0,1,by=0.1)
p1 <- seq(0,1,by=0.1)

pp_pair <- tidyr::crossing(p0,p1)
pp_pair$alpha <- 1-pp_pair$p0
pp_pair$beta <- 1-pp_pair$p1
pp_pair <- as.data.frame(pp_pair)


agg <- function(a){
  dum <- a[1:184,1:184,1] + a[1:184,1:184,2] +a[1:184,1:184,3]+a[1:184,1:184,4] + a[1:184,1:184,5]
  dum[dum >0] <- 1
  return(dum)
}

agg2 <- function(a,b){
  dum <- a+b
  dum[dum >0] <- 1
  return(dum)
}

alg1 <- function(a,b,alpha,beta){
  w_g <- agg2(a,b)
  w_gg <- graph_from_adjacency_matrix(w_g,mode = "undirected")
  w_g_comp <- 1-w_g
  w_g_compg <- graph_from_adjacency_matrix(w_g_comp,mode = "undirected")
  if(alpha >0){
    g_0 <- erdos.renyi.game(184,alpha,type = "gnp")
    g_00 <- graph.intersection(g_0,w_g_compg, keep.all.vertices =FALSE)
  }else{g_00 <- w_gg}
  
  if(beta >0){
    g_01 <- erdos.renyi.game(184,beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =FALSE)
  }else {
    g1_1 <- w_gg
  }
  pg <- w_g + as.matrix(get.adjacency(g_00)) - as.matrix(get.adjacency(g1_1))
  pg[pg>0] <- 1
  pg[pg<0] <- 0
  pg_g <- graph_from_adjacency_matrix(pg,mode = "undirected")
  return(pg_g)
}


alg2_h <- function(alpha,beta){
  w_g <- ss1
  w_gg <- graph_from_adjacency_matrix(w_g,mode = "undirected")
  w_g_comp <- 1-w_g
  w_g_comp_g <- graph_from_adjacency_matrix(w_g_comp,mode = "undirected")
  if(alpha >0){
    g_0 <- erdos.renyi.game(184,alpha,type = "gnp")
    g_00 <- graph.intersection(g_0,w_g_comp_g, keep.all.vertices =FALSE)
  }else{g_00 <- w_gg}
  
  
  if(beta >0){
    g_01 <- erdos.renyi.game(184,beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =FALSE)
  }else {
    g1_1 <- w_gg
  }
  l <- as.matrix(get.adjacency(g_00)) - as.matrix(get.adjacency(g1_1))
  return(l)
}

alg2 <- function(b,alpha,beta){
  pg <- b + alg2_h(alpha,beta)
  pg[pg>0] <- 1
  pg[pg<0] <- 0
  pg_g <- graph_from_adjacency_matrix(pg,mode = "undirected")
  return(pg_g)
}



#snapshots for grps
ss1 <- agg(enron_dgr4[,,1:5])
ss2 <- agg(enron_dgr4[,,6:10])
ss3 <- agg(enron_dgr4[,,11:15])
ss4 <- agg(enron_dgr4[,,16:20])
ss5 <- agg(enron_dgr4[,,21:25])
ss6 <- agg(enron_dgr4[,,26:30])



res <- data.frame(p1.ss2.mod= rep(0,121), density.ss2=rep(0,121), nmi.ss2 = rep(0,121),
                  p1.ss3.mod= rep(0,121), density.ss3=rep(0,121), nmi.ss3 = rep(0,121),
                  p1.ss4.mod= rep(0,121), density.ss4=rep(0,121), nmi.ss4 = rep(0,121),
                  p1.ss5.mod= rep(0,121), density.ss5=rep(0,121), nmi.ss5 = rep(0,121),
                  p1.ss6.mod= rep(0,121), density.ss6=rep(0,121), nmi.ss6 = rep(0,121),
                  p2.ss2.mod= rep(0,121), density.ss2=rep(0,121), nmi.ss2 = rep(0,121),
                  p2.ss3.mod= rep(0,121), density.ss3=rep(0,121), nmi.ss3 = rep(0,121),
                  p2.ss4.mod= rep(0,121), density.ss4=rep(0,121), nmi.ss4 = rep(0,121),
                  p2.ss5.mod= rep(0,121), density.ss5=rep(0,121), nmi.ss5 = rep(0,121),
                  p2.ss6.mod= rep(0,121), density.ss6=rep(0,121), nmi.ss6 = rep(0,121))


nmi <- data.frame(ss2_3_1 = rep(0,121), ss3_4_1 = rep(0,121), ss4_5_1 = rep(0,121),ss5_6_1 = rep(0,121),
                  ss2_3_2 = rep(0,121), ss3_4_2 = rep(0,121), ss4_5_2 = rep(0,121),ss5_6_2 = rep(0,121))

for(i in 1:nrow(pp_pair)){
  alpha <- pp_pair[i,3]
  beta <- pp_pair[i,4]
  
  #protection with alg1
  ss2_p1 <- alg1(ss1,ss2,alpha,beta)
  ss3_p1 <- alg1(ss2,ss3,alpha,beta)
  ss4_p1 <- alg1(ss3,ss4,alpha,beta)
  ss5_p1 <- alg1(ss4,ss5,alpha,beta)
  ss6_p1 <- alg1(ss5,ss6,alpha,beta)
  
  #protection with alg2
  ss2_p2 <- alg2(ss2,alpha,beta)
  ss3_p2 <- alg2(ss3,alpha,beta)
  ss4_p2 <- alg2(ss4,alpha,beta)
  ss5_p2 <- alg2(ss5,alpha,beta)
  ss6_p2 <- alg2(ss6,alpha,beta)
  
  nmi[i,1] <- NMI(cluster_louvain(ss2_p1)$membership,cluster_louvain(ss3_p1)$membership)
  nmi[i,2] <- NMI(cluster_louvain(ss3_p1)$membership,cluster_louvain(ss4_p1)$membership)
  nmi[i,3] <- NMI(cluster_louvain(ss4_p1)$membership,cluster_louvain(ss5_p1)$membership)
  nmi[i,4] <- NMI(cluster_louvain(ss5_p1)$membership,cluster_louvain(ss6_p1)$membership)
  
  nmi[i,5] <- NMI(cluster_louvain(ss2_p2)$membership,cluster_louvain(ss3_p2)$membership)
  nmi[i,6] <- NMI(cluster_louvain(ss3_p2)$membership,cluster_louvain(ss4_p2)$membership)
  nmi[i,7] <- NMI(cluster_louvain(ss4_p2)$membership,cluster_louvain(ss5_p2)$membership)
  nmi[i,8] <- NMI(cluster_louvain(ss5_p2)$membership,cluster_louvain(ss6_p2)$membership)
}
  # res[i,1] <- modularity(cluster_louvain(ss2_p1))
  # res[i,2] <- graph.density(ss2_p1)
  # res[i,3] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss2,mode = "undirected"))$membership,
  #                  cluster_louvain(ss2_p1)$membership)
  # res[i,4] <- modularity(cluster_louvain(ss3_p1))
  # res[i,5] <- graph.density(ss3_p1)
  # res[i,6] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss3,mode = "undirected"))$membership,
  #                  cluster_louvain(ss3_p1)$membership)
  # res[i,7] <- modularity(cluster_louvain(ss4_p1))
  # res[i,8] <- graph.density(ss4_p1)
  # res[i,9] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss4,mode = "undirected"))$membership,
  #                  cluster_louvain(ss4_p1)$membership)
  # res[i,10] <- modularity(cluster_louvain(ss5_p1))
  # res[i,11] <- graph.density(ss5_p1)
  # res[i,12] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss5,mode = "undirected"))$membership,
  #                  cluster_louvain(ss5_p1)$membership)
  # res[i,13] <- modularity(cluster_louvain(ss6_p1))
  # res[i,14] <- graph.density(ss6_p1)
  # res[i,15] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss6,mode = "undirected"))$membership,
  #                  cluster_louvain(ss6_p1)$membership)
  # res[i,16] <- modularity(cluster_louvain(ss2_p2))
  # res[i,17] <- graph.density(ss2_p2)
  # res[i,18] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss2,mode = "undirected"))$membership,
  #                  cluster_louvain(ss2_p2)$membership)
  # res[i,19] <- modularity(cluster_louvain(ss3_p2))
  # res[i,20] <- graph.density(ss3_p2)
  # res[i,21] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss3,mode = "undirected"))$membership,
  #                  cluster_louvain(ss3_p2)$membership)
  # res[i,22] <- modularity(cluster_louvain(ss4_p2))
  # res[i,23] <- graph.density(ss4_p2)
  # res[i,24] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss4,mode = "undirected"))$membership,
  #                  cluster_louvain(ss4_p2)$membership)
  # res[i,25] <- modularity(cluster_louvain(ss5_p2))
  # res[i,26] <- graph.density(ss5_p2)
  # res[i,27] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss5,mode = "undirected"))$membership,
  #                  cluster_louvain(ss5_p2)$membership)
  # res[i,28] <- modularity(cluster_louvain(ss6_p2))
  # res[i,29] <- graph.density(ss6_p2)
  # res[i,30] <-  NMI(cluster_louvain(graph_from_adjacency_matrix(ss6,mode = "undirected"))$membership,
  #                  cluster_louvain(ss6_p2)$membership)
  
#}
write.csv(res,"gr1.csv",sep = ",")
write.csv(res,"gr2.csv",sep = ",")
write.csv(res,"gr3.csv",sep = ",")
write.csv(res,"gr4.csv",sep = ",")

write.csv(nmi,"nmigr1.csv",sep = ",")
write.csv(nmi,"nmigr2.csv",sep = ",")
write.csv(nmi,"nmigr3.csv",sep = ",")
write.csv(nmi,"nmigr4.csv",sep = ",")