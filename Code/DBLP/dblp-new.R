set.seed(2000)
library(sqldf)
library(igraph)
library(tidyr)
library(dplyr)
library(aricode)



dblpg0 <- read.csv("/media/spaul/My Passport/dblp/g0.csv")
dblpg1 <- read.csv("/media/spaul/My Passport/dblp/g1.csv")
dblpg2 <- read.csv("/media/spaul/My Passport/dblp/g2.csv")
dblpg3 <- read.csv("/media/spaul/My Passport/dblp/g3.csv")

dblpg0 <- dblpg0[,2:3]
dblpg1 <- dblpg1[,2:3]
dblpg2 <- dblpg2[,2:3]
dblpg3 <- dblpg3[,2:3]

vertex <- read.csv("/media/spaul/My Passport/dblp/dblp-vertices.csv")
ver <- vertex[,2]
ver_g <- tidyr::crossing(ver,ver)
colnames(ver_g) <- c("name1","name2")

p0 <- seq(0,1,by=0.1)
p1 <- seq(0,1,by=0.1)

pp_pair <- tidyr::crossing(p0,p1)
pp_pair$alpha <- 1-pp_pair$p0
pp_pair$beta <- 1-pp_pair$p1
pp_pair$c_alpha <- rep(0,121)
pp_pair$c_beta <- rep(0,121)
pp_pair <- as.data.frame(pp_pair)




g0_w <- dblpg0
g0_w_comp <- setdiff(ver_g,g0_w)
g0_plot <- graph_from_data_frame(g0_w,directed = FALSE, vertices = ver)
adj0 <- as.matrix(get.adjacency(g0_plot, sparse = FALSE))
plot(g0_plot, vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.5)
g0_c <- cluster_louvain(g0_plot)

g1_w <- dblpg1
g1_w_comp <- setdiff(ver_g,g1_w)
g1_plot <- graph_from_data_frame(g1_w,directed = FALSE, vertices = ver)
adj1_w <- as.matrix(get.adjacency(g1_plot, sparse = FALSE))
plot(g1_plot, vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.5)
g1_c <- cluster_louvain(g1_plot)
mod_g1 <- modularity(g1_c)

g2_w <- dblpg2
g2_w_comp <- setdiff(ver_g,g2_w)
g2_plot <- graph_from_data_frame(g2_w,directed = FALSE, vertices = ver)
adj2_w <- as.matrix(get.adjacency(g2_plot, sparse = FALSE))
plot(g2_plot, vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.5)
g2_c <- cluster_louvain(g2_plot)
mod_g2 <- modularity(g2_c)

g3_w <- dblpg3
g3_w_comp <- setdiff(ver_g,g3_w)
g3_plot <- graph_from_data_frame(g3_w,directed = FALSE, vertices = ver)
adj3_w <- as.matrix(get.adjacency(g3_plot, sparse = FALSE))
plot(g3_plot, vertex.color = "red", vertex.size = 2, vertex.label.cex = 0.5)
g3_c <- cluster_louvain(g3_plot)
mod_g3 <- modularity(g3_c)

res <- data.frame(protected.g1.mod= rep(0,121), protected.g1.edges=rep(0,121), density.g1=rep(0,121),
                  protected.g2.mod= rep(0,121), protected.g2.edges=rep(0,121),density.g1=rep(0,121),
                  protected.g3.mod= rep(0,121), protected.g3.edges=rep(0,121),density.g1=rep(0,121))

nmi <- data.frame(g0_g1 = rep(0,121), g1_g2 = rep(0,121), g2_g3 = rep(0,121))

#alg1
for(i in 1:nrow(pp_pair)){
  alpha <- pp_pair[i,3]
  beta <- pp_pair[i,4]
  
  w_g <- unique(rbind(g0_w,g1_w))
  w_gg <- graph_from_data_frame(w_g,directed = FALSE, vertices = ver)
  w_g_comp <- unique(rbind(g0_w_comp,g1_w_comp))
  w_g_comp_g <- graph_from_data_frame(w_g_comp,directed = FALSE, vertices = ver)
  if(alpha >0){
    g_00 <- erdos.renyi.game(length(ver),alpha, type = "gnp")
    g <- graph.intersection(g_00,w_g_comp_g, keep.all.vertices =FALSE)
  } else {
    g <- w_gg
  }
  
  f <- as.data.frame(as_edgelist(g))
  colnames(f) <- c("name1","name2")
  
  if(beta >0){
    g_01 <- erdos.renyi.game(length(ver),beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =FALSE)
  } else {
    g1_1 <- w_gg
  }
  f1 <- as.data.frame(as_edgelist(g1_1))
  colnames(f1) <- c("name1","name2")
  p_g1 <- setdiff(unique(rbind(w_g,f)),f1)
  #p_g1 <- data.frame(name1 = as.numeric(p_g1$name1), name2 = as.numeric(p_g1$name2))
  p_g1g <- graph_from_data_frame(p_g1, directed = FALSE)
  adj1 <- as.matrix(get.adjacency(p_g1g, sparse = FALSE))
  
  pg1g_c <- cluster_louvain(p_g1g)
  mod_g1p <- modularity(pg1g_c)
  res[i,1] <- mod_g1p
  res[i,2] <- nrow(p_g1)
  res[i,3] <- graph.density(p_g1g)
  
  
  if(length(g0_c$membership) != length(pg1g_c$membership)){
    cld <- rep(0,abs(length(g0_c$membership)-length(pg1g_c$membership)))
    clcd1 <- c(pg1g_c$membership,cld)
    nmi[i,1] <-NMI(g0_c$membership,clcd1)
  } else{nmi[i,1] <-NMI(g0_c$membership,pg1g_c$membership)}
  
  
  
  
  w_g <- unique(rbind(g1_w,g2_w))
  w_gg <- graph_from_data_frame(w_g,directed = FALSE, vertices = ver)
  w_g_comp <- unique(rbind(g1_w_comp,g2_w_comp))
  w_g_comp_g <- graph_from_data_frame(w_g_comp,directed = FALSE, vertices = ver)
  if(alpha >0){
    g_00 <- erdos.renyi.game(length(ver),alpha, type = "gnp")
    g <- graph.intersection(g_00,w_g_comp_g, keep.all.vertices =FALSE)
  } else {
    g <- w_gg
  }
  f <- as.data.frame(as_edgelist(g))
  colnames(f) <- c("name1","name2")
  if(beta >0){
    g_01 <- erdos.renyi.game(length(ver),beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =FALSE)
  } else {
    g1_1 <- w_gg
  }
  f1 <- as.data.frame(as_edgelist(g1_1))
  colnames(f1) <- c("name1","name2")
  p_g2 <- setdiff(unique(rbind(w_g,f)),f1)
  #p_g2 <- data.frame(name1 = as.numeric(p_g2$name1), name2 = as.numeric(p_g2$name2))
  p_g2g <- graph_from_data_frame(p_g2, directed = FALSE)
  adj2 <- as.matrix(get.adjacency(p_g2g, sparse = FALSE))
  pg2g_c <- cluster_louvain(p_g2g)
  mod_g2p <- modularity(pg2g_c)
  res[i,4] <- mod_g2p
  res[i,5] <- nrow(p_g2)
  res[i,6] <- graph.density(p_g1g)
  if(length(g1_c$membership) != length(pg2g_c$membership)){
    cld <- rep(0,abs(length(g1_c$membership)-length(pg2g_c$membership)))
    clcd1 <- c(pg2g_c$membership,cld)
    nmi[i,2] <-NMI(g1_c$membership,clcd1)
  } 
  else{nmi[i,2] <-NMI(g1_c$membership,pg2g_c$membership)}
  
  
  w_g <- unique(rbind(g2_w,g3_w))
  w_gg <- graph_from_data_frame(w_g,directed = FALSE, vertices = ver)
  w_g_comp <- unique(rbind(g2_w_comp,g3_w_comp))
  w_g_comp_g <- graph_from_data_frame(w_g_comp,directed = FALSE, vertices = ver)
  if(alpha >0){
    g_00 <- erdos.renyi.game(length(ver),alpha, type = "gnp")
    g <- graph.intersection(g_00,w_g_comp_g, keep.all.vertices =FALSE)
  } else {
    g <- w_gg
  }  
  f <- as.data.frame(as_edgelist(g))
  colnames(f) <- c("name1","name2")
  if(beta >0){
    g_01 <- erdos.renyi.game(length(ver),beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =FALSE)
  } else {
    g1_1 <- w_gg
  }
  f1 <- as.data.frame(as_edgelist(g1_1))
  colnames(f1) <- c("name1","name2")
  p_g3 <- setdiff(unique(rbind(w_g,f)),f1)
  #p_g3 <- data.frame(name1 = as.numeric(p_g3$name1), name2 = as.numeric(p_g3$name2))
  p_g3g <- graph_from_data_frame(p_g3, directed = FALSE)
  adj3 <- as.matrix(get.adjacency(p_g3g, sparse = FALSE))
  pg3g_c <- cluster_louvain(p_g3g)
  mod_g3p <- modularity(pg3g_c)
  res[i,7] <- mod_g3p
  res[i,8] <- nrow(p_g3)
  res[i,9] <- graph.density(p_g1g)
  
  if(length(g2_c$membership) != length(pg3g_c$membership)){
    cld <- rep(0,abs(length(g2_c$membership)-length(pg3g_c$membership)))
    clcd1 <- c(pg3g_c$membership,cld)
    nmi[i,3] <-NMI(g2_c$membership,clcd1)
  } 
  else{nmi[i,3] <-NMI(g2_c$membership,pg3g_c$membership)}
  
  
}



#alg2
for(i in 1:nrow(pp_pair)){
  alpha <- pp_pair[i,3]
  beta <- pp_pair[i,4]
  
  
  w_g <- g0_w
  w_gg <- graph_from_data_frame(w_g,directed = FALSE, vertices = ver)
  w_g_comp <- g0_w_comp
  w_g_comp_g <- graph_from_data_frame(w_g_comp,directed = FALSE, vertices = ver)
  if(alpha >0){
    g_00 <- erdos.renyi.game(length(ver),alpha, type = "gnp")
    g <- graph.intersection(g_00,w_g_comp_g, keep.all.vertices =TRUE)
  } else {
    g <- w_gg
  }
  
  f <- as.data.frame(as_edgelist(g))
  colnames(f) <- c("name1","name2")
  
  if(beta >0){
    g_01 <- erdos.renyi.game(length(ver),beta, type = "gnp")
    g1_1 <- graph.intersection(g_01,w_gg, keep.all.vertices =TRUE)
  } else {
    g1_1 <- w_gg
  }
  f1 <- as.data.frame(as_edgelist(g1_1))
  colnames(f1) <- c("name1","name2")
  
  
  
  
  
  p_g1 <- setdiff( rbind(g1_w,f),f1)
  p_g2 <- setdiff( rbind(g2_w,f),f1)
  p_g3 <- setdiff( rbind(g3_w,f),f1)
  
  # p_g1 <- data.frame(name1 = as.numeric(p_g1$name1), name2 = as.numeric(p_g1$name2))
  # p_g2 <- data.frame(name1 = as.numeric(p_g2$name1), name2 = as.numeric(p_g2$name2))
  # p_g3 <- data.frame(name1 = as.numeric(p_g3$name1), name2 = as.numeric(p_g3$name2))
  
  p_g1g <- graph_from_data_frame(p_g1, directed = FALSE)
  p_g2g <- graph_from_data_frame(p_g2, directed = FALSE)
  p_g3g <- graph_from_data_frame(p_g3, directed = FALSE)
  
  adj1 <- as.matrix(get.adjacency(p_g1g, sparse = FALSE))
  adj2 <- as.matrix(get.adjacency(p_g1g, sparse = FALSE))
  adj3 <- as.matrix(get.adjacency(p_g1g, sparse = FALSE))
  
  pg1g_c <- cluster_louvain(p_g1g)
  mod_g1p <- modularity(pg1g_c)
  res[i,1] <- mod_g1p
  res[i,2] <- nrow(p_g1)
  res[i,3] <- graph.density(p_g1g)
  
  pg2g_c <- cluster_louvain(p_g2g)
  mod_g2p <- modularity(pg2g_c)
  res[i,4] <- mod_g2p
  res[i,5] <- nrow(p_g2)
  res[i,6] <- graph.density(p_g1g)
  
  pg3g_c <- cluster_louvain(p_g3g)
  mod_g3p <- modularity(pg3g_c)
  res[i,7] <- mod_g3p
  res[i,8] <- nrow(p_g3)
  res[i,9] <- graph.density(p_g1g)
  
  if(length(g0_c$membership) != length(pg1g_c$membership)){
    cld <- rep(0,abs(length(g0_c$membership)-length(pg1g_c$membership)))
    clcd1 <- c(pg1g_c$membership,cld)
    nmi[i,1] <-NMI(g0_c$membership,clcd1)
  } 
  else{nmi[i,1] <-NMI(g0_c$membership,pg1g_c$membership)}
  
  if(length(g1_c$membership) != length(pg2g_c$membership)){
    cld <- rep(0,abs(length(g1_c$membership)-length(pg2g_c$membership)))
    clcd1 <- c(pg2g_c$membership,cld)
    nmi[i,2] <-NMI(g1_c$membership,clcd1)
  } 
  else{nmi[i,2] <-NMI(g1_c$membership,pg2g_c$membership)}
  
  if(length(g2_c$membership) != length(pg3g_c$membership)){
    cld <- rep(0,abs(length(g2_c$membership)-length(pg3g_c$membership)))
    clcd1 <- c(pg3g_c$membership,cld)
    nmi[i,3] <-NMI(g2_c$membership,clcd1)
  } 
  else{nmi[i,3] <-NMI(g2_c$membership,pg3g_c$membership)}
  
}