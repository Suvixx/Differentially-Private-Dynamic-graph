

library(plotly)
setwd("/media/spaul/My Passport/short_paper_ccia/")
res <- read.csv("nmigr1.csv",sep = ",")
res <- read.csv("nmigr2.csv")
res <- read.csv("nmigr3.csv")
res <- read.csv("nmigr4.csv")

nmi2 <- matrix(res[,2], nrow = 11, ncol = 11)
nmi2 <- nmi2[2:10,2:10]
nmi3 <- matrix(res[,3], nrow = 11, ncol = 11)
nmi3 <- nmi3[2:10,2:10]
nmi4 <- matrix(res[,4], nrow = 11, ncol = 11)
nmi4 <- nmi4[2:10,2:10]
nmi5 <- matrix(res[,5], nrow = 11, ncol = 11)
nmi5 <- nmi5[2:10,2:10]
# nmi6 <- matrix(res[,16], nrow = 11, ncol = 11)
# nmi6 <- nmi6[2:10,2:10]

eps_m <- matrix(e_ps$epsilon, nrow = 11, ncol = 11)
eps_m <- eps_m[2:10,2:10]
k <- nmi2[,1]
for(i in 2:ncol(nmi2)){
  k <- c(k,nmi2[,i])
}
k1 <- eps_m[,1]
for(i in 2:ncol(eps_m)){
  k1 <- c(k1,eps_m[,i])
}
k2<- nmi3[,1]
for(i in 2:ncol(nmi3)){
  k2 <- c(k2,nmi3[,i])
}

k3<- nmi4[,1]
for(i in 2:ncol(nmi4)){
  k3 <- c(k3,nmi4[,i])
}
k4<- nmi5[,1]
for(i in 2:ncol(nmi5)){
  k4 <- c(k4,nmi5[,i])
}
# k5<- nmi6[,1]
# for(i in 1:ncol(nmi6)){
#   k5 <- c(k5,nmi6[,i])
# }

fig <- plot_ly(x=k1,y=k, name = "NMI G2-G3", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k2, name = "NMI G3-G4", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k3, name = "NMI G4-G5", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k4, name = "NMI G5-G6", type = "scatter", mode = "markers")
#fig <- fig%>% add_trace(y =k5, name = "NMI G6-p.G6", type = "scatter", mode = "markers")
fig <- fig %>% layout(xaxis = list(title = "Epsilon"),
                      yaxis = list (title = "NMI"))
fig



nmi2 <- matrix(res[,6], nrow = 11, ncol = 11)
nmi2 <- nmi2[2:10,2:10]
nmi3 <- matrix(res[,7], nrow = 11, ncol = 11)
nmi3 <- nmi3[2:10,2:10]
nmi4 <- matrix(res[,8], nrow = 11, ncol = 11)
nmi4 <- nmi4[2:10,2:10]
nmi5 <- matrix(res[,9], nrow = 11, ncol = 11)
nmi5 <- nmi5[2:10,2:10]
# nmi6 <- matrix(res[,30], nrow = 11, ncol = 11)
# nmi6 <- nmi6[2:10,2:10]

eps_m <- matrix(e_ps$epsilon, nrow = 11, ncol = 11)
eps_m <- eps_m[2:10,2:10]
k <- nmi2[,1]
for(i in 1:ncol(nmi2)){
  k <- c(k,nmi2[,i])
}
k1 <- eps_m[,2]
for(i in 1:ncol(eps_m)){
  k1 <- c(k1,eps_m[,i])
}
k2<- nmi3[,1]
for(i in 1:ncol(nmi3)){
  k2 <- c(k2,nmi3[,i])
}

k3<- nmi4[,1]
for(i in 1:ncol(nmi4)){
  k3 <- c(k3,nmi4[,i])
}
k4<- nmi5[,1]
for(i in 1:ncol(nmi5)){
  k4 <- c(k4,nmi5[,i])
}
# k5<- nmi6[,1]
# for(i in 1:ncol(nmi6)){
#   k5 <- c(k5,nmi6[,i])
# }

fig <- plot_ly(x=k1,y=k, name = "NMI G2-G3", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k2, name = "NMI G3-G4", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k3, name = "NMI G4-G5", type = "scatter", mode = "markers")
fig <- fig%>% add_trace(y =k4, name = "NMI G5-G6", type = "scatter", mode = "markers")
#fig <- fig%>% add_trace(y =k5, name = "NMI G6-p.G6", type = "scatter", mode = "markers")
fig <- fig %>% layout(xaxis = list(title = "Epsilon"),
                      yaxis = list (title = "NMI"))
fig
