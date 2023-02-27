# library(rgl)
# 
# # This is to output a rgl plot in a rmarkdown document.
# # setupKnitr()
# 
# # Data: the iris data is provided by R
# data <- iris
# 
# # Add a new column with color
# mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
# data$color <- mycolors[ as.numeric(data$Species) ]
# 
# # Plot
# plot3d( 
#   x=data$`Sepal.Length`, y=data$`Sepal.Width`, z=data$`Petal.Length`, 
#   col = data$color, 
#   type = 's', 
#   radius = .1,
#   xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")
# 
# # To display in an R Markdown document:
# # rglwidget()
# 
# # To save to a file:
# htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
#                         file = "HtmlWidget/3dscatter.html",
#                         libdir = "libs",
#                         selfcontained = FALSE
# )
# 
# 
# res <- read.csv("/media/spaul/My Passport/CAIDA/alg2-part-res-caida.csv")
# g1 <- data.frame(mod =res$protected.g1.mod, group = rep("mod1", nrow(res)))
# g2 <- data.frame(mod =res$protected.g2.mod, group = rep("mod2", nrow(res)), alpha = res$alpha, beta = res$beta)
# g3 <- data.frame(mod =res$protected.g3.mod, group = rep("mod3", nrow(res)), alpha = res$alpha, beta = res$beta)
# g <- rbind(g1,g2,g3)
# g$group <- as.factor(g$group)
# mycolors <- c('red', 'green', 'blue')
# g$color <- mycolors[ as.numeric(g$group) ]
# 
# library(rgl)
# plot3d(x= g$alpha, y=g$beta, z=g$mod,
#        col = g$color,
#        size = 2,
#        type = "s",
#        lwd = 1,
#        radius = 0.01,
#        xlab = "Alpha", ylab = "Beta", zlab = "Modularity")
# rglwidget()
# 
# 
# library(plotly)
# fig <- plot_ly(g,x= ~alpha, y=~beta, z=~mod,type = "scatter3d", mode = "lines",
#                color = ~color)
# fig <- fig %>% add_trace(x= ~alpha, y=~beta, z=~protected.g2.mod,
#                          line = list(width = 1, color = "green"))
# fig <- fig %>% add_trace(x= ~alpha, y=~beta, z=~protected.g2.mod,
#                          line = list(width = 1, color = "blue"))
# fig <- fig %>% layout(showlegend = TRUE)
# 
# 
# 
# 
# #estimated alpha-beta from the alg1
# dab <- read.csv("/media/spaul/My Passport/CAIDA/alpha-beta-caida.csv")
# library(ggplot2)
# plotab1 <-ggplot(dab, aes(1:121,1:121))+ 
#   geom_point(aes(0.18, 0.79, col = "actual alpha and beta"), size = 3)+
#   geom_point(aes(c_alpha, c_beta, col = "estimated alpha and beta")) +
#   labs(x = "Alpha", y = "Beta", color ="actual vs estimated")+
#   annotate(geom = "text", x = 0.2, y=0.8125, label ="p0 = 0.7, p1 = 0.5", color = "black")
# 
# 
# 
# g1 <- data.frame(mod =res$protected.g1.mod, group = rep("mod1", nrow(res)), density = res$density.g1)
# g2 <- data.frame(mod =res$protected.g2.mod, group = rep("mod2", nrow(res)), density = res$density.g1.1)
# g3 <- data.frame(mod =res$protected.g3.mod, group = rep("mod3", nrow(res)), density = res$density.g1.2)
# g <- rbind(g1,g2,g3)
# g$group <- as.factor(g$group)
# mycolors <- c('red', 'green', 'blue')
# g$color <- mycolors[ as.numeric(g$group) ]
# 
# ggplot(g,aes(x=density,y=mod))+geom_point()+scale_fill_manual(g$color,aesthetics = "fill")
# 
# 
# plot(res$density.g1,res$protected.g1.mod,col = "red",xlab = "Graph density", ylab = "Modularity")
# points(res$density.g1.1,res$protected.g2.mod, col ="blue")
# points(res$density.g1.2,res$protected.g3.mod, col ="green")
# legend("topright", c("Protected G1","Protected G2","Protected G3"),fill = c("red","blue","green"))
# 
# 
# 
# 
# 
# 
# 
# des1 <- read.csv("F:/CAIDA/densityalg1.csv")
# nmi1 <- read.csv("F:/CAIDA/nmialg.csv")
# 
# plot(1:121,nmi1$g0_g1, col ="green", type = "l", ylim = c(0,0.35), xlab = "p0,p1 pair",
#      ylab = "NMI", xaxt="n")
# lines(1:121,y=nmi1$g1_g2,col ="blue")
# lines(1:121,y=nmi1$g2_g3,col ="red")
# legend("topleft",c("g0-g1","g1-g2","g2-g3"),fill = c("green","blue","red"))
# axis(side= 1, at = ytick, labels = ytick)
# text(x=1:121,y = 0,srt = 60, labels = ytick, cex = 0.8, adj = 1.8, xpd = NA)
# 
# 
# 
# plot(1:121,des1$density.g1, col ="green", type = "l",  xlab = "p0,p1 pair",
#      ylab = "Density",xaxt ="n")
# lines(1:121,y=des1$density.g1.1,col ="blue")
# lines(1:121,y=des1$density.g1.2,col ="red")
# legend("topright",c("g1","g2","g3"),fill = c("green","blue","red"))
# axis(side= 1, at = ytick, labels = FALSE)
# text(x=1:121,y = 0,srt = 45, labels = ytick, cex = 0.6, adj = 1, xpd = NA)
# 
# des2 <- read.csv("F:/CAIDA/densityalg2.csv")
# nmi2 <- read.csv("F:/CAIDA/nmialg2.csv")
# 
# plot(1:121,nmi2$g0_g1, col ="green", type = "l", ylim = c(0,0.4), xlab = "p0,p1 pair",
#      ylab = "NMI",xaxt ="n")
# lines(1:121,y=nmi2$g1_g2,col ="blue")
# lines(1:121,y=nmi2$g2_g3,col ="red")
# legend("topleft",c("g0-g1","g1-g2","g2-g3"),fill = c("green","blue","red"))
# axis(side= 1, at = ytick, labels = ytick)
# text(x=1:121,y = 0,srt = 45, labels = ytick, cex = 0.6, adj = 1.8, xpd = NA)
# 
# plot(1:121,des2$density.g1, col ="green", type = "l",  xlab = "p0,p1 pair",
#      ylab = "Density", xaxt="n")
# lines(1:121,y=des2$density.g1.1,col ="blue")
# lines(1:121,y=des2$density.g1.2,col ="red")
# legend("topright",c("g1","g2","g3"),fill = c("green","blue","red"))
# axis(side= 1, at = ytick, labels = FALSE)
# text(x=1:121,y = 0,srt = 45, labels = ytick, cex = 0.6, adj = 1, xpd = NA)
# 
# 
# plot(des1$density.g1,nmi1$g0_g1, col ="green", ylim = c(0,0.4), xlab = "Density",
#      ylab = "NMI")
# points(des1$density.g1.1,y=nmi1$g1_g2,col ="blue")
# points(des2$density.g1.2,y=nmi1$g2_g3,col ="red")
# legend("topright",c("g0-g1","g1-g2","g2-g3"),fill = c("green","blue","red"))
# 
# plot(1:121,des1$density.g1, col ="red", type = "l",  xlab = "p0,p1 pair",
#      ylab = "Density", xaxt="n")
# lines(1:121,y=des2$density.g1,col ="blue")
# legend("topright",c("ALgorithm 1","Algorithm 2"),fill = c("red", "blue"))
# axis(side= 1, at = ytick, labels = FALSE)
# text(x=1:121,y = 0,srt = 45, labels = ytick, cex = 0.6, adj = 1, xpd = NA)
# 
# 
# 
# 
setwd("F:/CAIDA")
res <- read.csv("densityalg1.csv")
res <- read.csv("densityalg2.csv")#has to be written again
library(plotly)

mod1 <- matrix(res[,2], nrow = 11, ncol = 11)
mod2 <- matrix(res[,5], nrow = 11, ncol = 11)
mod3 <- matrix(res[,8], nrow = 11, ncol = 11)
alpha <- seq(0,1,0.1)
beta <- seq(0,1,0.1)
m <- plot_ly(showscale = TRUE)
m <- m%>%add_surface(x = alpha,y= beta,z =mod1, opacity =1, colorbar= list(title = "G1")) 
m <- m%>% add_surface(x = alpha,y= beta,z = mod2, opacity = 0.5, colorbar= list(title = "G2"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod3, opacity = 0.3, colorbar= list(title = "G3"))
m <- m%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Modularity")))


den1 <- matrix(res[,4], nrow = 11, ncol = 11)
den2 <- matrix(res[,7], nrow = 11, ncol = 11)
den3 <- matrix(res[,10], nrow = 11, ncol = 11)
n <- plot_ly(showscale = TRUE)
n <- n%>%add_surface(x = alpha,y= beta,z =den1, opacity =1, colorbar= list(title = "G1")) 
n <- n%>% add_surface(x = alpha,y= beta,z = den2, opacity = 0.5, colorbar= list(title = "G2"))
n <- n%>% add_surface(x = alpha,y= beta,z = den3, opacity = 0.3, colorbar= list(title = "G3"))
n <- n%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Density")))
nmi <- read.csv("nmialg.csv")
nmi <- read.csv("nmialg2.csv")
nmi1 <- matrix(nmi[,2], nrow = 11, ncol = 11)
nmi2 <- matrix(nmi[,3], nrow = 11, ncol = 11)
nmi3 <- matrix(nmi[,4], nrow = 11, ncol = 11)
o <- plot_ly(showscale = TRUE)
o <- o%>%add_surface(x = alpha,y= beta,z =nmi1, opacity =1, colorbar= list(title = "G0-G1")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi2, opacity = 0.5, colorbar= list(title = "G1-G2"))
o <- o%>% add_surface(x = alpha,y= beta,z = nmi3, opacity = 0.3, colorbar= list(title = "G2-G3"))
o <- o%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "NMI")), showlegend = TRUE)

