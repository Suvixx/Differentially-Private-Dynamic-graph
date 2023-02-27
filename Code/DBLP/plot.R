
setwd("F:/dblp")
res <- read.csv("resalg1.csv")
res <- read.csv("resalg2.csv")
library(plotly)
# 
# g1 <- data.frame(mod = res$protected.g1.mod, group = rep("g1",nrow(res)), alpha = res$alpha, beta = res$beta, density = res$density.g1)
# g2 <- data.frame(mod = res$protected.g2.mod, group = rep("g2",nrow(res)), alpha = res$alpha, beta = res$beta, density = res$density.g1.1)
# g3 <- data.frame(mod = res$protected.g3.mod, group = rep("g3",nrow(res)), alpha = res$alpha, beta = res$beta, density = res$density.g1.2)
# g <- rbind(g1,g2,g3)
# g$group <- as.factor(g$group)
# mycolors <- c('red', 'green', 'blue')
# 
# nmig0g1 <- data.frame(nmi = res$g0_g1, group = rep("g0_g1",nrow(res)), alpha = res$alpha, beta = res$beta)
# nmig1g2 <- data.frame(nmi = res$g1_g2, group = rep("g1_g2",nrow(res)), alpha = res$alpha, beta = res$beta)
# nmig2g3 <- data.frame(nmi = res$g2_g3, group = rep("g2_g3",nrow(res)), alpha = res$alpha, beta = res$beta)
# g_nmi <- rbind(nmig0g1,nmig1g2,nmig2g3) 
# g_nmi$group <- as.factor(g_nmi$group)
# mycolors <- c('red', 'green', 'blue')
# 
# 
# 
# f <- plot_ly(g,x=~alpha,y=~beta, z=~mod,color = ~group, colors = mycolors, mode ="lines") 
# f1 <- plot_ly(g,x=~alpha,y=~beta, z=~density,color = ~group, colors = mycolors, mode ="lines") 
# f2 <- plot_ly(g_nmi,x=~alpha,y=~beta, z=~nmi,color = ~group, colors = mycolors, mode ="lines")

# mod <- as.matrix(res[,c(2,5,8)])
mod1 <- matrix(res[,2], nrow = 11, ncol = 11)
mod2 <- matrix(res[,5], nrow = 11, ncol = 11)
mod3 <- matrix(res[,8], nrow = 11, ncol = 11)
# density <- as.matrix(res[,c(4,7,10)])


alpha <- seq(0,1,0.1)
beta <- seq(0,1,0.1)
m <- plot_ly(showscale = TRUE)
m <- m%>%add_surface(x = alpha,y= beta,z =mod1, opacity =1, colorbar= list(title = "G1")) 
m <- m%>% add_surface(x = alpha,y= beta,z = mod2, opacity = 0.5, colorbar= list(title = "G2"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod3, opacity = 0.3, colorbar= list(title = "G3"))
m <- m%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                       zaxis = list(title = "Modularity")))

# library(plot3D)
# contour2D(mod, xlab ="Alpha", ylab = "Beta")


den1 <- matrix(res[,4], nrow = 11, ncol = 11)
den2 <- matrix(res[,7], nrow = 11, ncol = 11)
den3 <- matrix(res[,10], nrow = 11, ncol = 11)
n <- plot_ly(showscale = TRUE)
n <- n%>%add_surface(x = alpha,y= beta,z =den1, opacity =1, colorbar= list(title = "G1")) 
n <- n%>% add_surface(x = alpha,y= beta,z = den2, opacity = 0.5, colorbar= list(title = "G2"))
n <- n%>% add_surface(x = alpha,y= beta,z = den3, opacity = 0.3, colorbar= list(title = "G3"))
n <- n%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Density")))

nmi1 <- matrix(res[,11], nrow = 11, ncol = 11)
nmi2 <- matrix(res[,12], nrow = 11, ncol = 11)
nmi3 <- matrix(res[,13], nrow = 11, ncol = 11)
o <- plot_ly(showscale = TRUE)
o <- o%>%add_surface(x = alpha,y= beta,z =nmi1, opacity =1, colorbar= list(title = "G0-G1")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi2, opacity = 0.5, colorbar= list(title = "G1-G2"))
o <- o%>% add_surface(x = alpha,y= beta,z = nmi3, opacity = 0.3, colorbar= list(title = "G2-G3"))
o <- o%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "NMI")), showlegend = TRUE)











# fig <- plot_ly(res,x= ~alpha, y=~beta, z=~density.g1,type = "scatter3d",
#                color = "red", colors = "Set2")
# fig <- fig %>% add_trace(x= ~alpha, y=~beta, z=~density.g1.1,
#                           color = "green")
# fig <- fig %>% add_trace(x= ~alpha, y=~beta, z=~density.g1.2,
#                           color = "blue")
# fig <- fig %>% layout(showlegend = TRUE)
# fig
# 
# 
# fig1 <- plot_ly(res,x= ~alpha, y=~beta, z=~protected.g1.mod,type = "scatter3d", mode = "lines",
#                color = "red", colors = "Set2")
# fig1 <- fig1 %>% add_trace(x= ~alpha, y=~beta, z=~protected.g2.mod,
#                          line = list(width = 1, color = "green"))
# fig1 <- fig1 %>% add_trace(x= ~alpha, y=~beta, z=~protected.g3.mod,
#                          line = list(width = 1, color = "blue"))
# fig1 <- fig1 %>% layout(showlegend = TRUE)
# fig1
# 
# 
# fig2 <- plot_ly(res,x= ~alpha, y=~beta, z=~g0_g1,type = "scatter3d", mode = "lines",
#                 color = "red", colors = "Set2")
# fig2 <- fig2 %>% add_trace(x= ~alpha, y=~beta, z=~g1_g2,
#                            line = list(width = 1, color = "green"))
# fig2 <- fig2 %>% add_trace(x= ~alpha, y=~beta, z=~g2_g3,
#                            line = list(width = 1, color = "blue"))
# fig2 <- fig2 %>% layout(showlegend = TRUE)
# fig2
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
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
