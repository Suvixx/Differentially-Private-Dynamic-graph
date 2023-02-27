setwd("F:/ytg")
res <- read.csv("res1.csv")
res <- read.csv("al2res.csv")#has to be written again
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

nmi1 <- matrix(res[,11], nrow = 11, ncol = 11)
nmi2 <- matrix(res[,12], nrow = 11, ncol = 11)
nmi3 <- matrix(res[,13], nrow = 11, ncol = 11)
o <- plot_ly(showscale = TRUE)
o <- o%>%add_surface(x = alpha,y= beta,z =nmi1, opacity =1, colorbar= list(title = "G0-G1")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi2, opacity = 0.5, colorbar= list(title = "G1-G2"))
o <- o%>% add_surface(x = alpha,y= beta,z = nmi3, opacity = 0.3, colorbar= list(title = "G2-G3"))
o <- o%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "NMI")), showlegend = TRUE)

