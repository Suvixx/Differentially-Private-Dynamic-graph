setwd("/media/spaul/My Passport/short_paper_ccia/")
#res <- read.csv("gr1.csv")
#res <- read.csv("gr2.csv")
#res <- read.csv("gr3.csv")
res <- read.csv("gr4.csv")
alpha <- seq(0,1,0.1)
beta <- seq(0,1,0.1)
library(plotly)

mod2 <- matrix(res[,2], nrow = 11, ncol = 11)
mod3 <- matrix(res[,5], nrow = 11, ncol = 11)
mod4 <- matrix(res[,8], nrow = 11, ncol = 11)
mod5 <- matrix(res[,11], nrow = 11, ncol = 11)
mod6 <- matrix(res[,14], nrow = 11, ncol = 11)


m <- plot_ly(showscale = TRUE)

m <- m%>% add_surface(x = alpha,y= beta,z = mod2, opacity = 0.5, colorbar= list(title = "G2"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod3, opacity = 0.4, colorbar= list(title = "G3"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod4, opacity = 0.3, colorbar= list(title = "G4"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod5, opacity = 0.2, colorbar= list(title = "G5"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod6, opacity = 0.1, colorbar= list(title = "G6"))
m <- m%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Modularity")))
m


mod2 <- matrix(res[,17], nrow = 11, ncol = 11)
mod3 <- matrix(res[,20], nrow = 11, ncol = 11)
mod4 <- matrix(res[,23], nrow = 11, ncol = 11)
mod5 <- matrix(res[,26], nrow = 11, ncol = 11)
mod6 <- matrix(res[,29], nrow = 11, ncol = 11)
m <- plot_ly(showscale = TRUE)

m <- m%>% add_surface(x = alpha,y= beta,z = mod2, opacity = 0.5, colorbar= list(title = "G2"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod3, opacity = 0.4, colorbar= list(title = "G3"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod4, opacity = 0.3, colorbar= list(title = "G4"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod5, opacity = 0.2, colorbar= list(title = "G5"))
m <- m%>% add_surface(x = alpha,y= beta,z = mod6, opacity = 0.1, colorbar= list(title = "G6"))
m <- m%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Modularity")))
m


den2 <- matrix(res[,3], nrow = 11, ncol = 11)
den3 <- matrix(res[,6], nrow = 11, ncol = 11)
den4 <- matrix(res[,9], nrow = 11, ncol = 11)
den5 <- matrix(res[,12], nrow = 11, ncol = 11)
den6 <- matrix(res[,15], nrow = 11, ncol = 11)
n <- plot_ly(showscale = TRUE)
n <- n%>%add_surface(x = alpha,y= beta,z =den2, opacity =1, colorbar= list(title = "G2")) 
n <- n%>% add_surface(x = alpha,y= beta,z = den3, opacity = 0.5, colorbar= list(title = "G3"))
n <- n%>% add_surface(x = alpha,y= beta,z = den4, opacity = 0.3, colorbar= list(title = "G4"))
n <- n%>% add_surface(x = alpha,y= beta,z = den5, opacity = 0.5, colorbar= list(title = "G5"))
n <- n%>% add_surface(x = alpha,y= beta,z = den6, opacity = 0.3, colorbar= list(title = "G6"))
n <- n%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Density")))
n

den2 <- matrix(res[,18], nrow = 11, ncol = 11)
den3 <- matrix(res[,21], nrow = 11, ncol = 11)
den4 <- matrix(res[,24], nrow = 11, ncol = 11)
den5 <- matrix(res[,27], nrow = 11, ncol = 11)
den6 <- matrix(res[,30], nrow = 11, ncol = 11)
n <- plot_ly(showscale = TRUE)
n <- n%>%add_surface(x = alpha,y= beta,z =den2, opacity =1, colorbar= list(title = "G2")) 
n <- n%>% add_surface(x = alpha,y= beta,z = den3, opacity = 0.5, colorbar= list(title = "G3"))
n <- n%>% add_surface(x = alpha,y= beta,z = den4, opacity = 0.3, colorbar= list(title = "G4"))
n <- n%>% add_surface(x = alpha,y= beta,z = den5, opacity = 0.5, colorbar= list(title = "G5"))
n <- n%>% add_surface(x = alpha,y= beta,z = den6, opacity = 0.3, colorbar= list(title = "G6"))
n <- n%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "Density")))
n


nmi2 <- matrix(res[,4], nrow = 11, ncol = 11)
nmi3 <- matrix(res[,7], nrow = 11, ncol = 11)
nmi4 <- matrix(res[,10], nrow = 11, ncol = 11)
nmi5 <- matrix(res[,13], nrow = 11, ncol = 11)
nmi6 <- matrix(res[,16], nrow = 11, ncol = 11)
o <- plot_ly(showscale = TRUE)
o <- o%>% add_surface(x = alpha,y= beta,z =nmi2, opacity =1, colorbar= list(title = "G2-p.G2")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi3, opacity = 0.5, colorbar= list(title = "G3-p.G3"))
o <- o%>% add_surface(x = alpha,y= beta,z = nmi4, opacity = 0.3, colorbar= list(title = "G4-p.G4"))
o <- o%>% add_surface(x = alpha,y= beta,z =nmi5, opacity =1, colorbar= list(title = "G5-p.G5")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi6, opacity = 0.5, colorbar= list(title = "G6-p.G6"))
o <- o%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "NMI")), showlegend = TRUE)
o


nmi2 <- matrix(res[,19], nrow = 11, ncol = 11)
nmi3 <- matrix(res[,22], nrow = 11, ncol = 11)
nmi4 <- matrix(res[,25], nrow = 11, ncol = 11)
nmi5 <- matrix(res[,28], nrow = 11, ncol = 11)
nmi6 <- matrix(res[,31], nrow = 11, ncol = 11)
o <- plot_ly(showscale = TRUE)
o <- o%>% add_surface(x = alpha,y= beta,z =nmi2, opacity =1, colorbar= list(title = "G2-p.G2")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi3, opacity = 0.5, colorbar= list(title = "G3-p.G3"))
o <- o%>% add_surface(x = alpha,y= beta,z = nmi4, opacity = 0.3, colorbar= list(title = "G4-p.G4"))
o <- o%>% add_surface(x = alpha,y= beta,z =nmi5, opacity =1, colorbar= list(title = "G5-p.G5")) 
o <- o%>% add_surface(x = alpha,y= beta,z = nmi6, opacity = 0.5, colorbar= list(title = "G6-p.G6"))
o <- o%>% layout(scene= list(xaxis = list(title = "Alpha"),yaxis = list(title = "Beta"), 
                             zaxis = list(title = "NMI")), showlegend = TRUE)
o

