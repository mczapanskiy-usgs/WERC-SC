
# rm(list = ls())
#
# library(devtools)
# library(siar)
# library(SIBER)
# 
# graphics.off()
setwd("~/WERC-SC/Vuln_Index")
scores <- read.csv("PV.CV.DVscores.csv", header = T) ## matrix of final PV, CV, and DV
# scores$DispBest <- as.numeric(scores$DispBest)

# Splining a polygon.
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.(Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain a smooth periodic spline.
#   Returns an array of points. 
spline.poly <- function(xy, vertices, k=3, ...) {
  # Assert: xy is an n by 2 matrix with n >= k.
  # Wrap k vertices around each end.
  n <- dim(xy)[1]
  if (k >= 1) {
    data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
  } else {
    data <- xy
  }
  # Spline the x and y coordinates.
  data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
  x <- data.spline$x
  x1 <- data.spline$y
  x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
  
  # Retain only the middle part.
  cbind(x1, x2)[k < x & x <= n+k, ]
}

for(i in levels(scores$Taxonomy)){
  test <- subset(scores, Taxonomy ==i)
  plot(test$ColBest, test$DipsBest)
  ploygon(spline.poly(test,100))
}

plot(NA,xlim=c(0,15),ylim=c(0,15))
points(test,pch=19)
polygon(spline.poly(test,100),border="red",lwd=2)

# now loop through the data and calculate the ellipses
ngroups <- length(unique(scores$Taxonomy))
# split data based on group
collision <- split(scores$ColBest, scores$Taxonomy)
displacement <- split(scores$DispBest, scores$Taxonomy)
# create some empty vectors for recording our metrics
x <- numeric(ngroups)
y <- numeric(ngroups)
z <- numeric(ngroups)

colorgroup = c(rep("green",length(which(scores$Taxonomy==1))),
               rep("royalblue",length(which(scores$Taxonomy==2))),
               rep("skyblue",length(which(scores$Taxonomy==3))),
               rep("yellow",length(which(scores$Taxonomy==4))),
               rep("red",length(which(scores$Taxonomy==5))), 
               rep("black",length(which(scores$Taxonomy==6))),
               rep("purple",length(which(scores$Taxonomy==7))),
               rep("darkorange",length(which(scores$Taxonomy==8))),
               rep("yellowgreen",length(which(scores$Taxonomy==9))),
               rep("gray48",length(which(scores$Taxonomy==10))),
               rep("cyan",length(which(scores$Taxonomy==11))))

pchgroup = c(rep(16,length(which(scores$Taxonomy==1))),
             rep(16,length(which(scores$Taxonomy==2))),
             rep(16,length(which(scores$Taxonomy==3))),
             rep(16,length(which(scores$Taxonomy==4))),
             rep(16,length(which(scores$Taxonomy==5))), 
             rep(16,length(which(scores$Taxonomy==6))),
             rep(16,length(which(scores$Taxonomy==7))),
             rep(16,length(which(scores$Taxonomy==8))),
             rep(16,length(which(scores$Taxonomy==9))),
             rep(16,length(which(scores$Taxonomy==10))),
             rep(16,length(which(scores$Taxonomy==11))))		
			
			
plot(scores$ColBest,scores$DispBest,
     col=colorgroup,type="p",axes=F,pch=pchgroup, # cex=1.5, 
     xlab = "Collision Vulnerability", ylab = "Displacement Vulnerability") # ,xlim=c(-20,-12),ylim=c(14,22)
box()
# axis(1,at=seq(-20,-12,2),labels=T)
# axis(2,at=seq(14,22,2),labels=F)
# axis(2,at=seq(14,22,2),labels=T)

for (j in unique(scores$Taxonomy)){
  # Fit a standard ellipse to the data
  SE <- standard.ellipse(collision[[j]],displacement[[j]],steps=1)
  # Extract the estimated x and y from this object
  x[j] <- SE$x
  y[j] <- SE$y
  # plot the standard ellipse with d.f. = 2 (i.e. y)
  # These are plotted here as thick solid lines'
colorgroup = c('green','royalblue','skyblue','yellow','red','black', 'purple', 'darkorange', 'yellowgreen', 'gray48', 'cyan')
  lines(SE$y,SE$y,lty=1,lwd=3,col=colorgroup[j])
}  


library(ellipse)
library(ggplot2)
scores_hull <- data.frame()
for(i in levels(scores$Taxonomy)){
  scores_hull <- rbind(scores_hull, 
                       cbind(as.data.frame(with(scores[scores$Taxonomy==i, ], 
                                                ellipse(cor(ColBest, DispBest),
                                                        scale = c(sd(ColBest), sd(DispBest)),
                                                        centre = c(mean(ColBest), mean(DispBest)),
                                                        npoints = length(which(scores$Taxonomy == i))))),
                             group = i))
}

vulnHulls <- ggplot(data=scores, aes(x=ColBest, y=DispBest, colour=Taxonomy)) + 
  geom_point() +
  geom_path(data=scores_hull, aes(x=x, y=x, colour=group))
vulnHulls


