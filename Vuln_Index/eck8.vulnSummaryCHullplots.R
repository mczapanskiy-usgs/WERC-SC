
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
spline.poly <- function(xy, vertices, k=2, ...) {
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
  test <- filter(scores, Taxonomy == 1)%>% 
    select(ColBest, DispBest) 
  chuld <- lapply(test, "[", chull(test))
  
  plot(NA,xlim=c(0,15),ylim=c(0,15))
  points(test,pch=19)
  polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border="red",lwd=2)
}

gulls <- filter(scores, Taxonomy == "Gulls")%>% 
  select(ColBest, DispBest) 
cgull <- lapply(gulls, "[", chull(gulls))

terns <- filter(scores, Taxonomy == "Terns")%>% 
  select(ColBest, DispBest) 
cterns <- lapply(terns, "[", chull(terns))

jaeg <- filter(scores, Taxonomy == "Jaegers & Skuas")%>% 
  select(ColBest, DispBest) 
cjaeg <- lapply(jaeg, "[", chull(jaeg))

alcids <- filter(scores, Taxonomy == "Alcids")%>% 
  select(ColBest, DispBest) 
calcids <- lapply(alcids, "[", chull(alcids))

corms <- filter(scores, Taxonomy == "Cormorants")%>% 
  select(ColBest, DispBest) 
ccorms <- lapply(corms, "[", chull(corms))

proc <- filter(scores, Taxonomy == "Procellariids")%>% 
  select(ColBest, DispBest) 
cproc <- lapply(proc, "[", chull(proc))

duck <- filter(scores, Taxonomy == "Sea Ducks")%>% 
  select(ColBest, DispBest) 
cduck <- lapply(duck, "[", chull(duck))

loon <- filter(scores, Taxonomy == "Loons")%>% 
  select(ColBest, DispBest) 
cloon <- lapply(loon, "[", chull(loon))

phal <- filter(scores, Taxonomy == "Phalaropes")%>% 
  select(ColBest, DispBest) 
cphal <- lapply(phal, "[", chull(phal))

pelican <- filter(scores, Taxonomy == "Pelicans")%>% 
  select(ColBest, DispBest) 
cpelican <- lapply(pelican, "[", chull(pelican))

grebe <- filter(scores, Taxonomy == "Grebes")%>% 
  select(ColBest, DispBest) 
cgrebe <- lapply(grebe, "[", chull(grebe))


plot(NA,xlim=c(0,20),ylim=c(0,20), xlab = "Collision Vulnerability", ylab = "Displacement Vulnerability")
points(gulls,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cgull)),100),border="red",lwd=2)

points(terns,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cterns)),100),border="green",lwd=2)

points(jaeg,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cjaeg)),100),border="royalblue",lwd=2)

points(alcids,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(calcids)),100),border="skyblue",lwd=2)

points(corms,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(ccorms)),100),border="yellow",lwd=2)

points(proc,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cproc)),100),border="red",lwd=2)

points(duck,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cduck)),100),border="black",lwd=2)

points(loon,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cloon)),100),border="darkorange",lwd=2)

points(phal,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cphal)),100),border="darkviolet",lwd=2)

points(pelican,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cpelican)),100),border="gray48",lwd=2)

points(grebe,pch=19)
polygon(spline.poly(as.matrix(as.data.frame(cgrebe)),100),border="magenta",lwd=2)


