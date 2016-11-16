
rm(list = ls())
library(devtools)
library(siar)
library(SIBER)

graphics.off()

setwd("~/WERC-SC/Vuln_Index")
scores <- read.csv("PV.CV.DVscores.csv", header = T) ## matrix of final PV, CV, and DV
siber <- createSiberObject(scores)

#
# Splining a polygon.
#
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an array of points. 
# 
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

# print the area metrics to screen for comparison
# NB if you are working with real data rather than simulated then you wont be
# able to calculate the population SEA (pop.SEA)
# If you do this enough times or for enough groups you will easily see the
# bias in SEA as an estimate of pop.SEA as compared to SEAc which is unbiased.
# Both measures are equally variable.
print(cbind(x,y))

# So far we have fitted the standard ellipses based on frequentist methods
# and calculated the relevant metrics (SEA and SEAc). Now we turn our attention
# to producing a Bayesian estimate of the standard ellipse and its area SEA_B

reps <- 10^4 # the number of posterior draws to make

# Generate the Bayesian estimates for the SEA for each group using the 
# utility function siber.ellipses
SEA.B <- siber.ellipses(scores$ColBest, scores$DispBest, scores$Taxonomy, R = reps)

# Write the bayesian estimates to a .csv file
  # I need R to write the .csv file with the names of the species, instead of V1, V2, etc...
  SEA.B<- data.frame(SEA.B)
  write.csv(SEA.B, file = "vuln_chull.csv") 

siardensityplot(SEA.B,
                xlab="Group",ylab="Area (permil^2)",
                main="Different estimates of Standard Ellipse Area (SEA)")
 
points(1:ngroups, SEAc, pch = 15, col = "red")
legend("topright", c("SEAc"),
       pch = c(15, 17), col = c("red", "blue"))
 
}

# ------------------------------------------------------------------------------
# Compare two ellipses for significant differences in SEA
# ------------------------------------------------------------------------------

# to test whether Group 1 SEA is smaller than Group 2...
# you need to calculate the proportion of G1 ellipses that are less 
# than G2, G3, G4, G5, and G6

Pg1.lt.g2 <- sum( SEA.B[,1] < SEA.B[,2] ) / nrow(SEA.B)

Pg1.lt.g3 <- sum( SEA.B[,1] < SEA.B[,3] ) / nrow(SEA.B)

Pg1.lt.g4 <- sum( SEA.B[,1] < SEA.B[,4] ) / nrow(SEA.B)

Pg1.lt.g5 <- sum( SEA.B[,1] < SEA.B[,5] ) / nrow(SEA.B)

Pg1.lt.g6 <- sum( SEA.B[,1] < SEA.B[,6] ) / nrow(SEA.B)

# In this case, all the posterior ellipses for G1 are less than G2 so 
# we can conclude that G1 is smaller than G2 with p approx = 0, and 
# certainly p < 0.0001.

# An alternative approach is to calulate the effect size. That is, the 
# difference in size between the two ellipses. If this difference is close 
# to zero, then there is little difference, and if it is far away from zero 
# then the effect size is large. Again, this difference is a distribution.

diff.g1.g2 <- SEA.B[,1] - SEA.B[,2]
diff.g3.g4 <- SEA.B[,3] - SEA.B[,4]

# plot a histogram of this difference
hist(diff.g1.g2, 50)
abline(v = 0, col = 'red') # add a vertical line at zero


# ------------------------------------------------------------------------------
# To calculate the overlap between two ellipses you can use the following code
# NB: the degree of overlap is sensitive to the size of ellipse you 
# choose to draw around each group of data. However, regardless of the choice
# of ellipse, the extent of overlap will range from 0 to 1, with values closer
# to 1 representing more overlap. So, at worst it is a semi-quantitative 
# measure regardless of extent of the ellipse, but the finer detials and 
# magnitudes of the effect size will be sensitive to this choice.
#
# Additional coding will be required if you wish to calculate the overlap 
# between ellipses other than those described by SEA or SEAc. 
# ------------------------------------------------------------------------------

# The overlap between the SEAc for groups 1 and 2 is given by:

# Fit a standard ellipse to the data
# NB, I use a small step size to make sure i get more "round" ellipses,
# as this method is computatonal and based on the discretisation of the
# ellipse boundaries.

overlap.G1.G2 <- overlap(spx[[1]],spy[[1]],spx[[2]],spy[[2]],steps=1)

overlap.G1.G3 <- overlap(spx[[1]],spy[[1]],spx[[3]],spy[[3]],steps=1)

overlap.G2.G3 <- overlap(spx[[2]],spy[[2]],spx[[3]],spy[[3]],steps=1)

overlap.G1.G4 <- overlap(spx[[1]],spy[[1]],spx[[4]],spy[[4]],steps=1)

overlap.G1.G5 <- overlap(spx[[1]],spy[[1]],spx[[5]],spy[[5]],steps=1)

overlap.G2.G6 <- overlap(spx[[2]],spy[[2]],spx[[6]],spy[[6]],steps=1)

#-------------------------------------------------------------------------------
# you can also cacluate the overlap between two of the convex hulls,
# or indeed any polygon using the code that underlies the overlap() function.

# fit a hull to the Group 1 data
hullG1 <- convexhull(spx[[1]],spy[[1]])

# create a list object of the unique xy coordinates of the hull
# the first and last entries are coincident for plotting, so ignore the first...
# hence the code to subset [2:length(hullG1$xcoords)] 
h1 <- list( x = hullG1$xcoords[2:length(hullG1$xcoords)] , y = hullG1$ycoords[2:length(hullG1$xcoords)] )

# Do the same for the Group 2 data
hullG2 <- convexhull(spx[[2]],spy[[2]])
h2 <- list( x = hullG2$xcoords[2:length(hullG2$xcoords)] , y = hullG2$ycoords[2:length(hullG2$xcoords)] )

# and calculate the overlap using the function in spatstat package.
hull.overlap.G1.G2 <- overlap.xypolygon(h1,h2)
