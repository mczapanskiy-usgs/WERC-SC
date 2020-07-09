#### STATISTICAL RETHINKING
## practice problems

# library(rstan)
# install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking")
# library(rethinking)

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
pkgbuild::has_build_tools(debug = TRUE)

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
      if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7" else
        "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)

install.packages(c('coda','mvtnorm'))
options(repos=c(getOption('repos'),rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')

# To double check that it works:
library(rethinking)
help(package=rethinking)

### ch. 2
## GRID APPROXIMATION
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 1000)

#define prior
prior <- rep(1,1000)
# prior <- ifelse(p_grid >= 0.5, 0, 1)
# prior <- exp(-5*abs(p_grid - 0.5))


#compute likelihood at each value of grid
likelihood <- dbinom(8, size = 15, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(posterior)

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# plot(p_grid, posterior, type = "b",
#      xlab = "probability of water", ylab = "posterior probability")
#        mtext("7 points")

#---------------------------------------------------------------------
### ch. 3
## LINEAR MODELS
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 10000)
#define prior
prior <- rep(1,1000)
#compute likelihood at each value of grid
likelihood <- dbinom(8, size = 15, prob = p_grid)
# compute product of likelihood and prior
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(posterior)
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

# 3E1
sum(samples < 0.2)/1e4
# 5e-04

# 3E2
sum(samples > 0.8)/1e4
# 0.117

# 3E3
sum(samples > 0.2 & samples < 0.8)/1e4
# 0.8878

# 3E4
quantile(samples, c(.2))
# 0.5195195

# 3E5
quantile(samples, c(.8))
# 0.756768

# 3E6
HPDI(samples, prob = 0.66)
# 0.52 - 0.78

# 3E7
PI(samples, prob = 0.66)
# 17% = 0.50, 83% = 0.77

# 3M1
plot(p_grid, posterior, type = "l",
     xlab = "probability of water", ylab = "posterior probability")
mtext("3M1")
 
# 3M2
HPDI(samples, prob = 0.9)
# 0.33 - 0.722

# 3M3
w <- rbinom(1e4, size = 15, prob = samples)
sum(w==8)/1e4
simplehist(w)
# 0.14

# 3M4
w <- rbinom(1e4, size = 9, prob = samples)
sum(w==6)/1e4
simplehist(w)
# 0.167

# 3M5
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(posterior)
posterior <- unstd.posterior/sum(unstd.posterior)
plot(p_grid, posterior, type = "l",
     xlab = "probability of water", ylab = "posterior probability")
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.9)
w <- rbinom(1e4, size = 15, prob = samples)
sum(w==8)/1e4
simplehist(w)

# 3H1
data(homeworkch3)
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 10000)
#define prior
prior <- rep(1, length(p_grid))
#compute likelihood at each value of grid
boys <- sum(birth1) + sum(birth2)
likelihood <- dbinom(boys, size = 200, prob = p_grid)
# compute product of likelihood and prior
posterior <- likelihood*prior
# standardize the posterior, so it sums to 1
posterior <- posterior/sum(posterior)

plot(posterior ~ p_grid, type="l")
p_grid[which.max(posterior)]
# 0.56

# 3H2
p.samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(p.samples, prob = 0.50)
# 0.53 - 0.58
HPDI(p.samples, prob = 0.89)
# 0.49 - 0.60
HPDI(p.samples, prob = 0.97)
# 0.48 - 0.63

# 3H3
bsim <- rbinom(10000, size = 200, prob = p.samples)
dens(bsim, adj = 0.1)
abline(v = sum(birth1) + sum(birth2), col="red")

# 3H4
b1sim <- rbinom(10000, size = 100, prob = p.samples)
dens(b1sim, adj = 0.1)
abline(v = sum(birth1), col="red")

# 3H5
b01 <- birth2[birth1==0]
b01sim <- rbinom(10000, size = length(b01), prob = p.samples)
dens(b01sim, adj = 0.1)
abline(v=sum(b01), col = "red")


#---------------------------------------------------------------------
### ch. 3
## LINEAR MODELS

# M1
mu <- rnorm(1000, 0, 10)
sigma <- runif(1000, 0, 10)
sim <- rnorm(1000, mu, sigma)
dens(sim)

# H1
data(Howell1)
d <- Howell1
d2 <- d[d$age>=18, ]
xbar <- mean(d2$weight)
weight.vec <-  c(46.95, 43.72, 64.78, 32.59, 54.63)

m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar), 
    a ~ dnorm(150, 30),
    b ~ dnorm(0, 20),
    sigma ~ dunif(0, 50)
    ), data = d2,
  )

precis(m1)

post <- extract.samples(object = m1)

height.pred.mat <- matrix(NA, nrow = nrow(post),
                          ncol = length(weight.vec))

for(i in 1:length(weight.vec)) {
  height.pred.mat[ ,1] <- post$a + post$b * (weight.vec[i]-xbar)
}

head(height.pred.mat)

apply(height.pred.mat, 2, mean)
apply(height.pred.mat, 2, PI)

pred.m1 <- link(fit = m1,
                data = data.frame(weight.vec))

head(pred.m1)
dim(pred.m1)
apply(pred.m1, 2, mean)
apply(pred.m1, 2, PI)


# H2
d3 <- d[d$age < 18, ]
xbar3 <- mean(d3$weight)
weight.seq <- seq(min(d3$weight),
                  max(d3$weight),
                  by = 1)

m3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar3), 
    a ~ dnorm(100, 30),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)),
  data = d3,
  )

precis(m3)

plot(height ~ weight, data = d3)

pred.m3 <- link(fit = m3,
                data = data.frame(weight = weight.seq))

mu.pred.m3 <- apply(pred.m3, 2, mean)
HPDI.pred.m3 <- apply(pred.m3, 2, HPDI)

ind.pred.m3 <- sim(fit = m3, 
                   data = data.frame(weight = weight.seq))

mu.ind.pred.m3 <- apply(ind.pred.m3, 2, mean)
HPDI.ind.pred.m3 <- apply(ind.pred.m3, 2, HPDI)

plot(height ~ weight, data = d3)
lines(weight.seq, mu.pred.m3)
shade(HPDI.pred.m3, weight.seq, col = col.alpha(rangi2))
shade(HPDI.ind.pred.m3, weight.seq, col = col.alpha(rangi2))


# M3
d$log_weight <- log(d$weight)
log_xbar <- mean(d$log_weight)

m4 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (log_weight - log_xbar), 
    a ~ dnorm(150, 30),
    b ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ), data = d,
)

precis(m4)

weight.seq <- seq(min(d$weight), max(d$weight), length = 100)
log_weight.seq <- log(weight.seq)
pred.m4 <- link(m4, data = data.frame(log_weight = log.weight.seq))

mu.pred.m4 <- apply(pred.m4, 2, mean)
HPDI.pred.m4 <- apply(pred.m4, 2, HPDI, prob = 0.97)

ind.pred.m4 <- sim(fit = m4, 
                   data = data.frame(weight = weight.seq))

mu.ind.pred.m4 <- apply(ind.pred.m4, 2, mean)
HPDI.ind.pred.m4 <- apply(ind.pred.m4, 2, HPDI, prob = 0.97)



plot(height ~ weight, data = d,
     col = col.alpha(rangi2, 0.4))
lines(weight.seq, mu.pred.m4, lwd = 3)
shade(HPDI.pred.m4, weight.seq, col = col.alpha(rangi2))
shade(HPDI.ind.pred.m4, weight.seq, col = col.alpha(rangi2))


#---------------------------------------------------------------------
### ch 5
# M4
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )

d_std <- d %>% 
  mutate(
    Marriage.s <- standardize(Marriage),
    MedianAgeMarriage.s <- standardize(MedianAgeMarriage),
    pct_LDS.s <- standardize(pct_LDS),
    Divorce.s <- standardize(Divorce)
  )

mormon <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b1*Marriage + b2*MedianAgeMarriage + b3*pct_LDS,
    sigma ~ dunif(0, 50),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    a ~ dnorm(50, 50)
  ), data = d)

precis(mormon)

#H1
data(foxes)

fox.1 <- map(alist(
  weight <- dnorm(mu, sigma),
  mu <- a + b1*area,
  a ~ dnorm(0, 10),
  b1 ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data = foxes)

fox.2 <- map(alist(
  weight <- dnorm(mu, sigma),
  mu <- a + b2*groupsize,
  a ~ dnorm(0, 10),
  b2 ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data = foxes)

precis(fox.1)
precis(fox.2)

x <- seq(0, 8, 1) # setting range of x-axis

# area
fox.1.line <- link(fox.1, data = data.frame(area = x))
line.mean <- apply(fox.1.line, 2, mean)
line.ci <- apply(fox.1.line, 2, PI)
plot(weight ~ area, data = foxes)
line(x, line.mean)
lines(x, line.ci[1,])
lines(x, line.ci[2,])

# group size
fox.2.line <- link(fox.2, data = data.frame(area = x))
line.mean <- apply(fox.2.line, 2, mean)
line.ci <- apply(fox.2.line, 2, PI)
plot(weight ~ groupsize, data = foxes)
lines(x, line.mean)
lines(x, line.ci[1,])
lines(x, line.ci[2,])

#H1
fox.3 <- map(alist(
  weight <- dnorm(mu, sigma),
  mu <- a + b1*area + b2*groupsize,
  a ~ dnorm(0, 10),
  b1 ~ dnorm(0, 10),
  b2 ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data = foxes)

precis(fox.3)

x.area <- seq(0, max(foxes$area), 1) # setting range of x-axis
x.groupsize <- seq(0, max(foxes$groupsize), 1) # setting range of x-axis

fox.3.line <- link(fox.3, data = data.frame(groupsize = x.groupsize, area = mean(foxes$area)))
line.mean <- apply(fox.3.line, 2, mean)
line.ci <- apply(fox.3.line, 2, PI)
plot(weight ~ groupsize, data = foxes)
lines(x.groupsize, line.mean)
lines(x.groupsize, line.ci[1,])
lines(x.groupsize, line.ci[2,])

fox.3.1.line <- link(fox.3, data = data.frame(area = x.area, groupsize = mean(foxes$groupsize)))
line.mean <- apply(fox.3.1.line, 2, mean)
line.ci <- apply(fox.3.1.line, 2, PI)
plot(weight ~ area, data = foxes)
lines(x.area, line.mean)
lines(x.area, line.ci[1,])
lines(x.area, line.ci[2,])

# H3
fox.4 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bG*groupsize + bF*avgfood,
    a ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bF ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = foxes)

fox.5 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area + bG*groupsize + bF*avgfood,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bF ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = foxes)

precis(fox.4)
precis(fox.5)

fox.4.line <- link(fox.4, data = data.frame(groupsize = x.groupsize, area = mean(foxes$area)))
line.mean <- apply(fox.4.line, 2, mean)
line.ci <- apply(fox.4.line, 2, PI)
plot(weight ~ groupsize, data = foxes)
lines(x.groupsize, line.mean)
lines(x.groupsize, line.ci[1,])
lines(x.groupsize, line.ci[2,])

x.food <- seq(0, max(foxes$avgfood), 0.1)
fox.5.line <- link(fox.5, data = data.frame(avgfood = x.food, area = mean(foxes$area), groupsize = mean(foxes$groupsize)))
line.mean <- apply(fox.5.line, 2, mean)
line.ci <- apply(fox.5.line, 2, PI)
plot(weight ~ avgfood, data = foxes)
lines(x.food, line.mean)
lines(x.food, line.ci[1,])
lines(x.food, line.ci[2,])


#---------------------------------------------------------------------
### ch 6
# E2
p2 <- c(0.3, 0.7)
-sum(p2*log(p2))

# E3
p3 <- c(0.2, 0.25, 0.3, 0.25)
-sum(p3*log(p3))

# E4
p4 <- c(0.33, 0.33, 0.33)
-sum(p4*log(p4))


library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed(1000)
i <- sample(1:nrow(d), size = nrow(d)/2)
d1 <- d[i, ]
d2 <- d[-i, ]


f1 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age,
  c(a,b1) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

f2 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age + b2*age^2,
  c(a,b1,b2) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

f3 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3,
  c(a,b1,b2,b3) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

f4 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4,
  c(a,b1,b2,b3,b4) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

f5 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5,
  c(a,b1,b2,b3,b4,b5) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

f6 <- alist(
  height ~ dnorm(mu,sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 +
    b6*age^6,
  c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0,100),
  sigma ~ dunif(0,50))

a.start <- mean(d1$height)
sigma.start <- sd(d1$height)

# fit models 5.7
m1 <- map(f1, data=d1,
           start=list(a=a.start,sigma=sigma.start,b1=0))

m2 <- map(f2, data=d1,
           start=list(a=a.start,sigma=sigma.start, b1=0, b2=0))

m3 <- map(f3, data=d1,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0))

m4 <- map(f4, data=d1,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0))

m5 <- map(f5, data=d1,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0,b5=0))

m6 <- map(f6, data=d1,
           start=list(a=a.start,sigma=sigma.start,b1=0,b2=0,
                      b3=0,b4=0,b5=0,b6=0))

compare(m1, m2, m3, m4, m5, m6)


# 6H2
# select out model to plot
m_plot <- m4
# define sequence of ages to compute values over
age.seq <- seq(from=-2, to=3, length.out=30)
# compute posterior predictions for mu
mu <- link(m_plot, data=list(age=age.seq))
# compute average prediction
mu.mean <- apply(mu, 2, mean)

# compute 97% interval of average
mu.ci <- apply(mu, 2, PI, prob=0.97)
# compute interval of height
h <- sim(m_plot, data=list(age=age.seq)) # function = "sim" b/c its trying to simulate data
height.ci <- apply(h, 2, PI)
# plot it all
plot(height ~ age, d1, col="slateblue", xlim=c(-2,3))
lines(age.seq, mu.mean)
shade(mu.ci, age.seq)
shade(height.ci, age.seq)

# 6H3
h.ensemble <- ensemble( m4,m5,m6 , data=list(age=age.seq) )
mu.mean <- apply( h.ensemble$link , 2 , mean )
mu.ci <- apply( h.ensemble$link , 2 , PI )
height.ci <- apply( h.ensemble$sim , 2 , PI )
plot( height ~ age , d1 , col="slateblue" , xlim=c(-2,3) )
lines( age.seq , mu.mean )
shade( mu.ci , age.seq )
shade( height.ci , age.seq )


# 6H4
k <- coef(m1)
mu <- k['a'] + k['b1']*d2$age
dev.m1 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )
k <- coef(m2)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2
dev.m2 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )
k <- coef(m3)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2 + k['b3']*d2$age^3
dev.m3 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )
k <- coef(m4)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2 + k['b3']*d2$age^3 +
  k['b4']*d2$age^4
dev.m4 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )
k <- coef(m5)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2 + k['b3']*d2$age^3 +
  k['b4']*d2$age^4 + k['b5']*d2$age^5
dev.m5 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )
k <- coef(m6)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2 + k['b3']*d2$age^3 +
  k['b4']*d2$age^4 + k['b5']*d2$age^5 + k['b6']*d2$age^6
dev.m6 <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )


# 6H5
compare.tab <- compare(m1,m2,m3,m4,m5,m6,sort=FALSE) 
tab <- data.frame(
  WAIC=compare.tab@output$WAIC,
  dev_out=c(dev.m1,dev.m2,dev.m3,dev.m4,dev.m5,dev.m6)
)
rownames(tab) <- rownames(compare.tab@output)
# display, sorted by dev out
tab[ order(tab$dev_out) , ]


# 6H6
f <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  c(b1, b2, b3, b4, b5, b6) ~ dnorm(0, 5))

m <- map(f, data = d1,
         start = list(a = mean(d1$height),
                      b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
                      sigma = sd(d1$height)))

mpredict <- link(m, data = data.frame(age = age.seq))
mu.mean <- apply(mpredict, 2, mean)
mu.pi <- apply(mpredict, 2, pi)
heightout <- sim(m, data = data.frame(age = age.seq))
height.pi <- apply(m)

plot(d1$age, d1$height)
lines(mu.mean, age.seq)
shade(mu.pi, age.seq)

k <- coef(m)
mu <- k['a'] + k['b1']*d2$age + k['b2']*d2$age^2 + k['b3']*d2$age^3 +
  k['b4']*d2$age^4 + k['b5']*d2$age^5 + k['b6']*d2$age^6

dev.m <- (-2)*sum( dnorm( d2$height , mu , k['sigma'] , log=TRUE ) )

WAIC(m)

#------------------------------------------------------------------------
### chapter 7
# 7H1
data("tulips")
d <- tulips
d$blooms_std <- d$blooms/max(d$blooms)
d$shade_cent <- d$shade - mean(d$shade)
d$water_cent <- d$water - mean(d$water)

m7H1 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a[bed] + bW*water_cent + bS*shade_cent + bSW*shade_cent*water_cent,
    a[bed] ~ dnorm(0.5, 0.25),
    bW ~ dnorm(0, 0.25),
    bS ~ dnorm(0, 0.25),
    bSW ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m7H1, depth = 2)

# 7H2
m7H2 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bW*water_cent + bS*shade_cent + bSW*shade_cent*water_cent,
    a ~ dnorm(0.5, 0.25),
    bW ~ dnorm(0, 0.25),
    bS ~ dnorm(0, 0.25),
    bSW ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m7H2, depth = 2)

compare(m7H1, m7H2)

# 6H3 
data(rugged)
r <- rugged
rug <- r[ complete.cases(r$rgdppc_2000) , ]
rug$logrgdppc_2000 <- log(rug$rgdppc_2000)
rug2 <- rug[ rug$country!="Seychelles" , ]


  

f7h3.1 <- map(
  alist(
    logrgdppc_2000 ~ dnorm( mu , sigma ) ,
    mu <- a + bA*cont_africa + br*rugged +
      bAr*cont_africa*rugged,
    a ~ dnorm(5, 10),
    c(bA,br,bAr) ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) ,
  data=rug2 )



rug2 <- rug[!rug$country==c("Seychelles"),]


f7h3.2 <- map(
  alist(
    logrgdppc_2000 ~ dnorm(mu, sigma),
    mu <- a + bC*cont_africa + bR*rugged + bCR*cont_africa*rugged,
    a ~ dnorm(5, 10),
    bC ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    bCR ~ dnorm(0, 10)
  ), data = rug2,
)

mu.rug <- link(f7h3.2, data = data.frame(rugged = seq(0,7, length = 30), cont_africa = 1))
mu.rug <- link(f7h3.2, data = data.frame(rugged = seq(0,7, length = 30), cont_africa = 0))

plot(rug$rugged, rug$logrgdppc_2000)
str(mu.rug1)

mu.rug1.mean <- apply(mu.rug1, 2, mean)
mu.rug0.mean <- apply(mu.rug0, 2, mean)

lines(rugged.seq, mu.rug1.mean)
lines(rugged.seq, mu.rug0.mean)


# 6H4
data("nettle")
d2 <- nettle
head(d2)
d2$langCap <- d2$num.lang/d2$k.pop
d2$loglang <- log(d2$langCap)
d2$logarea <- log(d2$area)


fH4.1 <- map(
  alist(
    loglang ~ dnorm(mu, theta),
    mu <- a + bA*logarea + bG*mean.growing.season,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bG ~ dnorm(0, 10),
    theta ~ dunif(0,10)
  ), data = d2
)
precis(fH4.1)

fH4.2 <- map(
  alist(
    loglang ~ dnorm(mu, theta),
    mu <- a + bA*logarea + bG*sd.growing.season,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bG ~ dnorm(0, 10),
    theta ~ dunif(0,10)
  ), data = d2
)
precis(fH4.2)

fH4.3 <- map(
  alist(
    loglang ~ dnorm(mu, theta),
    mu <- a + bA*logarea + bS*sd.growing.season + bG*mean.growing.season + bGS*sd.growing.season*mean.growing.season,
    a ~ dnorm(0,10), 
    bA ~ dnorm(0,10),
    bS ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bGS ~ dnorm(0, 10),
    theta ~ dunif(0,10)
  ), data = d2
)

precis(fH4.2)
compare(fH4.1, fH4.2, fH4.3)
plot(coeftab(fH4.1, fH4.2, fH4.3))

#------------------------------------------------------------------------
### chapter 8
# 8M1
data(rugged)
d <- rugged %>% 
  mutate(log_gdp = log(rgdppc_2000))
dd <- d[complete.cases(d$rgdppc_2000), ]

dd <- dd %>% 
  mutate(log_gdp_std = log_gdp/mean(log_gdp),
         rugged_std = rugged/max(rugged))

dat_slim <- dd %>% 
  select(log_gdp, rugged_std, cont_africa)

m1_unif <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data = dat_slim , chains = 2)

m1_exp <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ),
  data = dat_slim , chains = 2)

precis(m1_unif)
precis(m1_exp)

plot(m1_unif)
plot(m1_exp)

#8M1
m2_cau1 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ),
  data = dat_slim , chains = 2)

m2_cau2 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ),
  data = dat_slim , chains = 2)

m2_cau0.5 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,0.5)
  ),
  data = dat_slim , chains = 2)

m1_exp1 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ),
  data = dat_slim , chains = 2)

m1_exp0.5 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(0.5)
  ),
  data = dat_slim , chains = 2)

m1_exp0.25 <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged_std + bA*cont_africa + bAR*rugged_std*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(0.25)
  ),
  data = dat_slim , chains = 2)

precis(m1_exp1)
precis(m1_exp0.5)
precis(m1_exp0.25)

precis(m2_cau0.5)
precis(m2_cau1)
precis(m2_cau2)


plot(m1_exp1)
plot(m1_exp0.5)
plot(m1_exp0.25)

plot(m2_cau0.5)
plot(m2_cau1)
plot(m2_cau2)


#------------------------------------------------------
### CH 10

data(chimpanzees)
dchimp <- chimpanzees

h10.quad <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ), data = dchimp)

h10.mcmc <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ), data = dchimp, chains = 2, iter = 2500, warmup = 500)


plot(coeftab(h10.mcmc, h10.quad))

h10.mcmc.simp <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + (bp + bpC*condition)*prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ), data = dchimp, chains = 2, iter = 2500, warmup = 500)

compare(h10.mcmc, h10.mcmc.simp)

library(MASS)
data(eagles)
head(eagles)

eagles$P1 <- ifelse(eagles$P == "L", 1, 0)
eagles$A1 <- ifelse(eagles$A == "A", 1, 0)
eagles$V1 <- ifelse(eagles$V == "L", 1, 0)


h10.eagles <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bp*P1 + bv*V1 + ba*A1,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 5),
    bv ~ dnorm(0, 5), 
    ba ~ dnorm(0, 5)
  ), data = eagles, chains = 2, iter = 2500, warmup = 500
)

post.stan <- extract.samples(h10.eagles)

pred.eg <- link(h10.eagles, post = post.stan, data = eagles)

pred.mu <- apply(pred.eg, 2, mean)
pred.PI <- apply(pred.eg, 2, PI, prob = 0.89)


#10H4
data("salamanders")
head(salamanders)
sala <- salamanders

stdz <- function(x) (x-mean(x))/sd(x)
sala$PCTCOVERstd <- stdz(sala$PCTCOVER)
sala$FORESTAGEstd <- stdz(sala$FORESTAGE)

h10salam.1 <- map(
  alist(
    SALAMAN ~ dpois(lambda),
    log(lambda) <- a + bc*PCTCOVERstd,
    a ~ dnorm(0, 1),
    bc ~ dnorm(0, 1)), 
  data = sala)

h10salam.2 <- map2stan(
  alist(
    SALAMAN ~ dpois(lambda),
    log(lambda) <- a + bc*PCTCOVERstd,
    a ~ dnorm(0, 1),
    bc ~ dnorm(0, 1)), 
  data = sala, chains = 2, iter = 2500, warmup = 500)


#------------------------------------------------------
### CH 11
library(dplyr)
data("Hurricanes")

#11H1
m1 <- quap(
  alist(
    deaths ~ dpois(lambda),
    log(lambda) <- a + dF*femininity,
    a ~ dnorm(0, 10),
    dF ~ dnorm(0, 10)
  ), Hurricanes
)

precis(m1, digits = 3)
plot(precis(m1))

pred.link <- link(m1)

deadly <- Hurricanes %>% 
  filter(deaths >= 50)


#11H2
m2 <- map2stan(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) <- a + dF*femininity,
    a ~ dnorm(0, 10),
    dF ~ dnorm(0, 10),
    theta ~ dunif(0, 10)
  ), data = Hurricanes, chains = 4, log_lik = TRUE)

apply(data, 2, anyNA)





