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

# M2
map4.2 <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dunif(0, 10))

# M3
y ~ normal(mu, sigma)
mu <- a +b*x
a ~ normal(0, 50)
b ~ uniform(0, 10)
sigma ~ uniform(0, 50)

# M4
height ~ rnorm(mu, sigma)
mu <- a + b*year
a ~ rnorm(40, 5)
b ~ dunif(2, 3)
sigma ~ dunif(0, 10)

# M4
height ~ rnorm(mu, sigma)
mu <- a + b*year
a ~ rnorm(120, 5)
b ~ dunif(0, 5)
sigma ~ dunif(0, 10)

# M6
height ~ rnorm(mu, sigma)
mu <- a + b*year
a ~ rnorm(120, 5)
b ~ dunif(0, 5)
sigma ~ dunif(0, 64)

