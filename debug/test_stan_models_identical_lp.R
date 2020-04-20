# Install posteriordb to acces the model and data directly
# remotes::install_github("MansMeg/posteriordb", subdir = "rpackage")
library(posteriordb)
library(rstan)
po <- posterior_names()
po <- posterior("covid19imperial_v2-covid19imperial_v2")
sd1 <- stan_data(po)
sc <- stan_code(po)

# Reconstruct the data to general format
sd2 <- sd1
sd2$P <- 6
sd2$X <- array(c(sd2$covariate1, sd2$covariate2, sd2$covariate3, unname(as.matrix(sd2$covariate4)), sd2$covariate5, sd2$covariate6) , 
               dim = c( 90 , 14 , 6 ))
sd2$X <- aperm(sd2$X, c(2,1,3))
sd2$covariate1 <- 
  sd2$covariate2 <-
  sd2$covariate3 <-
  sd2$covariate4 <-
  sd2$covariate5 <-
  sd2$covariate6 <- NULL

# Setup models to compute lp__
sm1 <- rstan::stan(file = "stan-models/base.stan", data = sd1, chains = 0)
sm2 <- rstan::stan(file = "stan-models/base_general.stan", data = sd2, chains = 0)
sm3 <- rstan::stan(file = "stan-models/base_general_speed.stan", data = sd2, chains = 0)
sm4 <- rstan::stan(file = "stan-models/base_general_speed2.stan", data = sd2, chains = 0)

# Mean parameter from previous runs to use as test
test_params <- list(mu = c(3.83, 3.47, 4.65, 4.48, 3.97, 4.69, 3.49, 5.76, 4.01, 3.07, 3.53, 2.28, 3.45, 3.47),
                    alpha_hier = c(0.32, 0.03, 0.07, 0.02, 1.06, 0.04),
                    kappa = 1.001,
                    y = c(51, 58, 18, 83, 46, 12, 40, 22, 76, 55, 38, 88, 81, 106),
                    phi = 7.45,
                    tau = 56,
                    ifr_noise = c(0.99, 0.99, 0.98, 0.99, 0.99, 0.99, 0.99, 1, 1, 1, 0.99, 1, 1, 1))
upars <- unconstrain_pars(sm1, test_params)

# Assert that the log_prob is more or less identical
log_prob(sm1, upars)
log_prob(sm2, upars)
log_prob(sm3, upars)
log_prob(sm4, upars)

speed3 <- rstan::stan(file = "stan-models/base_general_speed.stan", data = sd2, chains = 1, warmup = 100, iter = 200, seed = 4711)
speed4 <- rstan::stan(file = "stan-models/base_general_speed2.stan", data = sd2, chains = 1, warmup = 100, iter = 200, seed = 4711)
