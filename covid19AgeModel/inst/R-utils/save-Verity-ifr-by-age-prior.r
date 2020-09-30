library(covid19AgeModel)

args = list()
args$indir = "~/git/R0t/covid19AgeModel/inst"

args$file_age_ifr <- file.path(args$indir,'data','ifr-by-age-prior_Verity_raw_200429.csv')

ifr.by.age <- as_tibble(read.csv(args$file_age_ifr))
stopifnot( c('age','ifr_mean')%in%colnames(ifr.by.age) )
ifr.by.age <- ifr.by.age %>% 
  select(age, ifr_mean, ifr_cl, ifr_cu)

# 
# Fit smoth spline to find ifr by 1 year age band
library(splines)
x = seq(mean(c(0,9)),mean(c(80,89)),10)
y.mean = unique(ifr.by.age$ifr_mean)
y.cl = unique(ifr.by.age$ifr_cl)
y.cu = unique(ifr.by.age$ifr_cu)
fit.smooth.mean = smooth.spline(x = x, y = y.mean, cv = TRUE)
fit.smooth.cl = smooth.spline(x = x, y = y.cl, cv = TRUE)
fit.smooth.cu = smooth.spline(x = x, y = y.cu, cv = TRUE)
ifr.by.age$ifr_mean = predict(fit.smooth.mean, x = 0:100)$y
ifr.by.age$ifr_cl = predict(fit.smooth.cl, x = 0:100)$y
ifr.by.age$ifr_cu = predict(fit.smooth.cu, x = 0:100)$y + 2*predict(fit.smooth.mean, x = 0:100)$y

#
# Aggregate by 5 year age band
GFNAME_us_population = file.path(args$indir,"data","us_population_withnyc.rds")
ifr.by.age = as.data.table(read_ifr_data_by_age(path_to_file_pop_us = GFNAME_us_population, ifr.by.age = ifr.by.age, max_age = 95))

#
# Find hyperparameters log normal distribution
library(lognorm)
ifr.by.age[, ln_mu := getParmsLognormForLowerAndUpper(lower = ifr_cl, upper = ifr_cu, sigmaFac=2)[1], by = c("age")]
ifr.by.age[, ln_sd := getParmsLognormForLowerAndUpper(lower = ifr_cl, upper = ifr_cu, sigmaFac=2)[2], by = c("age")]


#
# Find hyperparameters beta distribution
library("prevalence")
ifr.by.age = as.data.table(ifr.by.age)
ifr.by.age[, ifr_beta_alpha := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$alpha, by = c("age")]
ifr.by.age[, ifr_beta_beta := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$beta, by = c("age")]

#
# clean
setnames(ifr.by.age, "age", "age_cat")
ifr.by.age$age_cat_label = c(paste0(seq(0, 80, 5), "-", seq(4,84,5)), "85+")

write.csv(ifr.by.age, file =file.path(args$indir,'data','ifr-by-age-prior_Verity_200624.csv'), row.names = F)
