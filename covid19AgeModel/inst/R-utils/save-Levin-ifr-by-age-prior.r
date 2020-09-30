library(data.table)
library(dplyr)
library(ggplot2)
library(covid19AgeModel)

args = list()
args$indir = "~/git/R0t/covid19AgeModel/inst"

args$file_age_ifr <- file.path(args$indir,'data','ifr-by-age-prior_Levin_raw_200904.csv')

#
# Read and clean Levin et al. Supplementary material
ifr_lognormal = as.data.table(read.csv( args$file_age_ifr ))
tmp1 = select(ifr_lognormal, c("Age", "IFR", "Lower.Bound", "Upper.Bound"))
tmp1 = subset(tmp1, !Age %in% c("0-34", "95+"))
tmp1[, log_ifr_mean := log(IFR/100) ]
tmp1[, log_ifr_cl := log(Lower.Bound/100) ]
tmp1[, log_ifr_cu := log(Upper.Bound/100) ]
tmp1[, Age := as.numeric(Age)]
setnames(tmp1, "Age", "age")
tmp1 = select(tmp1, -c("IFR", "Lower.Bound", "Upper.Bound"))

#
# extrapolate straigh line
x = tmp1$age
b_M = (tmp1$log_ifr_mean[2] - tmp1$log_ifr_mean[1]) / (x[2] - x[1])
a_M = (x[2]*tmp1$log_ifr_mean[1] - x[1]*tmp1$log_ifr_mean[2]) / (x[2] - x[1])
b_CL = (tmp1$log_ifr_cl[2] - tmp1$log_ifr_cl[1]) / (x[2] - x[1])
a_CL = (x[2]*tmp1$log_ifr_cl[1] - x[1]*tmp1$log_ifr_cl[2]) / (x[2] - x[1])
b_CU = (tmp1$log_ifr_cu[2] - tmp1$log_ifr_cu[1]) / (x[2] - x[1])
a_CU = (x[2]*tmp1$log_ifr_cu[1] - x[1]*tmp1$log_ifr_cu[2]) / (x[2] - x[1])
age_missing = 0:34
tmp = data.table(age = age_missing, 
                 log_ifr_mean = a_M + b_M*age_missing, 
                 log_ifr_cl = a_CL + b_CL*age_missing, 
                 log_ifr_cu = a_CU + b_CU*age_missing)

tmp = rbind( tmp, tmp1 )

# 
# plot
if(0){
  ggplot(tmp, aes(x = age)) +
    geom_line(aes(y = log_ifr_mean)) +
    geom_ribbon(aes(ymin = log_ifr_cl, ymax = log_ifr_cu), alpha = 0.5)
}

#
# add raw scale
tmp[, ifr_mean := exp(log_ifr_mean)]
tmp[, ifr_cl := exp(log_ifr_cl)]
tmp[, ifr_cu := exp(log_ifr_cu)]

#
# Aggregate by 5 years age band
GFNAME_us_population = file.path(args$indir,"data","us_population_withnyc.rds")
ifr.by.age = as.data.table(read_ifr_data_by_age(path_to_file_pop_us = GFNAME_us_population, ifr.by.age = tmp, max_age = 95))

#
# Find hyperparameters log-normal distribution
library(lognorm)
ifr.by.age = ifr.by.age[, list(ln_mu = getParmsLognormForMeanAndUpper(mean = ifr_mean, quant = ifr_cu, sigmaFac=2)[1],
                                ln_sd = getParmsLognormForMeanAndUpper(mean = ifr_mean, quant = ifr_cu, sigmaFac=2)[2],
                                ifr_mean = ifr_mean,
                                ifr_cl = ifr_cl,
                                ifr_cu = ifr_cu), by = c("age")]
#
# Find hyperparameters beta distribution
library("prevalence")
ifr.by.age[, ifr_beta_alpha := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$alpha, by = c("age")]
ifr.by.age[, ifr_beta_beta := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$beta, by = c("age")]

#
# clean
setnames(ifr.by.age, "age", "age_cat")
ifr.by.age$age_cat_label = c(paste0(seq(0, 80, 5), "-", seq(4,84,5)), "85+")

#
# save
write.csv(ifr.by.age, file = file.path(args$indir, "data", 'ifr-by-age-prior_Levin_200904.csv'), row.names = F)

