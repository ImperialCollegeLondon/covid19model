library(data.table)

args$indir = "~/git/R0t/covid19AgeModel/inst"

ifr.by.age = data.table(age_cat = 1:18, 
                  age_cat_label = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
                  ifr_mean = c(10^(-4), 0.01, 0.01, 0.02, 0.02, 0.04, 0.06, 0.09, 0.15, 0.23, 0.36, 0.57, 0.89, 1.39, 2.17, 3.39, 5.3, (1/3*8.28+2/3*16.19))/100,
                  ifr_cl = c(0,0,0,0,0,0,0.01, 0.01, 0.02, 0.03, 0.05, 0.1, 0.18, 0.34, 0.64, 1.19, 2.19, (1/3*3.98+2/3*9.44))/100,
                  ifr_cu = c(0.03, 0.06, 0.1, 0.17, 0.28, 0.44, 0.67, 0.98, 1.37, 1.88, 2.52, 3.32, 4.34, 5.64, 7.35, 9.65, 12.81, (1/3*17.25 + 2/3* 27.78))/100)

#
# Find hyperparameters log-normal distribution
library(lognorm)
ifr.by.age = ifr.by.age[, list(ln_mu = getParmsLognormForMedianAndUpper(median = ifr_mean, quant = ifr_cu, sigmaFac=2)[1],
                               ln_sd = getParmsLognormForMedianAndUpper(median = ifr_mean, quant = ifr_cu, sigmaFac=2)[2],
                               ifr_mean = ifr_mean,
                               ifr_cl = ifr_cl,
                               ifr_cu = ifr_cu), by = c("age_cat", "age_cat_label")]

#
# save
write.csv(ifr.by.age, file = file.path(args$indir, "data", 'ifr-by-age-prior_Brazeau_201113.csv'), row.names = F)
