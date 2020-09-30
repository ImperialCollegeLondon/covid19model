library(data.table)
library(rstan)
library(lognorm)
library(dplyr)
library(covid19AgeModel)

args = list(
  seed = 42, 
  chain = 1,
  outdir= "~/Box\ Sync/2020/R0t/results/ifr_by_age_prior",
  indir = "~/git/R0t",
  cmdstan = 1,
  #stanModelFile = 'base_age_prior_ifr_200820a7_cmdstanv',
  stanModelFile = 'base_age_prior_ifr_200820b5_cmdstanv',
  job_tag= 'new_data_spain_England',
  job_id='41286'
)

pkg.dir <- system.file(package = "covid19AgeModel" )

args$job_dir <- file.path(args$outdir,paste0(args$stanModelFile,'-',args$job_tag,'-',args$job_id)) 

outfile.base <- paste0(args$job_dir, "/",  args$stanModelFile , "-", args$job_tag)

# Outputs of the log normal fit from 28/08/2020
load(file.path(args$job_dir, paste0(basename(args$job_dir), '_stanout.RData')) )
load(file.path(args$job_dir, paste0(basename(args$job_dir), '_stanin.RData')) )

fit = rstan:::sflist2stanfit(list(fit))

age_predict <- c(0, seq(2.5, 97.5, 5))

#	plot IFR and log IFR
difrp <- rstan::extract(fit)[['p_predict']]
difrp <- as.data.table(reshape2::melt(difrp))
difrp <- setnames(difrp, 1:3, c('it','age.idx','ifr'))
difrp <- merge(difrp, data.table(age.idx=seq_along(age_predict), age=age_predict), by='age.idx')
difrp[, log.ifr:= log(ifr)]
difrp[, logit.ifr:= log(ifr/(1-ifr))]
difrp <- melt(difrp, measure.vars=c('ifr','log.ifr','logit.ifr'))
difrp <- difrp[, list(qs=quantile(value, p=c(0.025, 0.25, 0.5, 0.75, 0.975)), ql=c('CL','IL','M','IU','CU')), by=c('variable','age.idx','age')]
difrp <- dcast.data.table(difrp, variable+age.idx+age~ql, value.var='qs')  
ggplot( subset(difrp, variable=='log.ifr'), aes(x=age)) +
  geom_ribbon(aes(ymin=CL, ymax=CU)) +
  geom_line(aes(y=M)) +
  geom_point(data=difr, aes(x=Median_Age, y=log_IFR), colour='blue') +
  theme_bw()
ggplot( subset(difrp, variable=='logit.ifr'), aes(x=age)) +
  geom_ribbon(aes(ymin=CL, ymax=CU)) +
  geom_line(aes(y=M)) +
  geom_point(data=difr, aes(x=Median_Age, y=logit_IFR), colour='blue') +
  theme_bw()
ggplot( subset(difrp, variable=='ifr')) +
  geom_ribbon(aes(x=age, ymin=CL, ymax=CU)) +
  geom_line(aes(x=age, y=M)) +
  geom_point(data=difr, aes(x=Median_Age, y=IFR), colour='blue') +
  theme_bw()

#	posterior predictive checks
difrx <- rstan::extract(fit)[['pred_deaths']]	
difrx <- as.data.table(reshape2::melt(difrx))
difrx <- setnames(difrx, 1:3, c('it','age.idx','pred_deaths'))
set(difrx, NULL, 'pred_deaths', difrx$pred_deaths/1e7)
set(difrx, NULL, 'logit_pred_deaths', difrx[, log(pred_deaths/(1-pred_deaths))])
set(difrx, NULL, 'log_pred_deaths', difrx[, log(pred_deaths)])
difrx <- melt(difrx, measure.vars=c('logit_pred_deaths','log_pred_deaths'))
difrx <- difrx[is.finite(value), list(qs=quantile(value, p=c(0.025, 0.25, 0.5, 0.75, 0.975)), ql=c('CL','IL','M','IU','CU')), by=c('variable','age.idx')]
difrx <- dcast.data.table(difrx, variable+age.idx~ql, value.var='qs')
difrx[, age:= age.idx-1]
difrx[, age:= 5*(age.idx-1)]
ggplot( subset(difrx, variable=='logit_pred_deaths')) +
  geom_ribbon(aes(x=age, ymin=CL, ymax=CU)) +
  geom_line(aes(x=age, y=M)) +
  geom_point(data=difr, aes(x=Median_Age, y=logit_IFR), colour='blue') +
  theme_bw()
ggplot( subset(difrx, variable=='log_pred_deaths')) +
  geom_ribbon(aes(x=age, ymin=CL, ymax=CU), alpha = 0.5, fill = "gray60") +
  geom_line(aes(x=age, y=M)) +
  geom_point(data=difr, aes(x=Median_Age, y=log_IFR, col = Study)) +
  theme_bw() +
  labs(y = "Infection Fatality Rate (log scale)") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) 
ggsave(filename = paste0(outfile.base, "-ifrbyage_fit.png"), w = 6, h = 6)

#
# comparison to Levin's prior 
# IFR estimates extracted from https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v4.supplementary-material
ifr_lognormal = as.data.table(read.csv(file.path(pkg.dir, "data", "ifr-by-age-prior_Levin_raw_200904.csv")))
tmp1 = select(ifr_lognormal, c("Age", "IFR", "Lower.Bound", "Upper.Bound"))
tmp1 = subset(tmp1, !Age %in% c("0-34", "95+"))
tmp1[, Model := "Meta-analysis from Levin et al." ]
tmp1[, M := log(IFR/100) ]
tmp1[, CL := log(Lower.Bound/100) ]
tmp1[, CU := log(Upper.Bound/100) ]
tmp1[, Age := as.numeric(Age)]
setnames(tmp1, "Age", "age")
tmp1 = select(tmp1, -c("IFR", "Lower.Bound", "Upper.Bound"))
tmp = subset(difrx, variable=='log_pred_deaths', select = c("age", "CL", "CU", "M"))
tmp[, Model := "BetaBinomial with logit link"]
tmp = rbind(tmp1, tmp)
ggplot(tmp) +
  geom_ribbon(aes(x=age, ymin=CL, ymax=CU, fill = Model), alpha = 0.5) +
  geom_line(aes(x=age, y=M, col = Model), alpha = 1.1) +
  theme_bw() +
  labs(y = "Infection Fatality Rate (log scale)") +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("black", "red"))+ 
  scale_fill_manual(values = c("gray60", "#F1A340")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE))
ggsave(file = paste0(outfile.base, "-comparison_ifr_prediction_betabinom_lognormal.png"), w = 6, h = 6)


# #
# # # comparison to Verity's prior 
# ifr_verity = read.csv(args$file_age_ifr_verity)
# ifr_lognormal = as.data.table(read.csv(args$file_age_ifr_lognormal))
# 
# ggplot( subset(difrx, variable=='log_pred_deaths')) +
#   geom_ribbon(aes(x=age, ymin=CL, ymax=CU), alpha = 0.5) +
#   geom_errorbar(data = ifr_verity, aes(x = age, ymin = log(ifr_cl), ymax = log(ifr_cu)), col = "red") +
#   geom_point(data = ifr_verity, aes(x = age, y = log(ifr_mean)), col = "red") +
#   geom_line(aes(x=age, y=M)) +
#   theme_bw() +
#   labs(title = "Comparison with Verity")
# 
# 

