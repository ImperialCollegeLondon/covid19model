library(data.table)
library(rstan)
library(lognorm)
library(dplyr)

args = list(
  seed = 42, 
  chain = 1,
  outdir= "~/Box\ Sync/2020/R0t/results/ifr_by_age_prior",
  indir = "~/git/R0t",
  datadir= "~/git/R0t/data",
  cmdstan = 1,
  #stanModelFile = 'base_age_prior_ifr_200820a7_cmdstanv',
  stanModelFile = 'base_age_prior_ifr_200820b5_cmdstanv',
  job_tag= 'new_data_spain_England',
  job_id='41286'
)

args$job_dir <- file.path(args$outdir,paste0(args$stanModelFile,'-',args$job_tag,'-',args$job_id)) 

outfile.base <- paste0(args$job_dir, "/",  args$stanModelFile , "-", args$job_tag)

# Outputs of the log normal fit from 28/08/2020 
load(file.path(args$job_dir, paste0(basename(args$job_dir), '_stanout.RData')) )

fit = rstan:::sflist2stanfit(list(fit))

age_predict <- c(0, seq(2.5, 97.5, 5))
difrx <- rstan::extract(fit)[['pred_deaths']]	
difrx <- as.data.table(reshape2::melt(difrx))
difrx <- setnames(difrx, 1:3, c('it','age.idx','pred_deaths'))
set(difrx, NULL, 'pred_deaths', difrx$pred_deaths/1e7)
set(difrx, NULL, 'logit_pred_deaths', difrx[, log(pred_deaths/(1-pred_deaths))])
set(difrx, NULL, 'log_pred_deaths', difrx[, log(pred_deaths)])
difrx <- melt(difrx, measure.vars=c('logit_pred_deaths','log_pred_deaths'))
difrx_sample <- difrx[is.finite(value),]
difrx <- difrx[is.finite(value), list(qs=quantile(value, p=c(0.025, 0.25, 0.5, 0.75, 0.975)), ql=c('CL','IL','M','IU','CU')), by=c('variable','age.idx')]
difrx <- dcast.data.table(difrx, variable+age.idx~ql, value.var='qs')
difrx[, age:= age.idx-1]
difrx[, age:= 5*(age.idx-1)]

# log ifr by age posterior
if(0){
  ggplot( subset(difrx, variable=='log_pred_deaths')) +
    geom_ribbon(aes(x=age, ymin=CL, ymax=CU)) +
    geom_line(aes(x=age, y=M)) 
}

#
# Find hyperparameters of log normal distribution
difrx = subset(difrx,  variable == "log_pred_deaths")
tmp = select(difrx, -variable)
# aggregate 85-100 to 85+
tmp1 = tmp[age >= 85, list(M = mean(M), IU = mean(IU), IL = mean(IL), CU = mean(CU), CL = mean(CL), age.idx = min(age.idx), age = min(age))]
tmp = rbind(subset(tmp, age < 85), tmp1)
tmp[, age_cat_label := ifelse(age == "85", "85+", paste0(age, "-", age + 4))]
tmp[, ifr_mean := exp(M)]
tmp[, ifr_cl := exp(CL)]
tmp[, ifr_cu := exp(CU)]
tmp = tmp[, list(ln_mu = getParmsLognormForMedianAndUpper(median = ifr_mean, quant = ifr_cu, sigmaFac=2)[1],
                 ln_sd = getParmsLognormForMedianAndUpper(median = ifr_mean, quant = ifr_cu, sigmaFac=2)[2],
                 ifr_mean = ifr_mean,
                 ifr_cl = ifr_cl,
                 ifr_cu = ifr_cu), by = c("age.idx", "age_cat_label", "age")]
#
# Find hyperparameters beta distribution
library("prevalence")
tmp[, ifr_beta_alpha := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$alpha, by = c("age_cat", "age.idx",  "age")]
tmp[, ifr_beta_beta := betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")$beta, by = c("age_cat", "age.idx",  "age")]


#
# check lognormal fit to posterior density estimate
if(0){
  tmp1 = list()
  begin_x = c(rep(-15, 10), rep(-10, 3), rep(-5, 4), rep(-2.5, 4))
  for(i in 1:nrow(tmp)){
    xx = seq(begin_x[i], 0, 0.01)
    
    tmp1[[i]] = data.table(x = xx, age = tmp$age[i], age.idx = tmp$age.idx[i], model = "log normal - gp")
    tmp1[[i]][, pd := dnorm(x, tmp$ln_mu[i], tmp$ln_sd[i])]
  }
  tmp1 = do.call("rbind", tmp1)
  
  difrx_sample[, age:= age.idx-1]
  difrx_sample[, age:= 5*(age.idx-1)]
  
  ggplot(subset(difrx_sample, variable == "log_pred_deaths"), aes(x = value)) +
    geom_density() +
    facet_wrap(age~., scales = "free") +
    geom_line(data = tmp1, aes(x = x, y = pd), col = "red", linetype = "dashed") +
    facet_wrap(age~., scales = "free") +
    theme_bw()
}

#
# Clean 
setnames(tmp, "age.idx", "age_cat")
tmp = select(tmp, -age)

#
# Plot comparison to Levin's prior 
prior_betabinom = tmp
prior_betabinom[, model := "BetaBinomial with logit link"]
prior_betabinom[, ln_cl := log(qlnorm(0.025, ln_mu, ln_sd))]
prior_betabinom[, ln_cu := log(qlnorm(0.975, ln_mu, ln_sd))]
prior_betabinom = select(prior_betabinom, model, age_cat, age_cat_label, ln_mu, ln_cl, ln_cu)

prior_levin = as.data.table(read.csv(file.path(args$indir,'data','ifr_age_Levin_200904.csv')))
prior_levin[, model := "Meta-analysis from Levin et al." ]
prior_levin[,age_cat_label := prior_betabinom$age_cat_label]
prior_levin[, ln_cl := log(qlnorm(0.025, ln_mu, ln_sd))]
prior_levin[, ln_cu := log(qlnorm(0.975, ln_mu, ln_sd))]
prior_levin = select(prior_levin, model, age_cat, age_cat_label, ln_mu, ln_cl, ln_cu)

priors = rbind(prior_betabinom, prior_levin)

ggplot(priors) +
  geom_line(aes(x = age_cat, y = ln_mu, col = model)) +
  geom_ribbon(aes(x = age_cat, ymin = ln_cl, ymax = ln_cu, fill = model), alpha = 0.5)+
  scale_x_continuous(breaks = prior_betabinom$age_cat, labels = prior_betabinom$age_cat_label) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.9)) +
  labs(y = "Infection Fatality Rate Prior (log scale)") +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("black", "red"))+ 
  scale_fill_manual(values = c("gray60", "#F1A340")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE))
ggsave(file = paste0(outfile.base, "-comparison_ifr_prior_betabinom_lognormal.png"), w = 6, h = 6)


#
# save new prior
file = file.path(args$datadir,'ifr_age_200902.csv')
cat("write ", file)
write.csv(tmp, file = file)

#
# Save hyperparameters for the paper
file = paste0(outfile.base, "-prior_hyperparameters.rds")
cat("write ", file)
tmp1 = matrix(ncol = nrow(tmp), nrow = 2, c(tmp$ln_mu, tmp$ln_sd), byrow = T)
saveRDS(tmp1, file = file, version = 2)

  