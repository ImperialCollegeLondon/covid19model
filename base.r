library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)

source('utils/read-data.r')
source('utils/process-covariates.r')

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}

if(is.null(cmdoptions$options$full)) {
  FULL = Sys.getenv("FULL") == "TRUE"
} else {
  FULL = cmdoptions$options$full
}

if(DEBUG && FULL) {
  stop("Setting both debug and full run modes at once is invalid")
}

if(length(cmdoptions$args) == 0) {
  StanModel = 'base'
} else {
  StanModel = cmdoptions$args[1]
}

print(sprintf("Running %s",StanModel))
if(DEBUG) {
  print("Running in DEBUG mode")
} else if (FULL) {
  print("Running in FULL mode")
}

# Read which countires to use
countries <- read.csv('data/regions.csv', stringsAsFactors = FALSE)
# Read deaths data for regions
d <- read_obs_data(countries)
# Read ifr 
ifr.by.country <- read_ifr_data()

# Read interventions
interventions <- read_interventions(countries)

N2 <- 100 # increase if you need more forecast

processed_data <- process_covariates(countries = countries, interventions = interventions, 
                                     d = d , ifr.by.country = ifr.by.country, N2 = N2)
stan_data = processed_data$stan_data
dates = processed_data$dates
deaths_by_country = processed_data$deaths_by_country
reported_cases = processed_data$reported_cases

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan'))

if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40,warmup=20,chains=2)
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=1800,warmup=1000,chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 15))
} else { 
  fit = sampling(m,data=stan_data,iter=1000,warmup=500,chains=4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))
}   

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

countries <- countries$Regions
save.image(paste0('results/',StanModel,'-',JOBID,'.Rdata'))
save(fit,prediction,dates,reported_cases,deaths_by_country,countries,estimated.deaths,estimated.deaths.cf,out,file=paste0('results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

## Ensure that output directories exist
dir.create("results/", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/data", showWarnings = FALSE, recursive = TRUE)

library(bayesplot)
filename <- paste0(StanModel,'-',JOBID)

print("Generating covariate size effects plot")
covariate_size_effects_error <- system(paste0("Rscript covariate-size-effects.r ", filename,'-stanfit.Rdata'),intern=FALSE)
if(covariate_size_effects_error != 0){
  stop(sprintf("Error while plotting covariate size effects! Code: %d", covariate_size_effects_error))
}

mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
ggsave(sprintf("results/%s-mu.png",filename),g,width=4,height=6)
tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
Rt_adj = do.call(cbind,tmp)
colnames(Rt_adj) = countries
g = (mcmc_intervals(Rt_adj,prob = .9))
ggsave(sprintf("results/%s-final-rt.png",filename),g,width=4,height=6)

print("Generate 3-panel plots")
plot_3_panel_error <- system(paste0("Rscript plot-3-panel.r ", filename,'-stanfit.Rdata'),intern=FALSE)
if(plot_3_panel_error != 0){
  stop(sprintf("Generation of 3-panel plots failed! Code: %d", plot_3_panel_error))
}

print("Generate forecast plot")
plot_forecast_error <- system(paste0("Rscript plot-forecast.r ",filename,'-stanfit.Rdata'),intern=FALSE)
if(plot_forecast_error != 0) {
  stop(sprintf("Generation of forecast plot failed! Code: %d", plot_forecast_error))
}

print("Make forecast table")
make_table_error <- system(paste0("Rscript make-table.r results/",filename,'-stanfit.Rdata'),intern=FALSE)
if(make_table_error != 0){
  stop(sprintf("Generation of alpha covar table failed! Code: %d", make_table_error))
}


verify_result_error <- system(paste0("Rscript web-verify-output.r ", filename,'.Rdata'),intern=FALSE)
if(verify_result_error != 0){
  stop(sprintf("Verification of web output failed! Code: %d", verify_result_error))
}
