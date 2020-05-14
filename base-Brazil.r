library(rstan)
library(matrixStats)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(tidyverse)
library(dplyr)
library(abind)

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(bayesplot)
library(cowplot)
library(optparse)

source("Brazil/code/preprocessing-subnation-brazil.r")

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
Sys.setenv(DEBUG = "TRUE") # to run  in debug mode
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}
# Sys.setenv(FULL = "TRUE") # to run  in full mode
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

args = cmdoptions$args
# if using rstudio change this, 1: pooling method, 2: formula to use, debug and full are either commandline or sys variable
if(length(args) == 0) {
  args = c('base')
} 

StanModel = args[1]
cat(sprintf("Running:\nStanModel = %s\nDebug: %s\n",
            StanModel,DEBUG))


####################################################################
#### Parameters to input:
forecast <- 7
N2 <- (max(as.Date(df$DateRep, format="%Y-%m-%d")) - min(as.Date(df$DateRep, format="%Y-%m-%d")) + 1 + forecast)[[1]]
countries <- c("RJ","SP","PE","CE","AM","BA","ES","MA","MG","PR","PA","RN","RS","SC","AL","PB")
####################################################################
processed_data <- process_data(countries,N2,df)
stan_data <- processed_data$stan_data
dates <- processed_data$dates
deaths_by_country <- processed_data$deaths_by_country
reported_cases <- processed_data$reported_cases

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('Brazil/code/stan-models/',StanModel,'.stan'))


if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40,warmup=20,chains=2)
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=1500,warmup=500,chains=8,thin=1, 
                 control = list(adapt_delta = 0.95, max_treedepth = 15))
} else { 
  fit = sampling(m,data=stan_data,iter=400,warmup=200,chains=4,thin=4,
                 control = list(adapt_delta = 0.95, max_treedepth = 10))
}  



out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))
filename <- paste0(StanModel,'-',JOBID)
save(fit, dates, reported_cases,deaths_by_country, countries,
     prediction, estimated.deaths,stan_data,JOBID,df_pop,filename,df_region_codes,
     file=paste0('Brazil/results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

################Code for plotting and making attack rate table#############
source('Brazil/code/plot-3-panel.r')
make_data_plot(filename)
