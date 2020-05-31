library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(abind)
library(scales)
library(zoo)
library(matrixStats)
library(optparse)

source("usa/code/utils/process-covariates.r")

simulate_scenarios <- function(JOBID, StanModel, scenario_type,
                               len_forecast = 0, subdir='usa', simulate_code='usa/code/stan-models/simulate-regional-state.stan',
                               mobility_vars, mobility_vars_partial_regional=mobility_vars, mobility_vars_partial_state=mobility_vars,
                               mobility_increases = 20, ext = ".png"){
  
  for (mobility_increase in mobility_increases){
    load(paste0(subdir, '/results/', StanModel, '-', JOBID, '-stanfit.Rdata'))
    
    old_dates <- dates
    print(sprintf("Simulating %s days from %s with a %s%% increase in mobility", len_forecast, max(old_dates[[1]]), mobility_increase))
    total_sim<- stan_data$N2 + len_forecast
    N3=total_sim-stan_data$N2
    # adjust SI
    stan_data$SI <- c(stan_data$SI,rep(stan_data$SI[stan_data$N2],N3))
    # adjust f
    stan_data$f2 <- matrix(NA, nrow=total_sim, ncol=dim(stan_data$f)[2])
    for(i in 1:dim(stan_data$f)[2]){
      stan_data$f2[,i] <- c(stan_data$f[,i],rep(stan_data$f[stan_data$N2,i],N3))
    }
    
    for(i in 1:length(dates)){
      dates_tmp = as.Date(dates[[i]],format='%Y-%m-%d') 
      new_dates = dates_tmp[length(dates_tmp)]
      how_many_days = total_sim-length(dates[[i]])
      pad_dates <- new_dates + days(1:how_many_days)
      dates_tmp  =c(dates_tmp, pad_dates)
      dates[[i]] = dates_tmp
    }
    for(i in 1:length(reported_cases)){
      how_many_days = total_sim-length(reported_cases[[i]])
      reported_cases[[i]] = c(reported_cases[[i]],rep(0,how_many_days))
    }
    for(i in 1:length(reported_deaths)){
      how_many_days = total_sim-length(reported_deaths[[i]])
      reported_deaths[[i]] = c(reported_deaths[[i]],rep(0,how_many_days))
    }
    
    stan_data$X2<-array(NA,c(dim(stan_data$X)[1], total_sim,dim(stan_data$X)[3]))
    for(i in 1:dim(stan_data$X)[1]){
      for(j in 1:dim(stan_data$X)[3]){
        if (j %in% mobility_vars){
          last7 = stan_data$X[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2)[2] - length(stan_data$X[i,1:stan_data$N[i],j])
          scenario = rep(last7*((100-mobility_increase)/100), ceiling(remaining/7))[1:remaining]
          stan_data$X2[i,,j] = c(stan_data$X[i,1:stan_data$N[i],j], scenario)
        } else {
          last7 = stan_data$X[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2)[2] - length(stan_data$X[i,1:stan_data$N[i],j])
          scenario = rep(last7, ceiling(remaining/7))[1:remaining]
          stan_data$X2[i,,j] = c(stan_data$X[i,1:stan_data$N[i],j], scenario)
        }
      }
    }
    
    stan_data$X2_partial_regional <-array(NA,c(dim(stan_data$X_partial_regional)[1], total_sim,dim(stan_data$X_partial_regional)[3]))
    for(i in 1:dim(stan_data$X_partial_regional)[1]){
      for(j in 1:dim(stan_data$X_partial_regional)[3]){
        if (j %in% mobility_vars_partial_regional){
          last7 = stan_data$X_partial_regional[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial_regional)[2] - length(stan_data$X_partial_regional[i,1:stan_data$N[i],j])
          scenario = rep(last7*((100-mobility_increase)/100), ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial_regional[i,,j] = c(stan_data$X_partial_regional[i,1:stan_data$N[i],j], scenario)
        } else {
          last7 = stan_data$X_partial_regional[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial_regional)[2] - length(stan_data$X_partial_regional[i,1:stan_data$N[i],j])
          scenario = rep(last7, ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial_regional[i,,j] = c(stan_data$X_partial_regional[i,1:stan_data$N[i],j], scenario)
        }
        
      }
    }
    
    stan_data$X2_partial_state <-array(NA,c(dim(stan_data$X_partial_state)[1], total_sim,dim(stan_data$X_partial_state)[3]))
    for(i in 1:dim(stan_data$X_partial_state)[1]){
      for(j in 1:dim(stan_data$X_partial_state)[3]){
        if (j %in% mobility_vars_partial_regional){
          last7 = stan_data$X_partial_state[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial_state)[2] - length(stan_data$X_partial_state[i,1:stan_data$N[i],j])
          scenario = rep(last7*((100-mobility_increase)/100), ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial_state[i,,j] = c(stan_data$X_partial_state[i,1:stan_data$N[i],j], scenario)
        } else {
          last7 = stan_data$X_partial_state[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial_state)[2] - length(stan_data$X_partial_state[i,1:stan_data$N[i],j])
          scenario = rep(last7, ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial_state[i,,j] = c(stan_data$X_partial_state[i,1:stan_data$N[i],j], scenario)
        }
        
      }
    }
    
    # update stan_data
    stan_data$N2 = total_sim
    stan_data$X = stan_data$X2
    stan_data$X_partial_regional = stan_data$X2_partial_regional
    stan_data$X_partial_state = stan_data$X2_partial_state
    stan_data$f = stan_data$f2
    
    if(!is.null(stan_data$week_index)) {
      stan_data$week_index2 = matrix(0,nrow(stan_data$week_index),total_sim)
      stan_data$week_index2[1:nrow(stan_data$week_index),1:ncol(stan_data$week_index)] = stan_data$week_index
      # AR week index setup
      for(state.i in 1:stan_data$M) {
        # find the week_index for today (stan_data$N[state.i]) and carry that forward to the end
        stan_data$week_index2[state.i,stan_data$N[state.i]:total_sim] = stan_data$week_index[state.i,stan_data$N[state.i]]
      }
      stan_data$week_index = stan_data$week_index2
    }
    
    m2 <- stan_model(simulate_code)
    
    fit <- gqs(m2, data=stan_data, draws = as.matrix(fit))
    
    out <- rstan::extract(fit)
    estimated_cases_raw <- out$prediction
    estimated_deaths_raw <- out$E_deaths
    estimated_deaths_cf <- out$E_deaths0
    
    print(paste0("saving: ", subdir,'/results/sim-', StanModel,'-', len_forecast, '-', mobility_increase, 
                 '-',JOBID,'-standata.Rdata'))
    save(fit, dates, reported_cases, reported_deaths, states,
         estimated_cases_raw, estimated_deaths_raw, estimated_deaths_cf, out, 
         file=paste0(subdir,'/results/sim-',StanModel,'-',len_forecast, '-', mobility_increase, '-', JOBID,'-stanfit.Rdata'))
    
  }

}
