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


simulate_scenarios <- function(JOBID, StanModel, scenario_type, plots = FALSE, 
                               len_forecast = 0, subdir='Italy', simulate_code='Italy/code/stan-models/simulate.stan',
                               mobility_vars, mobility_vars_partial=mobility_vars, mobility_increase = 20, ext = ".png"){
  
  load(paste0(subdir, '/results/', StanModel, '-', JOBID, '-stanfit.Rdata'))
  
  old_dates <- dates
  print(sprintf("Length of forecast: %s", len_forecast))
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
  
  if (scenario_type == "constant-mob"){
    print("Constant mobility simulation")
    # use this is you want mobility to stay as the last day
    stan_data$X2 <- array(NA, c(dim(stan_data$X)[1], total_sim, dim(stan_data$X)[3]))
    for(i in 1:dim(stan_data$X)[1]){
      for(j in 1:dim(stan_data$X)[3]){
        last7 = stan_data$X[i,(stan_data$N[i]-7):stan_data$N[i],j]
        remaining = dim(stan_data$X2)[2] - length(stan_data$X[i,1:stan_data$N[i],j])
        scenario = rep(last7, ceiling(remaining/7))[1:remaining]
        stan_data$X2[i,,j] = c(stan_data$X[i,1:stan_data$N[i],j], scenario)
      }
    }
    stan_data$X2_partial <- array(NA, c(dim(stan_data$X_partial)[1], total_sim, dim(stan_data$X_partial)[3]))
    for(i in 1:dim(stan_data$X_partial)[1]){
      for(j in 1:dim(stan_data$X_partial)[3]){
        stan_data$X2_partial[i,,j] = c(stan_data$X_partial[i,,j], rep(stan_data$X_partial[i,,j][stan_data$N2], N3))
      }
    }
  } else if (scenario_type ==  "increase-mob-current"){
    print("Increase mobility simulation from current")
    mobility_choice=0.2 # 20 % mobility for example
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
    
    stan_data$X2_partial <-array(NA,c(dim(stan_data$X_partial)[1], total_sim,dim(stan_data$X_partial)[3]))
    for(i in 1:dim(stan_data$X_partial)[1]){
      for(j in 1:dim(stan_data$X_partial)[3]){
        if (j %in% mobility_vars_partial){
          last7 = stan_data$X_partial[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial)[2] - length(stan_data$X_partial[i,1:stan_data$N[i],j])
          scenario = rep(last7*((100-mobility_increase)/100), ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial[i,,j] = c(stan_data$X_partial[i,1:stan_data$N[i],j], scenario)
        } else {
          last7 = stan_data$X_partial[i,(stan_data$N[i]-7):stan_data$N[i],j]
          remaining = dim(stan_data$X2_partial)[2] - length(stan_data$X_partial[i,1:stan_data$N[i],j])
          scenario = rep(last7, ceiling(remaining/7))[1:remaining]
          stan_data$X2_partial[i,,j] = c(stan_data$X_partial[i,1:stan_data$N[i],j], scenario)
        }
        
      }
    }
  }
  
  # update stan_data
  stan_data$N2 = total_sim
  stan_data$X = stan_data$X2
  stan_data$X_partial = stan_data$X2_partial
  stan_data$f = stan_data$f2
  
  m2 <- stan_model(simulate_code)
  
  fit <- gqs(m2, data=stan_data, draws = as.matrix(fit))
  
  out <- rstan::extract(fit)
  estimated_cases_raw <- out$prediction
  estimated_deaths_raw <- out$E_deaths
  estimated_deaths_cf <- out$E_deaths0
  
  print(paste0("saving: ", subdir,'/results/sim-', scenario_type, '-',StanModel,'-',len_forecast, '-', mobility_increase, 
               '-',JOBID,'-standata.Rdata'))
  save(fit, dates, reported_cases, reported_deaths, states,
      estimated_cases_raw, estimated_deaths_raw, estimated_deaths_cf, out, 
        file=paste0(subdir,'/results/sim-', scenario_type, '-',StanModel,'-',len_forecast, '-', mobility_increase, '-', JOBID,'-stanfit.Rdata'))
  
  #------------------------------------------------------------------------------------------
  print(sprintf("Last date forecast: %s", max(old_dates[[1]]) + len_forecast))
  # Does plotting
  if (plots == TRUE){
    source(paste0(subdir, "/code/plotting/make-plots.r"))
    if (scenario_type =="increase-mob-baseline"){
      print("Making increased mobility from baseline simulation plots ")
      label = paste0("sim_increase_mob_",  len_forecast, "_", mobility_increase, '_')
      make_plots_all(paste0(subdir, '/results/sim-increase-mob-baseline-',StanModel,'-', len_forecast, '-', mobility_increase, '-',
                            JOBID,'-stanfit.Rdata'), SIM=TRUE, label=label, 
                     last_date_data =max(old_dates[[1]]) + len_forecast)
    } else if (scenario_type == "increase-mob-current"){
        print("Making increased mobility from current simulation plots")
        label = paste0("sim_increase_mob_",  len_forecast, "_", mobility_increase)
        make_plots_all(paste0(subdir, '/results/sim-increase-mob-current-',StanModel,'-', len_forecast, '-' , mobility_increase, '-',
                              JOBID,'-stanfit.Rdata'), SIM=TRUE, label=label, 
                       last_date_data =max(old_dates[[1]]) + len_forecast, ext = ext)
    } else if (scenario_type == "constant-mob"){
      label = paste0("sim_constant_mob_",  len_forecast, "_", mobility_increase)
      make_plots_all(paste0(subdir, '/results/sim-constant-mob-',StanModel,'-', len_forecast, '-', mobility_increase, '-',
                            JOBID,'-stanfit.Rdata'), SIM=TRUE, label=label,
                     last_date_data = max(old_dates[[1]]) + len_forecast, ext = ext)
    } 
  }
}
