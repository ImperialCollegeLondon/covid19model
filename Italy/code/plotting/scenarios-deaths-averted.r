library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)
library(matrixStats)
library(scales)
library(gridExtra)
library(bayesplot)
library(cowplot)
library(scales)
library(stargazer)
source("Italy/code/plotting/format-data-plotting.r")

compute_deaths_averted <- function(JOBID, StanModel, baseline,  len_forecast, last_date_data, subdir = "usa", mobility_increase = 20){
  print(paste0("Calculating deaths averted for ", mobility_increase , "%"))
  load(paste0(subdir, "/results/sim-constant-mob-",StanModel, "-", len_forecast, "-0-", JOBID, "-stanfit.Rdata"))
  list_fit_constant <- vector("list", length = length(states))
  list_fit_constant_cases <- vector("list", length = length(states))
  for (i in 1:length(states)){
    list_fit_constant[[i]] <- out$E_deaths[,,i]
    list_fit_constant_cases[[i]] <- out$prediction[,,i]
  }
  if (baseline == TRUE){
    load(paste0(subdir, "/results/sim-increase-mob-baseline-", StanModel, "-", len_forecast, "-", mobility_increase, "-", JOBID, "-stanfit.Rdata"))
  } else {
    load(paste0(subdir, "/results/sim-increase-mob-current-", StanModel, "-", len_forecast, "-", mobility_increase, "-", JOBID, "-stanfit.Rdata"))
  }
  list_fit_increase <- vector("list", length = length(states))
  list_fit_increase_cases <- vector("list", length = length(states))
  for (i in 1:length(states)){
    list_fit_increase[[i]] <- out$E_deaths[,,i]
    list_fit_increase_cases[[i]] <- out$prediction[,,i]
  }
  
  mean_delta <- vector("list", length = length(states))
  lower_delta <- vector("list", length = length(states))
  upper_delta <- vector("list", length = length(states))
  
  mean_increase <- vector("list", length = length(states))
  lower_increase <- vector("list", length = length(states))
  upper_increase <- vector("list", length = length(states))
  
  mean_constant <- vector("list", length = length(states))
  lower_constant <- vector("list", length = length(states))
  upper_constant <- vector("list", length = length(states))
  
  mean_delta_cases <- vector("list", length = length(states))
  lower_delta_cases <- vector("list", length = length(states))
  upper_delta_cases <- vector("list", length = length(states))
  
  mean_increase_cases <- vector("list", length = length(states))
  lower_increase_cases <- vector("list", length = length(states))
  upper_increase_cases <- vector("list", length = length(states))
  
  mean_constant_cases <- vector("list", length = length(states))
  lower_constant_cases <- vector("list", length = length(states))
  upper_constant_cases <- vector("list", length = length(states))
  
  totals <- matrix(0, nrow = dim(out$E_deaths)[1], ncol = 28)
  totals_cases <- matrix(0, nrow = dim(out$E_deaths)[1], ncol = 28)
  for (i in 1:length(states)){
    idx = which(dates[[i]] == last_date_data) # this is the index of last date of sim
    death.delta =  list_fit_increase[[i]][,(idx-27):idx] - list_fit_constant[[i]][,(idx-27):idx]
    death.delta.cum = rowSums(death.delta)
    death.probs = quantile(death.delta.cum,probs=c(.025,.5,.975))
    mean_delta[[i]] <- mean(death.delta.cum)
    lower_delta[[i]] <- death.probs[1]
    upper_delta[[i]] <- death.probs[3]
    
    totals = totals + death.delta
    
    death.cum_increase = rowSums(list_fit_increase[[i]][,(idx-27):idx])
    death.probs_increase = quantile(death.cum_increase,probs=c(.025,.5,.975))
    mean_increase[[i]] <- mean(death.cum_increase)
    lower_increase[[i]] <- death.probs_increase[1]
    upper_increase[[i]] <- death.probs_increase[3]
    
    death.cum_constant = rowSums(list_fit_constant[[i]][,(idx-27):idx])
    death.probs_constant = quantile(death.cum_constant,probs=c(.025,.5,.975))
    mean_constant[[i]] <- mean(death.cum_constant)
    lower_constant[[i]] <- death.probs_constant[1]
    upper_constant[[i]] <- death.probs_constant[3]
    
    cases.delta =  list_fit_increase_cases[[i]][,(idx-27):idx] - list_fit_constant_cases[[i]][,(idx-27):idx]
    cases.delta.cum = rowSums(cases.delta)
    cases.probs = quantile(cases.delta.cum,probs=c(.025,.5,.975))
    mean_delta_cases[[i]] <- mean(cases.delta.cum)
    lower_delta_cases[[i]] <- cases.probs[1]
    upper_delta_cases[[i]] <- cases.probs[3]
    
    totals_cases = totals_cases + cases.delta
    
    cases.cum_increase = rowSums(list_fit_increase_cases[[i]][,(idx-27):idx])
    cases.probs_increase = quantile(cases.cum_increase,probs=c(.025,.5,.975))
    mean_increase_cases[[i]] <- mean(cases.cum_increase)
    lower_increase_cases[[i]] <- cases.probs_increase[1]
    upper_increase_cases[[i]] <- cases.probs_increase[3]
    
    cases.cum_constant = rowSums(list_fit_constant[[i]][,(idx-27):idx])
    cases.probs_constant = quantile(cases.cum_constant,probs=c(.025,.5,.975))
    mean_constant_cases[[i]] <- mean(cases.cum_constant)
    lower_constant_cases[[i]] <- cases.probs_constant[1]
    upper_constant_cases[[i]] <- cases.probs_constant[3]
  }
  
  df_delta <- data.frame("states" = states, "mean" = unlist(mean_delta), 
                         "lower" = unlist(lower_delta),  "upper" = unlist(upper_delta))
  df_increase <- data.frame("states" = states, "mean" = unlist(mean_increase), 
                            "lower" = unlist(lower_increase),  "upper" = unlist(upper_increase))
  df_constant <- data.frame("states" = states, "mean" = unlist(mean_constant), 
                            "lower" = unlist(lower_constant),  "upper" = unlist(upper_constant))
  
  df_delta_cases <- data.frame("states" = states, "mean" = unlist(mean_delta_cases), 
                               "lower" = unlist(lower_delta_cases),  "upper" = unlist(upper_delta_cases))
  df_increase_cases <- data.frame("states" = states, "mean" = unlist(mean_increase_cases), 
                                  "lower" = unlist(lower_increase_cases),  "upper" = unlist(upper_increase_cases))
  df_constant_cases <- data.frame("states" = states, "mean" = unlist(mean_constant_cases), 
                                  "lower" = unlist(lower_constant_cases),  "upper" = unlist(upper_constant_cases))
  
  
  total.death.delta.cum = rowSums(totals)
  death.probs = quantile(total.death.delta.cum,probs=c(.025,.5,.975))
  mean_total <- mean(total.death.delta.cum)
  lower_total <- death.probs[1]
  upper_total <- death.probs[3]
  
  total.cases.delta.cum = rowSums(totals_cases)
  cases.probs = quantile(total.cases.delta.cum,probs=c(.025,.5,.975))
  mean_total_cases <- mean(total.cases.delta.cum)
  lower_total_cases <- cases.probs[1]
  upper_total_cases <- cases.probs[3]
  
  roundCI <- function(lower,upper,k=2){
    if (any(c(lower,upper)<0)) 
      stop("not designed for negative values")
    lowerstring <-  format(10^(-k)*floor(lower*10^(k)),  nsmall=k)
    upperstring <-  format(10^(-k)*ceiling(upper*10^(k)), nsmall=k)
    paste("[",paste(lowerstring,upperstring,sep=", "),"]",sep="")
  }
  
  roundCIsigdigits <- function(lower,upper,k=2){
    if (any(c(lower,upper)<=0)) stop("not designed for negative values")
    digitup <- floor(log(lower)/log(10))
    lowerstring <-  format(10^(-k+1+digitup)*floor(lower*10^(k-1-digitup)),digits=k)
    digitup <- floor(log(upper)/log(10))
    upperstring <-  format(10^(-k+1+digitup)*ceiling(upper*10^(k-1-digitup)),digits=k)
    paste("[",paste(lowerstring,upperstring,sep=", "),"]",sep="")
  }
  
  deaths_averted <- NULL
  # Make table to be read into overleaf
  for (i in 1:length(states)){
    df <- data.frame("regions" = regions[i],
                     "deaths averted" = sprintf("%.0f %s", signif(df_delta$mean[i],2), 
                                                roundCIsigdigits(df_delta$lower[i],df_delta$upper[i],2)),
                     "cases averted" = sprintf("%.0f %s", signif(df_delta_cases$mean[i],2), 
                                               roundCIsigdigits(df_delta_cases$lower[i],df_delta_cases$upper[i],2)))
    deaths_averted <- rbind(deaths_averted, df)
  }
  
  deaths_averted <- rbind(deaths_averted, data.frame("regions" = "Total",
                                                     "deaths averted" = sprintf("%.0f %s", signif(mean_total,2), 
                                                                                roundCIsigdigits(lower_total, upper_total,2)),
                                                     "cases averted" = sprintf("%.0f %s", signif(mean_total_cases,2), 
                                                                               roundCIsigdigits(lower_total_cases, upper_total_cases,2))))
  
  
  if (baseline == TRUE){
    saveRDS(deaths_averted, paste0("Italy/results/", JOBID,"-deaths-averted-baseline", mobility_increase, "-", ".RDS"), version = 2)
  } else {
    saveRDS(deaths_averted, paste0("Italy/results/", JOBID,"-deaths-averted-current", mobility_increase, "-", ".RDS"), version = 2)
  }
  
  return(deaths_averted)
}
