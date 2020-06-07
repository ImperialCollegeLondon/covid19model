library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)

source('nature/utils/format-data.r')

make_table <- function(filename){
  
  load(paste0("nature/results/", filename,'-stanfit.Rdata'))
  out <- rstan::extract(fit)
  inf_mean_delta <- vector("list", length = 11)
  inf_lower_delta <- vector("list", length = 11)
  inf_upper_delta <- vector("list", length = 11)
  
  inf_mean_model <- vector("list", length = 11)
  inf_lower_model <- vector("list", length = 11)
  inf_upper_model <- vector("list", length = 11)
  
  inf_mean_null <- vector("list", length = 11)
  inf_lower_null <- vector("list", length = 11)
  inf_upper_null <- vector("list", length = 11)
  
  mean_delta <- vector("list", length = 11)
  lower_delta <- vector("list", length = 11)
  upper_delta <- vector("list", length = 11)
  
  mean_null <- vector("list", length = 11)
  lower_null <- vector("list", length = 11)
  upper_null <- vector("list", length = 11)
  
  mean_model <- vector("list", length = 11)
  lower_model <- vector("list", length = 11)
  upper_model <- vector("list", length = 11)
  
  cf_deaths_overall = rep(0,2000)
  our_deaths_overall = rep(0,2000)
  cf_infection_overall = rep(0,2000)
  our_infection_overall = rep(0,2000)
  for(i in 1:length(countries)){
    fit0 = make_data(filename, "Null model",counterfactual = TRUE, i, days = 0)
    fit1 = make_data(filename, "Our model",counterfactual = FALSE, i, days= 0)
    
    cf_deaths_overall = cf_deaths_overall + rowSums(fit0$estimated_deaths)
    our_deaths_overall = our_deaths_overall + rowSums(fit1$estimated_deaths)
    
    cf_infection_overall = cf_infection_overall + rowSums(fit0$estimated_infections)
    our_infection_overall = our_infection_overall + rowSums(fit1$estimated_infections)
    
    inf.delta =  fit0$estimated_infections - fit1$estimated_infections
    inf.delta.cum = rowSums(inf.delta)
    inf.probs = quantile(inf.delta.cum,probs=c(.025,.5,.975))
    inf_mean_delta[[i]] <- mean(inf.delta.cum)
    inf_lower_delta[[i]] <- inf.probs[1]
    inf_upper_delta[[i]] <- inf.probs[3]
    
    inf.cum = rowSums(fit0$estimated_infections)
    inf.probs = quantile(inf.cum,probs=c(.025,.5,.975))
    inf_mean_null[[i]] <- mean(inf.cum)
    inf_lower_null[[i]] <- inf.probs[1]
    inf_upper_null[[i]] <- inf.probs[3]
    
    inf.cum = rowSums(fit1$estimated_infections)
    inf.probs = quantile(inf.cum,probs=c(.025,.5,.975))
    inf_mean_model[[i]] <- mean(inf.cum)
    inf_lower_model[[i]] <- inf.probs[1]
    inf_upper_model[[i]] <- inf.probs[3]
    
    
    death.delta =  fit0$estimated_deaths - fit1$estimated_deaths
    death.delta.cum = rowSums(death.delta)
    death.probs = quantile(death.delta.cum,probs=c(.025,.5,.975))
    mean_delta[[i]] <- mean(death.delta.cum)
    lower_delta[[i]] <- death.probs[1]
    upper_delta[[i]] <- death.probs[3]
    
    death.cum = rowSums(fit0$estimated_deaths)
    death.probs = quantile(death.cum,probs=c(.025,.5,.975))
    mean_null[[i]] <- mean(death.cum)
    lower_null[[i]] <- death.probs[1]
    upper_null[[i]] <- death.probs[3]
    
    death.cum = rowSums(fit1$estimated_deaths)
    death.probs = quantile(death.cum,probs=c(.025,.5,.975))
    mean_model[[i]] <- mean(death.cum)
    lower_model[[i]] <- death.probs[1]
    upper_model[[i]] <- death.probs[3]
  }
  countries <- c(
    "Denmark",
    "Italy",
    "Germany",
    "Spain",
    "United_Kingdom",
    "France",
    "Norway",
    "Belgium",
    "Austria",
    "Sweden",
    "Switzerland",
    "Total"
  )
  # data frame deaths averted
  df_delta <- data.frame( unlist(mean_delta), unlist(lower_delta),  unlist(upper_delta))
  names(df_delta) <-c('mean', 'lower', 'higher')
  all_q = quantile(cf_deaths_overall-our_deaths_overall,prob=c(.025,.975))
  x <- data.frame(mean(cf_deaths_overall-our_deaths_overall),all_q[1],all_q[2])
  names(x)  <-c('mean', 'lower', 'higher')
  df_delta <- rbind(df_delta,x)
  df_delta <- data.frame(apply(signif(df_delta[,1:3],2),2,prettyNum,big.mark=','))
  df_delta['text'] <- paste0(df_delta[,1]," [",df_delta[,2]," - ",df_delta[,3],"]")
  df_delta <- cbind(df_delta, countries)
  write.csv(df_delta[,c('countries', 'text')], file = paste0('nature/results/deaths-averted-',filename, '.csv'))
  
  # deaths null
  df_null <- data.frame( unlist(mean_null), unlist(lower_null),  unlist(upper_null))
  names(df_null) <-c('mean', 'lower', 'higher')
  all_q = quantile(cf_deaths_overall,prob=c(.025,.975))
  x <- data.frame(mean(cf_deaths_overall),all_q[1],all_q[2])
  names(x)  <-c('mean', 'lower', 'higher')
  df_null <- rbind(df_null,x)
  df_null <- data.frame(apply(signif(df_null[,1:3],2),2,prettyNum,big.mark=','))
  df_null['text'] <- paste0(df_null[,1]," [",df_null[,2]," - ",df_null[,3],"]")
  df_null <- cbind(df_null, countries)
  write.csv(df_null[,c('countries', 'text')], file = paste0('nature/results/deaths-null-',filename, '.csv'))
  
  # deaths model
  df_model <- data.frame( unlist(mean_model), unlist(lower_model),  unlist(upper_model))
  names(df_model) <-c('mean', 'lower', 'higher')
  all_q = quantile(our_deaths_overall,prob=c(.025,.975))
  x <- data.frame(mean(our_deaths_overall),all_q[1],all_q[2])
  names(x)  <-c('mean', 'lower', 'higher')
  df_model <- rbind(df_model,x)
  df_model <- data.frame(apply(signif(df_model[,1:3],2),2,prettyNum,big.mark=','))
  df_model['text'] <- paste0(df_model[,1]," [",df_model[,2]," - ",df_model[,3],"]")
  df_model <- cbind(df_model, countries)
  write.csv(df_model[,c('countries', 'text')], file = paste0('nature/results/deaths-model-',filename, '.csv'))
  
  # cases model
  df_model <- data.frame( unlist(inf_mean_model), unlist(inf_lower_model),  unlist(inf_upper_model))
  names(df_model) <-c('mean', 'lower', 'higher')
  all_q = quantile(our_infection_overall,prob=c(.025,.975))
  x <- data.frame(mean(our_infection_overall),all_q[1],all_q[2])
  names(x)  <-c('mean', 'lower', 'higher')
  df_model <- rbind(df_model,x)
  df_model <- data.frame(apply(signif(df_model[,1:3],2),2,prettyNum,big.mark=','))
  df_model['text'] <- paste0(df_model[,1]," [",df_model[,2]," - ",df_model[,3],"]")
  df_model <- cbind(df_model, countries)
  write.csv(df_model[,c('countries', 'text')], file = paste0('nature/results/cases-model-',filename, '.csv'))
  
  df_pop = readRDS('nature/data/popt-ifr.rds')
  df_pop = df_pop[1:11, c('country', 'popt')]
  df_pop <- df_pop[match(countries[1:11], df_pop$country),]
  
  # % cases model
  df_model <- data.frame( unlist(inf_mean_model), unlist(inf_lower_model),  unlist(inf_upper_model))
  df_model <- (df_model / df_pop$popt) * 100
  names(df_model) <-c('mean', 'lower', 'higher')
  all_q = quantile(our_infection_overall,prob=c(.025,.975))
  x <- data.frame(mean(our_infection_overall),all_q[1],all_q[2])
  x <- (x/  sum(df_pop$popt)) * 100
  names(x)  <-c('mean', 'lower', 'higher')
  df_model <- rbind(df_model,x)
  df_model <- data.frame(apply(signif(df_model[,1:3],2),2,prettyNum,big.mark=','))
  df_model['text'] <- paste0(df_model[,1]," [",df_model[,2]," - ",df_model[,3],"]")
  df_model <- cbind(df_model, countries)
  write.csv(df_model[,c('countries', 'text')], file = paste0('nature/results/per-cases-model-',filename, '.csv'))
}