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


#---------------------------------------------------------------------------
format_data <- function(i, dates, countries, estimated_cases_raw, estimated_deaths_raw, 
                        reported_cases, reported_deaths, out, forecast=0, SIM = FALSE){
  
  N <- length(dates[[i]])
  if(forecast > 0) {
    dates[[i]] = c(dates[[i]], max(dates[[i]]) + 1:forecast)
    N = N + forecast
    reported_cases[[i]] = c(reported_cases[[i]],rep(NA,forecast))
    reported_deaths[[i]] = c(reported_deaths[[i]],rep(NA,forecast))
  }
    
  country <- countries[[i]]
  
  estimated_cases <- colMeans(estimated_cases_raw[,1:N,i])
  estimated_cases_li <- colQuantiles(estimated_cases_raw[,1:N,i], probs=.025)
  estimated_cases_ui <- colQuantiles(estimated_cases_raw[,1:N,i], probs=.975)
  estimated_cases_li2 <- colQuantiles(estimated_cases_raw[,1:N,i], probs=.25)
  estimated_cases_ui2 <- colQuantiles(estimated_cases_raw[,1:N,i], probs=.75)
  
  estimated_deaths <- colMeans(estimated_deaths_raw[,1:N,i])
  estimated_deaths_li <- colQuantiles(estimated_deaths_raw[,1:N,i], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated_deaths_raw[,1:N,i], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated_deaths_raw[,1:N,i], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated_deaths_raw[,1:N,i], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,i])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.75)
  
  if (SIM == FALSE){
    mu <- mean(out$mu[,i])
    mu_li <- quantile(out$mu[,i], probs=.025)
    mu_ui <- quantile(out$mu[,i], probs=.975)
  }
  

  
  data_state_plotting <- data.frame("date" = dates[[i]],
                                    "country" = rep(country, length(dates[[i]])),
                                    "reported_cases" = reported_cases[[i]], 
                                    "predicted_cases" = estimated_cases,
                                    "cases_min" = estimated_cases_li,
                                    "cases_max" = estimated_cases_ui,
                                    "cases_min2" = estimated_cases_li2,
                                    "cases_max2" = estimated_cases_ui2,
                                    "reported_deaths" = reported_deaths[[i]],
                                    "estimated_deaths" = estimated_deaths,
                                    "deaths_min" = estimated_deaths_li,
                                    "deaths_max"= estimated_deaths_ui,
                                    "deaths_min2" = estimated_deaths_li2,
                                    "deaths_max2"= estimated_deaths_ui2,
                                    "rt" = rt,
                                    "rt_min" = rt_li,
                                    "rt_max" = rt_ui,
                                    "rt_min2" = rt_li2,
                                    "rt_max2" = rt_ui2)
  
  if (SIM == FALSE){
    data_state_plotting$mu_rep = rep(mu, length(dates[[i]]))
    data_state_plotting$mu_li = rep(mu_li, length(dates[[i]]))
    data_state_plotting$mu_ui = rep(mu_ui, length(dates[[i]]))
  }
  
  return(data_state_plotting)
  
}

