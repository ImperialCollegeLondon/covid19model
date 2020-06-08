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
library(ggpubr)
library(bayesplot)
library(cowplot)
make_data = function(filename,label,counterfactual=FALSE, i, days=7) {
  out=rstan::extract(fit)
  N <- length(dates[[i]])
  N2 <- N + days
  times_forecast <- dates[[i]][N2]
  country <- countries[[i]]
  
  if(counterfactual == TRUE) {
    out$E_deaths = out$E_deaths0 
    estimated.deaths = out$E_deaths0
    out$prediction = out$prediction0
  }
  out$E_deaths <- out$E_deaths[,1:N2,i]
  out$prediction = out$prediction[,1:N2,i]
  estimated.deaths <- estimated.deaths[,1:N2,i]
  estimated_deaths <- colMeans(estimated.deaths)
  estimated_deaths_li <- colQuantiles(estimated.deaths, probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths, probs=.975)
  
  data_country <- data.frame("time" = as_date(as.character(dates[[i]][1:N2])),
                             "country" = rep(country, length(dates[[i]][1:N2])),
                             #"country_population" = rep(country_population, length(dates[[i]])),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui)
  
  return(list(data_country=data_country, estimated_deaths=out$E_deaths, estimated_infections=out$prediction))
}