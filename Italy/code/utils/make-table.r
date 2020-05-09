library(tidyverse)
library(matrixStats)
library(data.table)
library(lubridate)
library(gdata)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(abind)
library(optparse)

source("Italy/code/utils/read-data-subnational.r")


make_table <- function(filename, date_till_percentage){
  print(sprintf("Running %s up until %s",filename, date_till_percentage))
  load(filename)
  out <- rstan::extract(fit)
  
  ifrs <- read_ifr_data()
  #df_pop= read.csv("data/popt_ifr3.28.csv", stringsAsFactors = FALSE)
  #df_pop$country[df_pop$country == "United Kingdom"] = "United_Kingdom"
  
  dates_italy <- dates[[which(regions == "Lombardy")]]
  len_dates <- length(dates_italy)

  cases <- vector("list", length = length(regions))
  total_cases <- vector("list", length = length(regions))
  total_cases_ui <- vector("list", length = length(regions))
  total_cases_li <- vector("list", length = length(regions))
  deaths <- vector("list", length = length(regions))
  total_deaths <- vector("list", length = length(regions))
  rt <- vector("list", length = length(regions))
  fraction_infected <- vector("list", length = length(regions))
  fraction_infected_li <- vector("list", length = length(regions))
  fraction_infected_ui <- vector("list", length = length(regions))
  fraction_obs_infected <- vector("list", length = length(regions))
  fraction_total_obs_infected <- vector("list", length = length(regions))
  y <- vector("list", length = length(regions))
  
  for(i in 1:length(regions)) {
    Country = regions[i]
    print(Country)
    x = dates[[i]]
    N = length(x)
    forecast = 7
    x  = c(x,x[length(x)]+1:forecast)
    padding <- len_dates - length(dates[[i]])
    y[[i]] = c(rep(0, padding),reported_cases[[i]], rep(NA, forecast))
    
    cases[[i]] = c(rep(0, padding), round(colMeans(estimated_cases_raw[,1:length(x),i])))
    
    total_cases[[i]] = c( round(cumsum(colMeans(estimated_cases_raw[,1:length(x),i]))))
    # chk = c(round((colMeans(rowCumsums(estimated_cases_raw[,1:length(x),i])))))
    
    total_cases_li[[i]] = c(
      round((colQuantiles(rowCumsums(estimated_cases_raw[,1:length(x),i]),probs=.025))))
    total_cases_ui[[i]] = c(
      round((colQuantiles(rowCumsums(estimated_cases_raw[,1:length(x),i]),probs=.975))))
    
    deaths[[i]] =  c(rep(0, padding), round(colMeans(estimated_deaths_raw[,1:length(x),i])))
    total_deaths[[i]] =  c(rep(0, padding), round(cumsum(colMeans(estimated_deaths_raw[,1:length(x),i]))))
    rt[[i]] = c(rep(NA, padding), colMeans(out$Rt_adj[,1:length(x),i]))
    
    fraction_infected[[i]] = c(rep(0, padding), total_cases[[i]]/ ifrs$popt[ifrs$country==Country])
    fraction_infected_li[[i]] = c(rep(0, padding), 
                                  total_cases_li[[i]]/ ifrs$popt[ifrs$country==Country])
    fraction_infected_ui[[i]] = c(rep(0, padding), 
                                  total_cases_ui[[i]]/ ifrs$popt[ifrs$country==Country])
    fraction_obs_infected[[i]] = c(rep(0, padding), y[[i]] / cases[[i]])
    fraction_total_obs_infected[[i]] = c(rep(0, padding), cumsum(y[[i]]) / cases[[i]])
    
    total_cases[[i]]  = c(rep(0, padding),total_cases[[i]])
  }
  
  dates_italy  = c(dates_italy,dates_italy[length(dates_italy)]+1:forecast)
  cases <- do.call(rbind, cases)
  cases_df <- as.data.frame(cases)
  names(cases_df) <- dates_italy
  cases_df$regions <- regions
  # write.csv(cases_df, "figures/cases.csv")
  
  total_cases <- do.call(rbind, total_cases)
  total_cases_df <- as.data.frame(total_cases)
  names(total_cases_df) <- dates_italy
  total_cases_df$regions <- regions
  # write.csv(total_cases_df, "figures/total_cases.csv")
  
  deaths <- do.call(rbind, deaths)
  deaths_df <- as.data.frame(deaths)
  names(deaths_df) <- dates_italy
  deaths_df$regions <- regions
  # write.csv(deaths_df, "figures/deaths.csv")
  
  total_deaths <- do.call(rbind, total_deaths)
  total_deaths_df <- as.data.frame(total_deaths)
  names(total_deaths_df) <- dates_italy
  total_deaths_df$regions <- regions
  # write.csv(total_deaths_df, "figures/total_deaths.csv")
  
  rt <- do.call(rbind, rt)
  rt_df <- as.data.frame(rt)
  names(rt_df) <- dates_italy
  rt_df$regions <- regions
  # write.csv(rt_df, "figures/rt.csv")
  
  fraction_infected <- do.call(rbind, fraction_infected)
  fraction_infected_df <- as.data.frame(fraction_infected)
  names(fraction_infected_df) <- dates_italy
  fraction_infected_df$regions <- regions
  # write.csv(fraction_infected_df, "figures/fraction_infected.csv")
  
  fraction_infected_li <- do.call(rbind, fraction_infected_li)
  fraction_infected_li_df <- as.data.frame(fraction_infected_li)
  names(fraction_infected_li_df) <- dates_italy
  fraction_infected_li_df$regions <- regions
  # write.csv(fraction_infected_li_df, "figures/fraction_infected_li.csv")
  
  fraction_infected_ui <- do.call(rbind, fraction_infected_ui)
  fraction_infected_ui_df <- as.data.frame(fraction_infected_ui)
  names(fraction_infected_ui_df) <- dates_italy
  fraction_infected_ui_df$regions <- regions
  # write.csv(fraction_infected_ui_df, "figures/fraction_infected_ui.csv")
  
  total_infected = data.frame(regions=regions,mean=fraction_infected[,dates_italy == date_till_percentage],
                              li=fraction_infected_li[,dates_italy == date_till_percentage],ui=fraction_infected_ui[,dates_italy == date_till_percentage])
  
  ## Calculation for whole of italy.
  date = date_till_percentage
  infections = NULL
  for(i in 1:21) {
    today.ii = which(dates[[i]] == date)
    infections = cbind(infections, rowSums(out$prediction[, 1:today.ii,i]))
  }
  total.infections = NULL
  total.infections = c(total.infections,rowSums(infections))
  
  infections.by.date = mean(total.infections)
  lower.infections.by.date = quantile(total.infections,prob=.025)
  upper.infections.by.date = quantile(total.infections,prob=.975)
  
  # Read deaths data for regions
  d <- read_obs_data()
  regions<-unique(as.factor(d$country))
  
  # Read ifr 
  ifr.by.country <- read_ifr_data(unique(d$country))
  popt <- ifr.by.country[which(ifr.by.country$country == "Italy"),]$popt
  
  ar <- infections.by.date/popt
  ar.lower <- lower.infections.by.date/popt
  ar.upper <- upper.infections.by.date/popt

  italy_total_infected = data.frame(regions = "Italy", mean = ar, li = ar.lower, ui = ar.upper)
  print(italy_total_infected)
  total_infected <- bind_rows(total_infected, italy_total_infected)

  total_infected$text = sprintf("%.02f%% [%.02f%%-%.02f%%]",
                                total_infected$mean*100,total_infected$li*100,total_infected$ui*100)
  
  print(total_infected[order(total_infected$regions),c("regions","text")])
  print(total_infected[order(-total_infected$mean),c("regions","text")])
  
  table<-total_infected[order(-total_infected$mean),c("regions","text")]
  return(table)
}

make_table_simulation <- function(filename, date_till_percentage){
  print(sprintf("Running %s up until %s",filename, date_till_percentage))
  load(filename)
  
  out <- rstan::extract(fit)
  ifrs <- read_ifr_data()
  #df_pop= read.csv("data/popt_ifr3.28.csv", stringsAsFactors = FALSE)
  #df_pop$country[df_pop$country == "United Kingdom"] = "United_Kingdom"
  
  dates_italy <- dates[[which(regions == "Lombardy")]]
  len_dates <- length(dates_italy)
  
  cases <- vector("list", length = length(regions))
  total_cases <- vector("list", length = length(regions))
  total_cases_ui <- vector("list", length = length(regions))
  total_cases_li <- vector("list", length = length(regions))
  deaths <- vector("list", length = length(regions))
  total_deaths <- vector("list", length = length(regions))
  rt <- vector("list", length = length(regions))
  fraction_infected <- vector("list", length = length(regions))
  fraction_infected_li <- vector("list", length = length(regions))
  fraction_infected_ui <- vector("list", length = length(regions))
  fraction_obs_infected <- vector("list", length = length(regions))
  fraction_total_obs_infected <- vector("list", length = length(regions))
  y <- vector("list", length = length(regions))
  
  for(i in 1:length(regions)) {
    Country = regions[i]
    print(Country)
    x = dates[[i]]
    padding <- len_dates - length(dates[[i]])
    
    cases[[i]] = c(round(colMeans(estimated_cases_raw[,1:length(x),i])))
    
    total_cases[[i]] = c( round(cumsum(colMeans(estimated_cases_raw[,1:length(x),i]))))
    # chk = c(round((colMeans(rowCumsums(estimated_cases_raw[,1:length(x),i])))))
    
    total_cases_li[[i]] = c(
      round((colQuantiles(rowCumsums(estimated_cases_raw[,1:length(x),i]),probs=.025))))
    total_cases_ui[[i]] = c(
      round((colQuantiles(rowCumsums(estimated_cases_raw[,1:length(x),i]),probs=.975))))
    
    deaths[[i]] =  c(round(colMeans(estimated_deaths_raw[,1:length(x),i])))
    total_deaths[[i]] =  c(round(cumsum(colMeans(estimated_deaths_raw[,1:length(x),i]))))
    rt[[i]] = c( colMeans(out$Rt_adj[,1:length(x),i]))
    
    fraction_infected[[i]] = c(total_cases[[i]]/ ifrs$popt[ifrs$country==Country])
    fraction_infected_li[[i]] = c(total_cases_li[[i]]/ ifrs$popt[ifrs$country==Country])
    fraction_infected_ui[[i]] = c(total_cases_ui[[i]]/ ifrs$popt[ifrs$country==Country])
    total_cases[[i]]  = c(total_cases[[i]])
  }
  
  cases <- do.call(rbind, cases)
  cases_df <- as.data.frame(cases)
  names(cases_df) <- dates_italy
  cases_df$regions <- regions
  # write.csv(cases_df, "figures/cases.csv")
  
  total_cases <- do.call(rbind, total_cases)
  total_cases_df <- as.data.frame(total_cases)
  names(total_cases_df) <- dates_italy
  total_cases_df$regions <- regions
  # write.csv(total_cases_df, "figures/total_cases.csv")
  
  deaths <- do.call(rbind, deaths)
  deaths_df <- as.data.frame(deaths)
  names(deaths_df) <- dates_italy
  deaths_df$regions <- regions
  # write.csv(deaths_df, "figures/deaths.csv")
  
  total_deaths <- do.call(rbind, total_deaths)
  total_deaths_df <- as.data.frame(total_deaths)
  names(total_deaths_df) <- dates_italy
  total_deaths_df$regions <- regions
  # write.csv(total_deaths_df, "figures/total_deaths.csv")
  
  rt <- do.call(rbind, rt)
  rt_df <- as.data.frame(rt)
  names(rt_df) <- dates_italy
  rt_df$regions <- regions
  # write.csv(rt_df, "figures/rt.csv")
  
  fraction_infected <- do.call(rbind, fraction_infected)
  fraction_infected_df <- as.data.frame(fraction_infected)
  names(fraction_infected_df) <- dates_italy
  fraction_infected_df$regions <- regions
  # write.csv(fraction_infected_df, "figures/fraction_infected.csv")
  
  fraction_infected_li <- do.call(rbind, fraction_infected_li)
  fraction_infected_li_df <- as.data.frame(fraction_infected_li)
  names(fraction_infected_li_df) <- dates_italy
  fraction_infected_li_df$regions <- regions
  # write.csv(fraction_infected_li_df, "figures/fraction_infected_li.csv")
  
  fraction_infected_ui <- do.call(rbind, fraction_infected_ui)
  fraction_infected_ui_df <- as.data.frame(fraction_infected_ui)
  names(fraction_infected_ui_df) <- dates_italy
  fraction_infected_ui_df$regions <- regions
  # write.csv(fraction_infected_ui_df, "figures/fraction_infected_ui.csv")
  
  total_infected = data.frame(regions=regions,mean=fraction_infected[,dates_italy == date_till_percentage],
                              li=fraction_infected_li[,dates_italy == date_till_percentage],ui=fraction_infected_ui[,dates_italy == date_till_percentage])
  total_infected$text = sprintf("%.02f%% [%.02f%%-%.02f%%]",
                                total_infected$mean*100,total_infected$li*100,total_infected$ui*100)
  print(total_infected[order(total_infected$regions),c("regions","text")])
  
  # write.csv(total_infected,pasteo("results/total_infected_",date_till_percentage,".csv"),row.names=F)

  return (total_infected )
}
