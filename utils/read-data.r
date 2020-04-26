library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)

read_obs_data <- function(countries){
  # Read the deaths and cases data
  d <- readRDS('data/COVID-19-up-to-date.rds')
  colnames(d)[colnames(d) == "Countries.and.territories"] <- "Country"
  d <-d[d$Country %in% countries$Regions, c(1,5,6,7)]
  d$DateRep <- as.Date(d$DateRep, format = '%d/%m/%Y')
  return(d)
}

read_ifr_data <- function(){
  ifr.by.country <- read.csv("data/popt_ifr.csv")
  ifr.by.country$country <- as.character(ifr.by.country[,2])
  ifr.by.country$country[ifr.by.country$country == "United Kingdom"] <- "United_Kingdom"
  return(ifr.by.country)
  
}