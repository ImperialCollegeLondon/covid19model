library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)

source("utils/arg-parser.r")

read_obs_data <- function(countries, file_list=c('data/COVID-19-up-to-date.rds'), max_date=""){
  # Read the deaths and cases data
  d <- trim_data_to_date_range(
    do.call('rbind', lapply(data_files, readRDS)),
    max_date  # optional arguments allow data customisation
  )
  colnames(d)[colnames(d) == "Countries.and.territories"] <- "Country"
  tryCatch({
    d <-d[d$Country %in% countries$Regions, c(1,5,6,7)]
  },error = function(e) {
    d <-d[d$Country %in% names(countries), c(1,5,6,7)]
  })
  d$DateRep <- as.Date(d$DateRep, format = '%d/%m/%Y')
  return(d)
}

read_ifr_data <- function(){
  ifr.by.country <- read.csv("data/popt_ifr.csv")
  ifr.by.country$country <- as.character(ifr.by.country[,2])
  ifr.by.country$country[ifr.by.country$country == "United Kingdom"] <- "United_Kingdom"
  return(ifr.by.country)
  
}