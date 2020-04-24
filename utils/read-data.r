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

read_interventions <- function(countries){
  interventions = read.csv('data/interventions.csv', stringsAsFactors = FALSE)
  names_interventions = c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
  interventions <- interventions[interventions$Type %in% names_interventions,]
  interventions <- interventions[,c(1,2,4)]
  interventions <- spread(interventions, Type, Date.effective)
  names(interventions) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
  interventions <- interventions[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
  interventions$schools_universities <- as.Date(interventions$schools_universities, format = "%d.%m.%Y")
  interventions$lockdown <- as.Date(interventions$lockdown, format = "%d.%m.%Y")
  interventions$public_events <- as.Date(interventions$public_events, format = "%d.%m.%Y")
  interventions$self_isolating_if_ill <- as.Date(interventions$self_isolating_if_ill, format = "%d.%m.%Y")
  interventions$social_distancing_encouraged <- as.Date(interventions$social_distancing_encouraged, format = "%d.%m.%Y")
  ## using interventions as dates we want
  interventions$schools_universities[interventions$schools_universities > interventions$lockdown] <- interventions$lockdown[interventions$schools_universities > interventions$lockdown]
  interventions$public_events[interventions$public_events > interventions$lockdown] <- interventions$lockdown[interventions$public_events > interventions$lockdown]
  interventions$social_distancing_encouraged[interventions$social_distancing_encouraged > interventions$lockdown] <- interventions$lockdown[interventions$social_distancing_encouraged > interventions$lockdown]
  interventions$self_isolating_if_ill[interventions$self_isolating_if_ill > interventions$lockdown] <- interventions$lockdown[interventions$self_isolating_if_ill > interventions$lockdown]
  return(interventions)
}
