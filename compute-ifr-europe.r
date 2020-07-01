### This code was written by Patrick Walker and Charlie for the LMIC global report
### Edited by Ettie for European setting

# Load Required Packages and Source Functions 
library(tidyverse)
library(readxl)
library(socialmixr)
library(dfoptim)

source("utils/get-ar.r")

# Loading in Population Data and Severity Parameters
demog_WPP <- readRDS('data/country-inputs.rds')
pop_columns <- grep("pop", names(demog_WPP))
severity_inputs <- readRDS('data/severity-inputs.rds')
IFRs <- severity_inputs$IFR_adj

# Set Parameters for Optimisation 
R <- 3.28
guess_hom <- 0.85 # guess homogeneous attack rate
guess_modifier <- 0.7 # guess a lower bound for extent age-specific matrix alters average attack rate
reiterates <- 3
iterates <- 10000
restarts <- 10
tol <- 1e-07
init <- 1

country_data <- read.csv("data/popt_ifr.csv", stringsAsFactors = FALSE)
countries <- unique(country_data$country)

IFR <- vector(length = length(countries))
total_pop <- vector(length = length(countries))

for (i in 1:length(countries)){
  # Pick the state You want and Extract Population
  country <- countries[i]
  idx <- which(demog_WPP$Country_or_region == country)
  raw_country_pop <- unlist(demog_WPP[idx, pop_columns])
  country_pop <- c(raw_country_pop[1:15], sum(raw_country_pop[16:21])) * 1000
  
  # Pick the Country You want and Extract Relevant Contact Matrix
  #   Note: Only limited countries have contact matrices available.
  # Contact matrices are taken from PolyMod apart for France
  # Mossong J, Hens N, Jit M, Beutels P, Auranen K, Mikolajczyk R, Massari M, Salmaso S, Tomba GS, Wallinga J, Heijne J, Sadkowska-Todys M, Rosinska M, Edmunds WJ (2017). “POLYMOD social contact data.” doi:10.5281/zenodo.1157934 (URL: https://doi.org/10.5281/zenodo.1157934), Version 1.1.
  contact_mat_list<-readRDS("data/contact-matrices.rds")
  contact_mat <- data.matrix((contact_mat_list[[demog_WPP$Matrix[idx]]]))
  
  # Processing the Contact Matrix to Generate a Probability Matrix 
  # (i.e. Likelihood a person in each age group mixes with people in a different age group)
  MIJ <- t(sapply(seq(country_pop),function(x){
    contact_mat[x,]*country_pop[x]
  }))
  adjust_mat<-(MIJ+t(MIJ))/2
  new_mix_mat<-t(sapply(seq(country_pop),function(x){
    adjust_mat[x,]/country_pop[x]
  }))
  c_mat<-t(sapply(seq(country_pop),function(x){
    new_mix_mat[x,]/sum(new_mix_mat[x,])
  }))
  
  ai <- rowSums(new_mix_mat)
  ng_eigen <- Re(eigen(new_mix_mat)$val[1])
  rmod <- R/ng_eigen*ai
  tot_pop <- sum(country_pop)
  total_pop[i] <- tot_pop
  
  # Running an Optimiser to Get the Number Infected by Age for Each Age Group
  x <- get_AR(R = R, rmod = rmod, c_mat = c_mat, demog = country_pop, init = init, guess_hom = guess_hom,
              guess_modifiers = guess_modifier, iterates = iterates, reiterates = reiterates,
              restarts = restarts, tol = tol)
  
  # Number infected and attack rate for 5 year age bands up to 75+
  number_inf_by_age <- x$par
  attack_rates_by_age <- x$par/country_pop
  #plot(attack_rates_by_age, ylim = c(0, 1))
  
  # Splitting up 75+ into 75-80 and 80+ to incorporate the age-specific IFRs for these two
  # groups that we have
  infs_75_80 <- number_inf_by_age[16] * raw_country_pop[16]/(sum(raw_country_pop[16:21]))
  infs_80_plus <- number_inf_by_age[16] * sum(raw_country_pop[17:21])/(sum(raw_country_pop[16:21]))
  number_inf_by_age[16] <- infs_75_80
  number_inf_by_age[17] <- infs_80_plus
  
  
  
  # Calculating the number of Deaths in Each Age Group
  deaths <- number_inf_by_age * IFRs
  IFR[i] <- sum(deaths)/sum(number_inf_by_age)
}

ifrs <- data.frame("country" = countries, "popt" = total_pop, "ifr" = IFR)

