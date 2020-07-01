### This code was written by for the LMIC global report andd edited for the covid19 model 

# Load Required Packages and Source Functions 
library(tidyverse) 
library(readxl) 
library(socialmixr)
library(dfoptim)
source("utils/get-ar.r")
#----------------------------------------------------------------------------------------------------
# Loading in Population Data and Severity Parameters
demog <- readRDS("usa/data/us_states_wide.rds")
pop_columns <- 3:20
severity_inputs <- readRDS('data/severity-inputs.rds')
IFRs <- severity_inputs$IFR_adj

# Extract relevant contact matrix
#   Note: Only limited countries have contact matrices available.
contact_mat <- contact_matrix(polymod, countries = "United Kingdom", 
                              age.limits = seq(0, 75, by = 5))$matrix

# Set Parameters for Optimisation 
R <- 3.28
guess_hom <- 0.85 # guess homogeneous attack rate
guess_modifier <- 0.7 # guess a lower bound for extent age-specific matrix alters average attack rate
reiterates <- 3
iterates <- 10000
restarts <- 10
tol <- 1e-07
init <- 1

states <- demog$Region
IFR <- vector(length = length(states))

for (i in 1:length(states)){
  # Extract Population
  raw_state_pop <- unlist(demog[demog$Region == states[i], pop_columns])
  state_pop <- c(raw_state_pop[1:15], sum(raw_state_pop[16:18]))
  
  # Processing the Contact Matrix to Generate a Probability Matrix 
  # (i.e. Likelihood a person in each age group mixes with people in a different age group)
  MIJ <- t(sapply(seq(state_pop),function(x){
    contact_mat[x,]*state_pop[x]
  }))
  adjust_mat<-(MIJ+t(MIJ))/2
  new_mix_mat<-t(sapply(seq(state_pop),function(x){
    adjust_mat[x,]/state_pop[x]
  }))
  c_mat<-t(sapply(seq(state_pop),function(x){
    new_mix_mat[x,]/sum(new_mix_mat[x,])
  }))
  
  ai <- rowSums(new_mix_mat)
  ng_eigen <- Re(eigen(new_mix_mat)$val[1])
  rmod <- R/ng_eigen*ai
  tot_pop <- sum(state_pop)
  
  # Running an Optimiser to Get the Number Infected by Age for Each Age Group
  x <- get_AR(R = R, rmod = rmod, c_mat = c_mat, demog = state_pop, init = init, guess_hom = guess_hom,
              guess_modifiers = guess_modifier, iterates = iterates, reiterates = reiterates,
              restarts = restarts, tol = tol)
  
  # Number infected and attack rate for 5 year age bands up to 75+
  number_inf_by_age <- x$par
  
  # Splitting up 75+ into 75-80 and 80+ to incorporate the age-specific IFRs for these two
  # groups that we have
  infs_75_80 <- number_inf_by_age[16] * raw_state_pop[16]/(sum(raw_state_pop[16:18]))
  infs_80_plus <- number_inf_by_age[16] * sum(raw_state_pop[17:18])/(sum(raw_state_pop[16:18]))
  number_inf_by_age[16] <- infs_75_80
  number_inf_by_age[17] <- infs_80_plus
  
  # Calculating the number of Deaths in Each Age Group
  deaths <- number_inf_by_age * IFRs
  IFR[i] <- sum(deaths)/sum(number_inf_by_age)
}

ifrs <- data.frame("state" = states, "IFR" = IFR)
