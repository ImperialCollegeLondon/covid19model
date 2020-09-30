# This code reads in and processes the social intervention spreadsheet from
# https://faculty.washington.edu/cadolph/papers/AABFW2020.pdf?fbclid=IwAR0bXNITLzbgyREI8ZxVz3ft2PRmijohX_0uqkxbtr_boFy8bMoNyOnIEEc

library(lubridate)
library(tidyr)
library(dplyr)

# Read in data
data <- read.csv("usa/data/USstatesCov19distancingpolicy.csv", stringsAsFactors = FALSE)

# Reformat dates
data$DateIssued <- ymd(data$DateIssued)
data$DateEnacted <- ymd(data$DateEnacted)
data$DateExpiry <- ymd(data$DateExpiry)
data$DateEnded <- ymd(data$DateEnded)

# List of states
states <- unique(data$StatePostal)

# Get a list of unique state policy types
policy_headings <- unique(data$StatePolicy)

# Choose interventions to consider - choose those in list above which are StateWide
list_policies <- c("Quarantine", "SchoolClose", "GathRestrictAny", "StayAtHome", "EmergDec")
#list_policies <- policy_headings[which(policy_headings != "GathRecomAny")]
data1 <- data[which(data$StatePolicy %in% list_policies & data$StateWide == 1),]
data2 <- data[which(data$StatePolicy == "GathRecomAny"), ]
data <- rbind(data1, data2)

# Choose the first StateWide policy enactment date 
subset <- NULL
for (i in 1:length(states)){
    state_data <- data[which(data$StatePostal == states[i]),]
    unique_policy <- unique(state_data$StatePolicy)
    for (j in 1:length(unique_policy)){
      policy_state <- state_data[which(state_data$StatePolicy == unique_policy[j]),]
      if (length(policy_state$StatePolicy) == 1){
        subset <- rbind(subset, policy_state)
      } else {
        idx_min <- which.min(policy_state$DateEnacted)
        print(paste0("Have a problem with State ", states[i], " and Policy ", unique_policy[j]))
        subset <- rbind(subset, policy_state[idx_min, ])
      }
  }
}

subset_for_spread <- select(subset, StatePostal, StatePolicy, DateEnacted)
covariates <- spread(subset_for_spread, key = StatePolicy, value = DateEnacted, fill = NA)
for (i in 1:length(covariates$StatePostal)){
  if (is.na(covariates$GathRecomAny[i]) & !is.na(covariates$GathRestrictAny[i])){
    covariates$GathRecomAny[i] = covariates$GathRestrictAny[i]
  }
}


# Save as an RDS
saveRDS(data, "usa/data/interventions_all.RDS")
saveRDS(covariates, "usa/data/covariates.RDS")
write.csv2(covariates, "usa/data/covariates.csv")

# Questions:
# 1) I think we are interested in Date Enacted rather than DateIssued
# 2) Policy had to be implemented at StateWide level before I included it
# 3) If same policy in spreasheet twice - I chose the first date it entered
# 4) For gathring I chose first time a gathering was restricted.

# 5) Do we want to consider covariates such as GDP? Adherence? Political persuasion.