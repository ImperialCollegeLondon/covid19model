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
data$DateEased<- ymd(data$DateEased)
data$DateEnded <- ymd(data$DateEnded)

# List of states
states <- unique(data$StatePostal)

# Get a list of unique state policy types
policy_headings <- unique(data$StatePolicy)

# Choose interventions to consider - choose those in list above which are StateWide
list_policies <- c("Quarantine", "SchoolClose", "GathRestrictAny", "StayAtHome", "EmergDec", 
                   "RestaurantRestrict", "OtherBusinessClose")
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
        #print(paste0("Check State ", states[i], " and Policy ", unique_policy[j]))
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

subset_for_spread <- select(subset, StatePostal, StatePolicy, DateEased, DateEnded) 
for (i in 1:length(subset_for_spread$StatePostal)){
  if (is.na(subset_for_spread$DateEased[i]) & !is.na(subset_for_spread$DateEnded[i])){
    subset_for_spread$DateEased[i] = as.Date(subset_for_spread$DateEnded[i])
  }
}
subset_for_spread$DateEnded <- NULL
covariates_ended <- spread(subset_for_spread, key = StatePolicy, value = DateEased, fill = NA)


covariates$SchoolClose[which(covariates$StayAtHome < covariates$SchoolClose)] <- covariates$StayAtHome[which(covariates$StayAtHome < covariates$SchoolClose)]

# Save as an RDS
saveRDS(data, "usa/data/interventions_all.RDS")
saveRDS(covariates, "usa/data/covariates.RDS")
#write.csv2(covariates, "usa/data/covariates.csv")

saveRDS(covariates_ended, "usa/data/covariates_ended.RDS")
#write.csv2(covariates_ended, "usa/data/covariates_ended.csv")

