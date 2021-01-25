library(dplyr)

# This file will compute the IFR.  Currently don't know how to do this...

# Reads in population weighted IFRs for 2018 
demog_data <- read.csv("usa/data/weighted_ifrs_states.csv", stringsAsFactors = FALSE)
demog_data$X <- NULL
demog_data <- demog_data[which(demog_data$state != "United States"),]

# Read in state codes
codes <- read.csv("usa/data/states.csv", stringsAsFactors = FALSE)
names(codes) <- c("state", "code")

demog_data <- left_join(demog_data, codes)

names(demog_data) <- c("state", "ifr", "code")

saveRDS(demog_data, "usa/data/weighted_ifr.RDS")
