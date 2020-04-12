library(dplyr)
library(tidyr)


covariates_read <- function (covariates_file){

	covariates <- read.csv(covariates_file, stringsAsFactors = FALSE)
	# Modify names for covariates
	names_covariates <- c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
	covariates <- covariates %>%
	  filter((Type %in% names_covariates))
	covariates <- covariates[,c(1,2,4)]
	covariates <- spread(covariates, Type, Date.effective)
	names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
	covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
	# processing copied from base.r
	covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
	covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
	covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
	covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
	covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")
	covariates$schools_universities[covariates$schools_universities > covariates$lockdown] <- covariates$lockdown[covariates$schools_universities > covariates$lockdown]
	covariates$public_events[covariates$public_events > covariates$lockdown] <- covariates$lockdown[covariates$public_events > covariates$lockdown]
	covariates$social_distancing_encouraged[covariates$social_distancing_encouraged > covariates$lockdown] <- covariates$lockdown[covariates$social_distancing_encouraged > covariates$lockdown]
	covariates$self_isolating_if_ill[covariates$self_isolating_if_ill > covariates$lockdown] <- covariates$lockdown[covariates$self_isolating_if_ill > covariates$lockdown]
	return(covariates)

}
