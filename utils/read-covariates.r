library(dplyr)
library(tidyr)

source("utils/arg-parser.r")

covariates_read <- function (covariates_file, max_date=""){

    covariates <- trim_data_to_date_range(
        read.csv(covariates_file, stringsAsFactors = FALSE),
        max_date, date_field="Date.effective",
        format_field='%d.%m.%Y', format_max='%d/%m/%y'
    )

    # Modify names for covariates
    names_covariates <- c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
    covariates <- covariates %>%
      filter((Type %in% names_covariates))
    covariates <- covariates[,c(1,2,4)]
    covariates <- spread(covariates, Type, Date.effective)
    covariate_dates <- c(
        'lockdown',
        'public_events',
        'schools_universities',
        'self_isolating_if_ill',
        'social_distancing_encouraged'
     )
    names(covariates) <- c('Country', covariate_dates)
    # Reorder covariates
    covariates <- covariates[c('Country',
                               'schools_universities', 
                               'self_isolating_if_ill', 
                               'public_events', 
                               'lockdown', 
                               'social_distancing_encouraged'
                               )]
    # Any empty covariate is put at a very late data
    covariates[is.na(covariates)] = format(Sys.Date()+3000, format="%d.%m.%Y")
    covariates <- covariates_preprocess_dates(covariates, covariate_dates)

    return(covariates)
}

covariates_preprocess_dates <- function(covariates, covariate_dates){
    # Format dates consistently
    for (covar in covariate_dates) {
      covariates[[covar]] <- as.Date(covariates[[covar]], format = "%d.%m.%Y")
    }
    # Apply actions less stringent than a lockdown at the same time as the
    # lockdown if not done already
    for (covar in covariate_dates) {
      lockdown_passed <- covariates[[covar]] > covariates$lockdown        
      covariates[[covar]][lockdown_passed] <- covariates$lockdown[lockdown_passed]
    }
    return(covariates)
}


## TEST the equivalence to the preceding implementation.

covariates_read_deprecated <- function (covariates_file){
    ## Version fo preprocessing from base.r
    covariates <- read.csv(covariates_file, stringsAsFactors = FALSE)
    # Modify names for covariates
    names_covariates <- c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
    covariates <- covariates %>%
      filter((Type %in% names_covariates))
    covariates <- covariates[,c(1,2,4)]
    covariates <- spread(covariates, Type, Date.effective)
    names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
    covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
    # Format dates
    covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
    covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
    covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
    covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
    covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")
    # Apply actions less stringent than a lockdown at the same time as the
    # lockdown if not done already
    print(covariates)
    covariates$schools_universities[covariates$schools_universities > covariates$lockdown] <- covariates$lockdown[covariates$schools_universities > covariates$lockdown]
    covariates$public_events[covariates$public_events > covariates$lockdown] <- covariates$lockdown[covariates$public_events > covariates$lockdown]
    covariates$social_distancing_encouraged[covariates$social_distancing_encouraged > covariates$lockdown] <- covariates$lockdown[covariates$social_distancing_encouraged > covariates$lockdown]
    covariates$self_isolating_if_ill[covariates$self_isolating_if_ill > covariates$lockdown] <- covariates$lockdown[covariates$self_isolating_if_ill > covariates$lockdown]
    print(covariates)
    return(covariates)

}

TEST_covariates_read_check_equivalence <- function(covariates_file){
    print("==========================================================")
    print("Testing 'r-utils/read-covarites.r'")
    print("==========================================================")

    covariates_new <- covariates_read('data/interventions.csv')
    covariates_deprecated <- covariates_read_deprecated('data/interventions.csv')

    if(all(covariates_new==covariates_deprecated)){
        print("TEST PASSED: Old and new covariates reading processes are equivalent.")
        return(0)
    } else {
        print("TEST FAILED: Covariates reading processes are NOT equivalent.")
        return(-1)
    }
}