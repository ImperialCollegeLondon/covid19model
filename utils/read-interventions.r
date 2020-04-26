library(dplyr)
library(tidyr)

source("utils/arg-parser.r")

read_interventions <- function (interventions_file, max_date=""){

    interventions <- trim_data_to_date_range(
        read.csv(interventions_file, stringsAsFactors = FALSE),
        max_date, date_field="Date.effective",
        format_field='%d.%m.%Y', format_max='%d/%m/%y'
    )

    # Modify names for interventions
    names_interventions <- c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
    interventions <- interventions %>%
      filter((Type %in% names_interventions))
    interventions <- interventions[,c(1,2,4)]
    interventions <- spread(interventions, Type, Date.effective)
    intervention_dates <- c(
        'lockdown',
        'public_events',
        'schools_universities',
        'self_isolating_if_ill',
        'social_distancing_encouraged'
     )
    names(interventions) <- c('Country', intervention_dates)
    # Reorder interventions
    interventions <- interventions[c('Country',
                               'schools_universities', 
                               'self_isolating_if_ill', 
                               'public_events', 
                               'lockdown', 
                               'social_distancing_encouraged'
                               )]
    # Any empty intervention is put at a very late data
    interventions[is.na(interventions)] = format(Sys.Date()+3000, format="%d.%m.%Y")
    interventions <- interventions_preprocess_dates(interventions, intervention_dates)

    return(interventions)
}

interventions_preprocess_dates <- function(interventions, intervention_dates){
    # Format dates consistently
    for (intervene in intervention_dates) {
      interventions[[intervene]] <- as.Date(interventions[[intervene]], format = "%d.%m.%Y")
    }
    # Apply actions less stringent than a lockdown at the same time as the
    # lockdown if not done already
    for (intervene in intervention_dates) {
      lockdown_passed <- interventions[[intervene]] > interventions$lockdown        
      interventions[[intervene]][lockdown_passed] <- interventions$lockdown[lockdown_passed]
    }
    return(interventions)
}


## TEST the equivalence to the preceding implementation.

interventions_read_deprecated <- function (interventions_file){
    ## Version fo preprocessing from base.r
    interventions <- read.csv(interventions_file, stringsAsFactors = FALSE)
    # Modify names for interventions
    names_interventions <- c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
    interventions <- interventions %>%
      filter((Type %in% names_interventions))
    interventions <- interventions[,c(1,2,4)]
    interventions <- spread(interventions, Type, Date.effective)
    names(interventions) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
    interventions <- interventions[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
    # Format dates
    interventions$schools_universities <- as.Date(interventions$schools_universities, format = "%d.%m.%Y")
    interventions$lockdown <- as.Date(interventions$lockdown, format = "%d.%m.%Y")
    interventions$public_events <- as.Date(interventions$public_events, format = "%d.%m.%Y")
    interventions$self_isolating_if_ill <- as.Date(interventions$self_isolating_if_ill, format = "%d.%m.%Y")
    interventions$social_distancing_encouraged <- as.Date(interventions$social_distancing_encouraged, format = "%d.%m.%Y")
    # Apply actions less stringent than a lockdown at the same time as the
    # lockdown if not done already
    print(interventions)
    interventions$schools_universities[interventions$schools_universities > interventions$lockdown] <- interventions$lockdown[interventions$schools_universities > interventions$lockdown]
    interventions$public_events[interventions$public_events > interventions$lockdown] <- interventions$lockdown[interventions$public_events > interventions$lockdown]
    interventions$social_distancing_encouraged[interventions$social_distancing_encouraged > interventions$lockdown] <- interventions$lockdown[interventions$social_distancing_encouraged > interventions$lockdown]
    interventions$self_isolating_if_ill[interventions$self_isolating_if_ill > interventions$lockdown] <- interventions$lockdown[interventions$self_isolating_if_ill > interventions$lockdown]
    print(interventions)
    return(interventions)

}

TEST_interventions_read_check_equivalence <- function(interventions_file){
    print("==========================================================")
    print("Testing 'r-utils/read-covarites.r'")
    print("==========================================================")

    interventions_new <- interventions_read('data/interventions.csv')
    interventions_deprecated <- interventions_read_deprecated('data/interventions.csv')

    if(all(interventions_new==interventions_deprecated)){
        print("TEST PASSED: Old and new interventions reading processes are equivalent.")
        return(0)
    } else {
        print("TEST FAILED: Covariates reading processes are NOT equivalent.")
        return(-1)
    }
}