library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)


process_covariates <- function(states, mobility, intervention = NULL, 
                               interventions_lifted  = readRDS('usa/data/covariates_ended.RDS'),
                               death_data, ifr_by_state, 
                               num_days_sim, interventions, formula, 
                               formula_partial_regional, formula_partial_state){
  # Read in serial interval
  serial_interval = read.csv("data/serial_interval.csv")
  
  pad_serial.interval <- data.frame(
    "X"=(length(serial_interval$fit)+1):200,
    "fit"=rep(1e-17, 200)
  )
  serial_interval = rbind(serial_interval, pad_serial.interval)
  
  ## read population
  df_pop <- readRDS("usa/data/us_population.rds")
  df_pop <- df_pop[which(!is.na(df_pop$code)),]
  
  # various distributions required for modelling
  mean1 <- 5.1; cv1 <- 0.86; # infection to onset
  mean2 <- 17.8; cv2 <- 0.45 # onset to death
  x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
  x2 <- rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
  f_cached <- ecdf(x1+x2) # empirical cumulative distribtion function
  
  dates <- list()
  reported_cases <- list()
  reported_deaths <- list()
  stan_data <- list(M = length(states), # Number of states
                    N0 = 6, # Number of days in seeding
                    N = NULL, # Number of time points with data
                    N2 = NULL, # Number of time points with data plus forecast
                    cases = NULL, # daily cases
                    deaths =  NULL, # daily deaths
                    f = NULL, # Hazard times survival
                    X = NULL, # Covariates
                    P = NULL, # Number of covariates
                    SI = serial_interval$fit[1:num_days_sim], # Serial interval fit
                    EpidemicStart = NULL, # Date to start epidemic in each state
                    pop = NULL,
                    Q = NULL,
                    Region = NULL,
                    Pop_density = NULL) # state population
  
  covariate_list <- list()
  covariate_list_partial_regional <- list()
  covariate_list_partial_state <- list()
  
  k=1
  # same_mobility_states <- c('ME','VT','CT','NJ','MA','RI','NH','PA')
  same_mobility_states <- c('CT')
  # states for transit
  transit_states <- c('NY', 'DC', 'MA', 'CA', 'WA', 'IL', 'MD', 'NJ')
  colnames(interventions_lifted) <- c("StatePostal_lif", "EmergDec_lif", "GathRecomAny_lif", "GathRestrictAny_lif", 
                                      "OtherBusinessClose_lif", "Quarantine_lif", "RestaurantRestrict_lif", "SchoolClose_lif", "StayAtHome_lif")
  interventions_lifted$April1 = as.Date("2020-04-01")
  for(State in states) {
    # Selects correct IFR for each country
    ifr_state <- ifr_by_state$ifr[ifr_by_state$code == State]
    # Selects correct Interventions for each region
    intervention_state <- interventions[interventions$StatePostal == State, c(2:9)]
    # Selects correct Interventions Lifted for each region
    intervention_lifted_state <- interventions_lifted[interventions_lifted$StatePostal == State, c(2:10)]
    # Selects mobility data for each state
    covariates_state <- mobility[which(mobility$code == State),]
    # checking if states part of same mobility
    # if (State %in% same_mobility_states){
    #   # covariates_state <- mobility[which(mobility$code == 'NY'),]
    #   intervention_state <- interventions[interventions$StatePostal == 'NY', c(2:7)]
    # }
    # Selects population for each state
    pop_state <-  df_pop[df_pop$code==State, names(df_pop) == "Total"][[1]]
    # Subsets data by state 
    data_state <- death_data[death_data$code == State,]
    # Maks dates numeric
    data_state$t <- decimal_date(data_state$date)
    # Sorts data into time order
    data_state <- data_state[order(data_state$t),]
    
    # Selects index when first case
    index <- which(data_state$daily_cases > 0)[1]
    index1 <-  which(data_state$cumulative_deaths >= 10)[1] # also 5
    if(is.na(index1)) {
      index1 <-  which(data_state$cumulative_deaths >= 5)[1] 
    }
    index2 <- index1 - 30 # was 30. need more thought on this
    print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d", 
                  index, index2))
    
    
    # Works out state start point
    data_state <- data_state[index2:nrow(data_state),]
    # Work out how much padding need at end
    N <- length(data_state$daily_cases)
    forecast_length <- num_days_sim - N[[1]]
    print(sprintf("%s has %d days of data. Forecasting %d days", State, N, forecast_length))
    if(forecast_length < 0) {
      stop('num_days_sim is not long enough to model all data.  Increase forecast')
    }
    
    # IFR is the overall probability of dying given infection
    convolution = function(u) (ifr_state * f_cached(u))
    
    f = rep(0,num_days_sim) # f is the probability of dying on day i given infection
    f[1] = (convolution(1.5) - convolution(0))
    for(i in 2:num_days_sim) {
      f[i] = (convolution(i+.5) - convolution(i-.5)) 
    }
    
    
    deaths <- c(data_state$daily_deaths, rep(-1,forecast_length))
    cases <- c(data_state$daily_cases, rep(-1,forecast_length))
    
    # Format the interventions
    # Find minimum date for the data
    min_date <- min(data_state$date)
    num_pad <- (min(covariates_state$date) - min_date[[1]])[[1]]
    len_mobility <- ncol(covariates_state)
    padded_covariates <- pad_mobility(len_mobility, num_pad, min_date, covariates_state, forecast_length, data_state, State)
    # Indexes 1 or 0 if intervention has occured
    for (ii in 1:ncol(intervention_state)) {
      covariate <- names(intervention_state)[ii]
      data_state[covariate] <- (data_state$date >= intervention_state[1,covariate])*1  # should this be > or >=?
    }
    # Indexes 1 or 0 if intervention lifted
    for (ii in 1:ncol(intervention_lifted_state)) {
      covariate <- names(intervention_lifted_state)[ii]
      data_state[covariate] <- (data_state$date >= intervention_lifted_state[1,covariate])*1  # should this be > or >=?
    }
    # Extend covariates to cover forecast
    covariates_forecast <- as.data.frame(data_state[, colnames(intervention_state)])
    covariates_forecast[N:(N + forecast_length),] <- covariates_forecast[N,]
    covariates_forecast[is.na(covariates_forecast)] <- 0
    # Extend covariates_lifted to cover forecast
    covariates_lifted_forecast <- as.data.frame(data_state[, colnames(intervention_lifted_state)])
    covariates_lifted_forecast[N:(N + forecast_length),] <- covariates_lifted_forecast[N,]
    covariates_lifted_forecast[is.na(covariates_lifted_forecast)] <- 0
    # Transit covariate
    if(State %in% transit_states){
      transit_usage <- rep(1, (N + forecast_length) )
    } else{
      transit_usage <- rep(0, (N + forecast_length) )
    }
    # creating features
    df_features <- create_features(len_mobility, padded_covariates, covariates_forecast, covariates_lifted_forecast, transit_usage)
    features <- model.matrix(formula, df_features)
    features_partial_regional <- model.matrix(formula_partial_regional, df_features)
    features_partial_state <- model.matrix(formula_partial_state, df_features)
    
    covariate_list[[k]] <- features
    covariate_list_partial_regional[[k]] <- features_partial_regional
    covariate_list_partial_state[[k]] <- features_partial_state
    k <- k+1
    
    ## Append data to stan data
    stan_data$y <- c(stan_data$y, data_state$daily_cases[1]) # just the index case!
    stan_data$EpidemicStart <- c(stan_data$EpidemicStart, index1 + 1 - index2)
    stan_data$pop <- c(stan_data$pop, pop_state)
    stan_data$f <- cbind(stan_data$f,f)
    stan_data$deaths <- cbind(stan_data$deaths, deaths)
    stan_data$cases <- cbind(stan_data$cases, cases)
    stan_data$N2 <- num_days_sim
    stan_data$N <- c(stan_data$N, N)
    stan_data$Region <- c(stan_data$Region, data_state$region_code[1])
    stan_data$Pop_density <- c(stan_data$Pop_density, log(data_state$pop_density[1]))
    
    # Saves other data for each state
    dates[[State]] <- data_state$date
    reported_cases[[State]] <- data_state$daily_cases
    reported_deaths[[State]] <- data_state$daily_deaths
  }
  
  
  stan_data$P = dim(features)[2]
  stan_data$X = array(NA, dim = c(stan_data$M , stan_data$N2 ,stan_data$P ))
  stan_data$P_partial_regional = dim(features_partial_regional)[2]
  stan_data$P_partial_state = dim(features_partial_state)[2]
  if(stan_data$P_partial_regional==0){
    stan_data$X_partial_regional = array(0, dim = c(stan_data$M , stan_data$N2, 1))
  }
  else{
    stan_data$X_partial_regional = array(NA, dim = c(stan_data$M , stan_data$N2 ,stan_data$P_partial_regional))
  }
  if(stan_data$P_partial_state==0){
    stan_data$X_partial_state = array(0, dim = c(stan_data$M , stan_data$N2, 1))
  }
  else{
    stan_data$X_partial_state = array(NA, dim = c(stan_data$M , stan_data$N2 ,stan_data$P_partial_state))
  }
  
  for (i in 1:stan_data$M){
    stan_data$X[i,,] = covariate_list[[i]]
    if(stan_data$P_partial_regional != 0)
      stan_data$X_partial_regional[i,,] = covariate_list_partial_regional[[i]]
    if(stan_data$P_partial_state != 0)
      stan_data$X_partial_state[i,,] = covariate_list_partial_state[[i]]
  }
  if(stan_data$P_partial_regional == 0)
    stan_data$P_partial_regional = 1
  if(stan_data$P_partial_state == 0)
    stan_data$P_partial_state = 1
  stan_data$Pop_density <- scale(stan_data$Pop_density )[,1]
  stan_data$Q <- max(stan_data$Region)
  stan_data$W <- ceiling(stan_data$N2/7)
  stan_data$week_index <- matrix(1,stan_data$M,stan_data$N2)
  for(state.i in 1:stan_data$M) {
    stan_data$week_index[state.i,] <- rep(2:(stan_data$W+1),each=7)[1:stan_data$N2]
    last_ar_week = which(dates[[state.i]]==max(death_data$date) -28)
    stan_data$week_index[state.i,last_ar_week:ncol(stan_data$week_index)] <-  stan_data$week_index[state.i,last_ar_week]
  }
  return(list("stan_data" = stan_data, "dates" = dates, "reported_cases" = reported_cases, 
              "reported_deaths" = reported_deaths))
}

pad_mobility <- function(len_mobility, num_pad, min_date, covariates_state, forecast_length, data_state, State){
  if (num_pad <= 0){
    covariates_state <- covariates_state[covariates_state$date >=min_date, ]
    pad_dates_end <- max(covariates_state$date) + 
      days(1:(forecast_length - (min(data_state$date) - min(covariates_state$date)) + 
                (max(data_state$date) - max(covariates_state$date))))
    for_length <- length(pad_dates_end)
    
    len_covariates <- length(covariates_state$grocery.pharmacy)
    padded_covariates <- data.frame("code" = rep(State, length(covariates_state$date) + for_length),
                                    "date" = c(covariates_state$date, pad_dates_end),
                                    "grocery.pharmacy" = c(covariates_state$grocery.pharmacy, 
                                                           rep(median(covariates_state$grocery.pharmacy[(len_covariates-7):len_covariates],na.rm = TRUE),
                                                               for_length)),
                                    "parks" = c(covariates_state$parks, 
                                                rep(median(covariates_state$parks[(len_covariates-7):len_covariates],na.rm = TRUE), for_length)), 
                                    "residential" = c(covariates_state$residential, 
                                                      rep(median(covariates_state$residential[(len_covariates-7):len_covariates], na.rm = TRUE), for_length)),
                                    "retail.recreation" = c(covariates_state$retail.recreation, 
                                                            rep(median(covariates_state$retail.recreation[(len_covariates-7):len_covariates],na.rm = TRUE), 
                                                                for_length)),
                                    "transitstations" = c(covariates_state$transitstations, 
                                                          rep(median(covariates_state$transitstations[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                              for_length)),
                                    "workplace" = c(covariates_state$workplace, 
                                                    rep(median(covariates_state$workplace[(len_covariates-7):len_covariates], na.rm = TRUE), for_length)))  

  } else {
    pad_dates_front <- min_date + days(1:num_pad-1)
    pad_dates_end <- max(covariates_state$date) + 
      days(1:(forecast_length + (max(data_state$date) - max(covariates_state$date))))
    for_length <- length(pad_dates_end)

    len_covariates <- length(covariates_state$grocery.pharmacy)
    padded_covariates <- data.frame("code" = rep(State, num_pad + length(covariates_state$date) + for_length),
                                    "date" = c(pad_dates_front, covariates_state$date, pad_dates_end),
                                    "grocery.pharmacy" = c(as.integer(rep(0, num_pad)), covariates_state$grocery.pharmacy, 
                                                           rep(median(covariates_state$grocery.pharmacy[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                               for_length)),
                                    "parks" = c(as.integer(rep(0, num_pad)), covariates_state$parks, 
                                                rep(median(covariates_state$parks[(len_covariates-7):len_covariates],na.rm = TRUE), for_length)), 
                                    "residential" = c(as.integer(rep(0, num_pad)), covariates_state$residential, 
                                                      rep(median(covariates_state$residential[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                          for_length)),
                                    "retail.recreation" = c(as.integer(rep(0, num_pad)), covariates_state$retail.recreation,
                                                            rep(median(covariates_state$retail.recreation[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                                for_length)),
                                    "transitstations" = c(as.integer(rep(0, num_pad)), covariates_state$transitstations, 
                                                          rep(median(covariates_state$transitstations[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                              for_length)),
                                    "workplace" = c(as.integer(rep(0, num_pad)), covariates_state$workplace, 
                                                    rep(median(covariates_state$workplace[(len_covariates-7):len_covariates], na.rm = TRUE), 
                                                        for_length)))
      
  }
  
  return(padded_covariates)
}

create_features <- function(len_mobility, padded_covariates, covariates_forecast, covariates_lifted_forecast, transit_usage){
    return (data.frame('school' = covariates_forecast$SchoolClose, 'emergency' = covariates_forecast$EmergDec, 
               'stayHome' = covariates_forecast$StayAtHome, 'quarantine' = covariates_forecast$Quarantine,
               'gatherRestrict' = covariates_forecast$GathRestrictAny,
               'business' = covariates_forecast$OtherBusinessClose, "restaurant" = covariates_forecast$RestaurantRestrict,
               'firstIntervention' = 1*((covariates_forecast$SchoolClose+
                                           covariates_forecast$StayAtHome+
                                           covariates_forecast$GathRestrictAny+
                                           covariates_forecast$StayAtHome+
                                           covariates_forecast$OtherBusinessClose+
                                           covariates_forecast$RestaurantRestrict) >= 1),
               'school_lifted' = covariates_lifted_forecast$SchoolClose_lif, 'emergency_lifted' = covariates_lifted_forecast$EmergDec_lif, 
               'stayHome_lifted' = covariates_lifted_forecast$StayAtHome_lif, 'quarantine_lifted' = covariates_lifted_forecast$Quarantine_lif,
               'gatherRestrict_lifted' = covariates_lifted_forecast$GathRestrictAny_lif,
               'business_lifted' = covariates_lifted_forecast$OtherBusinessClose_lif,
               "restaurant_lifted" = covariates_lifted_forecast$RestaurantRestrict_lif,
               "April1" = covariates_lifted_forecast$April1,
               'firstIntervention_lifted' = 1*((covariates_lifted_forecast$SchoolClose_lif+
                                                  covariates_lifted_forecast$EmergDec_lif+
                                                  covariates_lifted_forecast$StayAtHome_lif+
                                                  covariates_lifted_forecast$Quarantine_lif+
                                                  covariates_lifted_forecast$GathRestrictAny_lif+
                                                  covariates_lifted_forecast$OtherBusinessClose_lif+
                                                  covariates_lifted_forecast$RestaurantRestrict_lif) >= 1),
               'transit_use' = transit_usage,
               'residential' = padded_covariates$residential, 
               'transit' = padded_covariates$transitstations, 
               'grocery' = padded_covariates$grocery.pharmacy,
               'parks' = padded_covariates$parks,
               'retail' =padded_covariates$retail.recreation,
               'workplace' = padded_covariates$workplace,
               'averageMobility' = rowMeans(padded_covariates[,c("grocery.pharmacy", "retail.recreation", "workplace")], 
                                            na.rm=TRUE))
            )
}
