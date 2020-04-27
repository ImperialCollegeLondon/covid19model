library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(abind)

process_covariates_region <- function(region_to_country_map, interventions,
    d, ifr.by.country, N2){

  serial.interval = read.csv("data/serial_interval.csv")
  # Pads serial interval with 0 if N2 is greater than the length of the serial
  # interval array
  if (N2 > length(serial.interval$fit)) {
    pad_serial.interval <- data.frame(
      "X"=(length(serial.interval$fit)+1):N2,
      "fit"=rep(1e-17, max(N2-length(serial.interval$fit), 0 ))
    )
    serial.interval = rbind(serial.interval, pad_serial.interval)
  }
  death_thresh_epi_start <- 10
  # various distributions required for modeling
  infection_to_onset <- c("mean"=5.1, "deviation"=0.86)
  onset_to_death <- c("mean"=18.8, "deviation"=0.45)
  # infection-to-onset distribution
  x1 = rgammaAlt(1e6,infection_to_onset["mean"], infection_to_onset["deviation"])
  # onset-to-death distribution
  x2 = rgammaAlt(1e6,onset_to_death["mean"], onset_to_death["deviation"])
  ecdf.saved = ecdf(x1+x2)
  # stan data definition
  stan_data <- list(M=length(names(region_to_country_map)),N=NULL,deaths=NULL,f=NULL,
                   N0=6,cases=NULL,SI=serial.interval$fit[1:N2],features=NULL,
                   EpidemicStart = NULL, pop = NULL)
  reported_cases <- list()
  deaths_by_country <- list()
  covariate_list <- list()
  

  log_simulation_inputs(run_name, region_to_country_map,  ifr.by.country,
    infection_to_onset, onset_to_death)

  preprocess_error = FALSE

  k=1
  # going over each region
  for(Region in names(region_to_country_map))
  {
    Country = region_to_country_map[[Region]]
    print(sprintf("Region: %s in country: %s ",Region,Country))
    if(any(ifr.by.country$country == Region)){
        # to add
      IFR <- ifr.by.country$ifr[ifr.by.country$country == Country]
      region_pop <- ifr.by.country[ifr.by.country$country==Country,]
    } else {
        IFR <- ifr.by.country$ifr[ifr.by.country$country == Country]
        region_pop <- ifr.by.country[ifr.by.country$country==Country,]

    }

    if(!any(d$Country==Region)){
      preprocess_error = TRUE
      message(sprintf(
        "ERROR: Region %s in country %s had no data (region length(region)==0)", Region, Country))
      next
    }
    region <- d[d$Country==Region,]
    region$DateRep <-region$DateRep
    region <-region[order(as.Date(region$DateRep)),]  # ensure date ordering
    
    # padding in raw data backwards ex. portugal
    date_min <- dmy('31/12/2019') 
    if (region$DateRep[1] > date_min){
      print(paste(Region,'In padding death data'))
      pad_days <-region$DateRep[1] - date_min
      pad_dates <- date_min + days(1:pad_days[[1]]-1)
      padded_data <- data.frame("Country" = rep(Region, pad_days),
                                "DateRep" = pad_dates,
                                "Cases" = as.integer(rep(0, pad_days)),
                                "Deaths" = as.integer(rep(0, pad_days)),
                                stringsAsFactors=F)
      
     region <- bind_rows(padded_data,region)
    }
    index = which(region$Cases>0)[1]
    index1 = which(cumsum(region$Deaths)>=death_thresh_epi_start)[1] # also 5
    if (is.na(index1)) {
      preprocess_error = TRUE
      message(sprintf(
        "ERROR: Region %s in country %s has not reached 10 deaths on %s, it cannot be processed\nremove from 'active-countries.cfg' or 'active-regions.cfg'\n",
        Region, Country, max_date))
      next
    }
    index2 = index1-30
    
    print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))
    region=region[index2:nrow(region),]
    stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2)
    stan_data$pop = c(stan_data$pop,region_pop$popt)
    # NPI interventionss are being used
    interventions_region <- interventions[interventions$Country == Country, c(2,3,4,5,6)] # school, self-isolation, public, lockdown, social-distancing
    for (ii in 1:ncol(interventions_region)) {
      covariate = names(interventions_region)[ii]
      region[covariate] <- (region$DateRep >= interventions_region[1,covariate])*1  # should this be > or >=?
    }
    
    dates[[Region]] =region$DateRep
    # hazard estimation
    N = length(region$Cases)
    print(sprintf("%s has %d days of data",Region,N))
    forecast = N2 - N
    if(forecast < 0) {
      print(sprintf("%s: %d", Region, N))
      print("ERROR!!!! increasing N2")
      N2 = N
      forecast = N2 - N
    }
    
    # IFR is the overall probability of dying given infection
    convolution = function(u) (IFR * ecdf.saved(u))
    
    f = rep(0,N2) # f is the probability of dying on day i given infection
    f[1] = (convolution(1.5) - convolution(0))
    for(i in 2:N2) {
      f[i] = (convolution(i+.5) - convolution(i-.5)) 
    }
    reported_cases[[Region]] = as.vector(as.numeric(region$Cases))
    deaths=c(as.vector(as.numeric(region$Deaths)),rep(-1,forecast))
    cases=c(as.vector(as.numeric(region$Cases)),rep(-1,forecast))
    deaths_by_country[[Region]] = as.vector(as.numeric(region$Deaths))
    region_intervention <- as.data.frame(region[, colnames(interventions_region)])
    # This line will prevent modelling of a future change of interventions
    region_intervention[N:(N+forecast),] <- region_intervention[N,]
    school =region_intervention[,1]
    selfIsolation =region_intervention[,2]
    publicEvents = region_intervention[,3]
    lockdown = region_intervention[,4]
    socialDistancing = region_intervention[,5]
    firstIntervention = 1*((school+ selfIsolation+ publicEvents+ lockdown + socialDistancing) >= 1)
    ## append data
    stan_data$N = c(stan_data$N,N)
    # stan_data$x = cbind(stan_data$x,x)
    stan_data$f = cbind(stan_data$f,f)
    stan_data$deaths = cbind(stan_data$deaths,deaths)
    stan_data$cases = cbind(stan_data$cases,cases)
    
    stan_data$N2=N2
    stan_data$x=1:N2
    if(length(stan_data$N) == 1) {
      stan_data$N = as.array(stan_data$N)
    }
    df_features = data.frame('school' = school, 'selfIsolation' = selfIsolation, 'publicEvents' = publicEvents,
                             'firstIntervention' = firstIntervention, 'lockdown' = lockdown, 'socialDistancing' = socialDistancing)
    features <- as.matrix(df_features)
    covariate_list[[k]] <- features
    k <- k+1
  }
  stan_data$P = dim(features)[2]
  stan_data$X = array(NA, dim = c(stan_data$M , stan_data$N2 ,stan_data$P ))
  for (i in 1:stan_data$M){
    stan_data$X[i,,] = covariate_list[[i]] 
  }
  return(list("stan_data" = stan_data, "dates" = dates, "reported_cases"=reported_cases, "deaths_by_country" = deaths_by_country))
}