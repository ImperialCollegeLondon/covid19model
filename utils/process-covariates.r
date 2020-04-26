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

process_covariates <- function(countries, interventions, d, ifr.by.country,N2){
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
  # various distributions required for modeling
  mean1 <- 5.1; cv1 <- 0.86; # infection to onset
  mean2 <- 17.8; cv2 <- 0.45 # onset to death
  x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
  x2 <- rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
  
  ecdf.saved <- ecdf(x1+x2)
  forecast <- 0
  dates <- list()
  reported_cases <- list()
  stan_data <- list(M=length(countries$Regions),N=NULL,deaths=NULL,f=NULL,
                   N0=6,cases=NULL,SI=serial.interval$fit[1:N2],features=NULL,
                   EpidemicStart = NULL, pop = NULL)
  deaths_by_country <- list()
  covariate_list <- list()
  k=1
  # going over each region
  for (Country in countries$Regions){
    IFR <- ifr.by.country$ifr[ifr.by.country$country == Country]
    region_pop <- ifr.by.country[ifr.by.country$country==Country,]
    region <- d[d$Country==Country,]
    region$DateRep <-region$DateRep
    region <-region[order(as.Date(region$DateRep)),]  # ensure date ordering
    
    # padding in raw data backwards ex. portugal
    date_min <- dmy('31/12/2019') 
    if (region$DateRep[1] > date_min){
      print(paste(Country,'In padding ECDC data'))
      pad_days <-region$DateRep[1] - date_min
      pad_dates <- date_min + days(1:pad_days[[1]]-1)
      padded_data <- data.frame("Country" = rep(Country, pad_days),
                                "DateRep" = pad_dates,
                                "Cases" = as.integer(rep(0, pad_days)),
                                "Deaths" = as.integer(rep(0, pad_days)),
                                stringsAsFactors=F)
      
     region <- bind_rows(padded_data,region)
    }
    index = which(region$Cases>0)[1]
    index1 = which(cumsum(region$Deaths)>=10)[1] # also 5
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
    
    dates[[Country]] =region$DateRep
    # hazard estimation
    N = length(region$Cases)
    print(sprintf("%s has %d days of data",Country,N))
    forecast = N2 - N
    if(forecast < 0) {
      print(sprintf("%s: %d", Country, N))
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
    reported_cases[[Country]] = as.vector(as.numeric(region$Cases))
    deaths=c(as.vector(as.numeric(region$Deaths)),rep(-1,forecast))
    cases=c(as.vector(as.numeric(region$Cases)),rep(-1,forecast))
    deaths_by_country[[Country]] = as.vector(as.numeric(region$Deaths))
    region_intervention <- as.data.frame(region[, colnames(interventions_region)])
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