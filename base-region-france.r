library(rstan)
library(data.table)
library(lubridate,warn.conflicts = FALSE)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)

source("utils/arg-parser.r")
source("utils/read-covariates.r")
source("utils/ifr-tools.r")
source("utils/log-and-process.r")

regions <- read_country_file("active-regions.cfg")
active_countries <- read_country_file("active-countries.cfg")

region_to_country_map = list()
for(Region in regions){
  region_to_country_map[[Region]] <- "France"
}
for(Country in active_countries){
  region_to_country_map[[Country]] <- Country
}

# Commandline options and parsing
parsedargs <- base_arg_parse()
DEBUG <- parsedargs[["DEBUG"]]
FULL_RUN <- parsedargs[["FULL"]]
StanModel <- parsedargs[["StanModel"]]
new_sub_folder <- parsedargs[["new_sub_folder"]]
max_date <- parsedargs[["max_date"]]

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))
fullstr <- ""
if (FULL_RUN){
  fullstr <- "fullrun"
} else if (DEBUG) {
  fullstr <- "debug"
}

run_name <- paste0(StanModel,'-',fullstr,'-', format(Sys.time(), '%Y%m%dT%H%M%S'),'-',JOBID)
if (new_sub_folder){
  result_folders <- c(
      "results", "figures"
    )
  for (fold in result_folders){
    dir.create(paste0(fold ,'/', run_name))
  }
  run_name <- paste0(run_name ,'/', run_name)
}
## Reading data from region file and world data and trimming it to max_date
data_files <- c(
  "data/COVID-19-up-to-date.rds",
  "data/all-france.rds"
)
d <- trim_data_to_date_range(
  do.call('rbind', lapply(data_files, readRDS)),
  max_date  # optional arguments allow data customisation
)
# Trim countries and regions that fail the number of death test.
death_thresh_epi_start = 10
keep_regions = logical(length = length(region_to_country_map))
for(i in 1:length(region_to_country_map))
{
  Region <- names(region_to_country_map)[i]
  Country = region_to_country_map[[Region]]  
  d1=d[d$Countries.and.territories==Region,c(1,5,6,7)] 
  keep_regions[i] = !is.na(which(cumsum(d1$Deaths)>=death_thresh_epi_start)[1]) # also 5
  if (!keep_regions[i]) {
    message(sprintf(
      "WARNING: Region %s in country %s has not reached 10 deaths on %s, it cannot be processed\nautomatically removed from analysis\n",
      Region, Country, max_date))
  }
}
region_to_country_map <- region_to_country_map[keep_regions]
## get IFR and population from same file

serial.interval = read.csv("data/serial_interval.csv")
ifr.by.country <- return_ifr()
covariates <- covariates_read('data/interventions.csv', max_date)

forecast = 0
N2 = 120 # increase if you need more forecast Max is 100 at the moment

dates = list()
reported_cases = list()
# Pads serial interval with 0 if N2 is greater than the length of the serial
# interval array
if (N2 > length(serial.interval$fit)) {
  pad_serial.interval <- data.frame(
    "X"=(length(serial.interval$fit)+1):N2,
    "fit"=rep(0.0, max(N2-length(serial.interval$fit), 0 ))
  )
  serial.interval = rbind(serial.interval, pad_serial.interval)
}
stan_data = list(
  M=length(names(region_to_country_map)),N=NULL,covariate1=NULL,
  covariate2=NULL,covariate3=NULL,covariate4=NULL,covariate5=NULL,
  covariate6=NULL,deaths=NULL,f=NULL,N0=6,cases=NULL,
  SI=serial.interval$fit[1:N2],
  EpidemicStart = NULL, pop = NULL) # N0 = 6 to make it consistent with Rayleigh
deaths_by_country = list()

# various distributions required for modeling
infection_to_onset <- c("mean"=5.1, "deviation"=0.86)
onset_to_death <- c("mean"=18.8, "deviation"=0.45)
# infection-to-onset distribution
x1 = rgammaAlt(1e7,infection_to_onset["mean"], infection_to_onset["deviation"])
# onset-to-death distribution
x2 = rgammaAlt(1e7,onset_to_death["mean"], onset_to_death["deviation"])

ecdf.saved = ecdf(x1+x2)

log_simulation_inputs(run_name, region_to_country_map,  ifr.by.country,
  infection_to_onset, onset_to_death)

preprocess_error = FALSE
for(Region in names(region_to_country_map))
{
  Country = region_to_country_map[[Region]]
  print(sprintf("Region: %s in country: %s ",Region,Country))
  IFR=ifr.by.country$ifr[ifr.by.country$country == Country]
  
  covariates1 <- covariates[covariates$Country == Country, c(2,3,4,5,6)]
  
  d1_pop = ifr.by.country[ifr.by.country$country==Country,]
  d1=d[d$Countries.and.territories==Region,c(1,5,6,7)]
  d1$date = as.Date(d1$DateRep,format='%d/%m/%Y')
  d1$t = decimal_date(d1$date) 
  d1=d1[order(d1$t),]
  if(length(d1$date) == 0){
    preprocess_error = TRUE
    message(sprintf(
      "ERROR: Region %s in country %s had no data (d1 length(d1)==0)", Region, Country))
    next
  }
  date_min <- dmy('31/12/2019') 
  if (as.Date(d1$DateRep[1], format='%d/%m/%Y') > as.Date(date_min, format='%d/%m/%Y')){
    pad_days <- as.Date(d1$DateRep[1], format='%d/%m/%Y') - date_min
    pad_dates <- date_min + days(1:pad_days[[1]]-1)
    padded_data <- data.frame("Countries.and.territories" = rep(Region, pad_days),
                              "DateRep" = format(pad_dates, '%d/%m/%Y'),
                              "t" = decimal_date(as.Date(pad_dates,format='%d/%m/%Y')),
                              "date" = as.Date(pad_dates,format='%d/%m/%Y'),
                              "Cases" = as.integer(rep(0, pad_days)),
                              "Deaths" = as.integer(rep(0, pad_days)),
                              stringsAsFactors=F)
    
    d1 <- bind_rows(padded_data, d1)
  }
  index = which(d1$Cases>0)[1]
  index1 = which(cumsum(d1$Deaths)>=death_thresh_epi_start)[1] # also 5
  if (is.na(index1)) {
    preprocess_error = TRUE
    message(sprintf(
      "ERROR: Region %s in country %s has not reached 10 deaths on %s, it cannot be processed\nremove from 'active-countries.cfg' or 'active-regions.cfg'\n",
      Region, Country, max_date))
    next
  }
  index2 = index1-30
 
  print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))
  d1=d1[index2:nrow(d1),]
  stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2)
  stan_data$pop = c(stan_data$pop, d1_pop$popt)
  
  
  for (ii in 1:ncol(covariates1)) {
    covariate = names(covariates1)[ii]
    d1[covariate] <- (as.Date(d1$DateRep, format='%d/%m/%Y') >= as.Date(covariates1[1,covariate]))*1  # should this be > or >=?
  }
  
  dates[[Region]] = d1$date
  # hazard estimation
  N = length(d1$Cases)
  print(sprintf("%s has %d days of data",Region,N))
  forecast = N2 - N
  if(forecast < 0) {
    print(sprintf("%s: %d", Country, N))
    print("ERROR!!!! increasing N2 to have at least 7 days")
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
  
  reported_cases[[Region]] = as.vector(as.numeric(d1$Cases))
  deaths=c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
  cases=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
  deaths_by_country[[Region]] = as.vector(as.numeric(d1$Deaths))
  covariates2 <- as.data.frame(d1[, colnames(covariates1)])
  # x=1:(N+forecast)
  covariates2[N:(N+forecast),] <- covariates2[N,]
  
  ## append data
  stan_data$N = c(stan_data$N,N)
  # stan_data$x = cbind(stan_data$x,x)
  stan_data$covariate1 = cbind(stan_data$covariate1,covariates2[,1])
  stan_data$covariate2 = cbind(stan_data$covariate2,covariates2[,2])
  stan_data$covariate3 = cbind(stan_data$covariate3,covariates2[,3])
  # stan_data$covariate4 is an "any" covariates marker calculated below
  stan_data$covariate4 = cbind(stan_data$covariate4,covariates2[,4])
  stan_data$covariate5 = cbind(stan_data$covariate5,covariates2[,4])
  stan_data$covariate6 = cbind(stan_data$covariate6,covariates2[,5])
  stan_data$f = cbind(stan_data$f,f)
  stan_data$deaths = cbind(stan_data$deaths,deaths)
  stan_data$cases = cbind(stan_data$cases,cases)
  
  stan_data$N2=N2
  stan_data$x=1:N2
  if(length(stan_data$N) == 1) {
    stan_data$N = as.array(stan_data$N)
  }
}

if(preprocess_error){
  stop(sprintf(
      "ERROR: There were errors during preprocessing, check for error messages."))
}

# create the `any intervention` covariate
stan_data$covariate4 = 1*as.data.frame((stan_data$covariate1+
                                          stan_data$covariate2+
                                          stan_data$covariate3+
                                          stan_data$covariate5+
                                          stan_data$covariate6) >= 1)

if(DEBUG) {
  for(i in 1:length(region_to_country_map)) {
    write.csv(
      data.frame(date=dates[[i]],
                 `school closure`=stan_data$covariate1[1:stan_data$N[i],i],
                 `self isolating if ill`=stan_data$covariate2[1:stan_data$N[i],i],
                 `public events`=stan_data$covariate3[1:stan_data$N[i],i],
                 `government makes any intervention`=stan_data$covariate4[1:stan_data$N[i],i],
                 `lockdown`=stan_data$covariate5[1:stan_data$N[i],i],
                 `social distancing encouraged`=stan_data$covariate6[1:stan_data$N[i],i]),
      file=sprintf("results/%s%s-check-dates.csv",run_name, names(region_to_country_map)[i]),row.names=F)
  }
}

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan'))


if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40,warmup=20,chains=2)
} else if (FULL_RUN){
  fit = sampling(m,data=stan_data,iter=4000,warmup=2000,chains=4,thin=4,
    control = list(adapt_delta = 0.95, max_treedepth = 10))

} else { 
  # uncomment the line below for a full run to replicate results and comment the second line below 
  fit = sampling(m,data=stan_data,iter=200,warmup=100,chains=4,thin=4,
    control = list(adapt_delta = 0.95, max_treedepth = 10))
}  

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

save.image(paste0('results/',run_name,'.Rdata'))

countries <- names(region_to_country_map)
save(
  fit, prediction, dates,reported_cases,deaths_by_country,countries,
  region_to_country_map, estimated.deaths, estimated.deaths.cf, 
  out,covariates,infection_to_onset, onset_to_death,
  file=paste0('results/',run_name,'-stanfit.Rdata'))

postprocess_simulation(run_name, out, countries, dates)
