library(rstan)
library(data.table)
library(lubridate,warn.conflicts = FALSE)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)

source("utils/arg-parser.r")
source("utils/read-interventions.r")
source("utils/process-covariates.r")
source("utils/process-covariates-region.r")
source("utils/ifr-tools.r")
source("utils/log-and-process.r")

# Commandline options and parsing
parsedargs <- base_arg_parse()
DEBUG <- parsedargs[["DEBUG"]]
FULL <- parsedargs[["FULL"]]
StanModel <- parsedargs[["StanModel"]]
new_sub_folder <- parsedargs[["new_sub_folder"]]
max_date <- parsedargs[["max_date"]]

regions <- read_country_file(parsedargs[["activeregions"]])
active_countries <- read_country_file(parsedargs[["activecountries"]])

region_to_country_map = list()
for(Region in regions){
  region_to_country_map[[Region]] <- "France"
}
for(Country in active_countries){
  region_to_country_map[[Country]] <- Country
}

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))
fullstr <- ""
if (FULL){
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
ifr.by.country <- return_ifr()
covariates <- read_interventions('data/interventions.csv', max_date)


processed_data <- process_covariates_region(region_to_country_map, interventions, d, ifr.by.country, N2)
stan_data = processed_data$stan_data
dates = processed_data$dates
deaths_by_country = processed_data$deaths_by_country
reported_cases = processed_data$reported_cases

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
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=1800,warmup=1000,chains=5,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 15))
} else { 
  fit = sampling(m,data=stan_data,iter=1000,warmup=500,chains=4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))
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
