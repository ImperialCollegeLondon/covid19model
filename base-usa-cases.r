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
library(scales)
library(zoo)
library(matrixStats)
library(optparse)

source('usa/code/utils/read-data-usa-cases.r')
source('usa/code/utils/process-covariates-usa-cases.r')
## to get latest data please run the scrape-required-data.r file or uncomment the line below
#system(paste0("Rscript usa/code/scrape-data.r"),intern=FALSE)

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
#Sys.setenv(DEBUG == "TRUE") # to run  in debug mode
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}
# Sys.setenv(FULL = "TRUE") # to run  in full mode
if(is.null(cmdoptions$options$full)) {
  FULL = Sys.getenv("FULL") == "TRUE"
} else {
  FULL = cmdoptions$options$full
}

if(DEBUG && FULL) {
  stop("Setting both debug and full run modes at once is invalid")
}

if(length(cmdoptions$args) == 0) {
  StanModel = 'base-usa-cases'
} else {
  StanModel = cmdoptions$args[1]
}

args = cmdoptions$args
# if using rstudio change this, 1: pooling method, 2: mobility  formula to use, debug and full are either commandline or sys variable
if(length(args) == 0) {
  args = c('base-usa-cases',
           '~ -1 + averageMobility + residential',
           '~  1 + averageMobility',
           '~ -1 + averageMobility'
  )
} 

StanModel = args[1]
cat(sprintf("Running:\nStanModel = %s\nFixed effects:%s\nRandom effects regional:%s\nRandom effects state:%s\nDebug: %s\n",
            StanModel,args[2],args[3], args[4],DEBUG))


# Read JHU and NYT data
death_data <- read_death_data(source = "jhu", smooth = FALSE)
df_pop <- readRDS("usa/data/us_population.rds")
testing_date <- as.Date("2020-05-11")

ny_data <- read_death_data(source = "nyt", smooth = FALSE)
ny_data <- ny_data[ny_data$code=='NY', ]

# NYT and JHU death data is different lengths
max_ny <- max(ny_data$date)
max_jhu <- max(death_data$date)
max_date <- min(max_ny, max_jhu)
death_data <- death_data[!death_data$code %in% c('NY'), ] 
death_data <- bind_rows(death_data, ny_data)
death_data <- death_data[which(death_data$date <= max_date),]

# Choose states
states <- unique(death_data$code)
#states <- c("NY", "WA")
# Read ifr 
ifr_by_state <- read_ifr_data()
# Read google mobility
mobility <- read_google_mobility()
# At times google has mobility na for some days in that cae you will need to impute those values
# else code will fail 
# read predictions of future days from foursquare
# if you need predictions from foursquare please run file mobility-regression.r in
# the folder usa/code/utils/mobility-reg
google_pred <- read.csv('usa/data/google-mobility-forecast.csv', stringsAsFactors = FALSE)
google_pred$date <- as.Date(google_pred$date, format = '%Y-%m-%d') 
google_pred$sub_region_2 <- ""
google_pred$country_region <- "United States"
google_pred$country_region_code <- "US"
colnames(google_pred)[colnames(google_pred) == 'state'] <- 'sub_region_1'
if (max(google_pred$date) > max(mobility$date)){
  google_pred <- google_pred[google_pred$date > max(mobility$date),]
  # reading mapping of states of csv
  un<-unique(mobility$sub_region_1)
  states_code = read.csv('usa/data/states.csv', stringsAsFactors = FALSE)
  google_pred$code = "!!"
  for(i in 1:length(un)){
    google_pred$code[google_pred$sub_region_1==un[i]] = states_code$Abbreviation[states_code$State==un[i]]
  }
  mobility <- rbind(as.data.frame(mobility),as.data.frame(google_pred[,colnames(mobility)]))
}

max_date <- as.Date("2020-06-01")
mobility <- mobility[which(mobility$date <= max_date),]
death_data <- death_data[which(death_data$date <= max_date),]
print(sprintf("Fitting until %s", max_date))

# read interventions
interventions <- readRDS('usa/data/covariates_all_cases.rds')
# read interventions lifted date
interventions_lifted <- readRDS('usa/data/covariates_all_cases_ended.rds')
# Number of days to forecast
forecast <- 21
# Maximum number of days to simulate
num_days_sim <- (max(death_data$date) - min(death_data$date) + 1 + forecast)[[1]]
formula = as.formula(args[2])
formula_partial_regional = as.formula(args[3])
formula_partial_state = as.formula(args[4])
processed_data <- process_covariates(states = states, 
                                     mobility = mobility,
                                     death_data = death_data , 
                                     ifr_by_state = ifr_by_state, 
                                     num_days_sim = num_days_sim, 
                                     interventions = interventions, 
                                     interventions_lifted = interventions_lifted,
                                     formula = formula, formula_partial_regional = formula_partial_regional,
                                     formula_partial_state = formula_partial_state)
stan_data <- processed_data$stan_data
stan_data$cases_start <- (max_date - testing_date)[[1]]
print(sprintf("Including cases from %s", testing_date))

dates <- processed_data$dates
reported_deaths <- processed_data$reported_deaths
reported_cases <- processed_data$reported_cases
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m <- stan_model(paste0('usa/code/stan-models/',StanModel,'.stan'))
JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))
fit = sampling(m,data=stan_data,iter=1800, warmup=1000, chains=5, thin=1, 
               control = list(adapt_delta = 0.99, max_treedepth = 15))


covariate_data = list(interventions, mobility)

out <- rstan::extract(fit)
estimated_cases_raw <- out$prediction
estimated_deaths_raw <- out$E_deaths
estimated_deaths_cf <- out$E_deaths0

save(fit, dates, reported_cases, reported_deaths, states,
     estimated_cases_raw, estimated_deaths_raw, estimated_deaths_cf,
     formula, formula_partial_regional,formula_partial_state, stan_data,covariate_data, JOBID,
     file=paste0('usa/results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

source("usa/code/plotting/make-plots.r")
make_plots_all(paste0('usa/results/', StanModel, '-', JOBID, '-stanfit.Rdata'), 
               last_date_data = max(dates[[1]]), 
               ext = ".pdf")
  


