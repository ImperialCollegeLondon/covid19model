require(data.table)
require(rstan)
require(EnvStats)
require(covid19AgeModel)

## command line parsing if any
args = list()
args[['location.index']] = 1
args[['with.flow']] = 1
args[['indir.results']] = '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin-2558246[1].pbs'


args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{  
  stopifnot(args_line[[1]]=='-indir.results')
  stopifnot(args_line[[3]]=='-location.index')
  stopifnot(args_line[[5]]=='-with.flow')
  args <- list()    
  args[['indir.results']] <- args_line[[2]]  
  args[['location.index']] <- args_line[[4]]  
  args[['with.flow']] <- args_line[[6]]  
}

#	print args
str(args)

#	read args
indir.results <- args$indir.results
location.index <- as.integer(args$location.index)
with.flow <- args$with.flow

#	read Stan input data and add location.index to parallelise computations
cat('\nReading Stan input data...')
infile.stanin <- list.files(indir.results, pattern='.*_stanin.RData$', recursive=TRUE)
stopifnot(length(infile.stanin)>0)
stopifnot(length(infile.stanin)<=1)
tmp <- load(file.path(indir.results, infile.stanin))
stopifnot(c('args','stan_data')%in%tmp)
stan_data$LOCATION_PROCESSING_IDX <- location.index
if(is.null(stan_data$counterfactual_school_effect)) stan_data$counterfactual_school_effect = -1

#	reset args
args[['work_dir']] <- getwd()

pkg.dir <- system.file(package = "covid19AgeModel" )

if(with.flow=='1')
{
  stan_data <- gqs_add_stan_data_for_flows(stan_data,dates)
}

if(is.null(stan_data$rev_iar_daysSinceInfection))
{
  stan_data <- gqs_add_stan_data_for_iar(stan_data)
}

#	read stanfits
cat('\nReading Stanfit ...')
infile.stanfits <- list.files(indir.results, pattern='.*_stanout.RData$', recursive=TRUE)
stopifnot(length(infile.stanfits)>0)
stopifnot(length(infile.stanfits)<=1)
tmp <- load(file.path(indir.results, infile.stanfits[1]))

#	find stan model and stan model for generating quantities
file_stanModel <- gsub('.*covid19AgeModel/(.*)','\\1',args$file_stanModel)
file_stanModel <- file.path(pkg.dir, file_stanModel)
file_stanModel_gqs <- gsub('cmdstanv','gqs',file_stanModel)
stopifnot( file.exists(file_stanModel_gqs) )

cat('\nCompiling gqs model file ...')
m2 <- rstan::stan_model(file_stanModel_gqs)

cat('\nGenerating quantities ...')
draws <- as.matrix(fit)
draws <- draws[,!grepl('rho0|Rt|RtByAge|E_deaths|E_deathsByAge|E_casesByAge|lp__', colnames(draws)) ]
fit2 <- rstan::gqs(m2, data=stan_data, draws=draws)
fit.gqs <- rstan::extract(fit2)

file <- file.path(indir.results, paste0(basename(args$job_dir), '_location',location.index,'_stangqs.RDS'))
io_saveRDS(fit.gqs, args[['work_dir']], dirname(file), basename(file), check_if_saved_n=10)

cat('\nFinished base-ages-generate-quantities.r ...')
