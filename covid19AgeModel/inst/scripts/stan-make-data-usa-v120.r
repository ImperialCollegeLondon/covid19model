library(dplyr)
library(tidyr)
library(data.table)
library(covid19AgeModel)

pkg.dir <- system.file(package = "covid19AgeModel" )

args <- list( 
  stanModelFile= 'covid19AgeModel_v120_cmdstanv',
  seed= 42,
  chain= 1,
  outdir= "~/Downloads",
  job_tag= '40states_Oct29',
  cmdstan = 0L,
  multiplier_cntct_school_closure = 1,
  school.cases.upper.bound.multiplier = 6,
  ifr_by_age_prior = "Levin_meta_analysis_v7",
  countries= "CO,CT,FL,NYC"
)

## command line parsing if any
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')
  stopifnot(args_line[[3]]=='-seed', !is.na(as.integer(args_line[[4]])))	
  stopifnot(args_line[[5]]=='-chain', !is.na(as.integer(args_line[[6]])))
  stopifnot(args_line[[7]]=='-outdir')
  stopifnot(args_line[[9]]=='-jobtag')
  stopifnot(args_line[[11]]=='-countries')
  stopifnot(args_line[[13]]=='-cmdstan')    
  stopifnot(args_line[[15]]=='-multiplier_cntct_school_closure')
  stopifnot(args_line[[17]]=='-ifr_by_age_prior')
  stopifnot(args_line[[19]]=='-school.cases.upper.bound.multiplier')
  
  args <- list()
  args[['stanModelFile']] <- args_line[[2]]
  args[['seed']] <- as.integer(args_line[[4]])
  args[['chain']] <- as.integer(args_line[[6]])
  args[['outdir']] <- args_line[[8]]
  args[['job_tag']] <- args_line[[10]]
  args[['countries']] <- args_line[[12]]  
  args[['cmdstan']] <- as.integer(args_line[[14]])  
  args[['multiplier_cntct_school_closure']] <- as.numeric(args_line[[16]])
  args[['ifr_by_age_prior']] <- as.character(args_line[[18]])
  args[['school.cases.upper.bound.multiplier']] <- as.numeric(args_line[[20]])
} 

## set other args
args$cntct_by <- 5L	
args$file_stanModel <- file.path(pkg.dir, 'stan-models',paste0(args$stanModelFile,'.stan'))
args$file_jhu_death_data_padded <- file.path(pkg.dir,"data-v120","jhu_death_data_padded_201030.rds")
args$file_nyt_death_data_padded <- file.path(pkg.dir,"data-v120","nyt_death_data_padded_201030.rds")
args$file_nyc_death_data_padded <- file.path(pkg.dir,"data-v120","NYC_deaths_201030.csv")
args$file_death_data_by_age <- file.path(pkg.dir,"data-v120","DeathsByAge_US_201029_cured.csv")
args$file_ihme_hospitalization <- file.path(pkg.dir,"data-v120","Hospitalization_all_locs.csv")
args$file_states <- file.path(pkg.dir,"data-v120","states.csv")
args$file_weighted_ifr <- file.path(pkg.dir, "data-v120","weighted_ifr.RDS")
args$file_fsq_mobility <- file.path(pkg.dir,'data-v120','fsq_visit_data_dec_refresh_201029.csv')
args$file_grouped <- file.path(pkg.dir,"data-v120","grouped.csv")
args$file_us_population <- file.path(pkg.dir,"data-v120","us_population_withnyc.rds")
args$file_serial_interval <- file.path(pkg.dir,'data-v120','serial_interval.csv')
args$file_covariates <- file.path(pkg.dir,"data-v120","covariates.RDS")
args$file_contact_weekday <- file.path(pkg.dir,'data-v120','polymod.tab.bin_GB_weekday.rda') 
args$file_contact_weekend <- file.path(pkg.dir,'data-v120','polymod.tab.bin_GB_weekend.rda') 
args$file_polymod_data <- file.path(pkg.dir,'data-v120','polymod_data_with_covariates_200623.rds')
args$file_pop_age <- file.path(pkg.dir,'data-v120','popByAge_v200421.csv')
args$file_us_area <- file.path(pkg.dir,"data-v120","us_states_area_measurements.csv")
args$file_ifr_by_age_prior_Verity <- file.path(pkg.dir,'data-v120','ifr-by-age-prior_Verity_200624.csv') # prior with smooth spline on Verity estimates https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext
args$file_ifr_by_age_prior_Levin <- file.path(pkg.dir,'data-v120','ifr-by-age-prior_Levin_200904.csv') # lognormal prior from Levin et al. https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v5
args$file_ifr_by_age_prior_Levin_v7 <- file.path(pkg.dir,'data-v120','ifr-by-age-prior_Levin_201117.csv') # lognormal prior from Levin et al. https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v5
args$file_ifr_by_age_prior_BetaBinomial <- file.path(pkg.dir,'data-v120','ifr-by-age-prior_BetaBinomial_200902.csv') # prior estimated with a betabinomial, logit link with gp https://www.medrxiv.org/content/10.1101/2020.09.18.20197376v1
args$file_CI_OR <- file.path(pkg.dir, "data-v120", "CI_OR_age_Zhang.csv")
args$file_region <- file.path(pkg.dir,"data-v120", "usa-regions.csv")
args$file_contact_intensities_outbreak_China <- file.path(pkg.dir, "data-v120", "estimate_contact_intensities_outbreak_China.rds")
args$file_contact_intensities_outbreak_UK <- file.path(pkg.dir, "data-v120", "estimate_contact_intensities_outbreak_UK.rds")
args$file_school_Ox_NPI_data <- file.path(pkg.dir, "data-v120", "OxCGRT_US_subnational_06Oct2020.csv") 
args$file_school_EduWeek_data <- file.path(pkg.dir, "data-v120", "Coronavirus_and_School_Closures_200910.csv") 
args$file_cdc_survey <- file.path(pkg.dir, "data-v120", "cdc_survey.csv")
args$file_cross_sectional_national <- file.path(pkg.dir, "data-v120", "cross_sectional_national_survey.csv")
args$file_school_cases <- file.path(pkg.dir,"data-v120","school_cases.csv")
tmp <- Sys.getenv("PBS_JOBID")
args$job_id <- ifelse(tmp!='', tmp, as.character(abs(round(rnorm(1) * 1e6))) )
args$job_dir <- file.path(args$outdir,paste0(args$stanModelFile,'-',args$job_tag,'-',args$job_id)) 
args$DEBUG <- FALSE
args$makeplots <- FALSE
if(grepl("\\[1\\]", args$job_id)) args$makeplots = TRUE


# Do you want to pooled google mobility into 
# residential, transstation and avg mobility (mean of retail & recreation, grocery & pharmac, workplace and park)
args$with_avg_mobility_data <- 0	
args$decouple_mobility_trends_into_baseline_plus_rest_parts <- 0
args$decouple_mobility_trends_into_decline_plus_upswing_parts <- 0
args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts <- 0
args$with_dip_rnde <- 0
args$with_upswing_rnde <- 0
args$with_upswing_age2039_rnde <- 0
args$with_upswing_age2049_rnde <- 0
args$with_upswing_age2034_rnde <- 0
args$with_upswing_age3549_rnde <- 0
args$with_upswing_age5064_rnde <- 0
args$with_ifr_rnde_expmid1_expmid2_expold <- 0
args$with_upswing_expmid1_rnde <- 0
args$with_upswing_expmid2_rnde <- 0
args$with_upswing_expold_rnde <- 0
args$with_biweekly_upswing_time_effect <- 0
args$with_biweekly_upswing_time_effect_for_all_loc <- 0
args$effect_weeks <- 2
args$with_estimated_pairwise_mobility_trends <- 0

## start script
cat(sprintf("Running\n"))

## determine ifr by age prior 
if(args$ifr_by_age_prior == "Levin_meta_analysis"){
  args$with_lognormal_prior_on_each_ifr_by_age_band = 1
  args$file_ifr_age_prior = args$file_ifr_by_age_prior_Levin
} else if(args$ifr_by_age_prior == "Levin_meta_analysis_v7"){
  args$with_lognormal_prior_on_each_ifr_by_age_band = 1
  args$file_ifr_age_prior =  args$file_ifr_by_age_prior_Levin_v7
} else if(args$ifr_by_age_prior == "Verity_model_based_analysis"){
  args$with_beta_prior_on_each_ifr_by_age_band = 1
  args$file_ifr_age_prior = args$file_ifr_by_age_prior_Verity
} else{ # By default BetaBinomial prior presented in report 32 https://www.medrxiv.org/content/10.1101/2020.09.18.20197376v1
  args$with_lognormal_prior_on_each_ifr_by_age_band = 1
  args$file_ifr_age_prior = args$file_ifr_by_age_prior_BetaBinomial
}


## determine prior used from on stan file chosen
# use the new framework of non-res and res on the contact matrix (mot compatible with beta age yet)
args$with_res_nonres_contactmatrix_model <- 0
# use random effect on ifr by age prior
args$with_beta_prior_on_each_ifr_by_age_band <- 0
args$with_lognormal_prior_on_each_ifr_by_age_band <- 0
args$with_logitincrements_prior_on_ifr_by_age <- 0
args$with_ifr_rnde_mid1_mid2_old <- 0
args$with_ifr_rnde_mid1_mid2_expold <- 0
args$with_ifr_fixed_decay <- 0
args$with_ifr_var_decay <- 0
# what prior do you want to use on beta age (if both 0, beta age is not used)
args$with_truncatednormal_on_each_beta_age_by_age_band <- 0
args$with_lognormal_prior_relsusceptibility <- 0
# with google moblity
args$with_google_mobility <- 0
#with foursquare mobility data 
args$with_fsq_mobility <- 0
#with emodo cell phone data 
args$with_emodo_cell_phone_data <- 0
#with reduced age buckets for parameters
args$with_reduced_age_parameters <- 0
#with expected number of contacts by age adjusted for state population composition
args$with_contacts_adjusted_for_age <- 1
#with expected number of contacts by age adjusted for overall FSQ visits in baseline period 
args$scale_contactmatrix_bystate <- 0
#with expected number of contacts by age adjusted for age specific FSQ visits in baseline period 
args$scale_contactmatrix_bystateandage <- 0
# with week effects
args$with_week_effects <- 0
# add random effect before and after rebound
args$with_timeidx_rebound <- 0
# apply mobility trends to both rows and cols of baseline contact matrix
args$with_eta2 <- 0
# distribute initial cases before N0 among individuals aged 20 to 54
args$with_inits_20_54 <- 1
# distribute initial cases before N0 among individuals aged 5 to 29
args$with_inits_5_29 <- 0
# use zhang et al. values for outbreak contact matrix
args$with_contact_intensities_zhang <- 0
# fix beta1 
args$fix_beta_baseline <- 0
# with smoothed case data in likelihood
args$with_smoothed_case_data <- 0
# with seroprevalence data in likelihood
args$with_seroprevalence_data <- 0
# with Edu weekly data to find state that re-opened schools
args$with_EduWeek_data <- 1
# closing and opening kindergartens and elementary schools
args$with_elementary_schools_status <- 0
# closing and opening kindergartens, elementary, primary and high schools
args$with_elementary_high_schools_status <- 1
# add soft bounds on attack rate in schools
args$with_school_case_lkl <- 0
# set US states to include in inference 
args$states <- strsplit(args$countries,',')[[1]]
args$countries <- NULL


if(grepl("fsq_mobility_201015i|covid19AgeModel_v120", args$file_stanModel))
{
  args$with_fsq_mobility <- 1
  args$with_google_mobility <- 0	
  args$with_lognormal_prior_on_each_ifr_by_age_band <- 1
  args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts <- 1
  args$with_ifr_rnde_expmid1_expmid2_expold <- 1
  args$with_eta2 <- 1
  args$with_biweekly_upswing_time_effect_for_all_loc <- 1
  args$with_contact_intensities_zhang <- 1
  args$with_school_status <- 1
  args$with_dip_rnde <- 1
  args$with_smoothed_case_data <- 1
  args$with_upswing_expmid1_rnde <- 1
  args$effect_weeks <- 2
  args$with_school_case_lkl <- 1
}

str(args)
set.seed(args$seed)

## make job dir
dir.create( args$job_dir )


## save input args
saveRDS( args, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_args.RDS')))


# Read which countires to usa
death_data <- read_death_data(args$file_jhu_death_data_padded, 
                              args$file_nyt_death_data_padded, 
                              args$file_nyc_death_data_padded,
                              args$file_ihme_hospitalization, 
                              args$file_states,
                              args$file_region,
                              source = "jhu", 
                              smooth = FALSE)
ny_data <- read_death_data(args$file_jhu_death_data_padded, 
                           args$file_nyt_death_data_padded, 
                           args$file_nyc_death_data_padded,
                           args$file_ihme_hospitalization, 
                           args$file_states,
                           args$file_region,
                           source = "nyt", 
                           smooth = FALSE)
ny_data <- ny_data[ny_data$code=='NY', ]
# NYT and JHU death data is different lengths
max_ny <- max(ny_data$date)
max_jhu <- max(death_data$date)
max_date <- min(max_ny, max_jhu)
death_data <- death_data[death_data$code!='NY', ]
death_data <- dplyr::bind_rows(death_data, ny_data)
death_data <- death_data[which(death_data$date <= max_date),]
# NYC data
nyc_data <- read_death_data(args$file_jhu_death_data_padded, 
                            args$file_nyt_death_data_padded, 
                            args$file_nyc_death_data_padded,
                            args$file_ihme_hospitalization, 
                            args$file_states,
                            args$file_region,
                            source = "nyc", 
                            smooth = FALSE)

# same format as for europe analysis
death_data <- death_data %>% 
  ungroup() %>%
  rename(state = state_name, Deaths = daily_deaths, Cases = daily_cases) %>%
  mutate(DateRep = format(date, "%d/%m/%Y")) %>%
  select("date", "Cases", "Deaths", "state", "code") 
death_data <- rbind(subset(death_data, date %in% nyc_data$date), subset(nyc_data, date %in% death_data$date))

# read death data by age 
cat('\nReading age-specific death data ...')
deathByAge <- read_deathByAge( args$file_death_data_by_age )	

# keep dates until common earliest last date for states in analysis 		
deathByAge_selected_states <- merge(deathByAge, data.table(code=args$states), by='code')
deathByAge_earliest_last_date <- deathByAge_selected_states[, list(last_date = max(date)), by = "code"][, min(last_date)]
death_data <- subset(death_data, date<=deathByAge_earliest_last_date)
cat('\nFound death for selected states until common last date ',as.character(deathByAge_earliest_last_date))

# get death by age data into stan format
deathByAge_data <- process_deathByAge(deathByAge = deathByAge_selected_states, 
                                      range_date = range(death_data$date), 
                                      states = args$states,
                                      args$file_us_population)


# read ifr by age
ifr.by.age <- read_ifr_data_by_age(args$file_ifr_age_prior)		

# read interventions
covariates <- readRDS(args$file_covariates)

#read population count
pop_count <- read_pop_count_us( args$file_us_population )

# read population count by age
pop_by_age <- read_pop_count_by_age_us( args$file_us_population )
if(args$makeplots)  
{
  plot_demograpy(pop_by_age, plotdir=args$job_dir)
}

#	read land area size of US states
darea <- read_us_state_areas(args$file_us_area)

#	collect all pop info data sets into one
pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
setkey(pop_info, loc, age.cat)
pop_by_age <- NULL
pop_count <- NULL
darea <- NULL

# make death by age plots 
if(args$makeplots) 
{
  plot_deaths_by_age_data(deathByAge, pop_info, plotdir=args$job_dir)
  plot_death_by_age_vs_deaths_overall(deathByAge, death_data, pop_info, plotdir=args$job_dir)
}

#	predict expected contacts using logpop model
if(args$with_contacts_adjusted_for_age==1)
{				
  cat('\nPredicting contact matrix for weekday using log(m) ~ cont_pop_p + log(pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L ... ')
  dcontact <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekday', args$file_polymod_data)	
  dcontact[, type:='weekday']
  cat('\nPredicting contact matrix for weekend using log(m) ~ cont_pop_p + log(pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L ... ')
  tmp <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekend', args$file_polymod_data)	
  tmp[, type:='weekend']
  dcontact <- rbind(dcontact, tmp)	
  if(args$makeplots)
  {	  
    plots_baseline_contact_matrices(pop_info, dcontact,args$file_polymod_data, plotdir=args$job_dir) 
  }  	
}

# Read serial interval
serial_interval = read.csv(args$file_serial_interval)

# Read google mobility, apple mobility, interventions, stringency
if(args$with_google_mobility && args$with_res_nonres_contactmatrix_model==0)
{
  cat('\nLoading Google mobility statistics...\n')
  mobility_data <- read_google_mobility(args$file_global_mobility_report, args$file_states)	       
  mobility_data_NYC <- subset(mobility_data, code == "NY")
  mobility_data_NYC$code <- "NYC" 
  mobility_data_NYC$sub_region_1 <- "New_York_City" 
  mobility_data <- rbind(mobility_data, mobility_data_NYC)
}
if(args$with_google_mobility && args$with_res_nonres_contactmatrix_model==1)
{
  cat('\nLoading estimated residential and non-residential mobility trends...\n')
  mobility_data <- read_google_resnonrestrends( args$file_resnonres_trends )	
}

if(args$with_emodo_cell_phone_data)
{
  cat('\nLoading Emodo cell phone data...\n')
  #mobility_data <- read_emodo_cell_phone_data(pop_info)
  mobility_data <- read_emodo_cell_phone_contact_intensities(infile_emodo = NULL)
  # mobility_data <- read_emodo_cell_phone_contact_intensities(infile_emodo = '~/contacts_and_mobility/contacts_by_age_20200729.csv',type='idx')
  
  cat('\nProcessing Emodo mobility trends...\n')
  emo_age_cat_map <- make_emodo_age_cat_map(mobility_data, pop_info)
  mobility_data <- make_mobility_trends_emo(mobility_data, emo_age_cat_map)
}

if(args$with_fsq_mobility)
{
  cat('\nLoading Foursquare mobility data...\n')
  mobility_data <- read_foursquare_mobility(pop_info, infile_fsq=args$file_fsq_mobility)  	
  
  cat('\nFound mobility data until ',as.character(mobility_data[, max(date)]))
  
  cat('\nProcessing Foursquare mobility trends...\n')
  fsq_age_cat_map <- make_fsq_age_cat_map(mobility_data, pop_info)
  mobility_data <- make_mobility_trends_fsq(mobility_data, fsq_age_cat_map)	
  
  if (args$makeplots)
  {
    cat('\nPlotting Foursquare data...\n')
    plot_raw_mobility_data_fsq(mobility_data, plotdir=args$job_dir)
    plot_mobility_trends_fsq(mobility_data, plotdir=args$job_dir)
  }  
}

if( args$with_fsq_mobility==1 && args$decouple_mobility_trends_into_decline_plus_upswing_parts==1 )
{	
  cat('\nProcessing decoupled FSQ mobility trends into 2 parts ... ')	
  mobility_data <- make_decoupled_mobility_trends_into_2_parts_fsq(mobility_data, pop_info)	
  
  file <- file.path(args$job_dir,'fsq_mobility_trends.rds')
  cat("\nWrite to file",file)	
  saveRDS(list(mobility_data=mobility_data, 
               pop_info=pop_info,
               death_data=death_data), 
          file=file)	
  
  if(args$makeplots)
  {
    plot_decoupled_mobility_trends_2_parts_fsq(mobility_data, plotdir=args$job_dir, plotselect=1:4)				
  }	
}

if( args$with_fsq_mobility==1 && 
    ( 	args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts==1 | 
       args$decouple_mobility_trends_into_baseline_plus_rest_parts==1 ))
{	
  cat('\nProcessing decoupled FSQ mobility trends into 3 parts ... ')	
  mobility_data <- make_decoupled_mobility_trends_into_3_parts_fsq(mobility_data, pop_info)		
  
  file <- file.path(args$job_dir,'fsq_mobility_trends.rds')
  cat("\nWrite to file",file)	
  saveRDS(list(mobility_data=mobility_data, 
               pop_info=pop_info,
               death_data=death_data), 
          file=file)	
  if(args$makeplots)
  {				
    # plot trends with dahed lines at breakpoints
    plot_mobility_trends_fsq_withbreakpoints(mobility_data, plotdir=args$job_dir)
    # plot base, eased and upswing trends for plotselect states.
    plot_decoupled_mobility_trends_3_parts_fsq(mobility_data, plotdir=args$job_dir, plotselect=1:4)
  }	
}

#	make smoothed log cases, average 
if( args$with_smoothed_case_data==1 )
{
  smoothed_log_cases <- process_make_smoothed_logcases(death_data, plot_dir=args$job_dir)			
}

# Number of days to forecast
forecast <- 7

# Maximum number of days to simulate
num_days_sim <- (max(death_data$date) - min(death_data$date) + 1 + forecast)[[1]]

# process data
processed_data <- make_stan_data_core(states = args$states, 
                                      death_data = death_data, 
                                      deathByAge_data = deathByAge_data,                                           
                                      ifr.by.age = ifr.by.age,                                         
                                      serial_interval = serial_interval,
                                      pop_info = pop_info, 
                                      dcontact= dcontact,                                          
                                      num_days_sim = num_days_sim, 
                                      forecast = forecast)

if(args$with_google_mobility && 
   args$decouple_mobility_trends_into_baseline_plus_rest_parts==0 &&
   args$decouple_mobility_trends_into_decline_plus_upswing_parts==0 && 
   args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts==0)
{
  cat('\nAdding google mobility covariates to stan_data ...')
  processed_data <- stan_data_add_google_mobility(processed_data, mobility_data, args$with_avg_mobility_data)
}
if(args$with_fsq_mobility && 
   args$decouple_mobility_trends_into_baseline_plus_rest_parts==0 &&
   args$decouple_mobility_trends_into_decline_plus_upswing_parts==0 && 
   args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts==0)
{  
  cat('\nAdding Foursquare mobility covariates to stan_data ...')
  processed_data <- stan_data_add_mobility_trends_fsq(processed_data, mobility_data, log=TRUE)
}
if( args$with_google_mobility && 
    args$decouple_mobility_trends_into_decline_plus_upswing_parts==1 )
{	
  cat('\nAdding 2 part decoupled Google mobility trends to stan_data ... ')
  processed_data <- stan_data_add_google_mobility_covariates_after_breakpoint(processed_data)
}
if( args$with_fsq_mobility==1 && 
    args$decouple_mobility_trends_into_decline_plus_upswing_parts==1 )
{		
  cat('\nAdding decoupled FSQ mobility trends (decline + upswing) to stan_data ... ')
  processed_data <- stan_data_add_decoupled_mobility_trends_decline_plus_upswing_fsq(processed_data, mobility_data, log=TRUE)	
}
if( args$with_fsq_mobility==1 && 
    args$decouple_mobility_trends_into_baseline_plus_rest_parts==1 )
{		
  cat('\nAdding decoupled FSQ mobility trends (baseline + rest) to stan_data ... ')
  processed_data <- stan_data_add_decoupled_mobility_trends_decline_plus_rest_fsq(processed_data, mobility_data, log=TRUE)	
}
if( args$with_fsq_mobility==1 && 
    args$decouple_mobility_trends_into_baseline_plus_decline_plus_upswing_parts==1 )
{		
  cat('\nAdding decoupled FSQ mobility trends (baseline + decline + upswing) to stan_data ... ')
  processed_data <- stan_data_add_decoupled_mobility_trends_3_parts_fsq(processed_data, mobility_data, log=TRUE)	
}
if( args$with_fsq_mobility==1 && 
    args$with_timeidx_rebound==1 )
{		
  cat('\nAdding indicator for rebound date ... ')
  processed_data <- stan_data_add_indicator_rebound_date(processed_data, mobility_data)	
}
if( args$with_estimated_pairwise_mobility_trends)
{	
  # process_chi_emodo_mobilty(mobility_data,pop_info,infile_emodo='~/contacts_and_mobility/contacts_by_age_20200729.csv')
  cat('\n Add pairwise mobility trend ... ')	
  processed_data <- stan_data_add_pairwise_mobility_trends(processed_data,file.path(pkg.dir, 'data','chi_emodo_mobility_trends.rds'))
}
if( args$with_beta_prior_on_each_ifr_by_age_band )
{
  cat('\nAdding beta_prior_on_each_ifr_by_age_band to stan_data ... ')
  processed_data <- stan_data_add_beta_prior_on_ifr_by_age(processed_data, ifr.by.age)
}
if( args$with_lognormal_prior_on_each_ifr_by_age_band )
{
  cat('\nAdding lognormal prior on each ifr by age band to stan_data ... ')
  processed_data <- stan_data_add_lognormal_prior_on_ifr_by_age(processed_data, ifr.by.age)
}
if( args$with_logitincrements_prior_on_ifr_by_age )
{
  cat('\nAdding logit increments_prior_on_each_ifr_by_age_band to stan_data ... ')
  processed_data <- stan_data_add_logitincrements_prior_on_ifr_by_age(processed_data, ifr.by.age)
}
if( args$with_lognormal_prior_relsusceptibility )
{
  cat('\nAdding lognormal_prior_relsusceptibility to stan_data ...')
  processed_data <- stan_data_add_lognormal_prior_relsusceptibility(processed_data, args$file_CI_OR)
}
if( args$with_week_effects )
{
  cat('\nAdding week effects to stan_data ...')
  processed_data <- stan_data_add_week_effects(processed_data)
}
if( args$with_google_mobility && args$with_res_nonres_contactmatrix_model==1 )
{	
  cat('\nAdding resnonres_constraints_on_alpha_beta to stan_data ... ')
  processed_data <- stan_data_add_resnonres_constraints_on_alpha_beta(processed_data, contact_tab)
}
if( args$with_reduced_age_parameters )
{
  cat('\nAdding age map to use reduced age parameters ... ')
  #processed_data <- stan_data_add_age_band_map_7cat_refLast(processed_data)
  #processed_data <- stan_data_add_age_band_map_8cat_ref4(processed_data)
}
if( args$with_eta2 )
{
  cat('\nAdding switch for postprocessing that eta should be applied to both rows and cols ... ')
  processed_data$stan_data$with_eta2 <- 1	
}
if( args$with_inits_20_54 )
{
  cat('\nAdding inits A array for 20 to 54 year olds ... ')
  processed_data <- stan_data_add_initA_array_20_54(processed_data)	 	
}
if( args$with_inits_5_29 )
{
  cat('\nAdding inits A array for 5 to 29 year olds ... ')
  processed_data <- stan_data_add_initA_array_5_29(processed_data)	 	
}
if( args$with_biweekly_upswing_time_effect | 
    args$with_biweekly_upswing_time_effect_for_all_loc 
)
{
  cat('\nAdding upswing time effects ... ')
  processed_data <- stan_data_add_upswing_time_effect(processed_data, mobility_data, effect_weeks=args$effect_weeks)		 	
}
if( args$with_contact_intensities_zhang && !args$with_EduWeek_data )
{
  default_school_reopening_date <- as.Date("2020-08-24")
  cat('\nAdding school status ... ')
  processed_data <- stan_data_add_school_status(
    processed_data = processed_data, 
    path_to_file_school_intervention = args$file_school_Ox_NPI_data, 
    forecast_with_schools_reopened = 1, 
    reopening_Date= default_school_reopening_date
  )    
}
if( args$with_contact_intensities_zhang && args$with_EduWeek_data)
{
  default_school_reopening_date <- as.Date("2020-08-24")
  cat('\nAdding school status with Education Weekly data ... ')
  processed_data <- stan_data_add_school_status_EduWeek(
    processed_data = processed_data, 
    path_to_file_school_Ox_data = args$file_school_Ox_NPI_data, 
    path_to_file_school_EduWeek_data = args$file_school_EduWeek_data,
    default_school_reopening_date = default_school_reopening_date
  )    
}
if( args$with_contact_intensities_zhang && args$with_elementary_schools_status ){
  cat('\nAdding contact intensities during elementary school closure from Zhang et al. and mutliplier... ')
  processed_data <- stan_data_add_contact_school_closure(
    processed_data, 
    dcontact, 
    args$file_contact_intensities_outbreak_China, 
    args$multiplier_cntct_school_closure
  )
}
if( args$with_contact_intensities_zhang && args$with_elementary_high_schools_status ){
  cat('\nAdding contact intensities during elementary and high school closure from Zhang et al. and mutliplier... ')
  processed_data <- stan_data_add_contact_elementary_high_school_closure(
    processed_data, 
    dcontact, 
    args$file_contact_intensities_outbreak_China, 
    args$multiplier_cntct_school_closure
  )
}
if( args$with_contact_intensities_zhang && !args$with_EduWeek_data && args$with_elementary_schools_status )
{
  cat('\nAdding contact intensities after school re-opening from Zhang et al. and mutliplier... ')
  processed_data <- stan_data_add_contact_school_opening(
    processed_data, 
    dcontact, 
    args$file_contact_intensities_outbreak_China, 
    args$multiplier_cntct_school_closure, 
    multiplier_cntct_school_opening=1.0, 
    min_pc_contacts=0.05,
    reopening_date= default_school_reopening_date
  )	  
}
if(args$with_contact_intensities_zhang && args$with_EduWeek_data && args$with_elementary_schools_status){
  cat('\nAdding contact intensities after school re-opening from Zhang et al. and mutliplier... ')
  processed_data <- stan_data_add_contact_school_opening_EduWeek(
    processed_data, 
    dcontact, 
    args$file_contact_intensities_outbreak_China, 
    args$multiplier_cntct_school_closure, 
    multiplier_cntct_school_opening=1.0, 
    min_pc_contacts=0.05
  )	  
}
if(args$with_contact_intensities_zhang && args$with_EduWeek_data && args$with_elementary_high_schools_status){
  cat('\nAdding contact intensities after elementary and high school re-opening from Zhang et al. and mutliplier... ')
  processed_data <- stan_data_add_contact_elementary_high_school_opening_EduWeek(
    processed_data, 
    dcontact, 
    args$file_contact_intensities_outbreak_China, 
    args$multiplier_cntct_school_closure, 
    multiplier_cntct_school_opening=1.0, 
    min_pc_contacts=0.05
  )	  
}
if( args$fix_beta_baseline )
{
  cat('\n Adding fixed beta1 ... ')
  processed_data <- stan_data_fix_beta1(processed_data, pop_info, mobility_data)	 	
}
if( args$with_smoothed_case_data )
{
  cat('\n Adding smoothed log cases... ')
  processed_data <- stan_data_add_smoothed_logcases(processed_data, smoothed_log_cases)				
}
if( args$with_seroprevalence_data )
{
  cat('\n Adding seroprevalence data... ')
  spd <- read_seroprevalence_data(
    args$file_cdc_survey, 
    args$file_cross_sectional_national, 
    with_cross_sectional_national = 1)
  
  processed_data <- stan_data_add_seroprevalence_data(processed_data, spd)				
}
if( args$with_school_case_lkl )
{
  cat('\n Adding school case data... ')  
  processed_data <- stan_data_add_school_cases(processed_data, upper.bound.multiplier= args$school.cases.upper.bound.multiplier,
                                               args$file_school_cases)	
}

stan_data <- processed_data$stan_data
dates <- processed_data$dates
deaths_by_state <- processed_data$deaths_by_state
reported_cases <- processed_data$reported_cases
pop_info <- add_date_to_pop_info(pop_info, stan_data, dates)

# write csv tables for reporting
if(args$makeplots) 
{
  make_tables_for_paper(pop_info, 
                        stan_data, 
                        dates, 
                        deathByAge_data, 
                        death_data,
                        max_jhu, 
                        max_ny,
                        mobility_data,
                        args$states,
                        outfile.base = file.path(args$job_dir, basename(args$job_dir)))
  
  plot_school_status_timeline(stan_data, dates, pop_info, plotdir=args$job_dir)
}

## save image before running Stan
tmp <- names(.GlobalEnv)
tmp <- tmp[!grepl('^.__|^\\.|^model$',tmp)]
save(list=tmp, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_stanin.RData')) )

## make initialisations
stan_init <- list()
stan_init$R0 <- rep(3.28, stan_data$M) + rnorm(stan_data$M, 0,0.2)		
if(args$with_lognormal_prior_on_each_ifr_by_age_band)
{
  stan_init$log_ifr_age <- rnorm(stan_data$A, stan_data$hyperpara_ifr_age_lnmu, stan_data$hyperpara_ifr_age_lnsd/2)
}
if(args$with_ifr_rnde_mid1_mid2_old)
{
  stan_init$log_ifr_age_rnde_mid1 <- rnorm(stan_data$M, 0,0.005)
  stan_init$log_ifr_age_rnde_mid2 <- rnorm(stan_data$M, 0,0.005)
  stan_init$log_ifr_age_rnde_old <- rnorm(stan_data$M, 0,0.005)			
}
if(args$with_ifr_rnde_mid1_mid2_expold)
{
  stan_init$log_ifr_age_rnde_mid1 <- rnorm(stan_data$M, 0,0.005)
  stan_init$log_ifr_age_rnde_mid2 <- rnorm(stan_data$M, 0,0.005)
  stan_init$log_ifr_age_rnde_old <- rexp(stan_data$M, 100)			
}
if(args$with_dip_rnde)
{
  stan_init$dip_rnde <- rnorm(stan_data$M, 0,0.005)
}
if(args$with_upswing_rnde)
{
  stan_init$upswing_rnde <- rnorm(stan_data$M, 0,0.005)
}
if(args$with_upswing_age2034_rnde)
{
  stan_init$timeeff_shift_young2 <- rnorm(stan_data$M, 0, 0.005)
}
if(args$with_upswing_age2039_rnde)
{
  stan_init$timeeff_shift_mid1 <- rnorm(stan_data$M, 0, 0.005)
}
if(args$with_upswing_age3549_rnde)
{
  stan_init$timeeff_shift_mid1 <- rnorm(stan_data$M, 0, 0.005)
}
if(args$with_upswing_age2049_rnde)
{
  stan_init$timeeff_shift_mid1 <- rnorm(stan_data$M, 0, 0.005)
}
if(args$with_upswing_age5064_rnde)
{
  stan_init$timeeff_shift_mid2 <- rnorm(stan_data$M, 0, 0.005)
}
if( args$with_biweekly_upswing_time_effect )
{
  stan_init$upswing_timeeff_reduced <- rep(0.01, stan_data$N_IMP)
}
if( args$with_biweekly_upswing_time_effect_for_all_loc )
{
  stan_init$upswing_timeeff_reduced <- matrix(0.01, nrow=stan_data$N_IMP, ncol=stan_data$M)
}
if( args$with_ifr_rnde_expmid1_expmid2_expold )
{
  stan_init$log_ifr_age_rnde_mid1 <- rep(0.01, stan_data$M)
  stan_init$log_ifr_age_rnde_mid2 <- rep(0.01, stan_data$M)
  stan_init$log_ifr_age_rnde_old <- rep(0.01, stan_data$M)
}
if( args$with_upswing_expmid1_rnde )
{
  stan_init$timeeff_shift_mid1 <- rep(0.01, stan_data$M)
  stan_init$hyper_timeeff_shift_mid1 <- 100
}
if( args$with_upswing_expmid2_rnde )
{
  stan_init$timeeff_shift_mid2 <- rep(0.01, stan_data$M)
  stan_init$hyper_timeeff_shift_mid2 <- 100
}
if( args$with_upswing_expold_rnde )
{
  stan_init$timeeff_shift_old <- rep(0.01, stan_data$M)
  stan_init$hyper_timeeff_shift_old <- 100
}
if( args$with_timeidx_rebound )
{
  stan_init$log_ifr_overall_upswing_effect <- -0.0001
}


if(args$cmdstan)
{
  ## write data file
  rstan::stan_rdump( names(stan_data), file=file.path(args$job_dir, paste0(basename(args$job_dir), '_cmdstanin.R')), envir=list2env(stan_data))  	
  ## write init file
  rstan::stan_rdump( names(stan_init), file=file.path(args$job_dir, paste0(basename(args$job_dir), '_cmdstaninit.R')), envir=list2env(stan_init))	  	
}

if(!args$cmdstan)
{	
  ## run Stan
  cat('\nRunning Stan... \n')
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(args$file_stanModel)
  
  if(args$DEBUG) 
  {
    fit <- rstan::sampling(model,data=stan_data,iter=10,warmup=5,chains=1,seed=args$seed,init=list(stan_init),verbose=TRUE)
  } 
  else 
  { 
    # uncomment the line below for a full run to replicate results and comment the second line below 
    # fit <- rstan::sampling(model,data=stan_data,iter=2000,warmup=500,chains=1,seed=args$seed,thin=1, control = list(adapt_delta = 0.95, max_treedepth = 10))
    fit <- rstan::sampling(model,data=stan_data,iter=2000, warmup=1500,chains=1,seed=args$seed,thin=1, control = list(adapt_delta = 0.95, max_treedepth = 15))
    #fit = rstan::sampling(model,data=stan_data,iter=200,warmup=100,chains=1,seed=args$seed,thin=4,control = list(adapt_delta = 0.95, max_treedepth = 10))
    #fit <- rstan::sampling(model,data=stan_data,iter=10,warmup=5,chains=1,seed=args$seed,verbose=TRUE)
  }  
  
  ## save image after running Stan
  save(fit, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_stanout.RData')) )
}
