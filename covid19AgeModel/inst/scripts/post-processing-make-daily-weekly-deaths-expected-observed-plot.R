# post-processing-make-deaths-panel-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-deaths-panel-plot.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(magick, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
  args_dir[['job_tag']] <- '37states_Sep2'
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
}

if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
  args_dir[['job_tag']] <- '37states_Sep2'
  
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-overwrite')
  stopifnot(args_line[[9]]=='-with_forecast')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.integer(args_line[[8]])
  args_dir[['with_forecast']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

E_deathsByAge <- NULL

cat(" \n -------------------------------- \n summarise samples: start \n -------------------------------- \n")

#
# map age groups for state death data to model groupings
age_state <- map_deaths_ages(plot.pars.basic$deathByAge_data,
                             plot.pars.basic$dages,
                             plot.pars.basic$dc)

#
#
agg_weekly_deaths = 1

#
# summarise cumulated deaths
file <- paste0(outfile.base,'-summary-daily-deaths-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_deathsByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_deaths_byage_c ----------- \n")
  e_adeaths_daily <- make_daily_deaths_by_age_summaries(E_deathsByAge,
                                                plot.pars.basic$pop_info,
                                                plot.pars.basic$dates,
                                                age_state,
                                                plot.pars.basic$regions)	
  cat("\nWrite ",file," ... ")
  saveRDS(e_adeaths_daily, file=file)
}
if(file.exists(file))
{
  e_adeaths_daily <- readRDS(file)
}
file <- paste0(outfile.base,'-summary-weekly-deaths-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_deathsByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_deathsByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_deaths_byage_c ----------- \n")
  e_adeaths_weekly <- make_weekly_deaths_by_age_summaries(E_deathsByAge,
                                                  plot.pars.basic$pop_info,
                                                  plot.pars.basic$dates,
                                                  age_state,
                                                  plot.pars.basic$regions)	
  cat("\nWrite ",file," ... ")
  saveRDS(e_adeaths_weekly, file=file)
}
if(file.exists(file))
{
  e_adeaths_weekly <- readRDS(file)
}

df.daily = find_observed_death_fourageband(e_adeaths_daily, "daily")
df.weekly = find_observed_death_fourageband(e_adeaths_weekly, "weekly")

plot_expected_observed_death(df.daily, "daily")
plot_expected_observed_death(df.weekly, "weekly")
