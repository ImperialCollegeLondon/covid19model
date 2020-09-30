# post-processing-forecast-plot-deaths.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-forecast-plot-deaths.r \n \n -------------------------------- \n")

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

#	for dev purposes: melodie
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
args_dir[['job_tag']] <- '37states_Sep2'
args_dir[['overwrite']] = 0
args_dir[['with_forecast']] = 1
args_dir[['multiplier_cntct_school_opening']] = 0.5

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-overwrite')
  stopifnot(args_line[[9]]=='-with_forecast')
  stopifnot(args_line[[11]]=='-multiplier_cntct_school_opening')	
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.numeric(args_line[[8]])
  args_dir[['with_forecast']] <- as.numeric(args_line[[10]])
  args_dir[['multiplier_cntct_school_opening']] <- as.numeric(args_line[[12]])
} 


## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

multiplier = (args_dir$multiplier_cntct_school_opening)*100

suffix_sensitivity_school0 = '_sensitivity_school_reopen_0'
suffix_sensitivity_school1 = paste0('_sensitivity_school_reopen_1_multiplier_', multiplier)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

E_deathsByAge <- NULL

cat(" \n -------------------------------- \n  summarise deaths and case samples \n -------------------------------- \n")

#
# summarise overall deaths
file <- paste0(outfile.base,'-summary-deaths-overall',suffix_sensitivity_school0,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_deathsByAge))
  {
    file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_school0,'.RDS')
    cat("\n read RDS:", file2)
    E_deathsByAge <- readRDS(file2)
  }
  
  cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
  deaths_school_reopen_0 <- make_deathsoverall_summaries(E_deathsByAge, 
                                                         plot.pars.basic$dates, 
                                                         plot.pars.basic$regions, 
                                                         plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(deaths_school_reopen_0, file=file)
}
if(file.exists(file))
{
  deaths_school_reopen_0 <- readRDS(file)
}

file <- paste0(outfile.base,'-summary-deaths-overall',suffix_sensitivity_school1,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_deathsByAge))
  {
    file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_school1,'.RDS')
    cat("\n read RDS:", file2)
    E_deathsByAge <- readRDS(file2)
  }
  
  cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
  deaths_school_reopen_1 <- make_deathsoverall_summaries(E_deathsByAge, 
                                           plot.pars.basic$dates, 
                                           plot.pars.basic$regions, 
                                           plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(deaths_school_reopen_1, file=file)
}
if(file.exists(file))
{
  deaths_school_reopen_1 <- readRDS(file)
}
gc()


cat(" \n -------------------------------- \n  generating standalone plots \n -------------------------------- \n")

cat("\n ----------- plot deaths by age ----------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
  deaths_school_reopen_1 <- subset(deaths_school_reopen_1, date<=date.max)
  deaths_school_reopen_0 <- subset(deaths_school_reopen_0, date<=date.max)
}

#
# make overall deaths plots 
cat(" \n -------------------------------- \n generate overall death plots: start \n -------------------------------- \n")
cd_data <- copy(plot.pars.basic$death_data)
setnames(cd_data, c('state','code'), c('loc_label','loc'))
deaths.plots <- plot_deaths_overall_overtime_forecast_school_reopen( deaths_school_reopen_1, 
                                                                     deaths_school_reopen_0,
                                                                     cd_data, 
                                                                     plot.pars.basic$dates,
                                                                     plot.pars.basic$regions,
                                                                     multiplier,
                                                                     outfile.base)

cat(" \n -------------------------------- \n generate overall death plots: end \n -------------------------------- \n")

cat(" \n -------------------------------- \n \n End post-processing-forecast-plot-deaths.r \n \n -------------------------------- \n")
