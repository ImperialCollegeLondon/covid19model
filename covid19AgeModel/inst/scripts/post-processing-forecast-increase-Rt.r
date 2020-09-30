# post-processing-forecast-increase-Rt.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-forecast-increase-Rt.r \n \n -------------------------------- \n")

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
  stopifnot(args_line[[13]]=='-period_length')	
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.numeric(args_line[[8]])
  args_dir[['with_forecast']] <- as.numeric(args_line[[10]])
  args_dir[['multiplier_cntct_school_opening']] <- as.numeric(args_line[[12]])
  args_dir[['period_length']] <- as.numeric(args_line[[14]])
} 

outfile.base <- paste0(args_dir$out_dir, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

multiplier = (args_dir$multiplier_cntct_school_opening)*100

suffix_sensitivity_school0 = '_sensitivity_school_reopen_0'
suffix_sensitivity_school1 = paste0('_sensitivity_school_reopen_1_multiplier_', multiplier)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# add age labels to reduced flows
pop_info_2 = add_pop_info_age_school_children(plot.pars.basic$pop_info)
age_cat_map <- make_age_cat_map_7_school_chidren(pop_info_2)

# Set start and end date of the forecast period with school opened
start_date = as.Date("2020-08-24") # 3 months period
end_date = as.Date("2020-11-24") # 3 months period

#
# periodicity of the estimate (1 daily, 7 weekly etc.)
period_length =  args_dir$period_length

#
# Load Rt
file <- paste0(outfile.base,'-stanout-RtByAge-gqs',suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
RtByAge_gqs_school_reopen_0 <- readRDS(file)

file <- paste0(outfile.base,'-stanout-RtByAge-gqs',suffix_sensitivity_school1,'.RDS')
cat("\n read RDS:", file)
RtByAge_gqs_school_reopen_1 <- readRDS(file)

#
# Load effective infectious
file <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs',suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
E_effcasesByAge_reopen_0 <- readRDS(file)

file <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs',suffix_sensitivity_school1,'.RDS')
cat("\n read RDS:", file)
E_effcasesByAge_reopen_1 <- readRDS(file)

#
# Summarise Rt by age
RtByAge_summary_forecast_school_reopen = summarise_Rt_instantaneous_byage_forecast_school_reopen(E_effcasesByAge_reopen_0,
                                                                                   E_effcasesByAge_reopen_1,
                                                                                   RtByAge_gqs_school_reopen_0, 
                                                                                   RtByAge_gqs_school_reopen_1, 
                                                                                   period_length,
                                                                                   age_cat_map, 
                                                                                   pop_info_2, 
                                                                                   plot.pars.basic$dates, 
                                                                                   plot.pars.basic$regions)

# 
# How many states have an Rt above 1 at end date with school re-open compared to school close
tmp_school_reopen1 = subset(RtByAge_summary_forecast_school_reopen, variable == "school_reopen1" & date == end_date & loc_label != "United-States" & age_cat == 0)
tmp_school_reopen1[, isRtgreaterthan1 := ifelse(CU > 1, 1, 0)]
tmp_school_reopen0 = subset(RtByAge_summary_forecast_school_reopen, variable == "school_reopen0" & date == end_date & loc_label != "United-States" & age_cat == 0)
tmp_school_reopen0[, isRtgreaterthan1 := ifelse(CU > 1, 1, 0)]
abs_states = sum(tmp_school_reopen1$isRtgreaterthan1) - sum(tmp_school_reopen0$isRtgreaterthan1)
prop_states = abs_states/nrow(tmp_school_reopen1)
ans1 <- list(abs_states, paste0(prop_states*100, "\\%"), format(end_date, "%B %d, %Y") , format(end_date, "%B %d, %Y") )


# 
# How many states have an Rt above 1 at begin date and end date with school re-open 
tmp_school_reopen1_start = subset(RtByAge_summary_forecast_school_reopen, variable == "school_reopen1" & date == start_date & loc_label != "United-States" & age_cat == 0)
tmp_school_reopen1_start[, isRtgreaterthan1 := ifelse(CU > 1, 1, 0)]
states_Rtgreaterthan1_start = sum(tmp_school_reopen1_start$isRtgreaterthan1) 
states_Rtgreaterthan1_end = sum(tmp_school_reopen1$isRtgreaterthan1)
ans2 <- list(states_Rtgreaterthan1_start, states_Rtgreaterthan1_end, format(start_date, "%B %d, %Y") , format(end_date, "%B %d, %Y") )


#
# Save
file <- paste0(outfile.base,'-Rt-age-summary-forecast_multiplier', multiplier, '.rds')
cat("\nWrite table to",file)
saveRDS(RtByAge_summary_forecast_school_reopen, file = file, version = 2)

file <- paste0(outfile.base,'-excess-Rt-greater-than-1-end-date_multiplier', multiplier, '.rds')
cat("\nWrite table to",file)
saveRDS(ans1, file = file, version = 2)

file <- paste0(outfile.base,'-excess-Rt-greater-than-1-forecast-period_multiplier', multiplier, '.rds')
cat("\nWrite table to",file)
saveRDS(ans2, file = file, version = 2)

cat(" \n -------------------------------- \n \n End post-processing-forecast-increase-Rt.r \n \n -------------------------------- \n")
