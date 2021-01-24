# post-processing-forecast-plot-cases.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-forecast-plot-cases.R \n \n -------------------------------- \n")

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
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015i3_cmdstanv'
args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_201015i3_cmdstanv-40states_Oct29_Levin7_schoolbound6'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015i3_cmdstanv-40states_Oct29_Levin7_schoolbound6'
args_dir[['job_tag']] <- '40states_Oct29_Levin7_schoolbound6'
args_dir[['overwrite']] = 0
args_dir[['with_forecast']] = 1
args_dir[['school_level']] = "K12"
args_dir[['multiplier_cntct_school_opening']] = 1
args_dir[['counterfactual.scenario']] = 0

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
  stopifnot(args_line[[13]]=='-school_level')	
  stopifnot(args_line[[15]]=='-counterfactual.scenario')
  
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.numeric(args_line[[8]])
  args_dir[['with_forecast']] <- as.numeric(args_line[[10]])
  args_dir[['multiplier_cntct_school_opening']] <- as.numeric(args_line[[12]])
  args_dir[['school_level']] <- as.character(args_line[[14]])
  args_dir[['counterfactual.scenario']] <- args_line[[16]]
} 

outfile.base <- paste0(args_dir$out_dir, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
multiplier = (args_dir$multiplier_cntct_school_opening)*100

suffix_sensitivity_school0 = paste0('_sensitivity_school_reopen_0', '_level_', args_dir$school_level)
suffix_sensitivity_school1 = paste0('_sensitivity_school_reopen_1', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level)


# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_school1,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# add age labels to reduced flows
if(args_dir$school_level == "K5"){
  pop_info_2 = add_pop_info_age_school_children(plot.pars.basic$pop_info)
  age_cat_map <- make_age_cat_map_7_school_chidren(pop_info_2)
  age.school = "0-11"; age.school.new = "0-11"
}
if(args_dir$school_level == "K12"){
  pop_info_2 = add_pop_info_age_school_children_teen(plot.pars.basic$pop_info)
  age_cat_map <- make_age_cat_map_7_school_chidren_teen(pop_info_2)
  age.school = c("0-9", "10-18"); age.school.new = "0-18"
}

#
# summarise overall estimated cases
file <- paste0(outfile.base,'-summary-cases_s',suffix_sensitivity_school1,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school1,'.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge_gqs_school_reopen_1 <- readRDS(file2)
  
  cat("\n ----------- summarise_cases_s ----------- \n")
  
  cases_school_reopen_1 <- make_casesoverall_summaries(E_casesByAge_gqs_school_reopen_1, 
                                         plot.pars.basic$dates, 
                                         plot.pars.basic$regions, 
                                         plot.pars.basic$pop_info)
  E_casesByAge_gqs_school_reopen_1 <- NULL
  
  cat("\nWrite ",file," ... ")
  saveRDS(cases_school_reopen_1, file=file)		
}
if(file.exists(file))
{
  cases_school_reopen_1 <- readRDS(file)
}

#
# summarise overall estimated cases
file <- paste0(outfile.base,'-summary-cases_s',suffix_sensitivity_school0,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school0,'.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge_gqs_school_reopen_0 <- readRDS(file2)
  
  cat("\n ----------- summarise_cases_s ----------- \n")
  
  cases_school_reopen_0 <- make_casesoverall_summaries(E_casesByAge_gqs_school_reopen_0, 
                                                       plot.pars.basic$dates, 
                                                       plot.pars.basic$regions, 
                                                       plot.pars.basic$pop_info)
  E_casesByAge_gqs_school_reopen_0 <- NULL
  
  cat("\nWrite ",file," ... ")
  saveRDS(cases_school_reopen_0, file=file)		
}
if(file.exists(file))
{
  cases_school_reopen_0 <- readRDS(file)
}


#
#	handle if forecast period is not to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
  cases_school_reopen_0 <- subset(cases_school_reopen_0, date<=date.max)
  cases_school_reopen_1 <- subset(cases_school_reopen_1, date<=date.max)
}

cat(" \n -------------------------------- \n  generating standalone plots \n -------------------------------- \n")

cat("\n ----------- plot cases by age: start ----------- \n")

#
plot_cases_overall_overtime_forecast_school_reopen( 
  subset(cases_school_reopen_1, loc != "US"), 
  subset(cases_school_reopen_0, loc != "US"),
  plot.pars.basic$dates,
  plot.pars.basic$regions,
  plot.pars.basic$stan_data,
  paste0('_counterfactual_', args_dir$counterfactual.scenario, "_multiplier_", multiplier,"_level_", args_dir$school_level),
  outfile.base
)




cat(" \n -------------------------------- \n \n End post-processing-forecast-plot-cases.R \n \n -------------------------------- \n")


# cases_school_reopen_1_cum = copy(cases_school_reopen_1)
# cases_school_reopen_1_cum[, M := cumsum(M), by = c('loc', 'loc_label')]
# cases_school_reopen_1_cum[, CL := cumsum(CL), by = c('loc', 'loc_label')]
# cases_school_reopen_1_cum[, CU := cumsum(CU), by = c('loc', 'loc_label')]
# cases_school_reopen_0_cum = copy(cases_school_reopen_0)
# cases_school_reopen_0_cum[, M := cumsum(M), by = c('loc', 'loc_label')]
# cases_school_reopen_0_cum[, CL := cumsum(CL), by = c('loc', 'loc_label')]
# cases_school_reopen_0_cum[, CU := cumsum(CU), by = c('loc', 'loc_label')]
# 
# plot_cases_overall_overtime_forecast_school_reopen( 
#   subset(cases_school_reopen_1_cum, loc != "US"), 
#   subset(cases_school_reopen_0_cum, loc != "US"),
#   plot.pars.basic$dates,
#   plot.pars.basic$regions,
#   plot.pars.basic$stan_data,
#   paste0('_counterfactual_', args_dir$counterfactual.scenario, "_multiplier_", multiplier,"_level_", args_dir$school_level),
#   outfile.base
# )
# 
# #
# # summary of cumulated cases
# file <- paste0(outfile.base,'-cases-age-summary-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
# if(!file.exists(file)){
#   # Load cases
#   file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school0,'.RDS')
#   cat("\n read RDS:", file2)
#   E_casesByAge_gqs_school_reopen_0 <- readRDS(file2)
#   
#   file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school1,'.RDS')
#   cat("\n read RDS:", file2)
#   E_casesByAge_gqs_school_reopen_1 <- readRDS(file2)
#   
#   cum_cases_summary_forecast_school_reopen = summarise_cum_cases_deaths_by_age_forecast_school_reopen(E_casesByAge_gqs_school_reopen_0, 
#                                                                                                       E_casesByAge_gqs_school_reopen_1, 
#                                                                                                       pop_info_2, 
#                                                                                                       start_date,
#                                                                                                       plot.pars.basic$dates, 
#                                                                                                       age_cat_map, 
#                                                                                                       plot.pars.basic$regions)
#   cat("\nWrite table to",file)
#   saveRDS(cum_cases_summary_forecast_school_reopen, file = file, version = 2)
# } else{
#   cum_cases_summary_forecast_school_reopen = readRDS(file)
# }
# 
# subset(cases_school_reopen_1_cum, loc == 'TX'& date == as.Date('2020-10-29'))
# 
# plot_cases_overall_overtime_forecast_school_reopen(
#   subset(cum_cases_summary_forecast_school_reopen, age_band == 'Overall' & variable == 'school_reopen1'), 
#   subset(cum_cases_summary_forecast_school_reopen, age_band == 'Overall'& variable == 'school_reopen0'),
#   plot.pars.basic$dates,
#   plot.pars.basic$regions,
#   plot.pars.basic$stan_data,
#   paste0('_counterfactual_', args_dir$counterfactual.scenario, "_multiplier_", multiplier,"_level_", args_dir$school_level),
#   outfile.base
# )
# cases_school_reopen_1_cum_total = cases_school_reopen_1_cum[, list(M = sum(M)), by= c('date')]
# cases_school_reopen_0_cum_total = cases_school_reopen_0_cum[, list(M = sum(M)), by= c('date')]
# 
# subset(cases_school_reopen_1_cum_total, date == start_date - 1)
# subset(cases_school_reopen_0_cum_total, date ==  as.Date('2020-10-29'))
# subset(cum_cases_summary_forecast_school_reopen, loc == "AllStates" & age_band == 'Overall' & variable == 'school_reopen1' & date == as.Date('2020-10-29'))
# 
