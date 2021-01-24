# post-processing-forecast-increase-deaths-cases.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-forecast-increase-deaths-cases.r \n \n -------------------------------- \n")

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
args_dir[['stanModelFile']] <- 'covid19AgeModel_report32_cmdstanv'
args_dir[['out_dir']] <- '/Users/melodiemonod/short_run_test/covid19AgeModel_report32_cmdstanv-37states_tau10_report32'
args_dir[['job_tag']] <- '37states_tau10_report32'
args_dir[['overwrite']] = 0
args_dir[['with_forecast']] = 1
args_dir[['school_level']] = "K5"
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


# Set start and end date of the forecast period with school opened
start_date = as.Date("2020-08-24") # first day of school reopening across states
end_date = max(as.Date(sapply(plot.pars.basic$dates, function(x)max(as.character(x))))) # last day of death data
if(args_dir[['with_forecast']] == 1) end_date = min(as.Date(sapply(plot.pars.basic$dates, function(x) max(as.character(min(x) + plot.pars.basic$stan_data$N2 - 1))))) 

#
# summary of cumulated cases
file <- paste0(outfile.base,'-cases-age-summary-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
if(!file.exists(file)){
  # Load cases
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school0,'.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge_gqs_school_reopen_0 <- readRDS(file2)
  
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity_school1,'.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge_gqs_school_reopen_1 <- readRDS(file2)
  
  cum_cases_summary_forecast_school_reopen = summarise_cum_cases_deaths_by_age_forecast_school_reopen(E_casesByAge_gqs_school_reopen_0, 
                                                                                                      E_casesByAge_gqs_school_reopen_1, 
                                                                                                      pop_info_2, 
                                                                                                      start_date,
                                                                                                      plot.pars.basic$dates, 
                                                                                                      age_cat_map, 
                                                                                                      plot.pars.basic$regions)
  cat("\nWrite table to",file)
  saveRDS(cum_cases_summary_forecast_school_reopen, file = file, version = 2)
} else{
  cum_cases_summary_forecast_school_reopen = readRDS(file)
}


#
# summary of cumulated deaths
file <- paste0(outfile.base,'-deaths-age-summary-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
if(!file.exists(file))
{
  # Load deaths
  file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_school0,'.RDS')
  cat("\n read RDS:", file2)
  E_deathsByAge_gqs_school_reopen_0 <- readRDS(file2)
  
  file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_school1,'.RDS')
  cat("\n read RDS:", file2)
  E_deathsByAge_gqs_school_reopen_1 <- readRDS(file2)
  
  cum_deaths_summary_forecast_school_reopen = summarise_cum_cases_deaths_by_age_forecast_school_reopen(E_deathsByAge_gqs_school_reopen_0, 
                                                                                                       E_deathsByAge_gqs_school_reopen_1, 
                                                                                                       pop_info_2, 
                                                                                                       start_date,
                                                                                                       plot.pars.basic$dates, 
                                                                                                       age_cat_map, 
                                                                                                       plot.pars.basic$regions)
  
  cat("\nWrite table to",file)
  saveRDS(cum_deaths_summary_forecast_school_reopen, file = file, version = 2)
  
} else{
  cum_deaths_summary_forecast_school_reopen = readRDS(file)
}

#
# Table: Infection ratio school  close 
cases_schoolreopen0 = subset(cum_cases_summary_forecast_school_reopen, variable == "school_reopen0" & date == end_date )
# group by school children 
tmp = subset(cases_schoolreopen0, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
cases_schoolreopen0 = rbind(subset(cases_schoolreopen0, !age_band %in% age.school), tmp)
cases_schoolreopen0[, L:= paste0(prettyNum(round(M),big.mark=","),' [',prettyNum(round(CL),big.mark=","),'-',prettyNum(round(CU),big.mark=","),']')]
cases_schoolreopen0 <- dcast.data.table(cases_schoolreopen0, loc_label~age_band, value.var='L')
excess_cases_overall <- subset(cases_schoolreopen0, loc_label == "All States") # national level
cases_schoolreopen0 <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cases_schoolreopen0, by='loc_label',all.x=TRUE)
for(x in colnames(cases_schoolreopen0))
{
  set(cases_schoolreopen0, which(is.na(cases_schoolreopen0[[x]])), x, '-') 
}
cases_schoolreopen0 <- rbind(excess_cases_overall, cases_schoolreopen0)
cases_schoolreopen0 <- cbind(cases_schoolreopen0[, "loc_label"], cases_schoolreopen0[, "Overall"], cases_schoolreopen0[, -c("Overall", "loc_label")])

#
# Table: Excess infection difference between school reopening and school  close
excess_cases_abs = subset(cum_cases_summary_forecast_school_reopen, variable == "school_reopen1_reopen0_diff" & date == end_date )
# group by school children 
tmp = subset(excess_cases_abs, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
excess_cases_abs = rbind(subset(excess_cases_abs, !age_band %in% age.school), tmp)
excess_cases_abs[, L:= paste0(prettyNum(round(M),big.mark=","),' [',prettyNum(round(CL),big.mark=","),'-',prettyNum(round(CU),big.mark=","),']')]
excess_cases_abs <- dcast.data.table(excess_cases_abs, loc_label~age_band, value.var='L')
excess_cases_overall <- subset(excess_cases_abs, loc_label == "All States") # national level
excess_cases_abs <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), excess_cases_abs, by='loc_label',all.x=TRUE)
for(x in colnames(excess_cases_abs))
{
  set(excess_cases_abs, which(is.na(excess_cases_abs[[x]])), x, '-') 
}
excess_cases_abs <- rbind(excess_cases_overall, excess_cases_abs)
excess_cases_abs <- cbind(excess_cases_abs[, "loc_label"], excess_cases_abs[, "Overall"], excess_cases_abs[, -c("Overall", "loc_label")])


#
# Table: Excess infection ratio between school reopening and school staying close
excess_cases = subset(cum_cases_summary_forecast_school_reopen, variable == "school_reopen1_reopen0_ratio" & date == end_date )
# group by school children 
tmp = subset(excess_cases, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
excess_cases = rbind(subset(excess_cases, !age_band %in% age.school), tmp)
excess_cases[, L:= paste0(paste0(prettyNum(sprintf("%.1f", (M-1)*100),big.mark=","), '\\%'),' [',paste0(prettyNum(sprintf("%.1f", (CL-1)*100),big.mark=","), '\\%'),'-', paste0(prettyNum(sprintf("%.1f", (CU-1)*100),big.mark=","), '\\%'),']'), by = c('loc_label', 'age_cat')]
excess_cases <- dcast.data.table(excess_cases, loc_label~age_band, value.var='L')
excess_cases_overall <- subset(excess_cases, loc_label == "All States") # national level
excess_cases <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), excess_cases, by='loc_label',all.x=TRUE)
for(x in colnames(excess_cases))
{
  set(excess_cases, which(is.na(excess_cases[[x]])), x, '-') 
}
excess_cases <- rbind(excess_cases_overall, excess_cases)
excess_cases <- cbind(excess_cases[, "loc_label"], excess_cases[, "Overall"], excess_cases[, -c("Overall", "loc_label")])


#
# Table: Deaths school staying close
deaths_schoolreopen0 = subset(cum_deaths_summary_forecast_school_reopen, variable == "school_reopen0" & date == end_date )
# group by school children 
tmp = subset(deaths_schoolreopen0, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
deaths_schoolreopen0 = rbind(subset(deaths_schoolreopen0, !age_band %in% age.school), tmp)
deaths_schoolreopen0 = deaths_schoolreopen0[, list(L= paste0(  prettyNum(round(M), big.mark=",") , " ", roundCI(CL, CU, 0))), by = c("loc_label", "age_band")]
deaths_schoolreopen0 <- dcast.data.table(deaths_schoolreopen0, loc_label~age_band, value.var='L')
deaths_schoolreopen0_overall <- subset(deaths_schoolreopen0, loc_label == "All States") # national level
deaths_schoolreopen0 <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), deaths_schoolreopen0, by='loc_label',all.x=TRUE)
for(x in colnames(deaths_schoolreopen0))
{
  set(deaths_schoolreopen0, which(is.na(deaths_schoolreopen0[[x]])), x, '-') 
}
deaths_schoolreopen0 <- rbind(deaths_schoolreopen0_overall, deaths_schoolreopen0)
deaths_schoolreopen0 <- cbind(deaths_schoolreopen0[, "loc_label"], deaths_schoolreopen0[, "Overall"], deaths_schoolreopen0[, -c("Overall", "loc_label")])

#
# Table: Excess deaths absolute difference between school reopening and school staying close
excess_deaths_abs = subset(cum_deaths_summary_forecast_school_reopen, variable == "school_reopen1_reopen0_diff" & date == end_date )
# group by school children 
tmp = subset(excess_deaths_abs, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
excess_deaths_abs = rbind(subset(excess_deaths_abs, !age_band %in% age.school), tmp)
excess_deaths_abs = excess_deaths_abs[, list(L= paste0( prettyNum(round(M), big.mark=","), " ", roundCI(CL, CU, 0))), by = c("loc_label", "age_band")]
excess_deaths_abs <- dcast.data.table(excess_deaths_abs, loc_label~age_band, value.var='L')
excess_deaths_abs_overall <- subset(excess_deaths_abs, loc_label == "All States") # national level
excess_deaths_abs <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), excess_deaths_abs, by='loc_label',all.x=TRUE)
for(x in colnames(excess_deaths_abs))
{
  set(excess_deaths_abs, which(is.na(excess_deaths_abs[[x]])), x, '-') 
}
excess_deaths_abs <- rbind(excess_deaths_abs_overall, excess_deaths_abs)
excess_deaths_abs <- cbind(excess_deaths_abs[, "loc_label"], excess_deaths_abs[, "Overall"], excess_deaths_abs[, -c("Overall", "loc_label")])

#
# Table: Excess deaths ratio between school reopening and school staying close
excess_deaths = subset(cum_deaths_summary_forecast_school_reopen, variable == "school_reopen1_reopen0_ratio" & date == end_date )
# group by school children 
tmp = subset(excess_deaths, age_band %in% age.school)
tmp = tmp[, list(CL = sum(CL), CU = sum(CU), M = sum(M)), by = c("variable", "date", "loc_label", "loc")]
tmp[, age_band := age.school.new]
tmp[, age_cat := 1]
excess_deaths = rbind(subset(excess_deaths, !age_band %in% age.school), tmp)
excess_deaths[, L:= paste0(paste0(prettyNum(sprintf("%.1f", (M-1)*100),big.mark=","), '\\%'),' [',paste0(prettyNum(sprintf("%.1f", (CL-1)*100),big.mark=","), '\\%'),'-', paste0(prettyNum(sprintf("%.1f", (CU-1)*100),big.mark=","), '\\%'),']'), by = c('loc_label', 'age_cat')]
excess_deaths <- dcast.data.table(excess_deaths, loc_label~age_band, value.var='L')
excess_deaths_overall <- subset(excess_deaths, loc_label == "All States") # national level
excess_deaths <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), excess_deaths, by='loc_label',all.x=TRUE)
for(x in colnames(excess_deaths))
{
  set(excess_deaths, which(is.na(excess_deaths[[x]])), x, '-') 
}
excess_deaths <- rbind(excess_deaths_overall, excess_deaths)
excess_deaths <- cbind(excess_deaths[, "loc_label"], excess_deaths[, "Overall"], excess_deaths[, -c("Overall", "loc_label")])


#
# Create table: excess infections in 0-9, excess infections overall, excess deaths overall ratio, excess deaths absolute 
tmp = cbind(excess_cases[, "loc_label"], cases_schoolreopen0[,"Overall"], excess_cases_abs[,"Overall"], excess_cases[,"Overall"], deaths_schoolreopen0[,"Overall"], excess_deaths_abs[,"Overall"], excess_deaths[,"Overall"])

#
# Save
ans <- list(tmp, format(start_date, "%B %d, %Y"), format(end_date, "%B %d, %Y") )
file <- paste0(outfile.base,'-excess-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

cat(" \n -------------------------------- \n \n End: post-processing-forecast-increase-deaths-cases.r \n \n -------------------------------- \n")
