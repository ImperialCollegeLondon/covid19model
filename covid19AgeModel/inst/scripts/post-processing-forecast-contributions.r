# post-processing-forecast-contributions.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-forecast-contributions.r \n \n -------------------------------- \n")

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
args_dir[['school_level']] = "K5"

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
file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# add age labels to reduced flows
if(args_dir$school_level == "K5"){
  pop_info_2 = add_pop_info_age_school_children(plot.pars.basic$pop_info)
  age_cat_map <- make_age_cat_map_7_school_chidren(pop_info_2)
  age.school = "0-11"
}
if(args_dir$school_level == "K12"){
  pop_info_2 = add_pop_info_age_school_children_teen(plot.pars.basic$pop_info)
  age_cat_map <- make_age_cat_map_7_school_chidren_teen(pop_info_2)
  age.school = c("0-9", "10-18")
}

# Set start and end date of the forecast period with school opened
start_date = as.Date("2020-08-24") # first day of school reopening across states
end_date = as.Date("2020-10-29") # last day of death data

#
# Load flows
file <- paste0(outfile.base,'-stanout-flows-gqs', suffix_sensitivity_school0,'.RDS')
cat("\n read RDS:", file)
flows_gqs_school_reopen_0 <- readRDS(file)

file <- paste0(outfile.base,'-stanout-flows-gqs', suffix_sensitivity_school1,'.RDS')
cat("\n read RDS:", file)
flows_gqs_school_reopen_1 <- readRDS(file)

#
# Make cumulative flow summary with the two scenarios of school status
if(args_dir$school_level == "K5"){
cumpropflow_byage = summarise_cumprop_flow_byage_forecast_school_reopen_K5(flows_gqs_school_reopen_1$reduced_flows, 
                                                                        flows_gqs_school_reopen_0$reduced_flows,
                                                                        start_date,
                                                                        plot.pars.basic$stan_data$reduced_flows_Monday_idx, 
                                                                        age_cat_map,
                                                                        pop_info_2,
                                                                        plot.pars.basic$dates, 
                                                                        plot.pars.basic$regions)
}
if(args_dir$school_level == "K12"){
  cumpropflow_byage = summarise_cumprop_flow_byage_forecast_school_reopen_K12(flows_gqs_school_reopen_1$reduced_flows, 
                                                                             flows_gqs_school_reopen_0$reduced_flows,
                                                                             start_date,
                                                                             plot.pars.basic$stan_data$reduced_flows_Monday_idx, 
                                                                             age_cat_map,
                                                                             pop_info_2,
                                                                             plot.pars.basic$dates, 
                                                                             plot.pars.basic$regions)
}
#
# Table: contribution of children aged 0-9 to onwared spread from 2020-08-24 to 2020-11-24

# school closed
cumpropflow_byage_school_reopen0 = subset(cumpropflow_byage, variable == "school_reopen0" & date <= end_date )
cumpropflow_byage_school_reopen0 = subset(cumpropflow_byage_school_reopen0, date == max(date))
cumpropflow_byage_school_reopen0[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
cumpropflow_byage_school_reopen0 <- dcast.data.table(cumpropflow_byage_school_reopen0, loc_label~age_band, value.var='L')

propflow_byage_c_US <- subset(cumpropflow_byage_school_reopen0, loc_label == "United-States") # national level
cumpropflow_byage_school_reopen0 <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_school_reopen0, by='loc_label',all.x=TRUE)
for(x in colnames(cumpropflow_byage_school_reopen0))
{
  set(cumpropflow_byage_school_reopen0, which(is.na(cumpropflow_byage_school_reopen0[[x]])), x, '-') 
}
cumpropflow_byage_school_reopen0 <- rbind(propflow_byage_c_US, cumpropflow_byage_school_reopen0)
cumpropflow_byage_school_reopen0 <- cbind(cumpropflow_byage_school_reopen0[, "loc_label"], cumpropflow_byage_school_reopen0[, "Overall"], cumpropflow_byage_school_reopen0[, -c("Overall", "loc_label")])

# school opened
cumpropflow_byage_school_reopen1 = subset(cumpropflow_byage, variable == "school_reopen1" & date <= as.Date("2020-11-24") )
cumpropflow_byage_school_reopen1 = subset(cumpropflow_byage_school_reopen1, date == max(date))
cumpropflow_byage_school_reopen1[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
cumpropflow_byage_school_reopen1 <- dcast.data.table(cumpropflow_byage_school_reopen1, loc_label~age_band, value.var='L')

propflow_byage_c_US <- subset(cumpropflow_byage_school_reopen1, loc_label == "United-States") # national level
cumpropflow_byage_school_reopen1 <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_school_reopen1, by='loc_label',all.x=TRUE)
for(x in colnames(cumpropflow_byage_school_reopen1))
{
  set(cumpropflow_byage_school_reopen1, which(is.na(cumpropflow_byage_school_reopen1[[x]])), x, '-') 
}
cumpropflow_byage_school_reopen1 <- rbind(propflow_byage_c_US, cumpropflow_byage_school_reopen1)
cumpropflow_byage_school_reopen1 <- cbind(cumpropflow_byage_school_reopen1[, "loc_label"], cumpropflow_byage_school_reopen1[, "Overall"], cumpropflow_byage_school_reopen1[, -c("Overall", "loc_label")])

#
# Save
file <- paste0(outfile.base,'-flow-onward-summary-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
cat("\nWrite table to",file)
saveRDS(cumpropflow_byage, file = file, version = 2)

ans <- list(cumpropflow_byage_school_reopen0, cumpropflow_byage_school_reopen1, format(start_date, "%B %d, %Y"), format(end_date, "%B %d, %Y") )
file <- paste0(outfile.base,'-flow-onward-forecast', '_counterfactual_', args_dir$counterfactual.scenario, '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)


cat(" \n -------------------------------- \n \n End post-processing-forecast-contributions.r \n \n -------------------------------- \n")


