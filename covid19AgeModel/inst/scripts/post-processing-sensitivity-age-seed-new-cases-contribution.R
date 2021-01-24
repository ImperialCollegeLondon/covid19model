# post-processing-sensitivity-age-seed-new-cases-contribution.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-sensitivity-age-seed-new-cases-contribution.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200821b9_cmdstanv"
  args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
  args_dir[['job_tag']] <- "37states_tau10_sameLastDate"
  args_dir[['dev_job_tag']] <- "37states_tau10_seed529_updateFSQdata"
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-dev_job_tag')
  stopifnot(args_line[[9]]=='-overwrite')
  stopifnot(args_line[[11]]=='-with_forecast')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['dev_job_tag']] <- args_line[[8]]
  args_dir[['overwrite']] <- as.integer(args_line[[10]])
  args_dir[['with_forecast']] <- as.integer(args_line[[12]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$dev_job_tag, "/",
                           args_dir$stanModelFile , "-", args_dir$dev_job_tag)

# defined model names
model_names = c("Central model", "Model with age seed in 5-29")

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

cat(" \n -------------------------------- adding age cat labels to flows -------------------------------- \n")

#
#	summarise if prop flow by age
file <- paste0(outfile.base,'-summary-prop-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_prop_flow_byage_c ----------- \n")
  propflow_byage_c <- summarise_prop_flow_byage_c(flows_gqs$reduced_flows, 
                                                      plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                      age_cat_map, 
                                                      plot.pars.basic$pop_info, 
                                                      plot.pars.basic$dates, 
                                                      plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(propflow_byage_c, file=file)		
}
if(file.exists(file))
{
  propflow_byage_c <- readRDS(file)
}

file <- paste0(outfile.base.dev,'-summary-prop-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_prop_flow_byage_c ----------- \n")
  propflow_byage_c.dev <- summarise_prop_flow_byage_c(flows_gqs$reduced_flows, 
                                                  plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                  age_cat_map, 
                                                  plot.pars.basic$pop_info, 
                                                  plot.pars.basic$dates, 
                                                  plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(propflow_byage_c.dev, file=file)		
}
if(file.exists(file))
{
  propflow_byage_c.dev <- readRDS(file)
}

# 
# summarise new cases
cat("\n ----------- summarise_e_newcases_byage_c ----------- \n")
file <- paste0(outfile.base,'-summary-newcases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  e_newcases_byage_c <- summarise_e_newcases_byage_c(E_casesByAge,
                                                     age_cat_map, 
                                                     plot.pars.basic$pop_info, 
                                                     plot.pars.basic$dates, 
                                                     plot.pars.basic$regions)
  file <- paste0(outfile.base,'-summary-newcases-age.RDS')
  cat("\nWrite ",file," ... ")
  saveRDS(e_newcases_byage_c, file=file)
}
if(file.exists(file))
{
  e_newcases_byage_c <- readRDS(file)
}
cat("\n ----------- summarise_e_newcases_byage_c ----------- \n")
file <- paste0(outfile.base.dev,'-summary-newcases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  e_newcases_byage_c.dev <- summarise_e_newcases_byage_c(E_casesByAge,
                                                     age_cat_map, 
                                                     plot.pars.basic$pop_info, 
                                                     plot.pars.basic$dates, 
                                                     plot.pars.basic$regions)
  file <- paste0(outfile.base,'-summary-newcases-age.RDS')
  cat("\nWrite ",file," ... ")
  saveRDS(e_newcases_byage_c.dev, file=file)
}
if(file.exists(file))
{
  e_newcases_byage_c.dev <- readRDS(file)
}

#
# include forecast?
if(!args_dir$with_forecast){
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
  propflow_byage_c <- subset(propflow_byage_c, date<=date.max)	
  propflow_byage_c.dev <- subset(propflow_byage_c.dev, date<=date.max)	
  e_newcases_byage_c <- subset(e_newcases_byage_c, date<=date.max)	
  e_newcases_byage_c.dev <- subset(e_newcases_byage_c.dev, date<=date.max)	
}

#
# make plots onward transmissions (proportions)
# remove age overall and select only national average
tmp = subset(propflow_byage_c, age_cat !=0 & loc_label != "United-States")
plot_new_cases_flows_by_age_over_time(data = copy(tmp), parname = "onward_prop_over_time", 
                                      ylab = 'Age composition of contribution to transmission', outfile.base, suffix = "_seed_20-54")

tmp = subset(propflow_byage_c.dev, age_cat !=0 & loc_label != "United-States")
plot_new_cases_flows_by_age_over_time(data = copy(tmp), parname = "onward_prop_over_time", 
                                      ylab = 'Age composition of contribution to transmission', outfile.base, suffix = "_seed_5-29")

#
# make new cases 
plot_new_cases_flows_by_age_over_time(data = copy(e_newcases_byage_c), parname = "e_newcases", 
                       ylab = 'Age composition of new infections', outfile.base, suffix = "_seed_20-54")
plot_new_cases_flows_by_age_over_time(data = copy(e_newcases_byage_c.dev), parname = "e_newcases", 
                      ylab = 'Age composition of new infections', outfile.base, suffix = "_seed_5-29")



cat(" \n -------------------------------- \n \n Completed post-processing-sensitivity-age-seed-new-cases-contribution.R \n \n -------------------------------- \n")
