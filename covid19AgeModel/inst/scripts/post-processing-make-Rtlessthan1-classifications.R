# post-processing-make-Rtlessthan1-classifications.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-Rtlessthan1-classifications.R: start \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
  args_dir[['job_tag']] <- '37states_Sep2'
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
}

if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
  args_dir[['job_tag']] <- '37states_Sep2'
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
  stopifnot(args_line[[7]]=='-overwrite')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.integer(args_line[[8]])
} 


## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

RtByAge<- NULL

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)


cat(" \n -------------------------------- \n summarise case by age samples: start \n -------------------------------- \n")

#
#	summarise Rt by age
file <- paste0(outfile.base,'-summary-Rt-age_averageover', "1", 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_effcasesByAge <- readRDS(file2)
  
  file3 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
  cat("\n read RDS:", file3)
  RtByAge <- readRDS(file3)
  
  cat("\n ----------- summarise_Rt_instantaneous_byage_c ----------- \n")
  Rt_byage_c <- summarise_Rt_instantaneous_byage_c(E_effcasesByAge, 
                                                   RtByAge, 
                                                   period_length = 1,
                                                   age_cat_map, 
                                                   plot.pars.basic$pop_info, 
                                                   plot.pars.basic$dates, 
                                                   plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_byage_c, file=file)
  
}
if(file.exists(file))
{
  Rt_byage_c <- readRDS(file)
}

#
#	summarise if Rt by age < 1
file <- paste0(outfile.base,'-summary-Rt-age-smaller-one_averageover', "1", 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_effcasesByAge)){
    file3 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
    cat("\n read RDS:", file3)
    E_effcasesByAge <- readRDS(file3)
  }
  if(is.null(RtByAge)){
    file2 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    RtByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_Rt_instantaneous_byage_less_than_one ----------- \n")
  Rt_byage_less_than_one <- summarise_Rt_instantaneous_byage_less_than_one(E_effcasesByAge,
                                                                           RtByAge, 
                                                                           period_length = 1,
                                                                           threshold = 1,
                                                                           age_cat_map, 
                                                                           plot.pars.basic$pop_info, 
                                                                           plot.pars.basic$dates, 
                                                                           plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_byage_less_than_one, file=file)		
}
if(file.exists(file))
{
  Rt_byage_less_than_one <- readRDS(file)
}


#	cut forecast
# Rt_byage_less_than_one <-	Rt_byage_less_than_one[loc_label!='United-States']
date.max <- as.Date(sapply(plot.pars.basic$dates, function(x) max(as.character(x))))[1]
Rt_byage_less_than_one <- subset(Rt_byage_less_than_one, date<=date.max)

# clusters.list <- make_epidemiologic_scenarios_from_Rta(Rt_byage_less_than_one, 0.5)
mobility_data <- plot.pars.basic$mobility_data
mobility_data <- mobility_data[loc_label%in% unique(Rt_byage_less_than_one$loc_label)]
clusters.list <- make_exact_epidemiologic_scenarios_from_Rta(Rt_byage_less_than_one, 0.5, mobility_data)

cluster_membership <- data.table(loc_label=clusters.list[[2]],
                                 epi_scenario_label=clusters.list[[1]])
setkey(cluster_membership, epi_scenario_label)

cluster_membership_US <- subset(cluster_membership, loc_label == "United-States") # national average
tmp <- unique(subset(plot.pars.basic$pop_info, select=loc_label))
cluster_membership <- merge(tmp, cluster_membership, by = "loc_label", all.x = TRUE)
tmp <- which(cluster_membership[, is.na(epi_scenario_label)] )
set(cluster_membership, tmp, 'epi_scenario_label', '-')
cluster_membership <- rbind(cluster_membership_US, cluster_membership)


file <- paste0(outfile.base,'-Rtless_than_one_classification_of_locs_averageover.rds')
cat('\nWriting ',file,' ...')
saveRDS(list(cluster_membership,format(max(Rt_byage_less_than_one$date),"%B %d, %Y")), file=file, version = 2)


RtByAge <- NULL
gc()


cat(" \n -------------------------------- \n make Rt < 1 plot: start \n -------------------------------- \n")
tryCatch({
	#	add epi scenarios for plotting		
  Rt_byage_less_than_one <- merge(Rt_byage_less_than_one, cluster_membership, by='loc_label')
  rebound_date <- clusters.list[[3]]
  plot_rebound_date <- FALSE
  Rt_byage_less_than_one <- Rt_byage_less_than_one[loc!='US',]
  #	plot
  plot_Rt_less_than_one(Rt_byage_less_than_one, plot_rebound_date, rebound_date, outfile.base )
})
cat(" \n -------------------------------- \n make Rt < 1 plot: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n \n post-processing-make-Rtlessthan1-classifications.R:end \n \n -------------------------------- \n")