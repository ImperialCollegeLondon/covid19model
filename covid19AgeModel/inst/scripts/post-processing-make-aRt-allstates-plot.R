# post-processing-plot-aRt-allstates.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-make-aRt-allstates-plot.R \n \n -------------------------------- \n")

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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-37states_tau10_Oct21_Levin'
	args_dir[['job_tag']] <- '37states_tau10_Oct21_Levin'
	args_dir[['overwrite']] <- 0
	args_dir[["include_lambda_age"]] <- 0
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

cat(" \n -------------------------------- \n summarise case samples: start \n -------------------------------- \n")

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)


# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

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
if(nrow(subset(Rt_byage_c, loc == 'US')) > 0)
{
	Rt_byage_c = subset(Rt_byage_c, loc != 'US')
}


#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
	Rt_byage_c <- subset(Rt_byage_c, date<=date.max)
}

# plot R_mta for all states
g_aRt_c <- plot_Rt_byage_c_allstates(Rt_byage_c, 
								"Rtma", 
								ylab="Estimated age-specific reproduction numbers")
ggsave(paste0(outfile.base,"-Rtma",'_byage_c-allstates', '.png'), g_aRt_c, w = 21, h=29)	


cat(" \n -------------------------------- \n \n Completed post-processing-make-aRt-allstates-plot.R \n \n -------------------------------- \n")
