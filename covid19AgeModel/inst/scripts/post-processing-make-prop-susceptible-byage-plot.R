# post-processing-make-prop-susceptible-byage-plot.R.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-prop-susceptible-byage-plot.R \n \n -------------------------------- \n")

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
if(1)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rdsgpfs/general/user/ablenkin/home/covid/base_age_fsq_mobility_200703f_cmdstanv-4states_lifr_eta2_devcntct_dataJ29_test2'
	args_dir[['job_tag']] <- '4states_lifr_eta2_devcntct_dataJ29_test2'
}

if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
	args_dir[['job_tag']] <- '19states_stdctn_2'
	
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

outfile.base <- paste0(args_dir$out_dir, "/",
											 args_dir$stanModelFile , "-", args_dir$job_tag)

file <- paste0(outfile.base,'-stanout-transmission_pars.RDS')
cat("\n read RDS:", file)
plot.pars.trms <- readRDS(file)

cat(" \n -------------------------------- \n combinining plots to panel \n -------------------------------- \n")

tryCatch(
	if(all(c('prop_susceptibleByAge') %in% names(plot.pars.trms))) 
	{
		make_prop_susceptible_byage_plot(plot.pars.trms$prop_susceptibleByAge,outfile.base)
	}
)

cat(" \n -------------------------------- \n \n completed post-processing-make-prop-susceptible-byage-plot.R \n \n -------------------------------- \n")
