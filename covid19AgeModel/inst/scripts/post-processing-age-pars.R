# post-processing-age-pars.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-age-pars.R \n \n -------------------------------- \n")

library(data.table)
library(bayesplot)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(magick)
library(viridis)
library(covid19AgeModel)

#	for dev purposes: olli
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
args_dir[['out_dir']] <- '/rdsgpfs/general/user/ablenkin/home/covid/base_age_fsq_mobility_200703f_cmdstanv-4states_lifr_eta2_devcntct_dataJ29_test2'
args_dir[['job_tag']] <- '4states_lifr_eta2_devcntct_dataJ29_test2'

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
} 

## start script
cat(" \n --------------------------------  with post-processing arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
		args_dir$stanModelFile , "-", args_dir$job_tag)


# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
file <- paste0(outfile.base,'-stanout-transmission_pars.RDS')
cat("\n read RDS:", file)
plot.pars.trmspars <- readRDS(file)

# set colors by age 
ggplotColours <- function(n=6, h=c(0, 360) +15){
	if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
	hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
ggplotCol = ggplotColours(nrow(plot.pars.basic$dages))


tryCatch(
		if("ifr_age" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(plot.pars.trmspars[["ifr_age"]])
			make_posterior_intervals(tmp, 
					"ifr_age",
					xtick = plot.pars.basic$dages$age_band,
					xintercept=1,
					xmin=NULL,
					xlab=NULL, 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("log_ifr_age" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(exp(plot.pars.trmspars$log_ifr_age))
			make_posterior_intervals(tmp, 
					"ifr_age",
					xtick = plot.pars.basic$dages$age_band,
					xintercept=1,
					xmin=NULL,
					xlab=NULL, 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("log_ifr_age_base" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(exp(plot.pars.trmspars$log_ifr_age_base))
			make_posterior_intervals(tmp, 
					"ifr_age_base",
					plot.pars.basic$dages$age_band,
					xintercept=1,
					xmin=NULL, 
					xlab=NULL, 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1) 
		}
)

tryCatch(
		if("log_relsusceptibility_age" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(exp(plot.pars.trmspars$log_relsusceptibility_age))
			make_posterior_intervals(tmp, 
					"relsusceptibility_age",
					plot.pars.basic$dages$age_band,
					xintercept=1,
					xmin=NULL,
					xlab=expression("susceptibility"[a]), 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("sd_log_relsusceptibility_age" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(plot.pars.trmspars[["sd_log_relsusceptibility_age"]])
			make_posterior_intervals(tmp, 
					"sd_log_relsusceptibility_age",
					xtick = "sd_log_relsusceptibility_age",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("log_reltransmissibility_age" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(exp(plot.pars.trmspars$log_reltransmissibility_age))
			make_posterior_intervals(tmp, 
					"reltransmissibility_age",
					plot.pars.basic$dages$age_band,
					xintercept=1,
					xmin=NULL,
					xlab=expression("transmissibility"[a]), 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("sd_log_reltransmissibility_age" %in% names(plot.pars.trmspars)) 
		{
			tmp <- as.matrix(plot.pars.trmspars[["sd_log_reltransmissibility_age"]])
			make_posterior_intervals(tmp, 
					"sd_log_reltransmissibility_age",
					xtick = "sd_log_reltransmissibility_age",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base = outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if(all(c('log_ifr_age_base', 'log_relsusceptibility_age', 'log_reltransmissibility_age',
						'stan_data','dages') %in% names(plot.pars.trmspars)))
		{
			cat("\n making plot: make_parameter_ifr_relsusceptibility_reltransmissibility_plot ... ")
			make_parameter_ifr_relsusceptibility_reltransmissibility_plot(plot.pars.trmspars$log_ifr_age_base,
					plot.pars.trmspars$log_relsusceptibility_age,
					plot.pars.trmspars$log_reltransmissibility_age,					
					plot.pars.basic$stan_data$hyperpara_ifr_age_lnmu,
					plot.pars.basic$stan_data$hyperpara_ifr_age_lnsd,					
					plot.pars.basic$dages)
		}
)

cat(" \n -------------------------------- \n \n Completed post-processing-age-pars.R \n \n -------------------------------- \n")
