# post-processing-age-pars.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-age-pars.R \n \n -------------------------------- \n")

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
if(0){
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201009c7_cmdstanv'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_201009c7_cmdstanv-4states_AZCTFLNYC_Sep20_Levin'
  args_dir[['job_tag']] <- '4states_AZCTFLNYC_Sep20_Levin'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201009c7_cmdstanv-4states_AZCTFLNYC_Sep20_Levin'
}

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
			
			tmp <- summarise_ifr_age_base(log_ifr_age_base = plot.pars.trmspars$log_ifr_age_base,
			                                  dages = plot.pars.basic$dages)
			
			make_log_ifr_age_base_prior_posterior_plot(ifr_by_age = tmp, 
			                                           dages = plot.pars.basic$dages, 
			                                           stan_data = plot.pars.basic$stan_data, 
			                                           pop_info = plot.pars.basic$pop_info, 
			                                           outfile.base = outfile.base)
			
		}
)

tryCatch(
  if(all(c("log_ifr_age_base", "log_ifr_age_rnde_mid1", "log_ifr_age_rnde_mid2", "log_ifr_age_rnde_old") %in% names(plot.pars.trmspars))) 
  {
    tmp <- summarise_ifr_age_by_state(log_ifr_age_base = plot.pars.trmspars$log_ifr_age_base,
                                      log_ifr_age_rnde_mid1 = plot.pars.trmspars$log_ifr_age_rnde_mid1,
                                      log_ifr_age_rnde_mid2 = plot.pars.trmspars$log_ifr_age_rnde_mid2,
                                      log_ifr_age_rnde_old = plot.pars.trmspars$log_ifr_age_rnde_old,
                                      regions = plot.pars.basic$regions,
                                      dages = plot.pars.basic$dages,
                                      pop_info = plot.pars.basic$pop_info)
    file = paste0(outfile.base, "-summary_log_ifr_age_posterior.rds")
    cat("Write ", file)
    saveRDS(tmp, file = file)
    
    make_log_ifr_age_prior_posterior_plot(ifr_by_age_state = tmp, 
                                          regions = plot.pars.basic$regions, 
                                          dages = plot.pars.basic$dages, 
                                          stan_data = plot.pars.basic$stan_data, 
                                          pop_info = plot.pars.basic$pop_info, 
                                          outfile.base = outfile.base)
  }
)

tryCatch(
  if("timeeff_shift_age" %in% names(plot.pars.trmspars) & length(dim(plot.pars.trmspars$timeeff_shift_age)) == 2) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_age)
    make_posterior_intervals(tmp, 
                             "timeeff_shift_age",
                             plot.pars.basic$dages$age_band,
                             xintercept=0,
                             xmin=NULL,
                             xlab=expression("timeeff_shift_age"[a]), 
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
