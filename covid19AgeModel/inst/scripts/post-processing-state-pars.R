# post-processing-state-pars.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-state-pars.R \n \n -------------------------------- \n")


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

#	for dev purposes: olli
if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'covid19AgeModel_report32_cmdstanv'
	args_dir[['out_dir']] <- '/Users/melodiemonod/short_run_test/covid19AgeModel_report32_cmdstanv-4states_tau10_report32_shortrun'
	args_dir[['job_tag']] <- '4states_tau10_report32_shortrun'
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
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
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


tryCatch(
		if("R0" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["R0"]])
			g_R0 <- make_posterior_intervals_R0( tmp, 
						"R0", 
						xtick = plot.pars.basic$region_names[order(plot.pars.basic$region_names$region),]$loc_label, 
						xintercept=1, 
						xmin=NULL, 
						xlab=expression(R[0]),
						outfile.base = outfile.base)
		}
)

tryCatch(
		if("rho0" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["rho0"]])
			g_rho0 <- make_posterior_intervals( tmp,
					"rho0", 
					xtick = plot.pars.basic$regions,
					xintercept=NULL,
					xmin=NULL,
					xlab=expression(rho[0]),
					outfile.base = outfile.base, 
					logscale = 1) +
			    theme(axis.title.x=element_blank(),
									axis.text.x=element_blank(),
									axis.ticks.x=element_blank(),
									axis.text.y=element_text(size=14),
									axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
		if("e_cases_N0" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["e_cases_N0"]])
			g_e_cases_N0 <- make_posterior_intervals(tmp, 
					"e_cases_N0", 
					xtick = plot.pars.basic$regions,
					xintercept=NULL,
					xmin=0,
					xlab= 'Expected number of \ninitial cases',
					outfile.base = outfile.base, 
					logscale = 1) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
  if("beta" %in% names(plot.pars.trmspars)) 
  {
    tmp <- as.matrix(plot.pars.trmspars[["beta"]])
    g_beta <- make_posterior_intervals(tmp, 
                                             "beta", 
                                             xtick = NULL,
                                             xintercept=NULL,
                                             xmin=NULL,
                                             xlab= 'beta',
                                             outfile.base = outfile.base, 
                                             logscale = 0,
    																	 			 label=1)
  }
)

tryCatch(
	if("kappa" %in% names(plot.pars.trmspars)) 
	{
		tmp <- as.matrix(plot.pars.trmspars[["kappa"]])
		g_kappa <- make_posterior_intervals(tmp, 
																			 "kappa", 
																			 xtick = NULL,
																			 xintercept=NULL,
																			 xmin=NULL,
																			 xlab= 'kappa',
																			 outfile.base = outfile.base, 
																			 logscale = 0,
																			 label=1)
	}
)

tryCatch(
	if("phi" %in% names(plot.pars.trmspars)) 
	{
		tmp <- as.matrix(plot.pars.trmspars[["phi"]])
		g_phi <- make_posterior_intervals(tmp, 
																				"phi", 
																				xtick = NULL,
																				xintercept=NULL,
																				xmin=NULL,
																				xlab= 'phi',
																				outfile.base = outfile.base, 
																				logscale = 0,
																				label=1)
	}
)

tryCatch(
  if("elt_school_intv_effect" %in% names(plot.pars.trmspars)) 
  {
    tmp <- as.matrix(plot.pars.trmspars[["elt_school_intv_effect"]])
    g_phi <- make_posterior_intervals(tmp, 
                                      "elt_school_intv_effect", 
                                      xtick = NULL,
                                      xintercept=NULL,
                                      xmin=NULL,
                                      xlab= 'elt_school_intv_effect',
                                      outfile.base = outfile.base, 
                                      logscale = 0,
                                      label=1)
  }
)

tryCatch(
		if("logit_acquire_immunity" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["logit_acquire_immunity"]])
			tmp <- exp(tmp)/(1+exp(tmp))
			g_upswing_rnde <- make_posterior_intervals(tmp, 
					"logit_acquire_immunity",
					xtick = NULL,
					xintercept=NULL,
					xmin=NULL,
					xlab="prob acquire immunity", 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("upswing_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["upswing_rnde"]])
			g_upswing_rnde <- make_posterior_intervals(tmp, 
					"upswing_rnde",
					xtick = plot.pars.basic$regions,
					xintercept=0,
					xmin=NULL,
					xlab="Random effect \nupswing", 
					outfile.base =outfile.base, 
					logscale = 0) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
		if("sd_upswing_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["sd_upswing_rnde"]])
			make_posterior_intervals(tmp, 
					"sd_upswing_rnde",
					xtick = "sd_upswing_rnde",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("dip_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["dip_rnde"]])
			g_dip_rnde <- make_posterior_intervals(tmp, 
							"dip_rnde",
							xtick = plot.pars.basic$regions,
							xintercept=0,
							xmin=NULL,
							xlab="Random effect \ndip mobility trend", 
							outfile.base =outfile.base, 
							logscale = 0) +
					theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
		if("sd_dip_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["sd_dip_rnde"]])
			make_posterior_intervals(tmp, 
					"sd_dip_rnde",
					xtick = "sd_dip_rnde",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
)

tryCatch(
		if("ifr_noise" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["ifr_noise"]])
			make_posterior_intervals(tmp, 
					"ifr_noise", 
					xtick = plot.pars.basic$regions,
					xintercept=1,
					xmin=NULL,
					xlab='ifr noise', 
					outfile.base = outfile.base, 
					logscale = 1,
					label=1)
		}
)

tryCatch(
		if("log_ifr_age_rnde_mid1" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(exp(plot.pars.trmspars$log_ifr_age_rnde_mid1))
			g_ifr_age_rnde_mid1 <- make_posterior_intervals(tmp, 
					"ifr_age_rnde_mid1",
					plot.pars.basic$regions,
					xintercept=1,
					xmin=NULL,
					xlab="Random effect IFR\n [20-49]", 
					outfile.base = outfile.base, 
					logscale = 0) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
		if("log_ifr_age_rnde_mid2" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(exp(plot.pars.trmspars$log_ifr_age_rnde_mid2))
			g_ifr_age_rnde_mid2 <- make_posterior_intervals(tmp,
					"ifr_age_rnde_mid2",
					plot.pars.basic$regions,
					xintercept=1,
					xmin=NULL, 
					xlab="Random effect IFR\n [50-69]", 
					outfile.base = outfile.base, 
					logscale = 0) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch(
		if("log_ifr_age_rnde_old" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(exp(plot.pars.trmspars$log_ifr_age_rnde_old))
			g_ifr_age_rnde_old <- make_posterior_intervals(tmp, 
					"ifr_age_rnde_old",
					plot.pars.basic$regions,
					xintercept=1,
					xmin=NULL,
					xlab="Random effect IFR\n [70+]", 
					outfile.base = outfile.base, 
					logscale = 0) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
)

tryCatch({
	gpl <- list()	
	if(exists('g_R0'))
		gpl[[length(gpl)+1]] <- g_R0
	if(exists('g_rho0'))
		gpl[[length(gpl)+1]] <- g_rho0
	if(exists('g_e_cases_N0'))
		gpl[[length(gpl)+1]] <- g_e_cases_N0
	if(exists('g_dip_rnde'))
		gpl[[length(gpl)+1]] <- g_dip_rnde	
	if(exists('g_upswing_rnde'))
		gpl[[length(gpl)+1]] <- g_upswing_rnde
	if(exists('g_ifr_age_rnde_mid1'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_mid1
	if(exists('g_ifr_age_rnde_mid2'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_mid2
	if(exists('g_ifr_age_rnde_old'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_old
		
	make_state_parameter_summary_plot(gpl, outfile.base)
})

cat(" \n -------------------------------- \n \n Completed post-processing-state-pars.R \n \n -------------------------------- \n")
