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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
	args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin'
	args_dir[['job_tag']] <- '40states_tau10_Oct29_Levin'
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
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
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
  , error = function(err) { warning(err) }
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
	, error = function(err) { warning(err) }
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
	, error = function(err) { warning(err) }
)

tryCatch(
		if("impact_intv_children_effect" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["impact_intv_children_effect"]])
			g_impact_intv_children_effect <- make_posterior_density(tmp, 
					"impact_intv_children_effect",
					xtick = NULL,
					xlab="impact_intv_children_effect", 
					outfile.base =outfile.base)
			tmp1 = apply(1-tmp, 2, function(x) quantile(x, probs = c(0.5, 0.025, 0.975)) )
			tmp1 = paste0(sprintf("%.1f", tmp1[1,1]*100), '\\% [', sprintf("%.1f", tmp1[2,1]*100), '\\%', '-', sprintf("%.1f", tmp1[3,1]*100), '\\%]')
			saveRDS(tmp1, file = paste0(outfile.base, '-impact_intv_children_effect_CI.rds'), version = 2)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("impact_intv_onlychildren_effect" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["impact_intv_onlychildren_effect"]])
			g_impact_intv_onlychildren_effect <- make_posterior_density(tmp, 
					"impact_intv_onlychildren_effect",
					xtick = NULL,
					xlab="impact_intv_onlychildren_effect", 
					outfile.base =outfile.base)
			tmp1 = apply(1-tmp, 2, function(x) quantile(x, probs = c(0.5, 0.025, 0.975)) )
			tmp1 = paste0(sprintf("%.1f", tmp1[1,1]*100), '\\% [', sprintf("%.1f", tmp1[2,1]*100), '\\%', '-', sprintf("%.1f", tmp1[3,1]*100), '\\%]')
			saveRDS(tmp1, file = paste0(outfile.base, '-impact_intv_onlychildren_effect_CI.rds'), version = 2)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
  if("elt_school_intv_effect" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars[["elt_school_intv_effect"]])
    g_elt_school_intv_effect <- make_posterior_density(tmp, 
                                               "elt_school_intv_effect",
                                               xtick = NULL,
                                               xlab="elt_school_intv_effect", 
                                               outfile.base =outfile.base)
    tmp1 = apply(1-tmp, 2, function(x) quantile(x, probs = c(0.5, 0.025, 0.975)) )
    tmp1 = paste0(sprintf("%.1f", tmp1[1,1]*100), '\\% [', sprintf("%.1f", tmp1[2,1]*100), '\\%', '-', sprintf("%.1f", tmp1[3,1]*100), '\\%]')
    saveRDS(tmp1, file = paste0(outfile.base, '-elt_school_intv_effect_CI.rds'), version = 2)
  }
  , error = function(err) { warning(err) }
)


tryCatch(
		if("logit_acquire_immunity" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["logit_acquire_immunity"]])
			tmp <- exp(tmp)/(1+exp(tmp))
			g_logit_acquire_immunity <- make_posterior_intervals(tmp, 
					"logit_acquire_immunity",
					xtick = NULL,
					xintercept=NULL,
					xmin=NULL,
					xlab="prob acquire immunity", 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
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
					xlab="Spatial random effect\non upswing time trends", 
					outfile.base =outfile.base, 
					logscale = 0) +
				theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("sd_upswing_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["sd_upswing_rnde"]])
			g_sd_upswing_rnde = make_posterior_intervals(tmp, 
					"sd_upswing_rnde",
					xtick = "sd_upswing_rnde",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("upswing_timeeff_reduced" %in% names(plot.pars.trmspars) && length(dim(plot.pars.trmspars[["upswing_timeeff_reduced"]]))==2 ) 
		{									
			tmp <- as.matrix(plot.pars.trmspars[["upswing_timeeff_reduced"]])
			g_upswing_timeeff_reduced <- make_posterior_intervals(tmp, 
							"upswing_timeeff_reduced",
							xtick = 1:ncol(tmp),
							xintercept=0,
							xmin=NULL,
							xlab="Upswing random time effects", 
							outfile.base =outfile.base, 
							logscale = 0,
							label = 1)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("upswing_timeeff_reduced" %in% names(plot.pars.trmspars) && length(dim(plot.pars.trmspars[["upswing_timeeff_reduced"]]))==3 ) 
		{			
			vars <- plot.pars.trmspars[["upswing_timeeff_reduced"]]
			loc_label <- plot.pars.basic$region_names[order(plot.pars.basic$region_names$region),]$loc_label
			stopifnot(dim(vars)[3]==length(loc_label))
			
			dvars <- vector('list', length(loc_label))
			for(i in seq_along(loc_label))
			{
				dvar <- as.data.table(reshape2::melt(vars[,,i]))
				setnames(dvar, 1:3, c('it','time','value'))
				dvar <- dvar[, list(Q= quantile(value, prob=c(0.025,0.5,0.975)), Q_LAB=c('CL','M','CU') ), by=c('time')]
				dvar[, loc_label:= loc_label[i]]
				dvars[[i]] <- copy(dvar)
			}
			dvars <- do.call('rbind',dvars)
			dvars <- dcast.data.table(dvars, loc_label+time~Q_LAB, value.var='Q')
			dvars <- merge(dvars, dvars[, list(M_avg= mean(M)), by='time'], by='time')
			g_upswing_timeeff_reduced <- ggplot(dvars, aes(x=time)) + 
					theme_bw() +
					facet_wrap(.~loc_label, ncol=1) +
					geom_line(aes(y=M_avg)) +
					geom_point(aes(y=M), colour='blue') +
					geom_errorbar(aes(ymin=CL, ymax=CU), size=0.2, width=0.3, colour='blue') +
					labs(x='time index', y='upswing_timeeff_reduced')
			ggsave(file=paste0(outfile.base,'-',"upswing_timeeff_reduced",'.png'), g_upswing_timeeff_reduced, w=10, h= (length(loc_label)/1 + 4))						
		}
		, error = function(err) { warning(err) }
)

tryCatch(
	if("sd_upswing_timeeff_reduced" %in% names(plot.pars.trmspars)) 
	{			
			tmp <- as.matrix(plot.pars.trmspars[["sd_upswing_timeeff_reduced"]])
			g_sd_upswing_timeeff_reduced = make_posterior_intervals(tmp, 
					"sd_upswing_timeeff_reduced",
					xtick = "sd_upswing_timeeff_reduced",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
	}
	, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
)

tryCatch(
		if("sd_dip_rnde" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["sd_dip_rnde"]])
			g_sd_dip_rnde = make_posterior_intervals(tmp, 
					"sd_dip_rnde",
					xtick = "sd_dip_rnde",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
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
		, error = function(err) { warning(err) }
)

tryCatch(
		if("hyper_log_ifr_age_rnde_mid1" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["hyper_log_ifr_age_rnde_mid1"]])
			g_hyper_log_ifr_age_rnde_mid1 = make_posterior_intervals(tmp, 
					"hyper_log_ifr_age_rnde_mid1",
					xtick = "hyper_log_ifr_age_rnde_mid1",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("hyper_log_ifr_age_rnde_mid2" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["hyper_log_ifr_age_rnde_mid2"]])
			g_hyper_log_ifr_age_rnde_mid2 = make_posterior_intervals(tmp, 
					"hyper_log_ifr_age_rnde_mid2",
					xtick = "hyper_log_ifr_age_rnde_mid2",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
		if("hyper_log_ifr_age_rnde_old" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars[["hyper_log_ifr_age_rnde_old"]])
			g_hyper_log_ifr_age_rnde_old = make_posterior_intervals(tmp, 
					"hyper_log_ifr_age_rnde_old",
					xtick = "hyper_log_ifr_age_rnde_old",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
		}
		, error = function(err) { warning(err) }
)

tryCatch(
  if("log_ifr_overall_upswing_effect" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars[["log_ifr_overall_upswing_effect"]])
    g_log_ifr_overall_upswing_effect = make_posterior_intervals(tmp, 
                                             "log_ifr_overall_upswing_effect",
                                             xtick = "log_ifr_overall_upswing_effect",
                                             xintercept=NULL,
                                             xmin=NULL,
                                             xlab=NULL, 
                                             outfile.base =outfile.base, 
                                             logscale = 0,
                                             label=1)
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if("log_ifr_overall_upswing_rnde" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$log_ifr_overall_upswing_rnde)
    g_log_ifr_overall_upswing_rnde <- make_posterior_intervals(tmp, 
                                                     "log_ifr_overall_upswing_rnde",
                                                     plot.pars.basic$regions,
                                                     xintercept=0,
                                                     xmin=NULL,
                                                     xlab="Random effect ifr upswing", 
                                                     outfile.base = outfile.base, 
                                                     logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if("timeeff_shift_mid1" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_mid1)
    g_timeeff_shift_mid1 <- make_posterior_intervals(tmp, 
                                                    "timeeff_shift_mid1",
                                                    plot.pars.basic$regions,
                                                    xintercept=0,
                                                    xmin=NULL,
                                                    xlab="Random effect impact_intv \n [20-49]", 
                                                    outfile.base = outfile.base, 
                                                    logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
	if("hyper_timeeff_shift_mid1" %in% names(plot.pars.trmspars)) 
	{			
		tmp <- as.matrix(plot.pars.trmspars[["hyper_timeeff_shift_mid1"]])
		g_hyper_timeeff_shift_mid1 = make_posterior_intervals(tmp, 
					"hyper_timeeff_shift_mid1",
					xtick = "hyper_timeeff_shift_mid1",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
	}
	, error = function(err) { warning(err) }
)


tryCatch(
		if("timeeff_shift_mid2" %in% names(plot.pars.trmspars)) 
		{			
			tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_mid2)
			g_timeeff_shift_mid2 <- make_posterior_intervals(tmp, 
							"timeeff_shift_mid2",
							plot.pars.basic$regions,
							xintercept=0,
							xmin=NULL,
							xlab="Random effect impact_intv \n [50-69]", 
							outfile.base = outfile.base, 
							logscale = 0) +
					theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
		}
		, error = function(err) { warning(err) }
)

tryCatch(
	if("hyper_timeeff_shift_mid2" %in% names(plot.pars.trmspars)) 
	{			
		tmp <- as.matrix(plot.pars.trmspars[["hyper_timeeff_shift_mid2"]])
		g_hyper_timeeff_shift_mid2 = make_posterior_intervals(tmp, 
					"hyper_timeeff_shift_mid2",
					xtick = "hyper_timeeff_shift_mid2",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
	}
	, error = function(err) { warning(err) }
)

tryCatch(
	if("timeeff_shift_old" %in% names(plot.pars.trmspars)) 
	{			
		tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_old)
		g_timeeff_shift_old <- make_posterior_intervals(tmp, 
							"timeeff_shift_old",
							plot.pars.basic$regions,
							xintercept=0,
							xmin=NULL,
							xlab="Random effect impact_intv \n [70+]", 
							outfile.base = outfile.base, 
							logscale = 0) +
					theme(axis.title.x=element_blank(),
							axis.text.x=element_blank(),
							axis.ticks.x=element_blank(),
							axis.text.y=element_text(size=14),
							axis.title.y=element_text(size=18,angle=90))
	}
	, error = function(err) { warning(err) }
)

tryCatch(
	if("hyper_timeeff_shift_old" %in% names(plot.pars.trmspars)) 
	{			
		tmp <- as.matrix(plot.pars.trmspars[["hyper_timeeff_shift_old"]])
		g_hyper_timeeff_shift_old = make_posterior_intervals(tmp, 
					"hyper_timeeff_shift_old",
					xtick = "hyper_timeeff_shift_old",
					xintercept=NULL,
					xmin=NULL,
					xlab=NULL, 
					outfile.base =outfile.base, 
					logscale = 0,
					label=1)
	}
	, error = function(err) { warning(err) }
)

tryCatch(
  if("timeeff_shift_1" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_1)
    g_timeeff_shift_1 <- make_posterior_intervals(tmp, 
                                                    "timeeff_shift_1",
                                                    plot.pars.basic$regions,
                                                    xintercept=0,
                                                    xmin=NULL,
                                                    xlab="Random effect impact_intv \n [15-29]", 
                                                    outfile.base = outfile.base, 
                                                    logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if("timeeff_shift_2" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_2)
    g_timeeff_shift_2 <- make_posterior_intervals(tmp, 
                                                    "timeeff_shift_2",
                                                    plot.pars.basic$regions,
                                                    xintercept=0,
                                                    xmin=NULL,
                                                    xlab="Random effect impact_intv \n [30-49]", 
                                                    outfile.base = outfile.base, 
                                                    logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if("timeeff_shift_3" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_3)
    g_timeeff_shift_3 <- make_posterior_intervals(tmp, 
                                                    "timeeff_shift_3",
                                                    plot.pars.basic$regions,
                                                    xintercept=0,
                                                    xmin=NULL,
                                                    xlab="Random effect impact_intv \n [50-64]", 
                                                    outfile.base = outfile.base, 
                                                    logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if("timeeff_shift_4" %in% names(plot.pars.trmspars)) 
  {			
    tmp <- as.matrix(plot.pars.trmspars$timeeff_shift_4)
    g_timeeff_shift_4 <- make_posterior_intervals(tmp, 
                                                    "timeeff_shift_4",
                                                    plot.pars.basic$regions,
                                                    xintercept=0,
                                                    xmin=NULL,
                                                    xlab="Random effect impact_intv \n [65+]", 
                                                    outfile.base = outfile.base, 
                                                    logscale = 0) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=18,angle=90))
  }
  , error = function(err) { warning(err) }
)

tryCatch(
  if(all(c("upswing_timeeff_reduced", 'dip_rnde', 'timeeff_shift_mid1') %in% names(plot.pars.trmspars)) )
  {	
  p_random_effects_across_locations =  plot_random_effects_across_locations(plot.pars.trmspars, plot.pars.basic)
  ggsave(p_random_effects_across_locations, file = paste0(outfile.base, '-random_effects_across_locations.png'), w = 6, h = 8)
  
  }
  , error = function(err) { warning(err) }
)


tryCatch({
	gpl <- list()	
	if(exists('g_R0'))
		gpl[[length(gpl)+1]] <- g_R0
	if(exists('g_rho0'))
		gpl[[length(gpl)+1]] <- g_rho0
	if(exists('g_e_cases_N0'))
		gpl[[length(gpl)+1]] <- g_e_cases_N0
	if(exists('g_kappa'))
	  gpl[[length(gpl)+1]] <- g_kappa
	if(exists('g_elt_school_intv_effect'))
	  gpl[[length(gpl)+1]] <- g_elt_school_intv_effect  
  	if(exists('g_impact_intv_children_effect'))
	  gpl[[length(gpl)+1]] <- g_impact_intv_children_effect
  	if(exists('g_impact_intv_onlychildren_effect'))
	  gpl[[length(gpl)+1]] <- g_impact_intv_onlychildren_effect  
	if(exists('g_dip_rnde'))
		gpl[[length(gpl)+1]] <- g_dip_rnde	
	if(exists('g_sd_dip_rnde'))
	  gpl[[length(gpl)+1]] <- g_sd_dip_rnde	
	if(exists('g_upswing_rnde'))
		gpl[[length(gpl)+1]] <- g_upswing_rnde
	if(exists('g_sd_upswing_rnde'))
	  gpl[[length(gpl)+1]] <- g_sd_upswing_rnde
	if(exists('g_ifr_age_rnde_mid1'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_mid1	
	if(exists('g_hyper_log_ifr_age_rnde_mid1'))
		gpl[[length(gpl)+1]] <- g_hyper_log_ifr_age_rnde_mid1	
	if(exists('g_ifr_age_rnde_mid2'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_mid2
	if(exists('g_hyper_log_ifr_age_rnde_mid2'))
		gpl[[length(gpl)+1]] <- g_hyper_log_ifr_age_rnde_mid2	
	if(exists('g_ifr_age_rnde_old'))
		gpl[[length(gpl)+1]] <- g_ifr_age_rnde_old
	if(exists('g_hyper_log_ifr_age_rnde_old'))
		gpl[[length(gpl)+1]] <- g_hyper_log_ifr_age_rnde_old	
	if(exists('g_log_ifr_overall_upswing_effect'))
	  gpl[[length(gpl)+1]] <- g_log_ifr_overall_upswing_effect
	if(exists('g_log_ifr_overall_upswing_rnde'))
	  gpl[[length(gpl)+1]] <- g_log_ifr_overall_upswing_rnde
	if(exists('g_timeeff_shift_mid1'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_mid1  
  	if(exists('g_hyper_timeeff_shift_mid1'))
	  gpl[[length(gpl)+1]] <- g_hyper_timeeff_shift_mid1
  	if(exists('g_timeeff_shift_mid2'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_mid2
  	if(exists('g_hyper_timeeff_shift_mid2'))
	  gpl[[length(gpl)+1]] <- g_hyper_timeeff_shift_mid2  
  	if(exists('g_timeeff_shift_old'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_old
  	if(exists('g_hyper_timeeff_shift_old'))
	  gpl[[length(gpl)+1]] <- g_hyper_timeeff_shift_old  
	if(exists('g_timeeff_shift_1'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_1
	if(exists('g_timeeff_shift_2'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_2
	if(exists('g_timeeff_shift_3'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_3
	if(exists('g_timeeff_shift_4'))
	  gpl[[length(gpl)+1]] <- g_timeeff_shift_4
	make_state_parameter_summary_plot(gpl, outfile.base)
})

cat(" \n -------------------------------- \n \n Completed post-processing-state-pars.R \n \n -------------------------------- \n")
