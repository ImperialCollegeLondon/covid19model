# post-processing-flows.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-flows.R \n \n -------------------------------- \n")

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
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
args_dir[['out_dir']] <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-4states_lifr_eta2_devcntct_dataJ29_test'
args_dir[['job_tag']] <- '4states_lifr_eta2_devcntct_dataJ29_test'
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdcntct_newpostpr'
args_dir[['job_tag']] <- '19states_stdcntct_newpostpr'
# xx
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin/'
args_dir[['job_tag']] <- '40states_tau10_Oct29_Levin'

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

# add age labels to reduced flows
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)


file <- paste0(outfile.base,'-stanout-full-flows-summary-gqs.RDS')
cat("\n read RDS:", file)
full_flows_summary <- readRDS(file)

file <- paste0(outfile.base,'-stanout-reduced-flows-summary-gqs.RDS')
cat("\n read RDS:", file)
reduced_flows_summary <- readRDS(file)


cat(" \n -------------------------------- adding age cat labels to flows -------------------------------- \n")

tmp <- unique(subset(age_cat_map, select=c(age.cat2, age.cat2.label)))
set(tmp, NULL, 'age.cat2.label', tmp[, factor(age.cat2, levels=age.cat2, labels=age.cat2.label)])
setnames(tmp, c('age.cat2','age.cat2.label'), c('source_age_cat','source_age_cat_label'))
reduced_flows_summary <- merge(reduced_flows_summary, tmp, by='source_age_cat')
setnames(tmp, c('source_age_cat','source_age_cat_label'), c('rec_age_cat','rec_age_cat_label'))
reduced_flows_summary <- merge(reduced_flows_summary, tmp, by='rec_age_cat')

# add age labels to full flows
tmp <- unique(subset(age_cat_map, select=c(age.cat, age.cat.label)))
set(tmp, NULL, 'age.cat.label', tmp[, factor(age.cat, levels=age.cat, labels=age.cat.label)])
setnames(tmp, c('age.cat','age.cat.label'), c('source_age_cat','source_age_cat_label'))
full_flows_summary <- merge(full_flows_summary, tmp, by='source_age_cat')
setnames(tmp, c('source_age_cat','source_age_cat_label'), c('rec_age_cat','rec_age_cat_label'))
full_flows_summary <- merge(full_flows_summary, tmp, by='rec_age_cat')

#
# make comparison plots sources vs onward transmissions (proportions)
tryCatch({
	flow <- subset(reduced_flows_summary, stat%in%c("rec_prop","sources_prop"))
	make_flow_sources_prop_onward_prop_side_by_side_plot(flow, paste0(outfile.base,'-reduced_flow_sourcesonwardprop'))			
})

#
# make comparison plots sources vs onward transmissions (abs value)
tryCatch({
	flow <- subset(reduced_flows_summary, stat=="flow_abs")
	make_flow_sources_onward_side_by_side_plot(flow, paste0(outfile.base,'-reduced_flow_sourcesonwardabs'))
})

# make stacked source of transmissions (abs value)
tryCatch({
  flow <- subset(reduced_flows_summary, stat=="flow_abs")
  make_flow_stacked_sources_plot(flow,plot.pars.basic$region_names, paste0(outfile.base,'-reduced_flow_sourceabs_stacked_alllocations'))
})

tryCatch({
	#
	# make matrix plots			
	flow <- subset(full_flows_summary, stat=="flow_abs")
	plots.matrices <- make_flow_matrix_plot(flow, paste0(outfile.base,'-full_flow_abs'))
	
	#
	# make source plots
	flow <- subset(reduced_flows_summary, stat=="sources_prop")
	plots.sources <- make_flow_sources_plot(flow, plot.pars.basic$dates, paste0(outfile.base,'-reduced_flow_sources'))
	
	# make combined figure
	for(x in unique(flow$loc))
	{
		g <- ggarrange( plots.matrices[[x]], 
				plots.sources[[x]], 
				nrow=2, 
				ncol=1, 
				heights = c(0.5,0.5), 
				labels = c("A", "B") )
		ggsave(paste0(outfile.base,'-flow_summary-',x,'.png'), g, width=7, height=10)		
	}
})

#
#	make sources tables
tryCatch({			
	flow <- subset(reduced_flows_summary, stat=="sources_prop")
	make_flow_source_tables(flow, 
			plot.pars.basic$dates, 
			plot.pars.basic$pop_info, 
			outfile.base)
})

#
#	make onwards tables
tryCatch({	
	flow <- subset(reduced_flows_summary, stat=="rec_prop")
	make_flow_onward_table(flow, 
			plot.pars.basic$dates, 
			plot.pars.basic$pop_info, 
			outfile.base)
})

cat(" \n -------------------------------- \n \n Completed post-processing-flows.R \n \n -------------------------------- \n")
