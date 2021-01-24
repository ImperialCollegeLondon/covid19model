# post-processing-new-strain-make-deaths-cases-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start: post-processing-new-strain-make-deaths-cases-plot.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(1)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015i4_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015i4_cmdstanv-test_new_strain'
	args_dir[['job_tag']] <- 'test_new_strain'
	args_dir[['overwrite']] <- 0
	args_dir[['with_forecast']] <- 1
	args_dir[['rel_transmissibility_new_strain']] = 170
	args_dir[['prop_cases_new_strain_first_day']] = 01
	args_dir[['school.closure.2']] = '1'
	args_dir[['rebound.mobility']] = '1'
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
	stopifnot(args_line[[11]]=='-rel_transmissibility_new_strain')
	stopifnot(args_line[[13]]=='-prop_cases_new_strain_first_day')
	stopifnot(args_line[[15]]=='-school.closure.2')
	stopifnot(args_line[[17]]=='-rebound.mobility')
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
	args_dir[['overwrite']] <- as.integer(args_line[[8]])
	args_dir[['with_forecast']] <- as.integer(args_line[[10]])
	args_dir[['rel_transmissibility_new_strain']] <- args_line[[12]]
	args_dir[['prop_cases_new_strain_first_day']] <- args_line[[14]]
	args_dir[['school.closure.2']] <- args_line[[16]]
	args_dir[['rebound.mobility']] <- args_line[[18]]
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

school.closure.2 = args_dir$school.closure.2 == '1'
rebound.mobility = args_dir$rebound.mobility == '1'
suffix = paste0('_new_strain', '_school_closure_2_', school.closure.2, '_rebound_mobility_',rebound.mobility)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic',suffix,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# map for strain
ds = data.table(strain_cat = 1:2, strain_cat_label = c('initial strain', 'new strain'))

cat(" \n -------------------------------- \n summarise samples: start \n -------------------------------- \n")

#
# summarise daily deaths by strain
file <- paste0(outfile.base,'-summary-daily-deathsbystrain',suffix,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_deathsByStrain-gqs',suffix,'.RDS')
  cat("\n read RDS:", file2)
  E_deathsByStrain <- readRDS(file2)
  
  cat("\n ----------- summarise_deaths_by_strain ----------- \n")
  e_sdeaths <- make_var_by_strain_summaries(E_deathsByStrain,
                                            plot.pars.basic$pop_info,
                                            plot.pars.basic$dates,
                                            plot.pars.basic$regions,
                                            ds)	
  cat("\nWrite ",file," ... ")
  saveRDS(e_sdeaths, file=file)
}
if(file.exists(file))
{
  e_sdeaths <- readRDS(file)
}

#
# summarise daily deaths over strain
file <- paste0(outfile.base,'-summary-daily-deaths-overall',suffix,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_deaths-gqs',suffix,'.RDS')
  cat("\n read RDS:", file2)
  E_deaths <- readRDS(file2)
  
	cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
	deaths_o <- make_var_overall_summaries(E_deaths, 
	                                       plot.pars.basic$pop_info,
	                                       plot.pars.basic$dates,
	                                       plot.pars.basic$regions)
	cat("\nWrite ",file," ... ")
	saveRDS(deaths_o, file=file)
}
if(file.exists(file))
{
  deaths_o <- readRDS(file)
}

#
# summarise daily cases by strain
file <- paste0(outfile.base,'-summary-daily-casesbystrain',suffix,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByStrain-gqs',suffix,'.RDS')
  cat("\n read RDS:", file2)
  E_casesByStrain <- readRDS(file2)
  
  cat("\n ----------- summarise_deaths_by_strain ----------- \n")
  e_scases <- make_var_by_strain_summaries(E_casesByStrain,
                                            plot.pars.basic$pop_info,
                                            plot.pars.basic$dates,
                                            plot.pars.basic$regions,
                                           ds)	
  cat("\nWrite ",file," ... ")
  saveRDS(e_scases, file=file)
}
if(file.exists(file))
{
  e_scases <- readRDS(file)
}

#
# summarise daily cases over strain
file <- paste0(outfile.base,'-summary-daily-cases-overall',suffix,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_cases-gqs',suffix,'.RDS')
  cat("\n read RDS:", file2)
  E_cases <- readRDS(file2)
  
  cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
  cases_o <- make_var_overall_summaries(E_cases, 
                                         plot.pars.basic$pop_info,
                                         plot.pars.basic$dates,
                                         plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(cases_o, file=file)
}
if(file.exists(file))
{
  cases_o <- readRDS(file)
}


E_deathsByStrain <- NULL
E_casesByStrain <- NULL
E_deaths <- NULL
E_cases <- NULL
gc()


cat(" \n -------------------------------- \n summarise samples: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n  generating parameter plots: start \n -------------------------------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
	e_sdeaths <- subset(e_sdeaths, date<=date.max)
	e_scases <- subset(e_scases, date<=date.max)
	deaths_o <- subset(deaths_o, date<=date.max)
	cases_o <- subset(cases_o, date<=date.max)
}


#
# make overall deaths and cases plots 
cat(" \n -------------------------------- \n generate overall death / cases plots over time: start \n -------------------------------- \n")

cd_data <- copy(plot.pars.basic$death_data)
setnames(cd_data, c('state','code'), c('loc_label','loc'))

deaths.plots <- make_deathsoverall_overtime_plot(deaths_o, cd_data, outfile.base = NULL, with.title = T)
deaths.plots = gridExtra::grid.arrange(grobs = deaths.plots, nrow = length(plot.pars.basic$regions ))
ggsave(deaths.plots, file = paste0(outfile.base, '-new-deaths', suffix, '.png'), w = 6, h = length(plot.pars.basic$regions )*2.5,limitsize = FALSE)

cases.plots <- make_casesoverall_overtime_plot(cases_o, cd_data, outfile.base = NULL, with.title = T)
cases.plots = gridExtra::grid.arrange(grobs = cases.plots, nrow = length(plot.pars.basic$regions ))
ggsave(cases.plots, file = paste0(outfile.base, '-new-cases', suffix, '.png'), w = 6, h = length(plot.pars.basic$regions )*2.5,limitsize = FALSE)


cat(" \n -------------------------------- \n generate overall death / cases plots over time: end \n -------------------------------- \n")

#
# make deaths and cases by strain plots 
cat(" \n -------------------------------- \n generate death / cases by strain plots over time: start \n -------------------------------- \n")

deathsbystrain.plots <- make_deathsbystrain_overtime_plot(e_sdeaths,NULL, T)
deathsbystrain.plots = ggpubr::ggarrange(plotlist = deathsbystrain.plots, nrow = length(plot.pars.basic$regions ), common.legend = T, legend = 'bottom')
ggsave(deathsbystrain.plots, file = paste0(outfile.base, '-new-deathsbystrain', suffix, '.png'), w = 6, h = length(plot.pars.basic$regions )*2.5,limitsize = FALSE)

casesbystrain.plots <- make_casesbystrain_overtime_plot(e_scases, NULL, T)
casesbystrain.plots = ggpubr::ggarrange(plotlist = casesbystrain.plots, nrow = length(plot.pars.basic$regions ), common.legend = T, legend = 'bottom')
ggsave(casesbystrain.plots, file = paste0(outfile.base, '-new-casesbystrain', suffix, '.png'), w = 6, h = length(plot.pars.basic$regions )*2.5,limitsize = FALSE)


cat(" \n -------------------------------- \n generate death / cases by strain plots over time: end \n -------------------------------- \n")


cat(" \n -------------------------------- \n \n End: post-processing-new-strain-make-deaths-cases-plot.R \n \n -------------------------------- \n")
