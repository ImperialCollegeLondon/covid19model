# post-processing-make-newcases-overalldeaths-Rt-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-make-newcases-overalldeaths-Rt-plot.R \n \n -------------------------------- \n")

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
if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b9_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b9_cmdstanv-37states_tau10_sameLastDate'
  args_dir[['job_tag']] <- '37states_tau10_sameLastDate'
  args_dir[['overwrite']] <- 0
	args_dir[['with_forecast']] <- 0
}

if(0)
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

outfile.base <- paste0(args_dir$out_dir, "/",
		args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

cat(" \n -------------------------------- \n start summarising samples \n -------------------------------- \n")

#
# collect observed deaths and cases
cd_data <- copy(plot.pars.basic$death_data)
setnames(cd_data, c('state','code'), c('loc_label','loc'))

#
# summarise overall estimated cases
file <- paste0(outfile.base,'-summary-cases_s.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_cases_s ----------- \n")
  
  cases_s <- make_casesoverall_summaries(E_casesByAge, 
                                         plot.pars.basic$dates, 
                                         plot.pars.basic$regions, 
                                         plot.pars.basic$pop_info)
  E_casesByAge <- NULL
  
  cat("\nWrite ",file," ... ")
  saveRDS(cases_s, file=file)		
}
if(file.exists(file))
{
  cases_s <- readRDS(file)
}


#
#	summarise overall estimated deaths
file <- paste0(outfile.base,'-summary-deaths_s.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_deathsByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_deaths_s ----------- \n")
  
  deaths_s <- make_deathsoverall_summaries(E_deathsByAge, 
                                           plot.pars.basic$dates, 
                                           plot.pars.basic$regions, 
                                           plot.pars.basic$pop_info)
  E_deathsByAge <- NULL
  cat("\nWrite ",file," ... ")
  saveRDS(deaths_s, file=file)		
}
if(file.exists(file))
{
  deaths_s <- readRDS(file)
}

#
# summarise overall estimated Rt
file <- paste0(outfile.base,'-summary-rt_s_averageover', "1", 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  cat("\n ----------- summarise_Rt ----------- \n")
  file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_effcasesByAge <- readRDS(file2)
  
  file3 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  RtByAge <- readRDS(file3)
  
  cat("\n ----------- summarise_Rt ----------- \n")
  rt_s <- summarise_Rt_instantaneous(E_effcasesByAge,
                                     RtByAge,
                                     period_length = 1,
                                     plot.pars.basic$pop_info, 
                                     plot.pars.basic$dates, 
                                     plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(rt_s, file=file)
  gc()
}
if(file.exists(file))
{
  rt_s <- readRDS(file)
}

E_effcasesByAge <- NULL
RtByAge <- NULL
E_casesByAge <- NULL
E_deathsByAge <- NULL


#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
	cases_s <- subset(cases_s, date<=date.max)	
	deaths_s <- subset(deaths_s, date<=date.max)
	rt_s <- subset(rt_s, date<=date.max)	
	cd_data <- subset(cd_data, date<=date.max)	
}

plot.pars.basic <- NULL
gc()

cat(" \n -------------------------------- \n start plotting \n -------------------------------- \n")


#
# make standalone plot of cases 
cases.plots <- make_casesoverall_overtime_plot(cases_s, 
	cd_data,  
	outfile.base=outfile.base) 

#
# make standalone plot of deaths
deaths.plots <- make_deathsoverall_overtime_plot(deaths_s,                                                
    cd_data, 
	outfile.base=outfile.base)
#
# make standalone plot of Rt
rt.plots <- make_Rtoverall_overtime_plot(rt_s,                                                
    outfile.base=outfile.base)

#
# make three panel plot
for(x in unique(rt_s$loc))
{
	g <- ggarrange( cases.plots[[x]] +
										theme_bw(base_size=14) + 
										theme(axis.text.x = element_text(angle = 45, hjust = 1), 
													legend.position = "None"), 
			deaths.plots[[x]] +
				theme_bw(base_size=14) + 
				theme(axis.text.x = element_text(angle = 45, hjust = 1), 
							legend.position = "None"), 
			rt.plots[[x]] +
				theme_bw(base_size=14) + 
				theme(axis.text.x = element_text(angle = 45, hjust = 1), 
							legend.position = "None"),
			nrow=1, 
			ncol=3, 
			widths = c(0.33,0.33,0.33), 
			labels = c("A", "B", "C"),
			font.label=list(size=18))
	ggsave(paste0(outfile.base,'-cases_deaths_Rt_panel_plot-',x,'.png'), g, width=14, height=4)		
}

cat(" \n -------------------------------- \n \n Completed post-processing-make-newcases-overalldeaths-Rt-plot.R \n \n -------------------------------- \n")
