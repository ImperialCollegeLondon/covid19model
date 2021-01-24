# post-processing-make-deathsage-casesage-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-deathsage-casesage-plot.R \n \n -------------------------------- \n")

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
pkg.dir <- system.file(package = "covid19AgeModel" )

#	for dev purposes
if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201204b_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201204b_cmdstanv-4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'
	args_dir[['job_tag']] <- '4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'	
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

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

cat(" \n -------------------------------- \n  summarise deaths and cases \n -------------------------------- \n")

# get start date for plots
date.min <- min( as.Date( sapply( plot.pars.basic$dates, function(x) min(as.character(x)) ) ) )

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

# read predicted death by reporting age groups 
file <- file.path(pkg.dir,"data-v110","df_predict_reporting_age_strata_201126.rds")
death.predict = as.data.table( readRDS( file ) )

cat("\n ----------- summarise_e_newcases_byage_c ----------- \n")
file <- paste0(outfile.base,'-summary-newcases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
	file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_casesByAge <- readRDS(file2)
	
	e_acases_byage_c <- summarise_e_newcases_byage_c(E_casesByAge,
																										 age_cat_map, 
																										 plot.pars.basic$pop_info, 
																										 plot.pars.basic$dates, 
																										 plot.pars.basic$regions)
	file <- paste0(outfile.base,'-summary-newcases-age.RDS')
	cat("\nWrite ",file," ... ")
	saveRDS(e_acases_byage_c, file=file)
}
if(file.exists(file))
{
	e_acases_byage_c <- readRDS(file)
}

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
	e_acases_byage_c <- subset(e_acases_byage_c, date<=date.max)
}

cat(" \n -------------------------------- \n  plot deaths, cases by age and infectious over time \n -------------------------------- \n")
snames <- c('NYC','FL','CA','AZ')
s_lab <- c('New York City','Florida','California','Arizona')
if(!all(snames %in% plot.pars.basic$regions )){
  tmp = data.table(loc = plot.pars.basic$regions[1:min(4, length(plot.pars.basic$regions))])
  tmp = merge(tmp, unique(select(plot.pars.basic$pop_info, loc, loc_label)), by = 'loc')
  tmp = tmp[order(loc)]
  snames = tmp$loc
  s_lab = tmp$loc_label
}
date.min <- min( as.Date( sapply( plot.pars.basic$dates[snames], function(x) min(as.character(x)) ) ) )

data <- subset(death.predict,code%in%snames & age_index != 0)
data$loc_label <- factor(data$loc_label,levels=s_lab)
plotdates <- c(min(subset(e_acases_byage_c,loc%in%snames)$date),max(subset(e_acases_byage_c,loc%in%snames)$date))
# proportion of cum deaths by age groups over time
g_deaths <- plot_proportion_monthly_death_by_age(data, plotdates, 1, T) +
  theme(axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x =  element_text(size = 30,face="bold",hjust=0),
        axis.title.y = element_text(size=22),
        legend.position="none")
# proportion of reported cases in 20-49 over time
data <- subset(death.predict,code%in%snames)
data$loc_label <- factor(data$loc_label,levels=s_lab)
g_obs_cases <- plot_crude_proportion_monthly_cases_by_age_withCI(data, "20-49",plotdates,shift=0) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size=22),
        legend.position="none")
# proportion of estimated new cases by age groups over time
data <- subset(e_acases_byage_c,loc%in%snames)
data$loc_label <- factor(data$loc_label,levels=s_lab)
g_cases <- plot_new_cases_flows_by_age_over_time(data = data, 
                                                 parname = "e_newcases", 
                                                 ylab = 'Age composition of \nnew infections', NULL) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_text(size=22),
        legend.position="none")

cat(" \n -------------------------------- \n combinining plots to panel \n -------------------------------- \n")

# extract a legend
legend_b <- get_legend(
	g_cases +
		guides(color = guide_legend(nrow = 1)) +
		theme(legend.position = "bottom")
)

legend_c <- get_legend(
	g_obs_cases +
		guides(color = guide_legend(nrow = 1)) +
		theme(legend.position = "bottom")
)
leg <- plot_grid(legend_b,legend_c,nrow=1,align="h",rel_widths=c(1,0.3))

fig <- plot_grid(g_deaths,g_obs_cases,g_cases,leg,nrow=4,align="v",rel_heights=c(1,0.8,1.15,0.1))
ggsave(paste0(outfile.base,'-panel-deaths_age_share-cases_age_share', '.png'), fig, w = 25, h=13)
ggsave(paste0(outfile.base,'-panel-deaths_age_share-cases_age_share', '.pdf'), fig, w = 25, h=13,dpi = 500)


cat(" \n -------------------------------- \n plot for all states \n -------------------------------- \n")

cat("\n ----------- plot_proportion_cumdeaths_byage_c ----------- \n")
plotdates <- c(min(e_acases_byage_c$date),max(e_acases_byage_c$date))
g_deaths <- plot_proportion_monthly_death_by_age(subset(death.predict, age_index != 0), plotdates, 1, T) +
  labs(x='', y='Proportion of COVID-19 monthly deaths', fill='Age band', col = 'Age band') 
g_obs_cases <- plot_crude_proportion_monthly_cases_by_age_withCI(death.predict, "20-49",plotdates) + 
  labs(x='', y='Proportion of COVID-19 monthly cases', fill='Age band', col = 'Age band') 
ggsave(paste0(outfile.base,'-deaths_age_share', '.png'), g_deaths, h=29,w=21)
ggsave(paste0(outfile.base,'-deaths_age_share', '.pdf'), g_deaths, h=29,w=21,dpi = 500)
ggsave(paste0(outfile.base,'-obscases_age_share', '.png'), g_obs_cases, h=29,w=21)
ggsave(paste0(outfile.base,'-obscases_age_share', '.pdf'), g_obs_cases, h=29,w=21,dpi = 500)
	
cat(" \n -------------------------------- \n \n completed post-processing-make-deathsage-casesage-plot.R \n \n -------------------------------- \n")
	