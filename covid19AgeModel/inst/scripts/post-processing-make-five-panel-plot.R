# post-processing-make-five-panel-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-make-five-panel-plot.R \n \n -------------------------------- \n")

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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b2_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b2_cmdstanv-39states_Aug20'
	args_dir[['job_tag']] <- '39states_Aug20'
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
	args_dir[["include_lambda_age"]] <- 0
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
#	summarise effectively infectious cases by age
file <- paste0(outfile.base,'-summary-eff-infectious-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
	file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_effcasesByAge <- readRDS(file2)
	
	cat("\n ----------- summarise_e_acases_eff_byage_c ----------- \n")
	e_acases_eff_byage_c <- summarise_e_acases_eff_byage_c(E_effcasesByAge,
		age_cat_map, 
		plot.pars.basic$pop_info, 
		plot.pars.basic$dates, 
		plot.pars.basic$regions)
	cat("\nWrite ",file," ... ")
	saveRDS(e_acases_eff_byage_c, file=file)
}
if(file.exists(file))
{
	e_acases_eff_byage_c <- readRDS(file)
}

#
#	summarise cases by age
file <- paste0(outfile.base,'-summary-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
	cat("\n ----------- summarise_e_acases_byage_c ----------- \n")
	e_acases_byage_c <- summarise_e_acases_byage_c(E_casesByAge,
		age_cat_map, 
		plot.pars.basic$pop_info, 
		plot.pars.basic$dates, 
		plot.pars.basic$regions)
	cat("\nWrite ",file," ... ")
	saveRDS(e_acases_byage_c, file=file)
}
if(file.exists(file))
{
	e_acases_byage_c <- readRDS(file)
}

E_effcasesByAge <- NULL
RtByAge <- NULL
E_casesByAge <- NULL
gc()


#
#	summarise cumulative attack rate by age just for plotting
cat("\n ----------- summarise_attackrate_byage_c ----------- \n")
attackrate_byage_c <- summarise_attackrate_byage_c(e_acases_byage_c, 
		age_cat_map,
        plot.pars.basic$pop_info, 
		plot.pars.basic$regions)

#
#	rescale for plotting
tmp <- subset(plot.pars.basic$pop_info, select=c( loc, age.cat, pop, pop_total))
tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat2, age.cat)), by=c('age.cat'))
pop_c <- tmp[, list(prop_c=sum(pop)/pop_total), by=c('loc','age.cat2')]
pop_c <- unique(subset(pop_c,select=c(loc,age.cat2,prop_c)))
attackrate_byage_c <- subset(attackrate_byage_c, select=c(age_cat,age_band,date,M,time,loc,loc_label))
attackrate_byage_c <- merge( attackrate_byage_c, pop_c,by.x=c('age_cat','loc'),by.y=c('age.cat2','loc'))
attackrate_byage_c[, Mc:= M*prop_c]
attackrate_byage_c[, M:=NULL]
setnames(attackrate_byage_c,'Mc','M')

cat(" \n -------------------------------- \n summarise case samples: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n summarise transmission par samples: start \n -------------------------------- \n")


#
#	summarise force of infection
file <- paste0(outfile.base,'-summary-lambda-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
	file2 <- paste0(outfile.base,'-stanout-lambdaByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	lambdaByAge <- readRDS(file2)
	
	cat("\n ----------- summarise_lambda_byage_c ----------- \n")	
	lambda_byage_c <- summarise_lambda_byage_c(lambdaByAge,
		age_cat_map, 
		plot.pars.basic$pop_info, 
		plot.pars.basic$dates, 
		plot.pars.basic$regions)		
	cat("\nWrite ",file," ... ")
	saveRDS(lambda_byage_c, file=file)
}
if(file.exists(file))
{
	lambda_byage_c <- readRDS(file)
}

lambdaByAge <- NULL

cat(" \n -------------------------------- \n summarise transmission par samples: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n  generating parameter plots \n -------------------------------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
	Rt_byage_c <- subset(Rt_byage_c, date<=date.max)
	e_acases_eff_byage_c <- subset(e_acases_eff_byage_c, date<=date.max)
	e_acases_byage_c <- subset(e_acases_byage_c, date<=date.max)
	attackrate_byage_c <- subset(attackrate_byage_c, date<=date.max)
	lambda_byage_c <- subset(lambda_byage_c, date<=date.max)
}


p_aRt <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{
	p_aRt[[c]] <- plot_Rt_byage_c(Rt_byage_c, 
			"aRt", 
			ylab='Rt\n(posterior median by age band)', 
			c, 
			outfile.base=NULL)	+
			theme_bw(base_size=14) + 
			theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_text(size=12),
						legend.position="bottom")
		
}

p_eacases_eff <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{
	p_eacases_eff[[c]] <- plot_par_byage_c(e_acases_eff_byage_c, 
					"e_acases_eff", 
					ylab='Total number of \n infectious people \n(posterior median by age band)',
					c,
					outfile.base=NULL) +
					scale_y_continuous(labels = function(x) format(x, scientific = FALSE))	+	
					theme_bw(base_size=14) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_text(size=12),
								legend.position="bottom")
}	

p_acases <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{
	p_acases[[c]] <- plot_par_byage_c(e_acases_byage_c,
					"e_acases",
					ylab='Cumulative cases\n(posterior median by age band)',
					c,
					outfile.base=NULL) +
					scale_y_continuous(labels = function(x) format(x, scientific = FALSE))	+	
		theme_bw(base_size=14) + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_text(size=12),
					legend.position="bottom")
}

p_attrate <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{	
	p_attrate[[c]] <- plot_par_byage_c(attackrate_byage_c,
			"attrate",
			ylab='Cumulative attack rate\n(posterior median by age band)',
			c,
			outfile.base=NULL)	+	
		theme_bw(base_size=14) + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_text(size=12),
					legend.position="bottom")
}

p_lambda <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{		
	p_lambda[[c]] <- plot_par_byage_c(lambda_byage_c,
			"lambda",
			ylab='Infectious contacts \n(posterior median by age band)',
			c,
			outfile.base=NULL)	+	
		theme_bw(base_size=14) + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_text(size=12),
					legend.position="bottom")
}

cat(" \n -------------------------------- \n combinining plots to panel \n -------------------------------- \n")

panel <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{
  if(args_dir$include_lambda_age){
    panel[[c]] <- ggarrange( p_acases[[c]],
                             p_attrate[[c]],
                             p_eacases_eff[[c]],
                             p_lambda[[c]],
                             legend="bottom",
                             common.legend=TRUE,
                             labels=c('B','C','D','E'),
                             font.label=list(size=20),
    												 hjust=0,
                             vjust=0.5,
                             heights=c(2,2,2,2),
                             widths=c(3,3,3,3))
  } else
  {
    panel[[c]] <- ggarrange( p_acases[[c]],
                             p_attrate[[c]],
                             p_eacases_eff[[c]],
                             legend="bottom",
                             common.legend=TRUE,
                             labels=c('B','C','D'),
                             font.label=list(size=20),
    												 hjust=0,
    												 vjust=0.5,
                             heights=c(2,2,2,2),
                             widths=c(3,3,3,3))
  }

	p_aRt[[c]] <- p_aRt[[c]] + theme(legend.position="none")
	panel[[c]] <- ggarrange( p_aRt[[c]],
			panel[[c]],
			labels=c('A'),
			ncol=1,
			font.label=list(size=20),
			hjust=0,
			vjust=1,
			heights=c(2,4),
			widths=c(4,4))
	ggsave(paste0(outfile.base,'-five_panel_plot_new-', c, '.png'), panel[[c]], w = 14, h=10)
}

cat(" \n -------------------------------- \n \n Completed post-processing-make-five-panel-plot.R \n \n -------------------------------- \n")
