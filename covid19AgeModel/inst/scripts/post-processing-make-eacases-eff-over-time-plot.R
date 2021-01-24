# post-processing-make-eacases-eff-over-time-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-make-eacases-eff-over-time-plot.R \n \n -------------------------------- \n")

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
if(0)
{	
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-37states_tau10_Oct21_Levin'
	args_dir[['job_tag']] <- '37states_tau10_Oct21_Levin'
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

# map model age groups to middle age groups
age_cat_map_ad <- make_age_cat_map_adult(plot.pars.basic$pop_info)

#
#	summarise effectively infectious cases by age
file <- paste0(outfile.base,'-summary-eff-infectious-cases-middleage.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
	file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_effcasesByAge <- readRDS(file2)
	
	cat("\n ----------- summarise_e_acases_eff_byage_c ----------- \n")
		e_acases_eff_byage_c <- summarise_e_acases_eff_byage_c(E_effcasesByAge,
																												 age_cat_map_ad, 
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

E_effcasesByAge <- NULL

cat("\n ----------- summarise population by age ----------- \n")

pc <- unique(subset(plot.pars.basic$pop_info, select=c(loc,age.cat,pop)))
pc <- subset(pc, loc %in% plot.pars.basic$regions)
pc <- merge(pc, age_cat_map_ad, by='age.cat')
pt <- pc[, list(loc='US',pop=sum(pop)), by=c('age.cat2')]
pc <- pc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
pc <- merge(pc,pt,by=c('loc','age.cat2','pop'),all=T)
pc <- pc[, list(age.cat2=age.cat2, 
								pop=pop,
								pop_prop=pop/sum(pop)), 
				 by=c('loc')]
setnames(pc, 'age.cat2','age_cat')

eff_cases <- merge(subset(e_acases_eff_byage_c),pc, by=c('loc','age_cat'))

# national average (M is median of sum across states)
dt <- subset(eff_cases,loc=='US')
dt[, average:= M/pop*1e5]

eff_cases <- subset(eff_cases,loc!='US')
eff_cases[, CL:= CL/pop*1e5]
eff_cases[, CU:= CU/pop*1e5]
eff_cases[, M:= M/pop*1e5]

eff_cases <- merge(eff_cases,subset(dt,select=c('age_cat','date','average')),by=c('age_cat','date'))

cat("\n ----------- plot_e_acases_eff_20-49_over_time ----------- \n")
tmp = unique(select(eff_cases, loc, date))
tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region

g <-  ggplot(eff_cases) +
	geom_ribbon(data=subset(eff_cases,age_cat==2),aes(x=date, ymin = CL, ymax = CU,fill=age_band), alpha = .4) +
	geom_line(data=subset(eff_cases,age_cat==2),aes(x=date, y=M,col=age_band), col="steelblue",stat='identity',show.legend=TRUE) +
	geom_line(data=subset(eff_cases,age_cat==2),aes(x=date, y=average), col="black",stat='identity',linetype="dashed",show.legend=TRUE) +
	labs(x='',y="Effective infectious (per 100,000 individuals)",col="") +
	scale_x_date(expand=c(0,0),date_breaks = "4 weeks", labels = date_format("%e %b"), 
							 limits = c(eff_cases$date[1], 
							 					 max(tmp$date))) + 
	facet_wrap(loc_label~.,ncol=5) +
	theme_bw() + 
	theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
				axis.text.y = element_text(size=16),
				axis.title.x = element_text(size=16),
				axis.title.y = element_text(size=16),
				strip.background = element_blank(),
				strip.text = element_text(size = 16),
				legend.text = element_text(size = 16),
				legend.position="bottom") +
	scale_color_manual(name = "",
										 values = c( "20-49" = "steelblue")) +
	scale_fill_manual(name = "",
										values = c( "20-49"  = "steelblue")) +
	guides(col = guide_legend(nrow=4))
ggsave(paste0(outfile.base,'-','e_acases_eff_per1e5_byage_20-49', '.png'), g, w = 21, h=29)

cat(" \n -------------------------------- \n \n Completed post-processing-make-eacases-eff-over-time-plot.R \n \n -------------------------------- \n")