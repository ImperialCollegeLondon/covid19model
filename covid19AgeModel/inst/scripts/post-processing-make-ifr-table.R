# post-processing-ifr-table.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-ifr-table.R \n \n -------------------------------- \n")

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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200803c2_cmdstanv'
	args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200803c2_cmdstanv-4states_updateifrprior_cap85'
	args_dir[['job_tag']] <- '4states_updateifrprior_cap85'	
}

if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
	args_dir[['job_tag']] <- '19states_stdctn_2'
	
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	stopifnot(args_line[[7]]=='-overwrite')
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
	args_dir[['overwrite']] <- as.integer(args_line[[8]])
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

cat(" \n -------------------------------- \n start make ifr table \n -------------------------------- \n")

file <- paste0(outfile.base,'-summary-ifr_overall.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_deathsByAge <- readRDS(file2)

  cat("\n ----------- summarise_ifr ----------- \n")
  ifr <- make_ifroverall_summaries(E_casesByAge, 
                                   E_deathsByAge, 
                                   plot.pars.basic$dates, 
                                   plot.pars.basic$regions, 
                                   plot.pars.basic$pop_info)
  ifr[,L:= paste0( sprintf('%.2f', 100*M), ' [',sprintf('%.2f', 100*CL),'-', sprintf('%.2f', 100*CU),']' )]
  
  cat("\nWrite ",file," ... ")
  saveRDS(ifr, file=file)		
}
if(file.exists(file))
{
  ifr <- readRDS(file)
}

E_casesByAge <- NULL
E_deathsByAge <- NULL

tmp <- unique(subset(plot.pars.basic$pop_info, select=c(loc, loc_label)))
ifr <- merge(tmp, ifr, by=c('loc','loc_label'), all.x=TRUE)
set(ifr, ifr[, which(is.na(L))], 'L', '-')

file <- paste0(outfile.base,'-ifr-table.rds')
cat("\nWrite IFR table to", file)
saveRDS(ifr, file=file)

cat("\n ----------- plot ifr ----------- \n")
g <-  ggplot(subset(ifr,L!='-')) +
	geom_bar(aes(x=loc, y=M), stat='identity', fill=viridis_pal(alpha=1,begin=0.3)(1)) +
	geom_errorbar(aes(x=loc,ymin=CL, ymax=CU), stat='identity',width=.2,
								position=position_dodge(.9)) +
	labs(x='',y='IFR') + 
	theme_bw() + 
	theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") 
ggsave(paste0(outfile.base,'-ifroverall-bystate', '.png'), g, w = 21, h=5)


cat("\n ----------- summarise ifr by age across states ----------- \n")

file <- paste0(outfile.base,'-summary-ifr_us_byage.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
	file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_casesByAge <- readRDS(file2)
	
	file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_deathsByAge <- readRDS(file2)
	
	age_cat_map <- make_age_cat_map_6(plot.pars.basic$pop_info)
	
	ifr_allstates <- make_ifrbyage_us_summaries(age_cat_map,
																 E_casesByAge, 
																 E_deathsByAge, 
																 plot.pars.basic$dates, 
																 plot.pars.basic$regions, 
																 plot.pars.basic$pop_info)
	ifr_allstates[,L:= paste0( sprintf('%.2f', 100*M), ' [',sprintf('%.2f', 100*CL),'-', sprintf('%.2f', 100*CU),']' )]

	E_casesByAge <- NULL
	E_deathsByAge <- NULL
	
	set(ifr_allstates, ifr_allstates[, which(is.na(L))], 'L', '-')
	saveRDS(ifr_allstates, file=file)		
}

cat(" \n -------------------------------- \n \n Completed post-processing-ifr-table.R \n \n -------------------------------- \n")

