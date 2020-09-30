# post-processing-make-deaths-mobility-contacts-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-deaths-mobility-contacts-plot.R \n \n -------------------------------- \n")

suppressMessages(library(rstan, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))
suppressMessages(library(gdata, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))
suppressMessages(library(tidyr, quietly = TRUE))
suppressMessages(library(EnvStats, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(stringr, quietly = TRUE))
suppressMessages(library(splines, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(magick, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))


#	for dev purposes
if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rdsgpfs/general/user/ablenkin/home/covid/base_age_fsq_mobility_200703f_cmdstanv-4states_lifr_eta2_devcntct_dataJ29_test2'
	args_dir[['job_tag']] <- '4states_lifr_eta2_devcntct_dataJ29_test2'
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

cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)
outfile.base <- paste0(args_dir$out_dir, "/",
		args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
fsq.infile.dir <- file.path(args_dir$out_dir, paste0(args_dir$stanModelFile, "-", args_dir$job_tag,"-", plot.pars.basic$JOBID))
cat(" \n -------------------------------- \n combinining plots to panel \n -------------------------------- \n")
for(x in plot.pars.basic$regions){
	deaths <- ggdraw() + draw_image(image_read(paste0(outfile.base,'-E_deathsByAge-', x, '.png'),density=2000))
	mt <- ggdraw() + draw_image(image_read(file.path(fsq.infile.dir, paste0("fsq_mobilitytrends_usa_by_age_",x, ".png")),density=2000))
	cm <- ggdraw() + draw_image(image_read(paste0(outfile.base,'-contact_patterns_over_time-', x, '.png'),density=2000))
	mtcm <- ggarrange(mt,cm,ncol=1,heights=c(3,5),align="v")
	g <- ggarrange(deaths,mtcm, labels=c('A','B'),font.label=list(size=20),ncol=1,vjust=1,heights=c(3,6),widths=c(4,6),align="hv")
	ggsave(paste0(outfile.base,'-deaths_mobilitytrends_contactpatterns-', x, '.png'), g, w = 15, h=17)
}

cat(" \n -------------------------------- \n \n completed post-processing-make-deaths-mobility-contacts-plot.R \n \n -------------------------------- \n")