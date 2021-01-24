# post-processing-new-strain-make-Rt-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start: post-processing-new-strain-make-Rt-plot.R \n \n -------------------------------- \n")

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
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015i4_cmdstanv-test_new_strain2'
  args_dir[['job_tag']] <- 'test_new_strain2'
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

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

#
#	summarise Rt by age by strain
file <- paste0(outfile.base,'-summary-Rt-age-strain_averageover', "1", 'days',suffix,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
  file2 <- paste0(outfile.base,'-stanout-E_effcasesByAgeByStrain-gqs',suffix,'.RDS')
  cat("\n read RDS:", file2)
  E_effcasesByAgeByStrain <- readRDS(file2)
  
  file3 <- paste0(outfile.base,'-stanout-RtByAgeByStrain-gqs',suffix,'.RDS')
  cat("\n read RDS:", file3)
  RtByAgeByStrain <- readRDS(file3)
  
  cat("\n ----------- summarise_Rt_instantaneous_byage_c ----------- \n")
  Rt_byagebystrain_c <- summarise_Rt_instantaneous_byage_bystrain_c(E_effcasesByAgeByStrain, 
                                                                    RtByAgeByStrain, 
                                                                    period_length = 1,
                                                                    age_cat_map, 
                                                                    ds,
                                                                    plot.pars.basic$pop_info, 
                                                                    plot.pars.basic$dates, 
                                                                    plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_byagebystrain_c, file=file)	
}
if(file.exists(file))
{
  Rt_byagebystrain_c <- readRDS(file)
}


#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))	  
  Rt_byagebystrain_c <- subset(Rt_byagebystrain_c, date<=date.max)
}

# get start date for plots
date.min <- min( as.Date( sapply( plot.pars.basic$dates, function(x) min(as.character(x)) ) ) )


#
# Plot Rt by age over time for both strain
p_aRt = vector(mode = 'list', length = length(plot.pars.basic$regions))
names(p_aRt) = plot.pars.basic$regions
for(c in plot.pars.basic$regions)
{
  p_aRt[[c]] <- plot_Rt_byage_c(Rt_byagebystrain_c, 
                                "aRt", 
                                ylab='Time-varying \nreproduction numbers', 
                                c, 
                                outfile.base=NULL)	+ 
    scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
                 limits = c(date.min, 
                            max(Rt_byagebystrain_c$date))) +
    theme_bw(base_size=14) + 
    facet_wrap(~strain_cat_label, ncol =2) + 
    ggtitle(c) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom") 
}
p_aRt = ggpubr::ggarrange(plotlist = p_aRt, nrow = length(plot.pars.basic$regions ), common.legend = T, legend = 'bottom')
ggsave(p_aRt, file = paste0(outfile.base, '-Rt_byage_bystrain',suffix,'.png'),  w = 10, h=3*length(plot.pars.basic$regions),limitsize = FALSE)

cat(" \n -------------------------------- \n \n End: post-processing-new-strain-make-Rt-plot.R \n \n -------------------------------- \n")

