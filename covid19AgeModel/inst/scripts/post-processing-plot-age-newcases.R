# post-processing-plot-age-newcases.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-plot-age-newcases.R \n \n -------------------------------- \n")

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
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b4_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b4_cmdstanv-37states_Sep2'
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

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)


cat(" \n -------------------------------- \n summarise case samples: start \n -------------------------------- \n")

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)


# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)


cat("\n ----------- summarise_e_newcases_byage_c ----------- \n")
file <- paste0(outfile.base,'-summary-newcases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  e_newcases_byage_c <- summarise_e_newcases_byage_c(E_casesByAge,
                                                     age_cat_map, 
                                                     plot.pars.basic$pop_info, 
                                                     plot.pars.basic$dates, 
                                                     plot.pars.basic$regions)
  file <- paste0(outfile.base,'-summary-newcases-age.RDS')
  cat("\nWrite ",file," ... ")
  saveRDS(e_newcases_byage_c, file=file)
}
if(file.exists(file))
{
  e_newcases_byage_c <- readRDS(file)
}
cat("\n ----------- plot_proportion_newcases_byage_c ----------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
  e_acases_byage_c <- subset(e_newcases_byage_c, date<=date.max)
}

plot_new_cases_flows_by_age_over_time(data = copy(e_acases_byage_c), parname = "e_newcases", 
                      ylab = 'Age composition of new infections', outfile.base)


cat("\n ----------- summarise_age_newcases ----------- \n")
file <- paste0(outfile.base,'-summary-age-newcases.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  e_age_newcases <- make_age_cases_overtime_plot(E_casesByAge,
                                                 plot.pars.basic$regions,
                                                 plot.pars.basic$pop_info, 
                                                 plot.pars.basic$dates)
  file <- paste0(outfile.base,'-summary-age-newcases.RDS')
  cat("\nWrite ",file," ... ")
  saveRDS(e_age_newcases, file=file)
}
if(file.exists(file))
{
  e_age_newcases <- readRDS(file)
}
cat("\n ----------- plot_mean_age_new_cases ----------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
  e_age_newcases <- subset(e_age_newcases, date<=date.max)
}

age = sort(unique(round(c(e_age_newcases$CL,e_age_newcases$CU))))
ggplot(e_age_newcases, aes(x = date)) +
  geom_ribbon(aes(ymin = CL, ymax = CU),colour='#35608D33',alpha=0.2) +
  geom_line(aes(y = M),colour=viridis_pal(alpha=1,begin=0.3)(1)) +
  scale_y_continuous(expand=c(0,0),breaks = seq(min(age),max(age),by=4),labels= seq(min(age),max(age),by=4)) +
  theme_bw(base_size=12) +
  facet_wrap(~loc_label, ncol = 3) + 
  labs(x = "", y = "Mean age of infection") +
  theme(legend.position=c(0.7,0.85),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect( color=NA, fill="white", size=1)) +
  scale_x_date(expand=c(0,0),date_breaks = "months", labels = date_format("%e %b")) +
  scale_colour_viridis(begin=0,end=1,direction=-1) +
  ggsave(paste0(outfile.base,'-mean_ageofinfection.png'), w = 210, h = 310, units = "mm")

cat(" \n -------------------------------- \n \n Completed post-processing-plot-age-newcases.R \n \n -------------------------------- \n")