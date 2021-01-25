# post-processing-sensititivy-Rt-contribution.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-Rt-contribution.r \n \n -------------------------------- \n")

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
suppressMessages(library(grid, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

# for dev purpose
if(0){
  args_dir <- list()
  args_dir[['out_dir']] <- "/rds/general/project/ratmann_covid19/live/age_renewal_usa/"
  args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
  args_dir[['stanModelFile']] <- "base_age_fsq_mobility_201015f8_cmdstanv"
  args_dir[['job_tag']] <- "40states_tau10_Oct29_Levinv7"
  args_dir[['stanModelFileDev']] <- "base_age_fsq_mobility_201015f8_cmdstanv"
  args_dir[['dev_job_tag']] <- "40states_tau10_Oct29_Levin"
  args_dir[['overwrite']] <- 0
  args_dir[['period_length']] <- 7
  args_dir[['prefix']] <- 'ifr-prior'
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-stanModelFileDev')	
  stopifnot(args_line[[5]]=='-out_dir')
  stopifnot(args_line[[7]]=='-job_tag')
  stopifnot(args_line[[9]]=='-dev_job_tag')
  stopifnot(args_line[[11]]=='-overwrite')
  stopifnot(args_line[[13]]=='-period_length')
  stopifnot(args_line[[15]]=='-prefix')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['stanModelFileDev']] <- args_line[[4]]
  args_dir[['out_dir']] <- args_line[[6]]
  args_dir[['job_tag']] <- args_line[[8]]
  args_dir[['dev_job_tag']] <- args_line[[10]]
  args_dir[['overwrite']] <- as.integer(args_line[[12]])
  args_dir[['period_length']] <- as.integer(args_line[[14]])
  args_dir[['prefix']] <- as.character(args_line[[16]])
} 

## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFileDev , "-", args_dir$dev_job_tag, "/",
                           args_dir$stanModelFileDev , "-", args_dir$dev_job_tag)

# load inputs for this script
# central analysis
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file, "\n")
plot.pars.basic <- readRDS(file)
# alternative model
file <- paste0(outfile.base.dev,'-stanout-basic.RDS')
cat("\n read RDS:", file, "\n")
plot.pars.basic.dev <- readRDS(file)

# defined model names
model_names <- c(paste0(args_dir$stanModelFile,'\n',args_dir$job_tag), paste0(args_dir$stanModelFileDev,'\n',args_dir$dev_job_tag))
if(args_dir$prefix == 'ifr-prior'){
  model_names = c("log IFR prior from the Levin et al meta-analysis, version 7", "log IFR prior from Levin et al meta-analysis, version 5")
}
if(args_dir$prefix == 'susceptibility-prior'){
  model_names = c("using relative susceptibility estimates from Zhang and colleagues", "using relative susceptibility estimates from Viner and colleagues")
}


# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

#
#	summarise Rt by age
# base
file <- paste0(outfile.base,'-summary-Rt-age_averageover', args_dir$period_length, 'days.RDS')
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
                                                   period_length = args_dir$period_length,
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
#dev models
file <- paste0(outfile.base.dev,'-summary-Rt-age_averageover', args_dir$period_length, 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base.dev,'-stanout-E_effcasesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_effcasesByAge <- readRDS(file2)
  
  file3 <- paste0(outfile.base.dev,'-stanout-RtByAge-gqs.RDS')
  cat("\n read RDS:", file3)
  RtByAge <- readRDS(file3)
  
  cat("\n ----------- summarise_Rt_instantaneous_byage_c ----------- \n")
  Rt_byage_c.dev <- summarise_Rt_instantaneous_byage_c(E_effcasesByAge, 
                                                   RtByAge, 
                                                   period_length = args_dir$period_length,
                                                   age_cat_map, 
                                                   plot.pars.basic$pop_info, 
                                                   plot.pars.basic$dates, 
                                                   plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_byage_c.dev, file=file)
  
}
if(file.exists(file))
{
  Rt_byage_c.dev <- readRDS(file)
}

Rt_byage_c.all = list(Rt_byage_c, Rt_byage_c.dev)

#
#	summarise if cum prop flow by age
# base
file <- paste0(outfile.base,'-summary-cum-prop-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
  cumpropflow_byage_c <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                        plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                        age_cat_map, 
                                                        NULL,
                                                        plot.pars.basic$pop_info, 
                                                        plot.pars.basic$dates, 
                                                        plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(cumpropflow_byage_c, file=file)		
}
if(file.exists(file))
{
  cumpropflow_byage_c <- readRDS(file)
}
#dev models
file <- paste0(outfile.base.dev,'-summary-cum-prop-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base.dev,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
  cumpropflow_byage_c.dev <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                        plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                        age_cat_map, 
                                                        NULL,
                                                        plot.pars.basic$pop_info, 
                                                        plot.pars.basic$dates, 
                                                        plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(cumpropflow_byage_c.dev, file=file)		
}
if(file.exists(file))
{
  cumpropflow_byage_c.dev <- readRDS(file)
}

cumpropflow_byage_c.all = list(cumpropflow_byage_c, cumpropflow_byage_c.dev)


# last day without forecasting
tmp_cum = data.table(dates = unique(cumpropflow_byage_c$date))
tmp_rt = data.table(dates = unique(Rt_byage_c$date))
# tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic.dev$dates, function(x) any(x==dates)))), by='dates']
# tmp <- subset(tmp, ALL_NON_FORECAST)
max_date = as.Date('2020-08-23')
tmp_cum = subset(tmp_cum, dates <= max_date)
last_date_cum <- as.Date(max(tmp_cum$dates))
tmp_rt = subset(tmp_rt, dates <= (max_date - args_dir$period_length +1))
last_date_rt <- as.Date(max(tmp_rt$dates))

#
# Save
file = paste0(outfile.base,"-sensitivity-", args_dir$prefix, "-","Rt_contribution", '.rds')
cat("Write file ", file, '\n')
saveRDS(list(format(last_date_rt, "%B %d, %Y"), format(last_date_rt + args_dir$period_length - 1, "%B %d, %Y"), 
             format(last_date_cum, "%B %d, %Y")), file = file, version = 2)

#
# Plot

# Rt by age
g_Rt_byage = make_comparison_posterior_intervals_national_sensitivity(tmp = Rt_byage_c.all, 
                                                          ylab = "Estimated age-specific effective \nreproductive number \nin the United States", 
                                                          pars = "Rt_byage",
                                                          without_x_title = 1,
                                                          model_names = model_names, 
                                                          Date = last_date_rt,
                                                          yintercept = 1)

# contribution by age
g_flowbyage = make_comparison_posterior_intervals_national_sensitivity(tmp = cumpropflow_byage_c.all, 
                                                           ylab = "Estimated contribution of age groups \nto SARS-CoV-2 transmission \nin the United States", 
                                                           pars = "flow_byage",
                                                           without_x_title = 0,
                                                           model_names = model_names, 
                                                           Date = last_date_cum,
                                                           scale_percent = 1)

ggplot.list = list(g_Rt_byage, g_flowbyage)

file = paste0(outfile.base,"-sensitivity-", args_dir$prefix, "-","Rt_contribution", '.pdf')
p = grid.arrange(grobs = ggplot.list, 
                 heights = c(1, 0.1, 1),
                 layout_matrix = rbind(c(1, 1, 1),
                                       c(NA, NA, NA),
                                       c(2, 2, 2)))
ggsave(p, file = file, w = 7, h = 8.5)


cat(" \n -------------------------------- \n \n End post-processing-sensititivy-Rt-contribution.r \n \n -------------------------------- \n")

