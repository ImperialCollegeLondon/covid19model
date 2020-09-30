# post-processing-sensititivy-ifr-age-prior-Rt-contribution.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-ifr-age-prior-Rt-contribution.r \n \n -------------------------------- \n")

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
args_dir <- list()
args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200821b4_cmdstanv"
args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
args_dir[['job_tag']] <- "37states_Sep2"
args_dir[['dev_job_tag']] <- "37states_tau1_Levinprior_Sep2"
args_dir[['overwrite']] <- 0
args_dir[['period_length']] <- 30

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-dev_job_tag')
  stopifnot(args_line[[9]]=='-overwrite')
  stopifnot(args_line[[11]]=='-period_length')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['dev_job_tag']] <- args_line[[8]]
  args_dir[['overwrite']] <- as.integer(args_line[[10]])
  args_dir[['period_length']] <- as.integer(args_line[[12]])
} 

## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$dev_job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$dev_job_tag)

# load inputs for this script
# central analysis
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
# alternative model
file <- paste0(outfile.base.dev,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic.dev <- readRDS(file)

# defined model names
model_names = c("Central model", "Model with log IFR prior constructed from the Levin et al. meta-analysis")

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

#
# last day without forecasting
tmp = data.table(dates_flow = unique(as.vector(sapply(Rt_byage_c.all, function(x) as.character(x$date)))))
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic.dev$dates, function(x) any(x==dates_flow)))), by='dates_flow']
tmp <- subset(tmp, ALL_NON_FORECAST)
last_date <- as.Date(max(tmp$dates_flow))
cat("last date is ", as.character(last_date))

#
# Plots
g_Rt_byage = make_comparison_posterior_intervals_national_sensitivity_ifr_prior(tmp = Rt_byage_c.all, 
                                                          xlab = "Estimated age-specific effective reproductive number \n in the United States", 
                                                          pars = "Rt_byage",
                                                          without_facet_title = 0,
                                                          model_names = model_names, 
                                                          Date = last_date,
                                                          xintercept = 1)

g_flowbyage = make_comparison_posterior_intervals_national_sensitivity_ifr_prior(tmp = cumpropflow_byage_c.all, 
                                                           xlab = "Estimated contribution of age groups to Sars-Cov-2 transmission \n in the United States", 
                                                           pars = "flow_byage",
                                                           without_facet_title = 1,
                                                           model_names = model_names, 
                                                           Date = last_date+args_dir$period_length,
                                                           scale_percent = 1)

ggplot.list = list(g_Rt_byage, g_flowbyage)

file = paste0(outfile.base,"-sensitivity-ifr-prior","-","Rt_contribution", '.pdf')
pdf(file, w = 10, h = 8)
grid.arrange(
  grobs = ggplot.list,
  heights = c(1, 0.1, 1, 0.1),
  layout_matrix = rbind(c(1, 1, 1),
                        c(NA, NA, NA),
                        c(2, 2, 2),
                        c(NA, NA, NA)),
  left = textGrob("Age band", gp=gpar(fontsize=20), rot = 90, y = 0.5, x= 0.4)
)
grid.text(c("Estimated age-specific effective reproductive number \n in the United States"), 
          x = 0.5, y = 0.53, gp = gpar(fontsize=15))
grid.text(c("Estimated contribution of age groups to Sars-Cov-2 transmission \n in the United States"), 
          x = 0.5, y = 0.04, gp = gpar(fontsize=15))
dev.off()

file =  paste0(outfile.base,"-sensitivity-ifr-prior","-","Rt_contribution", '.rds')
saveRDS(format(last_date, "%B %d, %Y"), file, version = 2 )

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-ifr-age-prior-Rt-contribution.r \n \n -------------------------------- \n")

