
cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-contacts-0-14-Rt-flow.r \n \n -------------------------------- \n")

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
args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200730j2_cmdstanv"
args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
args_dir[['job_tag']] <- "37states_Sep8"
args_dir[['num_dev_mod']] <- 3
args_dir[['dev_models_tau']] <- "0.5,1,2"
args_dir[['dev_stanModelFile']] <- "base_age_fsq_mobility_200821b4_cmdstanv,base_age_fsq_mobility_200821b9_cmdstanv,base_age_fsq_mobility_200821b4_cmdstanv"
args_dir[['dev_job_tag']] <- "37states_tau05_Sep2,37states_tau10_sameLastDate,37states_tau1.5_Sep2"
args_dir[['overwrite']] <- 0
args_dir[['period_length']] <- 7

args_dir <- list()
args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200730j1_cmdstanv"
args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
args_dir[['job_tag']] <- "4states_updatedata"
args_dir[['num_dev_mod']] <- 3
args_dir[['dev_models_tau']] <- "0.5,1,2"
args_dir[['dev_stanModelFile']] <- "base_age_fsq_mobility_200821b4_cmdstanv,base_age_fsq_mobility_200821b9_cmdstanv,base_age_fsq_mobility_200821b4_cmdstanv"
args_dir[['dev_job_tag']] <- "37states_tau05_Sep2,37states_tau10_sameLastDate,37states_tau1.5_Sep2"
args_dir[['overwrite']] <- 0
args_dir[['period_length']] <- 7

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-num_dev_mod')
  stopifnot(args_line[[9]]=='-dev_models_tau')
  stopifnot(args_line[[11]]=='-dev_stanModelFile')
  stopifnot(args_line[[13]]=='-dev_job_tag')
  stopifnot(args_line[[15]]=='-overwrite')
  stopifnot(args_line[[17]]=='-period_length')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['num_dev_mod']] <- as.integer(args_line[[8]])
  args_dir[['dev_models_tau']] <- args_line[[10]]
  args_dir[['dev_stanModelFile']] <- args_line[[12]]
  args_dir[['dev_job_tag']] <- args_line[[14]]
  args_dir[['overwrite']] <- as.integer(args_line[[16]])
  args_dir[['period_length']] <- as.integer(args_line[[18]])
} 

args_dir$dev_models_tau <- strsplit(args_dir$dev_models_tau,',')[[1]]
args_dir$dev_job_tag <- strsplit(args_dir$dev_job_tag,',')[[1]]
args_dir$dev_stanModelFile <- strsplit(args_dir$dev_stanModelFile,',')[[1]]
stopifnot(length(args_dir[['dev_models_tau']]) == args_dir[['num_dev_mod']])


## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

for(i in 1:(args_dir[['num_dev_mod']])){
  assign(paste0("outfile.base", args_dir$dev_models_tau[i]), paste0(args_dir$out_dir,"/", args_dir$dev_stanModelFile[i] , "-", args_dir$dev_job_tag[i], "/",
                                                                    args_dir$dev_stanModelFile[i] , "-", args_dir$dev_job_tag[i]) )
}

# load inputs for this script
# mobility extrapolated for 0-14 model
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
# zhang contact matrix 
plot.pars.basic.dev = vector('list',args_dir$num_dev_mod)
for(i in args_dir$dev_models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-stanout-basic.RDS')
  cat("\n read RDS:", file)
  plot.pars.basic.dev[[i]] = readRDS(file)
}

# group
plot.pars.basic.all = c(list(plot.pars.basic), 
                        plot.pars.basic.dev)

# defined model names
model_names = c("extrapolated_mobility_0-14", args_dir$dev_models_tau)

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
                                                   args_dir$period_length,
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
Rt_byage_c.dev = vector('list',args_dir$num_dev_mod) 
j = 1
for(i in args_dir$dev_models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-summary-Rt-age_averageover', args_dir$period_length, 'days.RDS')
  if(!file.exists(file) | args_dir[['overwrite']])
  {	
    file2 <- paste0(get(paste0("outfile.base", i)),'-stanout-RtByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    RtByAge <- readRDS(file2)
    
    file3 <- paste0(get(paste0("outfile.base", i)),'-stanout-E_effcasesByAge-gqs.RDS')
    cat("\n read RDS:", file3)
    E_effcasesByAge <- readRDS(file3)
    
    cat("\n ----------- summarise_Rt_instantaneous_byage_c ----------- \n")
    Rt_byage_c.dev[[j]] <- summarise_Rt_instantaneous_byage_c(E_effcasesByAge, 
                                                              RtByAge, 
                                                              period_length = args_dir$period_length,
                                                              age_cat_map, 
                                                              plot.pars.basic.dev[[i]]$pop_info, 
                                                              plot.pars.basic.dev[[i]]$dates, 
                                                              plot.pars.basic.dev[[i]]$regions)
    
    cat("\nWrite ",file," ... ")
    saveRDS(Rt_byage_c.dev[[j]], file=file)
    j = j +1
  }
  if(file.exists(file))
  {
    Rt_byage_c.dev[[j]] <- readRDS(file)
    j = j +1
  }
}

Rt_byage_c.all = c(list(Rt_byage_c), Rt_byage_c.dev)

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
cumpropflow_byage_c.dev = vector('list',args_dir$num_dev_mod) 
j = 1
for(i in args_dir$dev_models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-summary-cum-prop-flow-age.RDS')
  if(!file.exists(file) | args_dir[['overwrite']])
  {	
    file2 <- paste0(get(paste0("outfile.base", i)),'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
    
    cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
    cumpropflow_byage_c.dev[[j]] <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                                   plot.pars.basic.dev[[i]]$stan_data$reduced_flows_Monday_idx,
                                                                   age_cat_map, 
                                                                   plot.pars.basic.dev[[i]]$pop_info, 
                                                                   plot.pars.basic.dev[[i]]$dates, 
                                                                   plot.pars.basic.dev[[i]]$regions)
    cat("\nWrite ",file," ... ")
    saveRDS(cumpropflow_byage_c.dev[[j]], file=file)
    j = j + 1
    
  }
  if(file.exists(file))
  {
    cumpropflow_byage_c.dev[[j]] <- readRDS(file)
    j = j + 1
  }
}

cumpropflow_byage_c.all = c(list(cumpropflow_byage_c), cumpropflow_byage_c.dev)

#
# last day without forecasting
tmp = data.table(dates_flow = unique( as.vector( sapply(Rt_byage_c.all, function(x) as.character(x$date)) ) ) )
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic.dev[["1"]]$dates, function(x) any(x==dates_flow)))), by='dates_flow']
tmp <- subset(tmp, ALL_NON_FORECAST)
last_date <- as.Date(max(tmp$dates_flow))

#
# Plots
g_Rt_byage = make_comparison_posterior_intervals_national_sensitivity_contacts_0_14(tmp = Rt_byage_c.all, 
                                                                                    xlab = "Estimated age-specific effective reproductive number \n in the United States", 
                                                                                    pars = "Rt_byage",
                                                                                    without_facet_title = 0,
                                                                                    model_names = model_names, 
                                                                                    outfile.base,
                                                                                    Date = last_date,
                                                                                    xintercept = 1)

g_flowbyage = make_comparison_posterior_intervals_national_sensitivity_contacts_0_14(tmp = cumpropflow_byage_c.all, 
                                                                                     xlab = "Estimated contribution of age groups to Sars-Cov-2 transmission \n in the United States", 
                                                                                     pars = "flow_byage",
                                                                                     without_facet_title = 1,
                                                                                     model_names = model_names, 
                                                                                     outfile.base,
                                                                                     Date = last_date + args_dir$period_length,
                                                                                     scale_percent = 1)

ggplot.list = c(g_Rt_byage, g_flowbyage)

file = paste0(outfile.base1,"-contacts-0-14","-","Rt_flow", '.pdf')
cat("Write ", file)
pdf(file, w = 11, h = 8)
grid.arrange(
  grobs = ggplot.list,
  widths = c(0.8, 0.1, 0.25),
  heights = c(0.12, 1.1, 0.05, 0.05, 1, 0.05),
  layout_matrix = rbind(c(1, NA, NA),
                        c(1, NA, 2),
                        c(NA, NA, NA),
                        c(3, NA, NA),
                        c(3, NA, 4),
                        c(NA, NA, NA)),
  left = textGrob("Age band", gp=gpar(fontsize=20), rot = 90, y = 0.5, x= 0.4)
)
grid.text(c("A"), x = 0.05, 
          y = 0.97, gp = gpar(fontsize=20, fontface = "bold"))
grid.text(c("B"), x = 0.8, 
          y = 0.97, gp = gpar(fontsize=20, fontface = "bold"))
grid.text(c("Estimated age-specific effective reproductive number \n in the United States"), 
          x = 0.5, y = 0.48, gp = gpar(fontsize=15))
grid.text(c("Estimated contribution of age groups to Sars-Cov-2 transmission \n in the United States"), 
          x = 0.5, y = 0.03, gp = gpar(fontsize=15))
dev.off()

# save rds
for(i in 1:length(model_names)){
  Rt_byage_c.all[[i]][, M_CL_CU := paste0(format(round(M, 2), nsmall = 2), " [", format(round(CL, 2), nsmall = 2), ", ", format(round(CU, 2), nsmall = 2), "]")]
  cumpropflow_byage_c.all[[i]][, M_CL_CU := paste0(sprintf("%.2f", M*100), "\\%",' [',sprintf("%.2f", CL*100), "\\%",'-',sprintf("%.2f", CU*100), "\\%",']')]
}

file = paste0(outfile.base1,'-contacts-0-14-', "Rt_flow", '.rds')
cat("Write ", file)
df = list(date = c(format( as.Date(last_date), "%B %d, %Y") ,format( as.Date(last_date + args_dir$period_length), "%B %d, %Y")),
          RN_09_alternative = subset(Rt_byage_c.all[[1]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          RN_1019_alternative = subset(Rt_byage_c.all[[1]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          RN_09_tau0.5 = subset(Rt_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          RN_1019_tau0.5 = subset(Rt_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          RN_09_tau1 = subset(Rt_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          RN_1019_tau1 = subset(Rt_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          RN_09_tau2 = subset(Rt_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          RN_1019_tau2 = subset(Rt_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          C_09_alternative = subset(cumpropflow_byage_c.all[[1]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          C_1019_alternative = subset(cumpropflow_byage_c.all[[1]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          C_09_tau0.5 = subset(cumpropflow_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          C_1019_tau0.5 = subset(cumpropflow_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          C_09_tau1 = subset(cumpropflow_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          C_1019_tau1 = subset(cumpropflow_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU,
          C_09_tau2 = subset(cumpropflow_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "0-9" & date == last_date)$M_CL_CU,
          C_1019_tau2 = subset(cumpropflow_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "10-19" & date == last_date)$M_CL_CU)
saveRDS(df, file=file, version = 2) 

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-contacts-0-14-Rt-flow.r \n \n -------------------------------- \n")

