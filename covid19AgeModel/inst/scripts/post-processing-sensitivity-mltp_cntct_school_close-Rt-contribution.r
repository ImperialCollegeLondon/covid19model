
cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-mltp_cntct_school_close-Rt-flow.r \n \n -------------------------------- \n")

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
args_dir[['stanModelFile']] <- "base_age_fsq_mobility_201015f8_cmdstanv,base_age_fsq_mobility_201015f8_cmdstanv,base_age_fsq_mobility_201015f10_cmdstanv"
args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
args_dir[['job_tag']] <- "40states_tau10_Oct29_Levinv7,40states_tau10_Oct29_Levin,40states_Oct29_Levin7"
args_dir[['num_mod']] <- 3
args_dir[['models_tau']] <- "0.5,1,2"
args_dir[['overwrite']] <- 0
args_dir[['period_length']] <- 7

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-num_mod')
  stopifnot(args_line[[9]]=='-models_tau')
  stopifnot(args_line[[11]]=='-overwrite')
  stopifnot(args_line[[13]]=='-period_length')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['num_mod']] <- as.integer(args_line[[8]])
  args_dir[['models_tau']] <- args_line[[10]]
  args_dir[['overwrite']] <- as.integer(args_line[[12]])
  args_dir[['period_length']] <- as.integer(args_line[[14]])
} 

args_dir$models_tau <- strsplit(args_dir$models_tau,',')[[1]]
args_dir$job_tag <- strsplit(args_dir$job_tag,',')[[1]]
args_dir$stanModelFile <- strsplit(args_dir$stanModelFile,',')[[1]]
stopifnot(length(args_dir[['models_tau']]) == args_dir[['num_mod']])


## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

for(i in 1:(args_dir[['num_mod']])){
  assign(paste0("outfile.base", args_dir$models_tau[i]), paste0(args_dir$out_dir,"/", args_dir$stanModelFile[i] , "-", args_dir$job_tag[i], "/",
                                                                    args_dir$stanModelFile[i] , "-", args_dir$job_tag[i]) )
}

# load inputs for this script
plot.pars.basic = vector('list',args_dir$num_mod)
for(i in args_dir$models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-stanout-basic.RDS')
  cat("\n read RDS:", file)
  plot.pars.basic[[i]] = readRDS(file)
}

# defined model names
model_names = args_dir$models_tau

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic[['1']]$pop_info)

#
#	summarise Rt by age
Rt_byage_c.all = vector('list',args_dir$num_mod) 
j = 1
for(i in args_dir$models_tau){
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
    Rt_byage_c.all[[j]] <- summarise_Rt_instantaneous_byage_c(E_effcasesByAge, 
                                                              RtByAge, 
                                                              period_length = args_dir$period_length,
                                                              age_cat_map, 
                                                              plot.pars.basic[[i]]$pop_info, 
                                                              plot.pars.basic[[i]]$dates, 
                                                              plot.pars.basic[[i]]$regions)
    
    cat("\nWrite ",file," ... ")
    saveRDS(Rt_byage_c.all[[j]], file=file)
    j = j +1
  }
  if(file.exists(file))
  {
    Rt_byage_c.all[[j]] <- readRDS(file)
    j = j +1
  }
}



#
#	summarise if cum prop flow by age
cumpropflow_byage_c.all = vector('list',args_dir$num_mod) 
j = 1
for(i in args_dir$models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-summary-cum-prop-flow-age.RDS')
  if(!file.exists(file) | args_dir[['overwrite']])
  {	
    file2 <- paste0(get(paste0("outfile.base", i)),'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
    
    cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
    cumpropflow_byage_c.all[[j]] <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                                   plot.pars.basic[[i]]$stan_data$reduced_flows_Monday_idx,
                                                                   age_cat_map, 
                                                                   NULL,
                                                                   plot.pars.basic[[i]]$pop_info, 
                                                                   plot.pars.basic[[i]]$dates, 
                                                                   plot.pars.basic[[i]]$regions)
    cat("\nWrite ",file," ... ")
    saveRDS(cumpropflow_byage_c.all[[j]], file=file)
    j = j + 1
    
  }
  if(file.exists(file))
  {
    cumpropflow_byage_c.all[[j]] <- readRDS(file)
    j = j + 1
  }
}


#
# last day without forecasting
tmp_cum = data.table(dates = unique(cumpropflow_byage_c.all[[1]]$date))
tmp_rt = data.table(dates = unique(Rt_byage_c.all[[1]]$date))
max_date = as.Date('2020-08-23')
tmp_cum = subset(tmp_cum, dates <= max_date)
last_date_cum <- as.Date(max(tmp_cum$dates))
tmp_rt = subset(tmp_rt, dates <= (max_date - args_dir$period_length +1))
last_date_rt <- as.Date(max(tmp_rt$dates))


#
# Plots
g_Rt_byage = make_comparison_posterior_intervals_national_sensitivity_multiplier_cntct_school_closure(tmp = Rt_byage_c.all, 
                                                                                    ylab = "Estimated age-specific effective \nreproductive number \nin the United States", 
                                                                                    pars = "Rt_byage",
                                                                                    without_legend = 1,
                                                                                    model_names = model_names, 
                                                                                    outfile.base,
                                                                                    Date = last_date_rt,
                                                                                    xintercept = 1)

g_flowbyage = make_comparison_posterior_intervals_national_sensitivity_multiplier_cntct_school_closure(tmp = cumpropflow_byage_c.all, 
                                                                                     ylab = "Estimated contribution of age groups \nto SARS-CoV-2 transmission \n in the United States", 
                                                                                     pars = "flow_byage",
                                                                                     without_legend = 0,
                                                                                     model_names = model_names, 
                                                                                     outfile.base,
                                                                                     Date = last_date_cum,
                                                                                     scale_percent = 1)

ggplot.list = list(g_Rt_byage, g_flowbyage)

file = paste0(outfile.base1,"-sensitivity-multiplier_cntct_school_closure","-","Rt_flow", '.pdf')
cat("Write ", file)
p = grid.arrange(grobs = ggplot.list)
ggsave(p, file = file, w = 7, h = 8.5)


# save rds
for(i in 1:length(model_names)){
  Rt_byage_c.all[[i]][, M_CL_CU := paste0(format(round(M, 2), nsmall = 2), " [", format(round(CL, 2), nsmall = 2), ", ", format(round(CU, 2), nsmall = 2), "]")]
  cumpropflow_byage_c.all[[i]][, M_CL_CU := paste0(sprintf("%.2f", M*100), "\\%",' [',sprintf("%.2f", CL*100), "\\%",'-',sprintf("%.2f", CU*100), "\\%",']')]
}

file = paste0(outfile.base1,'-sensitivity-multiplier_cntct_school_closure-', "Rt_flow", '.rds')
cat("Write ", file)
df = list(date = c(format(as.Date(last_date_cum), "%B %d, %Y"), format(as.Date(last_date_rt), "%B %d, %Y") ,format( as.Date(last_date_rt + args_dir$period_length - 1), "%B %d, %Y")),
          RN_09_tau0.5 = subset(Rt_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "0-9" & date == last_date_rt)$M_CL_CU,
          RN_1019_tau0.5 = subset(Rt_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "10-19" & date == last_date_rt)$M_CL_CU,
          RN_09_tau1 = subset(Rt_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "0-9" & date == last_date_rt)$M_CL_CU,
          RN_1019_tau1 = subset(Rt_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "10-19" & date == last_date_rt)$M_CL_CU,
          RN_09_tau2 = subset(Rt_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "0-9" & date == last_date_rt)$M_CL_CU,
          RN_1019_tau2 = subset(Rt_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "10-19" & date == last_date_rt)$M_CL_CU,
          C_09_tau0.5 = subset(cumpropflow_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "0-9" & date == last_date_cum)$M_CL_CU,
          C_1019_tau0.5 = subset(cumpropflow_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "10-19" & date == last_date_cum)$M_CL_CU,
          C_2034_tau0.5 = subset(cumpropflow_byage_c.all[[which(model_names == "0.5")]], loc == "US" & age_band == "20-34" & date == last_date_cum)$M_CL_CU,
          C_09_tau1 = subset(cumpropflow_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "0-9" & date == last_date_cum)$M_CL_CU,
          C_1019_tau1 = subset(cumpropflow_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "10-19" & date == last_date_cum)$M_CL_CU,
          C_2034_tau1 = subset(cumpropflow_byage_c.all[[which(model_names == "1")]], loc == "US" & age_band == "20-34" & date == last_date_cum)$M_CL_CU,
          C_09_tau2 = subset(cumpropflow_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "0-9" & date == last_date_cum)$M_CL_CU,
          C_1019_tau2 = subset(cumpropflow_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "10-19" & date == last_date_cum)$M_CL_CU,
          C_2034_tau2 = subset(cumpropflow_byage_c.all[[which(model_names == "2")]], loc == "US" & age_band == "20-34" & date == last_date_cum)$M_CL_CU)
saveRDS(df, file=file, version = 2) 

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-mltp_cntct_school_close-Rt-flow.r \n \n -------------------------------- \n")

