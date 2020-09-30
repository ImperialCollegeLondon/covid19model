# post-processing-sensititivy-contacts-0-14-contact_patterns.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-contacts-0-14-contact_patterns.r \n \n -------------------------------- \n")

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
args_dir[['dev_models_tau']] <- "0.5,1,1.5"
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
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['num_dev_mod']] <- as.integer(args_line[[8]])
  args_dir[['dev_models_tau']] <- args_line[[10]]
  args_dir[['dev_stanModelFile']] <- args_line[[12]]
  args_dir[['dev_job_tag']] <- args_line[[14]]
  args_dir[['overwrite']] <- as.integer(args_line[[16]])
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
  assign(paste0("outfile.base", args_dir$dev_models_tau[i]), paste0(args_dir$out_dir, "/", args_dir$dev_stanModelFile[i] , "-", args_dir$dev_job_tag[i], "/",
                                                                    args_dir$dev_stanModelFile[i] , "-", args_dir$dev_job_tag[i]) )
}

# load inputs for this script
# mobility extrapolated for 0-14 model
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
file <- paste0(outfile.base,'-stanout-impact_intv-gqs.RDS')
cat("\n read RDS:", file)
plot.pars.intv <- readRDS(file)

# zhang contact matrix 
plot.pars.intv.dev = vector('list',args_dir$num_dev_mod) 
plot.pars.basic.dev = vector('list',args_dir$num_dev_mod)
for(i in args_dir$dev_models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-stanout-basic.RDS')
  cat("\n read RDS:", file)
  plot.pars.basic.dev[[i]] = readRDS(file)
  file <- paste0(get(paste0("outfile.base", i)),'-stanout-impact_intv-gqs.RDS')
  cat("\n read RDS:", file)
  plot.pars.intv.dev[[i]] <- readRDS(file)
}

# defined model names
model_names = c("extrapolated_mobility_0-14", args_dir$dev_models_tau)

# states selected
figure.select.loc = "CA"

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)
apply_eta_to_index_and_cnt <- as.logical( plot.pars.basic$stan_data$with_eta2 )


#
# Summarise contact matrix
file <- paste0(outfile.base,'-contact_patterns_over_time.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
  cat(" \n -------------------------------- \n summarise contact matrices: start \n -------------------------------- \n")
  tryCatch({
    cnts <- make_contact_patterns_over_time_summaries(plot.pars.basic$pop_info,
                                                      plot.pars.basic$stan_data,
                                                      plot.pars.intv,
                                                      plot.pars.basic$regions,
                                                      plot.pars.basic$dates,
                                                      apply_eta_to_index_and_cnt, 
                                                      plot.pars.basic$with_contact_intensities_zhang,
                                                      plot.pars.basic$mobility_data)
    cat("\nWrite ",file," ... ")
    saveRDS(cnts, file=file)		
  })
  cat(" \n -------------------------------- \n summarise contact matrices: end \n -------------------------------- \n")
}
if(file.exists(file))
{
  cnts <- readRDS(file)
}
#dev models
cnts.dev = vector('list',args_dir$num_dev_mod) 
j = 1
for(i in args_dir$dev_models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-contact_patterns_over_time.rds')
  if(!file.exists(file) | args_dir[['overwrite']])
  {	
    cat(" \n -------------------------------- \n summarise contact matrices: start \n -------------------------------- \n")
    cnts.dev[[j]] <- make_contact_patterns_over_time_summaries(plot.pars.basic.dev[[i]]$pop_info,
                                                               plot.pars.basic.dev[[i]]$stan_data,
                                                               plot.pars.intv.dev[[i]],
                                                               plot.pars.basic.dev[[i]]$regions,
                                                               plot.pars.basic.dev[[i]]$dates,
                                                               apply_eta_to_index_and_cnt, 
                                                               plot.pars.basic.dev[[i]]$with_contact_intensities_zhang,
                                                               plot.pars.basic.dev[[i]]$mobility_data)
    

    cat("\nWrite ",file," ... ")
    saveRDS(cnts.dev[[j]], file=file)
    j = j + 1
    cat(" \n -------------------------------- \n summarise contact matrices: end \n -------------------------------- \n")
  }
  if(file.exists(file))
  {
    cnts.dev[[j]] <- readRDS(file)
    j = j + 1
  }
}

cnts.all = c(list(cnts), cnts.dev)

cat(" \n -------------------------------- \n plot contact matrices: start \n -------------------------------- \n")
tryCatch({
  plot_contact_patterns_sensitivity_contacts_0_14(cnts.all, plot.pars.basic.dev[["1"]]$dates, plot.pars.basic.dev[["1"]]$stan_data, 
                                                  figure.select.loc, outfile.base1, model_names)
 })
cat(" \n -------------------------------- \n plot contact matrices: end \n -------------------------------- \n")


cat(" \n -------------------------------- \n \n End post-processing-sensititivy-contacts-0-14-contact_patterns.r \n \n -------------------------------- \n")
