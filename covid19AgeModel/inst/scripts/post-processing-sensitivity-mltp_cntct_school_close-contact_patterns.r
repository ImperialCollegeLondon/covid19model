# post-processing-sensititivy-mltp_cntct_school_close-contact_patterns.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-sensititivy-mltp_cntct_school_close-contact_patterns.r \n \n -------------------------------- \n")

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
args_dir[['stanModelFile']] <- "base_age_fsq_mobility_201015f8_cmdstanv,base_age_fsq_mobility_201015f8_cmdstanv,base_age_fsq_mobility_201015f8_cmdstanv"
args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
args_dir[['job_tag']] <- "40states_tau05_Oct29_Levinv7,40states_tau10_Oct29_Levinv7,40states_tau20_Oct29_Levinv7"
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
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['num_mod']] <- as.integer(args_line[[8]])
  args_dir[['models_tau']] <- args_line[[10]]
  args_dir[['overwrite']] <- as.integer(args_line[[12]])
} 

args_dir$models_tau <- strsplit(args_dir$models_tau,',')[[1]]
args_dir$job_tag <- strsplit(args_dir$job_tag,',')[[1]]
args_dir$stanModelFile <- strsplit(args_dir$stanModelFile,',')[[1]]
stopifnot(length(args_dir[['models_tau']]) == args_dir[['num_mod']])


## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

for(i in 1:(args_dir[['num_mod']])){
  assign(paste0("outfile.base", args_dir$models_tau[i]), paste0(args_dir$out_dir, "/", args_dir$stanModelFile[i] , "-", args_dir$job_tag[i], "/",
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

# states selected
figure.select.loc = "CA"

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic[["1"]]$pop_info)
apply_eta_to_index_and_cnt <- as.logical( plot.pars.basic[["1"]]$stan_data$with_eta2 )


#
# Summarise contact matrix
cnts.all = vector('list',args_dir$num_mod) 
j = 1
for(i in args_dir$models_tau){
  file <- paste0(get(paste0("outfile.base", i)),'-contact_patterns_over_time.rds')
  if(!file.exists(file) | args_dir[['overwrite']])
  {	
    file <- paste0(get(paste0("outfile.base", i)),'-stanout-impact_intv-gqs.RDS')
    cat("\n read RDS:", file)
    plot.pars.intv <- readRDS(file)
    
    cat(" \n -------------------------------- \n summarise contact matrices: start \n -------------------------------- \n")
    cnts.all[[j]] <- make_contact_patterns_over_time_summaries(plot.pars.basic[[i]]$pop_info,
                                                               plot.pars.basic[[i]]$stan_data,
                                                               plot.pars.intv,
                                                               plot.pars.basic[[i]]$regions,
                                                               plot.pars.basic[[i]]$dates,
                                                               apply_eta_to_index_and_cnt, 
                                                               plot.pars.basic[[i]]$with_contact_intensities_zhang,
                                                               plot.pars.basic[[i]]$mobility_data)
    

    cat("\nWrite ",file," ... ")
    saveRDS(cnts.all[[j]], file=file)
    j = j + 1
    cat(" \n -------------------------------- \n summarise contact matrices: end \n -------------------------------- \n")
  }
  if(file.exists(file))
  {
    cnts.all[[j]] <- readRDS(file)
    j = j + 1
  }
}


cat(" \n -------------------------------- \n plot contact matrices: start \n -------------------------------- \n")
tryCatch({
  plot_contact_patterns_sensitivity_multiplier_cntct_school_closure(cnts.all, plot.pars.basic[["1"]]$dates, plot.pars.basic[["1"]]$stan_data, 
                                                  figure.select.loc, outfile.base1, model_names)
 })


cat(" \n -------------------------------- \n plot contact matrices: end \n -------------------------------- \n")


cat(" \n -------------------------------- \n \n End post-processing-sensititivy-mltp_cntct_school_close-contact_patterns.r \n \n -------------------------------- \n")
