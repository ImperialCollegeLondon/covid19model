# post-processing-make-flow-byage-table.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-flow-byage-table.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
  args_dir[['out_dir']] <- '/rdsgpfs/general/user/ablenkin/home/covid/base_age_fsq_mobility_200703f_cmdstanv-4states_lifr_eta2_devcntct_dataJ29_test2'
  args_dir[['job_tag']] <- '4states_lifr_eta2_devcntct_dataJ29_test2'
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

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# set flows_gqs to NULL
flows_gqs <- NULL

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

cat(" \n -------------------------------- \n summarise flow by age samples: start \n -------------------------------- \n")

#
#	summarise flow by age
file <- paste0(outfile.base,'-summary-total-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
  cat("\n read RDS:", file2)
  flows_gqs <- readRDS(file2)
  
  cat("\n ----------- summarise_total_flow_byage_c ----------- \n")
  totalflow_byage_c <- summarise_total_flow_byage_c(flows_gqs$reduced_flows, 
                                                    plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                    age_cat_map, 
                                                    plot.pars.basic$pop_info, 
                                                    plot.pars.basic$dates, 
                                                    plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(totalflow_byage_c, file=file)
  
}
if(file.exists(file))
{
  totalflow_byage_c <- readRDS(file)
}

#
#	summarise if prop flow by age
file <- paste0(outfile.base,'-summary-prop-flow-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_prop_flow_byage_c ----------- \n")
  propflow_byage_c <- summarise_prop_flow_byage_c(flows_gqs$reduced_flows, 
                                                  plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                  age_cat_map, 
                                                  plot.pars.basic$pop_info, 
                                                  plot.pars.basic$dates, 
                                                  plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(propflow_byage_c, file=file)		
}
if(file.exists(file))
{
  propflow_byage_c <- readRDS(file)
}

#
#	summarise if cum prop flow by age
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


flows_gqs <- NULL
gc()

# do the summaries include the national average
with_national <- 0
if(nrow(subset(totalflow_byage_c, loc == "US")) > 0 & nrow(subset(propflow_byage_c, loc == "US")) & nrow(subset(cumpropflow_byage_c, loc == "US")) > 0)
{
  with_national <- 1
}

cat(" \n -------------------------------- \n summarise flow by age samples: end \n -------------------------------- \n")

cat(" \n -------------------------------- \n make flow by age table: start \n -------------------------------- \n")


#
#	find last common date
n.locs <- length(plot.pars.basic$regions)
tmp <- totalflow_byage_c[, list( ALL_LOC=length(unique(loc))==(n.locs+with_national )), by='date'] # +1 for US: national 
tmp <- subset(tmp, ALL_LOC)
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic$dates, function(x) any(x==date)))), by='date']
tmp <- subset(tmp, ALL_NON_FORECAST)
last.common.date <- max(tmp$date)
cat("\nlast common date is", as.character(last.common.date))

#
#	make table total flow by age
totalflow_byage_c <- subset(totalflow_byage_c, date==last.common.date)
totalflow_byage_c[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
totalflow_byage_c <- dcast.data.table(totalflow_byage_c, loc_label~age_band, value.var='L')

totalflow_byage_c_US <- subset(totalflow_byage_c, loc_label == "United-States") # national level
totalflow_byage_c <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), totalflow_byage_c, by='loc_label',all.x=TRUE)
for(x in colnames(totalflow_byage_c))
{
  set(totalflow_byage_c, which(is.na(totalflow_byage_c[[x]])), x, '-') 
}
totalflow_byage_c <- rbind(totalflow_byage_c_US, totalflow_byage_c)
totalflow_byage_c <- cbind(totalflow_byage_c[, "loc_label"], totalflow_byage_c[, "Overall"], totalflow_byage_c[, -c("Overall", "loc_label")])


#
#	make table prop flow by age
propflow_byage_c <- subset(propflow_byage_c, date==last.common.date)
propflow_byage_c[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
propflow_byage_c <- dcast.data.table(propflow_byage_c, loc_label~age_band, value.var='L')

propflow_byage_c_US <- subset(propflow_byage_c, loc_label == "United-States") # national level
propflow_byage_c <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), propflow_byage_c, by='loc_label',all.x=TRUE)
for(x in colnames(propflow_byage_c))
{
  set(propflow_byage_c, which(is.na(propflow_byage_c[[x]])), x, '-') 
}
propflow_byage_c <- rbind(propflow_byage_c_US, propflow_byage_c)
propflow_byage_c <- cbind(propflow_byage_c[, "loc_label"], propflow_byage_c[, "Overall"], propflow_byage_c[, -c("Overall", "loc_label")])

#
#	make table cum prop flow by age
cumpropflow_byage_c <- subset(cumpropflow_byage_c, date==last.common.date)
cumpropflow_byage_c[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
cumpropflow_byage_c <- dcast.data.table(cumpropflow_byage_c, loc_label~age_band, value.var='L')

propflow_byage_c_US <- subset(cumpropflow_byage_c, loc_label == "United-States") # national level
cumpropflow_byage_c <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_c, by='loc_label',all.x=TRUE)
for(x in colnames(cumpropflow_byage_c))
{
  set(cumpropflow_byage_c, which(is.na(cumpropflow_byage_c[[x]])), x, '-') 
}
cumpropflow_byage_c <- rbind(propflow_byage_c_US, cumpropflow_byage_c)
cumpropflow_byage_c <- cbind(cumpropflow_byage_c[, "loc_label"], cumpropflow_byage_c[, "Overall"], cumpropflow_byage_c[, -c("Overall", "loc_label")])


#
#	save
ans <- list(totalflow_byage_c, propflow_byage_c, cumpropflow_byage_c, format(last.common.date,  "%B %d, %Y") )
file <- paste0(outfile.base,'-flow-onward-from-age-tables-lastdate.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

cat(" \n -------------------------------- \n \n completed post-processing-make-flow-byage-table.R \n \n -------------------------------- \n")
