# post-processing-make-Rt-byage-table.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-Rt-byage-table.R \n \n -------------------------------- \n")

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
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin'
  args_dir[['job_tag']] <- '40states_tau10_Oct29_Levin'
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
  args_dir[['period_length']] <- 7
}

if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin'
  args_dir[['job_tag']] <- '40states_tau10_Oct29_Levin'
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
  args_dir[['period_length']] <- 7
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	stopifnot(args_line[[7]]=='-overwrite')
	stopifnot(args_line[[9]]=='-period_length')
	
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
	args_dir[['overwrite']] <- as.integer(args_line[[8]])
	args_dir[['period_length']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# set RtByAge to NULL
E_effcasesByAge <- NULL
RtByAge <- NULL

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

# Find estimate over a period (weekly, monthly...)
period_length = args_dir$period_length

cat(" \n -------------------------------- \n summarise case by age samples: start \n -------------------------------- \n")

#
#	summarise Rt by age
file <- paste0(outfile.base,'-summary-Rt-age_averageover', period_length, 'days.RDS')
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
	                                                 period_length,
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

#
#	summarise if Rt by age < 1
file <- paste0(outfile.base,'-summary-Rt-age-smaller-one_averageover', period_length, 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_effcasesByAge)){
    file3 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
    cat("\n read RDS:", file3)
    E_effcasesByAge <- readRDS(file3)
  }
  if(is.null(RtByAge)){
    file2 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    RtByAge <- readRDS(file2)
  }
    
	cat("\n ----------- summarise_Rt_instantaneous_byage_less_than_one ----------- \n")
	Rt_byage_less_than_one <- summarise_Rt_instantaneous_byage_less_than_one(E_effcasesByAge,
	                                                                         RtByAge, 
	                                                                         period_length,
	                                                                         threshold = 1,
			                                                                     age_cat_map, 
			                                                                     plot.pars.basic$pop_info, 
			                                                                     plot.pars.basic$dates, 
			                                                                     plot.pars.basic$regions)
	cat("\nWrite ",file," ... ")
	saveRDS(Rt_byage_less_than_one, file=file)		
}
if(file.exists(file))
{
	Rt_byage_less_than_one <- readRDS(file)
}

# do the summaries include the national average
with_national <- 0
if(nrow(subset(Rt_byage_c, loc == "US")) > 0 & nrow(subset(Rt_byage_less_than_one, loc == "US")) > 0)
{
  with_national <- 1
}
  
cat(" \n -------------------------------- \n summarise case by age samples: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n summarise Rt samples: start \n -------------------------------- \n")


#
#	summarise Rt
file <- paste0(outfile.base,'-summary-rt_s_averageover', period_length, 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_effcasesByAge)){
    file3 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
    cat("\n read RDS:", file3)
    E_effcasesByAge <- readRDS(file3)
  }
  if(is.null(RtByAge)){
    file2 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    RtByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_Rt ----------- \n")
  Rt_summaries <- summarise_Rt_instantaneous(E_effcasesByAge,
                                             RtByAge,
                                             period_length,
                                             plot.pars.basic$pop_info, 
                                             plot.pars.basic$dates, 
                                             plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_summaries, file=file)
  gc()
}
if(file.exists(file))
{
  Rt_summaries <- readRDS(file)
}

#
# summary Rt < 1 
file <- paste0(outfile.base,'-summary-rt_s_lessthanone_averageover', period_length, 'days.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  cat("\n ----------- summarise_Rt ----------- \n")
  if(is.null(E_effcasesByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_effcasesByAge <- readRDS(file2)
  }
  if(is.null(RtByAge)){
    file3 <- paste0(outfile.base,'-stanout-RtByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    RtByAge <- readRDS(file3)
  }

  Rt_less_than_one <- summarise_Rt_instantaneous_less_than_one(E_effcasesByAge,
                                                               RtByAge,
                                                               period_length,
                                                               threshold = 1, 
                                                               plot.pars.basic$pop_info, 
                                                               plot.pars.basic$dates, 
                                                               plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(Rt_less_than_one, file=file)
  gc()
}
if(file.exists(file))
{
  Rt_less_than_one <- readRDS(file)
}

E_effcasesByAge <- NULL
RtByAge <- NULL
gc()

cat(" \n -------------------------------- \n summarise Rt samples: end \n -------------------------------- \n")

cat(" \n -------------------------------- \n make Rt by age table last date: start \n -------------------------------- \n")


#
#	find Rt on the last common date
n.locs <- length(plot.pars.basic$regions)
last_day_obs_common = as.Date(min(sapply(sapply(plot.pars.basic$dates, as.character), max))) 
Rt_byage_c.ld = subset(Rt_byage_c, date <= last_day_obs_common - period_length + 1)
Rt_byage_c.ld[, list(max_date = max(date)), by = "loc"]
tmp <- Rt_byage_c.ld[, list( ALL_LOC=length(unique(loc))==(n.locs+with_national )), by='date'] # +1 for US: national 
tmp <- subset(tmp, ALL_LOC)
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic$dates, function(x) any(x==date)))), by='date']
tmp <- subset(tmp, ALL_NON_FORECAST)
last.common.date <- max(tmp$date)
cat("\nlast common date is", as.character(last.common.date))

#	make table Rt by age
Rt_byage_c.ld <- subset(Rt_byage_c.ld, date==last.common.date)
Rt_byage_c.ld[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
Rt_byage_c.ld <- dcast.data.table(Rt_byage_c.ld, loc_label~age_band, value.var='L')

#	make table Rt
Rt_summaries.ld <- subset(Rt_summaries, date==last.common.date)
Rt_summaries.ld[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
Rt_summaries.ld <- subset(Rt_summaries.ld, select=c(loc_label, L))
setnames(Rt_summaries.ld, 'L', 'overall')

#	combine Rt and Rt by age
Rt_summaries.ld <- merge(Rt_summaries.ld, Rt_byage_c.ld, by='loc_label')
Rt_summaries_US <- subset(Rt_summaries.ld, loc_label == "United-States") # national level
Rt_summaries.ld <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), Rt_summaries.ld, by='loc_label',all.x=TRUE)
for(x in colnames(Rt_summaries.ld))
{
	set(Rt_summaries.ld, which(is.na(Rt_summaries.ld[[x]])), x, '-') 
}
Rt_summaries.ld <- rbind(Rt_summaries_US, Rt_summaries.ld)

#	make table Rt by age < 1
Rt_byage_less_than_one.ld <- subset(Rt_byage_less_than_one, date==last.common.date)
Rt_byage_less_than_one.ld[, L:= paste0(sprintf("%.1f", value*100), '\\%')]
Rt_byage_less_than_one.ld <- dcast.data.table(Rt_byage_less_than_one.ld, loc_label~age_band, value.var='L')

#	make table Rt < 1
Rt_less_than_one.ld <- subset(Rt_less_than_one, date==last.common.date)
Rt_less_than_one.ld[, L:= paste0(sprintf("%.1f", value*100), '\\%')]
Rt_less_than_one.ld <- subset(Rt_less_than_one.ld, select=c(loc_label, L))
setnames(Rt_less_than_one.ld, 'L', 'overall')

#	combine Rt and Rt by age < 1
Rt_less_than_one.ld <- merge(Rt_less_than_one.ld, Rt_byage_less_than_one.ld, by='loc_label')
Rt_less_than_one_US <- subset(Rt_less_than_one.ld, loc_label == "United-States") # national level
Rt_less_than_one.ld <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), Rt_less_than_one.ld, by='loc_label',all.x=TRUE)
for(x in colnames(Rt_less_than_one.ld))
{
	set(Rt_less_than_one.ld, which(is.na(Rt_less_than_one.ld[[x]])), x, '-') 
}
Rt_less_than_one.ld <- rbind(Rt_less_than_one_US, Rt_less_than_one.ld)

cat(" \n -------------------------------- \n make Rt by age table last date: end \n -------------------------------- \n")


#
#	find Rt before school reopening
cat(" \n -------------------------------- \n make Rt by age table before school reopening: start \n -------------------------------- \n")

last.day.before.school.reopening = as.Date('2020-08-23')
Rt_byage_c.sr = subset(Rt_byage_c, date <= last.day.before.school.reopening- period_length + 1)
max_date = Rt_byage_c.sr[, list(max_date = max(date)), by = "loc"]
last.day.before.school.reopening = max(max_date$max_date)

#	make table Rt by age
Rt_byage_c.sr <- subset(Rt_byage_c, date==last.day.before.school.reopening)
Rt_byage_c.sr[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
Rt_byage_c.sr <- dcast.data.table(Rt_byage_c.sr, loc_label~age_band, value.var='L')

#	make table Rt
Rt_summaries.sr <- subset(Rt_summaries, date==last.day.before.school.reopening)
Rt_summaries.sr[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
Rt_summaries.sr <- subset(Rt_summaries.sr, select=c(loc_label, L))
setnames(Rt_summaries.sr, 'L', 'overall')

#	combine Rt and Rt by age
Rt_summaries.sr <- merge(Rt_summaries.sr, Rt_byage_c.sr, by='loc_label')
Rt_summaries_US <- subset(Rt_summaries.sr, loc_label == "United-States") # national level
Rt_summaries.sr <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), Rt_summaries.sr, by='loc_label',all.x=TRUE)
for(x in colnames(Rt_summaries.sr))
{
  set(Rt_summaries.sr, which(is.na(Rt_summaries.sr[[x]])), x, '-') 
}
Rt_summaries.sr <- rbind(Rt_summaries_US, Rt_summaries.sr)

#	make table Rt by age < 1
Rt_byage_less_than_one.sr <- subset(Rt_byage_less_than_one, date==last.day.before.school.reopening)
Rt_byage_less_than_one.sr[, L:= paste0(sprintf("%.1f", value*100), '\\%')]
Rt_byage_less_than_one.sr <- dcast.data.table(Rt_byage_less_than_one.sr, loc_label~age_band, value.var='L')

#	make table Rt < 1
Rt_less_than_one.sr <- subset(Rt_less_than_one, date==last.day.before.school.reopening)
Rt_less_than_one.sr[, L:= paste0(sprintf("%.1f", value*100), '\\%')]
Rt_less_than_one.sr <- subset(Rt_less_than_one.sr, select=c(loc_label, L))
setnames(Rt_less_than_one.sr, 'L', 'overall')

#	combine Rt and Rt by age < 1
Rt_less_than_one.sr <- merge(Rt_less_than_one.sr, Rt_byage_less_than_one.sr, by='loc_label')
Rt_less_than_one_US <- subset(Rt_less_than_one.sr, loc_label == "United-States") # national level
Rt_less_than_one.sr <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), Rt_less_than_one.sr, by='loc_label',all.x=TRUE)
for(x in colnames(Rt_less_than_one.sr))
{
  set(Rt_less_than_one.sr, which(is.na(Rt_less_than_one.sr[[x]])), x, '-') 
}
Rt_less_than_one.sr <- rbind(Rt_less_than_one_US, Rt_less_than_one.sr)

cat(" \n -------------------------------- \n make Rt by age table before school reopening: end \n -------------------------------- \n")


#
#	save
ans <- list(Rt_summaries.ld, Rt_less_than_one.ld, format(last.common.date,  "%B %d, %Y"),  format(last.common.date + period_length - 1,  "%B %d, %Y"))
file <- paste0(outfile.base,'-Ralastday_averageover', period_length, 'days.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

ans <- list(Rt_summaries.sr, Rt_less_than_one.sr, format(last.day.before.school.reopening,  "%B %d, %Y"),  format(last.day.before.school.reopening + period_length - 1,  "%B %d, %Y") )
file <- paste0(outfile.base,'-Rabeforeschoolreopening_averageover', period_length, 'days.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

cat(" \n -------------------------------- \n \n completed post-processing-make-Rt-byage-table.R \n \n -------------------------------- \n")
