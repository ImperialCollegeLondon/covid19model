# post-processing-make-attackrate-table.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-attackrate-table.R \n \n -------------------------------- \n")

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

pkg.dir <- system.file(package = "covid19AgeModel" )

#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201204b_cmdstanv'
  args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201204b_cmdstanv-4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'
  args_dir[['job_tag']] <- '4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'	
}
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201204b_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201204b_cmdstanv-4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'
  args_dir[['job_tag']] <- '4states_Oct29_ifrdecay20_ifrdecaystartJune_Levinv7'	
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

file = paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
cat("\n read RDS:", file)
casesByAge <- readRDS(file)

# set E_casesByAge to NULL
E_casesByAge <- NULL

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

#	summarise cases by age
file <- paste0(outfile.base,'-summary-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_e_acases_byage_c ----------- \n")
  e_acases_byage_c <- summarise_e_acases_byage_c(E_casesByAge,
                                                 age_cat_map, 
                                                 plot.pars.basic$pop_info, 
                                                 plot.pars.basic$dates, 
                                                 plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_acases_byage_c, file=file)
}
if(file.exists(file))
{
  e_acases_byage_c <- readRDS(file)
}

#	summarise cases
file <- paste0(outfile.base,'-summary-cases.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_casesByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_casesByAge <- readRDS(file2)
  }
  
  cat("\n ----------- make_casesoverall_summaries ----------- \n")
  e_acases_overall_c <- make_casesoverall_c_summaries(E_casesByAge,
                                                      plot.pars.basic$dates, 
                                                      plot.pars.basic$regions,
                                                      plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(e_acases_overall_c, file=file)
}
if(file.exists(file))
{
  e_acases_overall_c <- readRDS(file)
}

E_casesByAge<- NULL

cat("\n ----------- summarise_attackrate_byage_c ----------- \n")
attackrate_byage_c <- summarise_attackrate_byage_c(e_acases_byage_c, age_cat_map,
                                                   plot.pars.basic$pop_info, plot.pars.basic$regions)

cat("\n ----------- summarise_attackrate_overall_c ----------- \n")
attackrate_overall_c <- summarise_attackrate_overall_c(e_acases_overall_c, 
                                                       plot.pars.basic$pop_info, plot.pars.basic$regions)

cat("\n ----------- make attack rate table ----------- \n")
attackrate <- make_attack_rate_table(attackrate_byage_c,attackrate_overall_c,plot.pars.basic$pop_info,plot.pars.basic$dates,outfile.base)

cat("\n ----------- plot attack rate by state ----------- \n")
last.common.date <- as.Date(attackrate[[2]],format="%B %d, %Y")
attackrate <- attackrate_overall_c[attackrate_overall_c$date==last.common.date]
g <-  ggplot(attackrate) +
  geom_bar(aes(x=loc, y=M), stat='identity', fill=viridis_pal(alpha=1,begin=0.3)(1)) +
  geom_errorbar(aes(x=loc,ymin=CL, ymax=CU), stat='identity',width=.2,
                position=position_dodge(.9)) +
  labs(x='',y='Overall attack rate') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none") 
ggsave(paste0(outfile.base,'-attackrateoverall-bystate', '.png'), g, w = 21, h=5)

cat("\n ----------- make_attackrate_school_summaries ----------- \n")
last_date = max(plot.pars.basic$deathByAge$date)
file = file.path(pkg.dir, "data", "school_cases.csv")
observed_attackrate_school <- data.table(read.csv(file))
observed_attackrate_school = observed_attackrate_school[,list(number_students=sum(number_students,na.rm = T),
                                 cumulative_students=sum(cumulative_students,na.rm = T)),
                           by=c('loc','start','date')]
observed_attackrate_school[,attackrate:=cumulative_students/number_students]
observed_attackrate_school = observed_attackrate_school[as.Date(date)<=last_date,]
observed_attackrate_school = observed_attackrate_school[, .SD[which.max(date)], by='loc']
setnames(observed_attackrate_school, c('start','date'),c('duration_start','duration_end'))
# observed_attackrate_school <- data.table(loc=c('FL','TX'),
#                                          duration_start = c('2020-09-06','2020-08-24'),
#                                          duration_end = c('2020-10-24','2020-10-25'),
#                                          attackrate = c(paste0(round(0.3034215,4),'%'),paste0(round(0.3375885,4),'%')))
observed_attackrate_school = subset(observed_attackrate_school, loc %in% plot.pars.basic$regions)
attackrate_school <- make_attackrate_school_summaries(casesByAge, plot.pars.basic$dates,plot.pars.basic$regions,
                                                      observed_attackrate_school,
                                                       plot.pars.basic$pop_info)
attackrate_school <- cbind(observed_attackrate_school, attackrate_school)

file <- paste0(outfile.base,'-summary-attackrate-school.RDS')
cat("\nWrite ",file," ... ")
saveRDS(attackrate_school, file=file)

cat(" \n -------------------------------- \n \n completed post-processing-make-attackrate-table.R \n \n -------------------------------- \n")
