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

#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200803c2_cmdstanv'
  args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200803c2_cmdstanv-4states_updateifrprior_cap85'
  args_dir[['job_tag']] <- '4states_updateifrprior_cap85'	
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


cat(" \n -------------------------------- \n \n completed post-processing-make-attackrate-table.R \n \n -------------------------------- \n")
