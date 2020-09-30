# post-processing-sensititivy-ifr-age-prior-attackrate.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-sensititivy-ifr-age-prior-attackrate.R \n \n -------------------------------- \n")

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
  args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200821b4_cmdstanv"
  args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
  args_dir[['job_tag']] <- "37states_Sep2"
  args_dir[['dev_job_tag']] <- "37states_tau1_Levinprior_Sep2"
  args_dir[['overwrite']] <- 0
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-dev_job_tag')
  stopifnot(args_line[[9]]=='-overwrite')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['dev_job_tag']] <- args_line[[8]]
  args_dir[['overwrite']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$dev_job_tag, "/",
                           args_dir$stanModelFile , "-", args_dir$dev_job_tag)

# load inputs for this script
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
file <- paste0(outfile.base.dev,'-summary-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base.dev,'-stanout-E_casesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_casesByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_e_acases_byage_c ----------- \n")
  e_acases_byage_c.dev <- summarise_e_acases_byage_c(E_casesByAge,
                                                 age_cat_map, 
                                                 plot.pars.basic$pop_info, 
                                                 plot.pars.basic$dates, 
                                                 plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_acases_byage_c.dev, file=file)
}
if(file.exists(file))
{
  e_acases_byage_c.dev <- readRDS(file)
}

#	summarise cases
file <- paste0(outfile.base,'-summary-cases.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
    file2 <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_casesByAge <- readRDS(file2)

  
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
file <- paste0(outfile.base.dev,'-summary-cases.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_casesByAge)){
    file2 <- paste0(outfile.base.dev,'-stanout-E_casesByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_casesByAge <- readRDS(file2)
  }
  
  cat("\n ----------- make_casesoverall_summaries ----------- \n")
  e_acases_overall_c.dev <- make_casesoverall_c_summaries(E_casesByAge,
                                                      plot.pars.basic$dates, 
                                                      plot.pars.basic$regions,
                                                      plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(e_acases_overall_c.dev, file=file)
}
if(file.exists(file))
{
  e_acases_overall_c.dev <- readRDS(file)
}

cat("\n ----------- summarise_attackrate_byage_c ----------- \n")
attackrate_byage_c <- summarise_attackrate_byage_c(e_acases_byage_c, age_cat_map,
                                                   plot.pars.basic$pop_info, plot.pars.basic$regions)
attackrate_byage_c[, Model := model_names[1]]
attackrate_byage_c.dev <- summarise_attackrate_byage_c(e_acases_byage_c.dev, age_cat_map,
                                                   plot.pars.basic.dev$pop_info, plot.pars.basic.dev$regions)
attackrate_byage_c.dev[, Model := model_names[2]]
attackrate_byage_c.all = rbind(attackrate_byage_c, attackrate_byage_c.dev)

cat("\n ----------- summarise_attackrate_overall_c ----------- \n")
attackrate_overall_c <- summarise_attackrate_overall_c(e_acases_overall_c, 
                                                       plot.pars.basic$pop_info, plot.pars.basic$regions)
attackrate_overall_c[, Model := model_names[1]]
attackrate_overall_c.dev <- summarise_attackrate_overall_c(e_acases_overall_c.dev, 
                                                       plot.pars.basic.dev$pop_info, plot.pars.basic.dev$regions)
attackrate_overall_c.dev[, Model := model_names[2]]
attackrate_overall_c.all = rbind(attackrate_overall_c, attackrate_overall_c.dev)

# as factor
attackrate_overall_c.all[, Model := factor(Model, levels = model_names)]
attackrate_byage_c.all[, Model := factor(Model, levels = model_names)]


cat("\n ----------- plot ----------- \n")

#
# last day without forecasting
tmp = data.table(dates = unique(attackrate_overall_c.all$date))
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic.dev$dates, function(x) any(x==dates)))), by='dates']
tmp <- subset(tmp, ALL_NON_FORECAST)
last_date <- as.Date(max(tmp$dates))
cat("last date is ", as.character(last_date))

#
# Plot by age
tmp = subset(attackrate_byage_c.all, date == last_date)
ggplot(tmp, aes(x=age_band, y = M)) +
  geom_errorbar(aes(ymin=CU, ymax=CL, group = Model), colour="black", width=.1,position = position_dodge(0.8)) +
  geom_point(aes(color = Model),size=3,fill="white", position = position_dodge(0.8)) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~loc_label, scale = "free", ncol = 5) + 
  theme(legend.position = "bottom",
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x= element_text(size = 16),
        axis.text.y=element_text(size = 15),
        axis.text.x=element_text(size=15, vjust = 0.5, hjust=0.5, angle = 30),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill="white")) +
  xlab("Age band") +
  ylab("") +
  #scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("gold", "black")) +
  scale_y_continuous(labels = scales::percent)
file = paste0(outfile.base,"-sensitivity-ifr-prior","-","attack_rate_age", '.pdf')
cat("Write ", file)
ggsave(file, w=17, h = 20)

#
# Plot overall
tmp = subset(attackrate_overall_c.all, date == last_date)
ggplot(tmp, aes(x=loc_label, y = M)) +
  geom_errorbar(aes(ymin=CU, ymax=CL, group = Model), colour="black", width=.1,position = position_dodge(0.6)) +
  geom_point(aes(color = Model),size=3,fill="white", position = position_dodge(0.6)) +
  coord_flip() +
  theme_bw() +
  #facet_wrap(~Model, labeller = label_wrap_gen(width=28)) + 
  theme(legend.position = "bottom",
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x= element_text(size = 14),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size=12, vjust = 0.5, hjust=0.5, angle = 30),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill="white")) +
  xlab("Age band") +
  ylab("") +
  #scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("gold", "black")) +
  scale_y_continuous(labels = scales::percent)
file = paste0(outfile.base,"-sensitivity-ifr-prior","-","attack_rate_overall", '.pdf')
cat("Write ", file)
ggsave(file, w= 9, h = 12)

file =  paste0(outfile.base,"-sensitivity-ifr-prior","-","attack_rate", '.rds')
saveRDS(format(last_date, "%B %d, %Y"), file, version = 2 )

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-ifr-age-prior-attackrate.R \n \n -------------------------------- \n")
