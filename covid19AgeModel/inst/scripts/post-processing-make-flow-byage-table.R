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
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'covid19AgeModel_report32_cmdstanv'
  args_dir[['out_dir']] <- '/Users/melodiemonod/short_run_test/covid19AgeModel_report32_cmdstanv-37states_tau10_report32'
  args_dir[['job_tag']] <- '37states_tau10_report32'
  args_dir[['overwrite']] <- 0
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

# find max date
max_date_analaysis = max(as.Date(sapply(plot.pars.basic$dates, function(x) as.character(max(x)))))

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
                                                        NULL,
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

#
#	summarise if cum prop flow by age for Oct
file <- paste0(outfile.base,'-summary-cum-prop-flow-age-oct.RDS')
if(!file.exists(file) & max_date_analaysis > as.Date("2020-10-01") | args_dir[['overwrite']] & max_date_analaysis > as.Date("2020-10-01"))
{	
  if(is.null(flows_gqs)){
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
  cumpropflow_byage_c.oct <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                        plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                        age_cat_map, 
                                                        "2020-10-01",
                                                        plot.pars.basic$pop_info, 
                                                        plot.pars.basic$dates, 
                                                        plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(cumpropflow_byage_c.oct, file=file)		
}
if(file.exists(file))
{
  cumpropflow_byage_c.oct <- readRDS(file)
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

#
#	find last common date
n.locs <- length(plot.pars.basic$regions)
tmp <- totalflow_byage_c[, list( ALL_LOC=length(unique(loc))==(n.locs+with_national )), by='date'] # +1 for US: national 
tmp <- subset(tmp, ALL_LOC)
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic$dates, function(x) any(x==date)))), by='date']
tmp <- subset(tmp, ALL_NON_FORECAST)
last.common.date <- max(tmp$date)
cat("\nlast common date is", as.character(last.common.date))

cat(" \n -------------------------------- \n \n plot-contributions-by-state-age-composition \n \n -------------------------------- \n")

# % population in age group c
tmp <- subset(plot.pars.basic$pop_info, loc %in% plot.pars.basic$regions, select=c(age.cat, pop,loc,loc_label))
tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat, age.cat2)), by='age.cat')
tmp <- tmp[, list(pop= sum(pop)), by=c('age.cat2','loc')]
tmp <- tmp[, list(age_cat=age.cat2,prop_pop= pop/sum(pop)), by=c('loc')]
tmp <- merge(tmp,unique(subset(plot.pars.basic$pop_info,select=c('loc','loc_label'))),by='loc')

# merge with contributions by age
data <- subset(cumpropflow_byage_c, date==last.common.date)
data <- subset(data,age_band!='Overall' & loc!='US')
data <- merge(data,tmp,by=c('age_cat','loc_label','loc'))

ggplot() +  
  geom_bar(data=data,aes(x=age_band, y=M,fill="Cumulated contribution"), stat='identity',alpha=0.7) +
  geom_bar(data=data,aes(x=age_band, y=prop_pop,fill='Proportion of population'), stat='identity',size=1.1,alpha=0,col="black") +
  geom_errorbar(data=data,aes(x=age_band, ymin = CL, ymax = CU), width=.2,
                position=position_dodge(0.7), col='black',show.legend=FALSE) +
  scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="",accuracy=1)) + coord_cartesian(ylim=c(0, 0.5)) +
  labs(x="",y = "Percent of total population") +
  facet_wrap(loc_label~., ncol=6) +
  theme_bw(base_size=30) + 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=50,hjust=1),
        legend.text = element_text(vjust=-1),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(name="",values = c("steelblue3", "black"),labels=c("Proportion of SARS-Cov-2 infections that originated from age group in October 2020","Proportion of age group in population"))
ggsave(file=paste0(outfile.base,'-contributions_agecomposition.png'),height=29,width=21)

if(max_date_analaysis > as.Date('2020-10-01')){
  data <- subset(cumpropflow_byage_c.oct, date==last.common.date)
  data <- subset(data,age_band!='Overall' & loc!='US')
  data <- merge(data,tmp,by=c('age_cat','loc_label','loc'))
  
  ggplot() +  
    geom_bar(data=data,aes(x=age_band, y=M,fill="Cumulated contribution"), stat='identity',alpha=0.7) +
    geom_bar(data=data,aes(x=age_band, y=prop_pop,fill='Proportion of population'), stat='identity',size=1.1,alpha=0,col="black") +
    geom_errorbar(data=data,aes(x=age_band, ymin = CL, ymax = CU), width=.2,
                  position=position_dodge(0.7), col='black',show.legend=FALSE) +
    scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="",accuracy=1)) + coord_cartesian(ylim=c(0, 0.5)) +
    labs(x="",y = "Percent of total population") +
    facet_wrap(loc_label~., ncol=6) +
    theme_bw(base_size=30) + 
    theme(legend.position="bottom",
          axis.text.x=element_text(angle=50,hjust=1),
          legend.text = element_text(vjust=-1),
          strip.background = element_blank(),
          panel.grid.major.x = element_blank()) +
    scale_fill_manual(name="",values = c("steelblue3", "black"),labels=c("Proportion of SARS-Cov-2 infections that originated from age group in October 2020","Proportion of age group in population"))
  ggsave(file=paste0(outfile.base,'-contributions_agecomposition_oct.png'),height=29,width=21)
  
}


cat(" \n -------------------------------- \n make flow by age table after last date: start \n -------------------------------- \n")

#
#	make table total flow by age
totalflow_byage_c.ld <- subset(totalflow_byage_c, date==last.common.date)
totalflow_byage_c.ld = rbind(totalflow_byage_c.ld,
                             totalflow_byage_c.ld[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
totalflow_byage_c.ld[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
totalflow_byage_c.ld <- dcast.data.table(totalflow_byage_c.ld, loc_label~age_band, value.var='L')
totalflow_byage_c_US <- subset(totalflow_byage_c.ld, loc_label == "United-States") # national level
totalflow_byage_c.ld <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), totalflow_byage_c.ld, by='loc_label',all.x=TRUE)
for(x in colnames(totalflow_byage_c.ld))
{
  set(totalflow_byage_c.ld, which(is.na(totalflow_byage_c.ld[[x]])), x, '-') 
}
totalflow_byage_c.ld <- rbind(totalflow_byage_c_US, totalflow_byage_c.ld)
totalflow_byage_c.ld <- cbind(totalflow_byage_c.ld[, "loc_label"], totalflow_byage_c.ld[, "Overall"], totalflow_byage_c.ld[, -c("Overall", "loc_label", '20-49')], totalflow_byage_c.ld[, "20-49"])


#
#	make table prop flow by age
propflow_byage_c.ld <- subset(propflow_byage_c, date==last.common.date)
propflow_byage_c.ld = rbind(propflow_byage_c.ld,
                            propflow_byage_c.ld[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
propflow_byage_c.ld[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
propflow_byage_c.ld <- dcast.data.table(propflow_byage_c.ld, loc_label~age_band, value.var='L')
propflow_byage_c_US <- subset(propflow_byage_c.ld, loc_label == "United-States") # national level
propflow_byage_c.ld <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), propflow_byage_c.ld, by='loc_label',all.x=TRUE)
for(x in colnames(propflow_byage_c.ld))
{
  set(propflow_byage_c.ld, which(is.na(propflow_byage_c.ld[[x]])), x, '-') 
}
propflow_byage_c.ld <- rbind(propflow_byage_c_US, propflow_byage_c.ld)
propflow_byage_c.ld <- cbind(propflow_byage_c.ld[, "loc_label"], propflow_byage_c.ld[, "Overall"], propflow_byage_c.ld[, -c("Overall", "loc_label", '20-49')], propflow_byage_c.ld[, "20-49"])

#
#	make table cum prop flow by age
cumpropflow_byage_c.ld <- subset(cumpropflow_byage_c, date==last.common.date)
cumpropflow_byage_c.ld = rbind(cumpropflow_byage_c.ld,
                            cumpropflow_byage_c.ld[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
cumpropflow_byage_c.ld[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
cumpropflow_byage_c.ld <- dcast.data.table(cumpropflow_byage_c.ld, loc_label~age_band, value.var='L')
propflow_byage_c_US <- subset(cumpropflow_byage_c.ld, loc_label == "United-States") # national level
cumpropflow_byage_c.ld <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_c.ld, by='loc_label',all.x=TRUE)
for(x in colnames(cumpropflow_byage_c.ld))
{
  set(cumpropflow_byage_c.ld, which(is.na(cumpropflow_byage_c.ld[[x]])), x, '-') 
}
cumpropflow_byage_c.ld <- rbind(propflow_byage_c_US, cumpropflow_byage_c.ld)
cumpropflow_byage_c.ld <- cbind(cumpropflow_byage_c.ld[, "loc_label"], cumpropflow_byage_c.ld[, "Overall"], cumpropflow_byage_c.ld[, -c("Overall", "loc_label", "20-49")], cumpropflow_byage_c.ld[, "20-49"])

cat(" \n -------------------------------- \n make flow by age table after last date: end \n -------------------------------- \n")



cat(" \n -------------------------------- \n make flow by age table before school reopening: start \n -------------------------------- \n")

last.day.before.school.reopening = as.Date('2020-08-23')
cumpropflow_byage_c.sr = subset(cumpropflow_byage_c, date <= last.day.before.school.reopening)
max_date = cumpropflow_byage_c.sr[, list(max_date = max(date)), by = "loc"]
last.day.before.school.reopening = max(max_date$max_date)

#
#	make table total flow by age
totalflow_byage_c.sr <- subset(totalflow_byage_c, date==last.day.before.school.reopening)
totalflow_byage_c.sr = rbind(totalflow_byage_c.sr,
                             totalflow_byage_c.sr[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
totalflow_byage_c.sr[, L:= paste0(sprintf("%.2f", M),' [',sprintf("%.2f", CL),'-',sprintf("%.2f", CU),']')]
totalflow_byage_c.sr <- dcast.data.table(totalflow_byage_c.sr, loc_label~age_band, value.var='L')
totalflow_byage_c_US <- subset(totalflow_byage_c.sr, loc_label == "United-States") # national level
totalflow_byage_c.sr <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), totalflow_byage_c.sr, by='loc_label',all.x=TRUE)
for(x in colnames(totalflow_byage_c.sr))
{
  set(totalflow_byage_c.sr, which(is.na(totalflow_byage_c.sr[[x]])), x, '-') 
}
totalflow_byage_c.sr <- rbind(totalflow_byage_c_US, totalflow_byage_c.sr)
totalflow_byage_c.sr <- cbind(totalflow_byage_c.sr[, "loc_label"], totalflow_byage_c.sr[, "Overall"], totalflow_byage_c.sr[, -c("Overall", "loc_label", '20-49')], totalflow_byage_c.sr[, "20-49"])

#
#	make table prop flow by age
propflow_byage_c.sr <- subset(propflow_byage_c, date==last.day.before.school.reopening)
propflow_byage_c.sr = rbind(propflow_byage_c.sr,
                            propflow_byage_c.sr[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
propflow_byage_c.sr[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
propflow_byage_c.sr <- dcast.data.table(propflow_byage_c.sr, loc_label~age_band, value.var='L')
propflow_byage_c_US <- subset(propflow_byage_c.sr, loc_label == "United-States") # national level
propflow_byage_c.sr <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), propflow_byage_c.sr, by='loc_label',all.x=TRUE)
for(x in colnames(propflow_byage_c.sr))
{
  set(propflow_byage_c.sr, which(is.na(propflow_byage_c.sr[[x]])), x, '-') 
}
propflow_byage_c.sr <- rbind(propflow_byage_c_US, propflow_byage_c.sr)
propflow_byage_c.sr <- cbind(propflow_byage_c.sr[, "loc_label"], propflow_byage_c.sr[, "Overall"], propflow_byage_c.sr[, -c("Overall", "loc_label", '20-49')], propflow_byage_c.sr[, "20-49"])

#
#	make table cum prop flow by age
cumpropflow_byage_c.sr <- subset(cumpropflow_byage_c, date==last.day.before.school.reopening)
cumpropflow_byage_c.sr = rbind(cumpropflow_byage_c.sr,
                               cumpropflow_byage_c.sr[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
cumpropflow_byage_c.sr[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
cumpropflow_byage_c.sr <- dcast.data.table(cumpropflow_byage_c.sr, loc_label~age_band, value.var='L')
propflow_byage_c_US <- subset(cumpropflow_byage_c.sr, loc_label == "United-States") # national level
cumpropflow_byage_c.sr <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_c.sr, by='loc_label',all.x=TRUE)
for(x in colnames(cumpropflow_byage_c.sr))
{
  set(cumpropflow_byage_c.sr, which(is.na(cumpropflow_byage_c.sr[[x]])), x, '-') 
}
cumpropflow_byage_c.sr <- rbind(propflow_byage_c_US, cumpropflow_byage_c.sr)
cumpropflow_byage_c.sr <- cbind(cumpropflow_byage_c.sr[, "loc_label"], cumpropflow_byage_c.sr[, "Overall"], cumpropflow_byage_c.sr[, -c("Overall", "loc_label", "20-49")], cumpropflow_byage_c.sr[, "20-49"])

## plot cum prop flow for 20-49 year olds
# % population in age group c
tmp <- subset(plot.pars.basic$pop_info, loc %in% plot.pars.basic$regions, select=c(age.cat, pop,loc,loc_label))
tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat, age.cat2)), by='age.cat')
tmp <- tmp[, list(pop= sum(pop)), by=c('age.cat2','loc')]
tmp <- tmp[, list(age_cat=age.cat2,prop_pop= pop/sum(pop)), by=c('loc')]
tmp <- merge(tmp,unique(subset(plot.pars.basic$pop_info,select=c('loc','loc_label'))),by='loc')

data <- subset(cumpropflow_byage_c, date==last.day.before.school.reopening)
data <- subset(data,age_band!='Overall' & loc!='US')
data <- merge(data,tmp,by=c('age_cat','loc_label','loc'))
data[,ratio:= M/prop_pop]

ggplot(subset(data,age_band %in% c("20-34","35-49"))) +  
  geom_bar(aes(x=loc_label, y=M,fill="Cumulated contribution"), stat='identity',alpha=0.7) +
  geom_bar(aes(x=loc_label, y=prop_pop,fill='Proportion of population'), stat='identity',size=1.1,alpha=0,col="black") +
  scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="",accuracy=1)) +
  labs(x="",y = "") +
  facet_wrap(~age_band, ncol=1,scales="free_y") +
  theme_bw(base_size=30) + 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=50,hjust=1),
        legend.text = element_text(vjust=-1),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(name="",values = c("steelblue3", "black"),labels=c("Proportion of SARS-Cov-2 infections that originated from age group until Aug 23rd 2020","Proportion of age group in population"))
ggsave(file=paste0(outfile.base,'-contributions_agecomposition_aug_20-49.png'),height=10,width=21)

ggplot(subset(data,age_band %in% c("20-34","35-49"))) +  
  geom_bar(aes(x=loc_label, y=ratio), stat='identity',fill="steelblue",alpha=0.7) +
  scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="",accuracy=1)) +
  labs(x="",y = "Proportion of infections from age group / \nproportion of population in age group") +
  facet_wrap(~age_band, ncol=1,scales="free_y") +
  theme_bw(base_size=30) + 
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=50,hjust=1),
        legend.text = element_text(vjust=-1),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(name="",values = c("steelblue3"),labels=c("Proportion of SARS-Cov-2 infections originating from age group until Aug 23rd 2020 relative to proportion of population in age group"))
ggsave(file=paste0(outfile.base,'-contributions_population_ratio_aug_20-49.png'),height=12,width=21)

cat(" \n -------------------------------- \n make flow by age table before school reopening: end \n -------------------------------- \n")

cat(" \n -------------------------------- \n make flow by age table after school reopening: start \n -------------------------------- \n")


#
#	make table cum prop flow by age october 
if(max_date_analaysis > as.Date('2020-10-01')){
  fromdate <- min(cumpropflow_byage_c.oct$date)
  cumpropflow_byage_c.oct <- subset(cumpropflow_byage_c.oct, date==last.common.date)
  cumpropflow_byage_c.oct = rbind(cumpropflow_byage_c.oct,
                                  cumpropflow_byage_c.oct[age_cat %in% 3:4, list( M = sum(M), CU = sum(CU), CL = sum(CL), age_band = "20-49", age_cat = 8), by = c("date", "loc", 'loc_label')] )
  cumpropflow_byage_c.oct[, L:= paste0(paste0(sprintf("%.1f", M*100), '\\%'),' [',paste0(sprintf("%.1f", CL*100), '\\%'),'-', paste0(sprintf("%.1f", CU*100), '\\%'),']')]
  cumpropflow_byage_c.oct <- dcast.data.table(cumpropflow_byage_c.oct, loc_label~age_band, value.var='L')
  
  cumpropflow_byage_c_US <- subset(cumpropflow_byage_c.oct, loc_label == "United-States") # national level
  cumpropflow_byage_c.oct <- merge( unique(subset(plot.pars.basic$pop_info, select=loc_label)), cumpropflow_byage_c.oct, by='loc_label',all.x=TRUE)
  for(x in colnames(cumpropflow_byage_c.oct))
  {
    set(cumpropflow_byage_c.oct, which(is.na(cumpropflow_byage_c.oct[[x]])), x, '-') 
  }
  cumpropflow_byage_c.oct <- rbind(cumpropflow_byage_c_US, cumpropflow_byage_c.oct)
  cumpropflow_byage_c.oct <- cbind(cumpropflow_byage_c.oct[, "loc_label"], cumpropflow_byage_c.oct[, "Overall"], cumpropflow_byage_c.oct[, -c("Overall", "loc_label", "20-49")], cumpropflow_byage_c.oct[, "20-49"])
}

cat(" \n -------------------------------- \n make flow by age table after school reopening: end \n -------------------------------- \n")

#
#	save

ans <- list(totalflow_byage_c.ld, propflow_byage_c.ld, cumpropflow_byage_c.ld, format(last.common.date,  "%B %d, %Y") )
file <- paste0(outfile.base,'-flow-onward-from-age-tables-lastdate.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

ans <- list(totalflow_byage_c.sr, propflow_byage_c.sr, cumpropflow_byage_c.sr, format(last.day.before.school.reopening,  "%B %d, %Y") )
file <- paste0(outfile.base,'-flow-onward-from-age-tables-beforeschoolreopening.rds')
cat("\nWrite table to",file)
saveRDS(ans, file = file, version = 2)

if(max_date_analaysis > as.Date('2020-10-01')){
  ans <- list(cumpropflow_byage_c.oct, format(fromdate,  "%B %d, %Y"),format(last.common.date,  "%B %d, %Y") )
  file <- paste0(outfile.base,'-flow-onward-from-age-tables-oct.rds')
  cat("\nWrite table to",file)
  saveRDS(ans, file = file, version = 2)
}

cat(" \n -------------------------------- \n \n completed post-processing-make-flow-byage-table.R \n \n -------------------------------- \n")
