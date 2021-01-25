# post-processing-forecast-plot-deaths.r
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-forecast-plot-deaths.r \n \n -------------------------------- \n")

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
suppressMessages(library(covid19AgeModel, quietly = TRUE))
pkg.dir <- system.file(package = "covid19AgeModel" )

#	for dev purposes: 
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015e8_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015e8_cmdstanv-4states_AZCTFKNYC_Sep20_Levin'
args_dir[['job_tag']] <- '4states_AZCTFKNYC_Sep20_Levin'
args_dir[['overwrite']] = 0
args_dir[['with_forecast']] = 0
args_dir[['multiplier_cntct_school_opening']] = 1
args_dir[['school_level']] = "K5"
args_dir[['shield']]=1
args_dir[['shielded_date']]=as.Date('2020-06-20')



#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-overwrite')
  stopifnot(args_line[[9]]=='-with_forecast')
  stopifnot(args_line[[11]]=='-multiplier_cntct_school_opening') 
  stopifnot(args_line[[13]]=='-school_level')	
  stopifnot(args_line[[15]]=='-shield')	
  stopifnot(args_line[[17]]=='-shielded_date')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.numeric(args_line[[8]])
  args_dir[['with_forecast']] <- as.numeric(args_line[[10]])
  args_dir[['multiplier_cntct_school_opening']] <- as.numeric(args_line[[12]])
  args_dir[['school_level']] <- as.character(args_line[[14]])
  args_dir[['shield']] <- as.character(args_line[[16]])
  args_dir[['shielded_date']] <- as.Date(args_line[[18]])
} 


## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
multiplier =  (args_dir$multiplier_cntct_school_opening)*100

# suffix_sensitivity_school0 = '_sensitivity_school_reopen_0'
# 
# # load inputs for this script
# file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_school0,'.RDS')
# cat("\n read RDS:", file)
# plot.pars.basic <- readRDS(file)
# 
# # add age labels to reduced flows
# if(args_dir$school_level == "K5"){
#   pop_info_2 = add_pop_info_age_school_children(plot.pars.basic$pop_info)
#   age_cat_map <- make_age_cat_map_7_school_chidren(pop_info_2)
# }
# if(args_dir$school_level == "K12"){
#   pop_info_2 = add_pop_info_age_school_children_teen(plot.pars.basic$pop_info)
#   age_cat_map <- make_age_cat_map_7_school_chidren_teen(pop_info_2)
# }

suffix_sensitivity_shield1 = paste0('_sensitivity_shielding_',as.integer(args_dir$shield),
                                    '_school_reopen_1_multiplier_', multiplier,
                                    '_level_', args_dir$school_level)

# E_deathsByAge_school_reopen_0 <- NULL
E_deathsByAge <- NULL
E_deathsByAge_shield_1 <- NULL
E_effcasesByAge <- NULL

cat(" \n -------------------------------- \n  summarise deaths and case samples \n -------------------------------- \n")


# summarise overall deaths
file <- paste0(outfile.base,'-summary-deaths-overall.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_deathsByAge))
  {
    file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_deathsByAge <- readRDS(file2)
  }
  
  cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
  deaths_s <- make_deathsoverall_summaries(E_deathsByAge, 
                                           plot.pars.basic$dates, 
                                           plot.pars.basic$regions, 
                                           plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(deaths_s, file=file)
}
if(file.exists(file))
{
  deaths_s <- readRDS(file)
}
gc()


# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic', suffix_sensitivity_shield1,'.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

file <- paste0(outfile.base,'-summary-deaths-overall',suffix_sensitivity_shield1,'.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_deathsByAge_shield_1))
  {
    file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_shield1,'.RDS')
    cat("\n read RDS:", file2)
    E_deathsByAge_shield_1 <- readRDS(file2)
  }
  
  cat("\n ----------- make_deathsoverall_summaries ----------- \n")	
  deaths_shield_1 <- make_deathsoverall_summaries(E_deathsByAge_shield_1, 
                                                         plot.pars.basic$dates, 
                                                         plot.pars.basic$regions, 
                                                         plot.pars.basic$pop_info)
  cat("\nWrite ",file," ... ")
  saveRDS(deaths_shield_1, file=file)
}
if(file.exists(file))
{
  deaths_shield_1 <- readRDS(file)
}
gc()


cat(" \n -------------------------------- \n  generating standalone plots \n -------------------------------- \n")

cat("\n ----------- plot deaths by age ----------- \n")

#
#	handle if forecast period is not to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
  deaths_shield_1 <- subset(deaths_shield_1, date<=date.max)
  deaths_s <- subset(deaths_s, date<=date.max)
  # deaths_school_reopen_0 <- subset(deaths_school_reopen_0, date<=date.max)
}

#
# make overall deaths plots 
cat(" \n -------------------------------- \n generate overall death plots: start \n -------------------------------- \n")
cd_data <- copy(plot.pars.basic$death_data)
setnames(cd_data, c('state','code'), c('loc_label','loc'))
deaths.plots <- plot_deaths_overall_overtime_forecast_school_reopen_shielding( 
  deaths_shield_1, 
  deaths_s,
  cd_data, 
  plot.pars.basic$dates,
  plot.pars.basic$regions,
  multiplier,
  args_dir$school_level,
  args_dir$shielded_date,
  outfile.base
)

cat(" \n -------------------------------- \n generate overall death plots: end \n -------------------------------- \n")

# #
# # make excess deaths plots 
# cat(" \n -------------------------------- \n generate excess death plots: start \n -------------------------------- \n")
# 
# # load summary of excess deaths forecasts
# file <- paste0(outfile.base,'-deaths-age-summary-forecast', '_multiplier_', multiplier, '_level_', args_dir$school_level, '.rds')
# ex_deaths <- readRDS(file)
# ex_deaths <- subset(ex_deaths, variable%in%c("school_reopen1_reopen0_ratio","school_reopen1_reopen0_diff") & age_band=='Overall' & loc!='AllStates' & date=='2020-11-24')
# 
# # load infectious individuals and summarise for 30-49 year olds
# age_cat_map_mid <- make_age_cat_map_middle(plot.pars.basic$pop_info)
# 
# # get prop in age group 30-49
# pc <- unique(subset(plot.pars.basic$pop_info, select=c(loc,age.cat,pop)))
# pc <- subset(pc, loc %in% plot.pars.basic$regions)
# pc <- merge(pc, age_cat_map_mid, by='age.cat')
# pc <- pc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
# pc <- pc[, list(age.cat2=age.cat2, 
#                 pop=pop,
#                 pop_prop=pop/sum(pop)), 
#          by=c('loc')]
# setnames(pc, 'age.cat2','age_index2')
# 
# cat("\n ----------- summarise_e_acases_eff_for_middle_aged ----------- \n")
# file <- paste0(outfile.base,'-eff-cases-over-population-size-middle-age.RDS')
# if(!file.exists(file) | args_dir[['overwrite']])
# {	
#   file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
#   cat("\n read RDS:", file2)
#   E_effcasesByAge <- readRDS(file2)		
#   
#   e_acases_eff_midage <- summarise_e_acases_eff_midage(E_effcasesByAge,
#                                                        age_cat_map_mid, 
#                                                        pc,
#                                                        plot.pars.basic$pop_info, 
#                                                        plot.pars.basic$dates, 
#                                                        plot.pars.basic$regions)
#   saveRDS(e_acases_eff_midage, file=file)
# }
# if(file.exists(file))
# {
#   e_acases_eff_midage <- readRDS(file)
# }
# 
# 
# #
# #
# ec <- subset(e_acases_eff_midage, date=="2020-08-23" & age_band=='Overall' & variable=='R_effcases', select=-c(variable, date, age_cat, age_band))
# exd <- subset(ex_deaths, variable=='school_reopen1_reopen0_diff', select=-c(variable, date, age_cat, age_band))
# exd <- merge(exd,unique(subset(plot.pars.basic$pop_info, select=c(loc, pop_total))), by='loc')
# exd[, CL:= CL/pop_total*1e6]
# exd[, CU:= CU/pop_total*1e6]
# exd[, M:= M/pop_total*1e6]
# setnames(ec, c('M','CL','CU'), c('M.cases','CL.cases','CU.cases'))
# setnames(exd, c('M','CL','CU'), c('M.deaths','CL.deaths','CU.deaths'))
# exd <- merge(ec, exd,by=c('loc','loc_label'))
# exd[, CL.cases:= CL.cases*1e6]
# exd[, CU.cases:= CU.cases*1e6]
# exd[, M.cases:= M.cases*1e6]
# exd <- exd[order(M.cases),]
# exd$loc_label <- factor(exd$loc_label,levels= unique(exd$loc_label),labels= unique(exd$loc_label))
# 
# g <- ggplot(data= exd) +
#   geom_errorbar(aes(x= M.cases, ymin = CL.deaths, ymax = CU.deaths),width=0.05, alpha=0.2) + 
#   geom_errorbar(aes(y= M.deaths, xmin = CL.cases, xmax = CU.cases),width=0.05, alpha=0.2) +
#   geom_point(aes(x = M.cases, y = M.deaths, col=loc_label), stat='identity',size=1.5) +
#   geom_text(aes(x = M.cases, y = M.deaths, label=loc)) +
#   labs(x= "infectious individuals\nper 1,000,000", y="excess COVID-19 attributable deaths\nper 1,000,000\n(school reopening forecast)",col="") +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14), 
#         legend.position = "bottom",
#         axis.text.y=element_text(size=14),
#         axis.title=element_text(size=24),
#         axis.title.x = element_text(vjust=-0.5),
#         strip.text = element_text(size = 20),
#         strip.background = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.title = element_text(size = 20),
#         legend.text = element_text(size = 12)) +
#   scale_color_viridis_d(aesthetics = c("col"),direction=-1) + 
#   guides(color = guide_legend(nrow=5,bycol=TRUE))
# ggsave(paste0(outfile.base,'-excessdeathsper1e6_casesper1e6_overall', '_multiplier_', multiplier, '_level_', args_dir$school_level,'.png'), g, w = 12, h=12)
# 
# 
# #
# #
# ec <- subset(e_acases_eff_midage, date=="2020-08-23" & age_band=='30-49' & variable=='R_effcases', select=-c(variable, date, age_cat, age_band))
# exd <- subset(ex_deaths, variable=='school_reopen1_reopen0_diff', select=-c(variable, date, age_cat, age_band))
# exd <- merge(exd,unique(subset(plot.pars.basic$pop_info, select=c(loc, pop_total))), by='loc')
# exd[, CL:= CL/pop_total*1e6]
# exd[, CU:= CU/pop_total*1e6]
# exd[, M:= M/pop_total*1e6]
# setnames(ec, c('M','CL','CU'), c('M.cases','CL.cases','CU.cases'))
# setnames(exd, c('M','CL','CU'), c('M.deaths','CL.deaths','CU.deaths'))
# exd <- merge(ec, exd,by=c('loc','loc_label'))
# exd <- exd[order(M.cases),]
# exd$loc_label <- factor(exd$loc_label,levels= unique(exd$loc_label),labels= unique(exd$loc_label))
# g <- ggplot(data= exd) +
#   geom_errorbar(aes(x= M.cases*1e6, ymin = CL.deaths, ymax = CU.deaths),width=0.05, alpha=0.2) + 
#   geom_errorbar(aes(y= M.deaths, xmin = CL.cases*1e6, xmax = CU.cases*1e6),width=0.05, alpha=0.2) +
#   geom_point(aes(x = M.cases*1e6, y = M.deaths, col=loc_label), stat='identity',size=1.5) +
#   geom_text(aes(x = M.cases*1e6, y = M.deaths, label=loc)) +
#   labs(x= "infectious individuals aged 30-49\nper 1,000,000 aged 30-49", y="excess COVID-19 attributable deaths\nper 1,000,000\n(school reopening forecast)",col="") +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14), 
#         legend.position = "bottom",
#         axis.text.y=element_text(size=14),
#         axis.title=element_text(size=24),
#         axis.title.x = element_text(vjust=-0.5),
#         strip.text = element_text(size = 20),
#         strip.background = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.title = element_text(size = 20),
#         legend.text = element_text(size = 12)) +
#   scale_color_viridis_d(aesthetics = c("col"),direction=-1) + 
#   guides(color = guide_legend(nrow=5,bycol=TRUE))
# ggsave(paste0(outfile.base,'-excessdeathsper1e6_casesper1e6_30to49', '_multiplier_', multiplier, '_level_', args_dir$school_level,'.png'), g, w = 12, h=12)
# 
# 
# cat(" \n -------------------------------- \n generate excess death plots: end \n -------------------------------- \n")
# 
# 
# #
# #	load summarised infectious cases by all age groups
# cat("\n ----------- read e_acases_eff ----------- \n")
# file <- paste0(outfile.base,'-summary-eff-infectious-cases-age.RDS')
# if(!file.exists(file) | args_dir[['overwrite']])
# {	
#   stop('expect summary file ',paste0(outfile.base,'-summary-eff-infectious-cases-age.RDS'))
# }
# if(file.exists(file))
# {
#   e_acases_eff <- readRDS(file)
# }
# 
# # map model age groups to report age groups
# age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)
# 
# 
# ec <- subset(e_acases_eff, date=="2020-08-23", select=-c(date, time))
# tmp <- subset(plot.pars.basic$pop_info, select=c(loc, age.cat, pop))
# tmp <- merge(tmp, age_cat_map, by=c('age.cat'))
# tmp <- tmp[, list(pop=sum(pop)),by=c('loc','age.cat2')]
# setnames(tmp, 'age.cat2','age_cat')
# ec <- merge(ec, tmp, by=c('loc','age_cat'))
# ec[, lR_eff_cases:= log(M/pop*1e6)]
# ec <- ec[, list(
#   loc=loc,
#   lR_eff_cases= lR_eff_cases,
#   lR_eff_cases_m= mean(lR_eff_cases),
#   lR_eff_cases_s= sd(lR_eff_cases),
#   lR_eff_cases_std= (lR_eff_cases-mean(lR_eff_cases)) / sd(lR_eff_cases)
# ), by='age_cat']
# ec[, age_cat_label:= paste0('lR_eff_cases_',age_cat)]
# 
# exd <- subset(ex_deaths, variable=='school_reopen1_reopen0_diff', select=-c(variable, date, age_cat, age_band))
# exd <- merge(exd,unique(subset(plot.pars.basic$pop_info, select=c(loc, pop_total))), by='loc')
# exd[, loc_idx:= seq_len(nrow(exd))]
# exd[, lR_ex_deaths:= log(M/pop_total*1e6)]
# exd <- subset(exd, select=c(loc, loc_idx, lR_ex_deaths))
# 
# tmp <- dcast.data.table(ec, loc~age_cat_label, value.var='lR_eff_cases')
# exd <- merge(exd, tmp, by='loc')
# 
# 
# #
# #	extract samples to identify significant predictors of excess deaths
# cat(" \n -------------------------------- \n \n Extract samples from posterior cases by age c and excess deaths \n \n -------------------------------- \n")
# 
# #
# # extract samples of eff infectious cases
# file <- paste0(outfile.base,'-summary-E_effcasesByAge-samples.RDS')
# if(!file.exists(file) | args_dir[['overwrite']])
# {	
#   if(is.null(E_effcasesByAge)){
#     file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
#     cat("\n read RDS:", file2)
#     E_effcasesByAge <- readRDS(file2)	
#   }
#   
#   samples_e_acases_eff_byage_c <- extract_samples_e_acases_eff_byage_c(
#     E_effcasesByAge,
#     n=2e3,
#     age_cat_map,
#     plot.pars.basic$pop_info, 
#     plot.pars.basic$regions
#   )
#   saveRDS(samples_e_acases_eff_byage_c, file=file)
#   E_effcasesByAge <- NULL
# }
# if(file.exists(file) & !args_dir[['overwrite']])
# {
#   samples_e_acases_eff_byage_c <- readRDS(file)
# }
# 
# #
# # extract samples of excess deaths
# file <- paste0(outfile.base,'-summary-excess_deaths-samples.RDS')
# if(!file.exists(file) | args_dir[['overwrite']])
# {	
#   if(is.null(E_deathsByAge_school_reopen_0))
#   {
#     file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_school0,'.RDS')
#     cat("\n read RDS:", file2)	
#     E_deathsByAge_school_reopen_0 <- readRDS(file2)
#   }
#   if(is.null(E_deathsByAge_shield_1))
#   {
#     file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity_shield1,'.RDS')
#     cat("\n read RDS:", file2)
#     E_deathsByAge_shield_1 <- readRDS(file2)
#   }
#   
#   # Set start and end date of the forecast period with school opened
#   start_date <- as.Date("2020-08-24") # 3 months period
#   end_date <- as.Date("2020-11-24") # 3 months period
#   
#   # add age labels to reduced flows
#   tmp <- add_pop_info_age_school_children(plot.pars.basic$pop_info)
#   age_cat_map_school_chidren <- make_age_cat_map_7_school_chidren(tmp)
#   
#   samples_excess_deaths <- extract_samples_excess_deaths(
#     E_deathsByAge_school_reopen_0,
#     E_deathsByAge_shield_1,
#     n=2e3,
#     plot.pars.basic$pop_info,
#     start_date,
#     end_date,
#     plot.pars.basic$dates,
#     age_cat_map_school_chidren,
#     plot.pars.basic$regions)
#   saveRDS(samples_excess_deaths, file=file)
# }
# 
# 
# cat(" \n -------------------------------- \n \n Identify significant predictors of excess deaths \n \n -------------------------------- \n")
# 
# #
# #	attempt to identify groups of infectious individuals most strongly associated with excess deaths
# exd <- subset(samples_excess_deaths, select=-c(date, time, age_cat, age_band, value_school_reopen0, value_school_reopen1, value_school_reopen1_reopen0_ratio))
# exd <- merge(exd,unique(subset(plot.pars.basic$pop_info, select=c(loc, pop_total))), by='loc')
# tmp <- unique(subset(exd, select=loc))
# tmp[, loc_idx:= seq_len(nrow(tmp))]
# exd <- merge(tmp, exd, by='loc')
# exd[, lR_ex_deaths:= log(value_school_reopen1_reopen0_diff/pop_total*1e6)]
# exd <- subset(exd, select=c(loc, loc_idx, iteration, lR_ex_deaths))
# 
# tmp <- subset(plot.pars.basic$pop_info, select=c(loc, age.cat, pop))
# tmp <- merge(tmp, age_cat_map, by=c('age.cat'))
# tmp <- tmp[, list(pop=sum(pop)),by=c('loc','age.cat2')]
# setnames(tmp, 'age.cat2','age_cat')
# ec <- merge(samples_e_acases_eff_byage_c, tmp, by=c('loc','age_cat'))
# ec[, lR_eff_cases:= log(value/pop*1e6)]
# ec[, age_cat_label:= paste0('lR_eff_cases_',age_cat)]
# 
# #	standardise covariates so most significant covariates can be directly read off from coefficients
# tmp <- ec[, list(loc=loc, lR_eff_cases_std= (lR_eff_cases-mean(lR_eff_cases))/sd(lR_eff_cases) ), by=c('age_cat','iteration')]
# ec <- merge(ec, tmp, by=c('loc','age_cat','iteration'))
# tmp <- dcast.data.table(ec, loc+iteration~age_cat_label, value.var='lR_eff_cases_std')
# exd <- merge(exd, tmp, by=c('loc','iteration'))
# 
# #	all infectious cases groups are highly correlated with excess deaths
# tmp <- melt(exd, id.vars=c('loc','iteration','loc_idx','lR_ex_deaths'))
# tmp <- tmp[, list(cor=cor(lR_ex_deaths,value)), by=c('iteration', 'variable')]
# tmp <- tmp[, list(
#   q= quantile(cor, prob=c(0.025,0.5,0.975)),
#   q_lab= c('CL','M','CU')
# ), by=c('variable')]
# tmp <- dcast.data.table(tmp, variable~q_lab, value.var='q')
# 
# #	least squares given linear relationship of all covariates with excess deaths
# exdr <- exd[,{
#   model <- lm( lR_ex_deaths ~ lR_eff_cases_1 + lR_eff_cases_2 + lR_eff_cases_3 + lR_eff_cases_4 + lR_eff_cases_5 + lR_eff_cases_6 + lR_eff_cases_7 )			
#   tmp <- coef(model)
#   list(coeff_label= names(tmp), coeff=tmp)			
# }, by='iteration']
# exdr <- exdr[, list(
#   q= quantile(coeff, prob=c(0.025,0.5,0.975)),
#   q_lab= c('CL','M','CU')
# ), by=c('coeff_label')]
# exdr <- dcast.data.table(exdr, coeff_label~q_lab, value.var='q')
# 
# tmp <- unique(subset(age_cat_map, select=c(age.cat2, age.cat2.label)))
# exdr[, age.cat2:= gsub('.*_([0-9])','\\1',coeff_label)]
# set(exdr, which(grepl('Intercept', exdr$age.cat2)), 'age.cat2', NA_character_)
# set(exdr, NULL, 'age.cat2', exdr[, as.integer(age.cat2)])
# exdr <- merge(exdr, tmp, by='age.cat2', all.x=TRUE)
# set(exdr, which(is.na(exdr$age.cat2)), 'age.cat2.label', 'Intercept')
# 
# #	plot
# g <- ggplot(exdr) +  
#   geom_vline(xintercept=0, linetype='dotted', colour='grey50') +
#   geom_errorbarh(aes(y=age.cat2.label, xmin=CL, xmax=CU), height=0.5) +  
#   geom_point(aes(y=age.cat2.label, x=M)) +
#   theme_bw() +
#   labs(x='regression coefficient', y='')
# ggsave(paste0(outfile.base,'-excessdeathsper1e6_casesper1e6_significant_covariates', '_multiplier_', multiplier, '_level_', args_dir$school_level,'.png'), g, w = 4, h=7)
# 
# 
# cat(" \n -------------------------------- \n \n End post-processing-forecast-plot-deaths.r \n \n -------------------------------- \n")
