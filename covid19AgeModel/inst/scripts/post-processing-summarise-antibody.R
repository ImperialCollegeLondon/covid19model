# post-processing-validate-prop_antibodys.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-validate-prop_antibodys.R \n \n -------------------------------- \n")

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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200729h13_cmdstanv'
	args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200729h13_cmdstanv-38states_deaths0801_levinifrprior'
	args_dir[['job_tag']] <- '38states_deaths0801_levinifrprior'
	args_dir[['overwrite']] <- 0
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
} 

args_dir[['overwrite']] <- TRUE
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

cat(" \n -------------------------------- \n input observed data from studies \n -------------------------------- \n")

nystudy <- data.table(state=c(rep('NYC',5)),
															dates=c(rep('2020-03-29',5)),
															age_band = c('overall','18-34','35-44','45-54','55+'),
											L_obs=c('22.7 [21.5-24.0]','21.8 [19.2-24.4]','23.4 [20.6-26.2]', '26.5 [23.8-29.2]', '21.5 [19.6-23.5]')
											)
nystudy$M_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\1', nystudy$L_obs)
nystudy$CL_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\2', nystudy$L_obs)
nystudy$CU_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\3', nystudy$L_obs)

cdc_r1 <- data.table(state=c(
                             rep('MO',5), rep('UT',5),
                             rep('NYC',5),rep('FL',5),rep('LA',5),rep('WA',5),rep('CT',5)),
										 dates=c(
										         rep('2020-04-26',5),rep('2020-05-03',5),
										         rep('2020-04-01',5),rep('2020-04-10',5),rep('2020-04-08',5),rep('2020-04-01',5),rep('2020-05-03',5)),
										 age_band=c(
										            rep(c('overall','0-18','19-49','50-64','65+'),7)
										            #rep(c('overall','0-18','19-49','50-64','65+'),5)
										            ),
										 L_obs=c(
										         '2.7 [1.7-3.9]', '1.4 [0.0-4.1]', '3.4 [1.4-5.5]', '2.0 [0.5-3.8]', '3.2 [1.9-4.6]',
										         '2.2 [1.2-3.4]', '', '1.8 [0.6-3.5]', '2.9 [0.9-5.2]', '2.7 [0.9-5.0]',
										         '6.9 [5.0-8.9]', '2.7 [0.9-5.0]', '8.3 [6.2-10.2]', '6.5 [4.3-9.6]', '3.7 [2.2-5.2]',
										 				'1.9 [1.0-3.2]', '2.4 [0.0-7.8]', '0.9 [0.2-2.2]', '2.0 [0.3-4.0]', '3.0 [1.7-4.5]',
										 				'5.8 [3.9-8.2]',	'2.8 [0.0-11.5]',	'7.4 [4.7-10.0]', '8.3 [4.5-11.9]',	'4.4 [1.5-8.0]',
										 				'1.1 [0.7-1.9]', '0.7 [0.0-2.5]', '1.3 [0.7-2.3]', '0.9 [0.3-1.9]', '1.7 [0.9-2.7]',
										 				'4.9 [3.6-6.5]','0.8 [0.0-2.9]','6.1 [3.1-9.3]','8.1 [4.8-11.6]','4.2 [2.3-6.0]')
)
cdc_r1$M_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\1', cdc_r1$L_obs)
cdc_r1$CL_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\2', cdc_r1$L_obs)
cdc_r1$CU_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\3', cdc_r1$L_obs)
cdc_r1$round <- rep(1,nrow(cdc_r1))

cdc_r2 <- data.table(state=c(
                             rep('MO',5), rep('UT',5),
                             rep('NYC',5),rep('FL',5),rep('WA',5),rep('CT',5)),
									dates=c(
									        rep('2020-05-30',5),rep('2020-06-05',5),
									        rep('2020-05-06',5),rep('2020-04-24',5),rep('2020-05-11',5),rep('2020-05-26',5)),
									age_band=c(
									  rep(c('overall','0-18','19-49','50-64','65+'),6)
									  #rep(c('overall','0-18','19-49','50-64','65+'),4)
									  ),
									L_obs=c(
									        '2.8 [1.7-4.1]','3.4 [0.6-7.0]','2.4 [1.1-4.0]','2.2 [0.9-3.9]','1.8 [0.7-3.3]',
									        '1.1 [0.6-2.1]','','1.2 [0.4-2.5]','0.8 [0.2-2.1]','1.0 [0.2-2.4]',
									        '23.2 [19.9-26.3]','21.5 [15.8-27.1]','23.6 [17.9-19.2]','29.1 [21.5-36.5]','15.8 [12.0-19.4]',
															'2.9 [1.9-4.3]','','2.6 [1.1-4.5]','4.7 [1.9-8.0]','3.8 [1.9-5.7]',
															'2.1 [1.2-3.5]','0.8 [0.0-4.6]','2.1 [0.8-4.1]','2.7 [1.1-4.8]','1.4 [0.6-2.7]',
															'5.2 [3.8-6.6]','2.9 [0.0-7.5]','7.1 [4.8-9.5]','5.8 [3.7-8.0]','3.2 [1.6-5.1]')
									)
cdc_r2$M_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\1', cdc_r2$L_obs)
cdc_r2$CL_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\2', cdc_r2$L_obs)
cdc_r2$CU_obs <- gsub("([0-9. ]+) \\[([0-9.]+)-([0-9.]+)\\]", '\\3', cdc_r2$L_obs)
cdc_r2$round <- rep(2,nrow(cdc_r2))

cdc <- rbind(cdc_r1,cdc_r2)

cat(" \n -------------------------------- \n map to model age bands \n -------------------------------- \n")

age_cat_map_NYstudy <- data.table(age.cat=seq(1:18),
																age.cat.label=c('0-4',
																								'5-9',       
																								'10-14',         
																								'15-19',
																								'20-24',
																								'25-29',  
																								'30-34',
																								'35-39',
																								'40-44',
																								'45-49',
																								'50-54',
																							  '55-59',
																								'60-64',
																								'65-69',
																								'70-74',
																								'75-79',
																								'80-84',
																								'85+'),	
																age.cat2=c(NA,NA,NA,NA,1,1,1,2,2,3,3,4,4,4,4,4,4,4),
																age.cat2.label=c(NA,NA,NA,NA,'18-34','18-34','18-34','35-44','35-44','45-54','45-54','55+','55+','55+','55+','55+','55+','55+')
																)

age_cat_map_18plus <- data.table(age.cat=seq(1:18),
													age.cat.label=c('0-4',
																					'5-9',       
																					'10-14',         
																					'15-19',
																					'20-24',
																					'25-29',  
																					'30-34',
																					'35-39',
																					'40-44',
																					'45-49',
																					'50-54',
																					'55-59',
																					'60-64',
																					'65-69',
																					'70-74',
																					'75-79',
																					'80-84',
																					'85+'),	
													age.cat2=c(NA,NA,NA,NA,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
													age.cat2.label=c(NA,NA,NA,NA,'18+','18+','18+','18+','18+','18+','18+','18+','18+','18+','18+','18+','18+','18+')
)

age_cat_map_CDC <- data.table(age.cat=seq(1:18),
																age.cat.label=c('0-4',
																								'5-9',       
																								'10-14',         
																								'15-19',
																								'20-24',
																								'25-29',  
																								'30-34',
																								'35-39',
																								'40-44',
																								'45-49',
																								'50-54',
																								'55-59',
																								'60-64',
																								'65-69',
																								'70-74',
																								'75-79',
																								'80-84',
																								'85+'),	
																age.cat2=c(1,1,1,1,2,2,2,2,2,2,3,3,3,4,4,4,4,4),
																age.cat2.label=c('0-18','0-18','0-18','0-18','19-49','19-49','19-49','19-49','19-49','19-49','50-64','50-64','50-64','65+','65+','65+','65+','65+')
)
cat("\n ----------- summarise_prop_antibody_byage_c overall ----------- \n")

#	summarise antibody/prop_antibody overall
file <- paste0(outfile.base,'-summary-antibody.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-E_antibodyByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_antibodyByAge <- readRDS(file2)
  
	cat("\n ----------- make_casesoverall_summaries ----------- \n")
	e_antibody_overall_c <- make_casesoverall_c_summaries(E_antibodyByAge,
																											plot.pars.basic$dates, plot.pars.basic$regions,
																											plot.pars.basic$pop_info)
	cat("\nWrite ",file," ... ")
	saveRDS(e_antibody_overall_c, file=file)
}
if(file.exists(file))
{
	e_antibody_overall_c <- readRDS(file)
}


prop_antibody_overall_c <- summarise_attackrate_overall_c(e_antibody_overall_c, 
																											 plot.pars.basic$pop_info, plot.pars.basic$regions)



cat("\n ----------- summarise antibody for 18plus population only ----------- \n")
#	summarise antibody 18plus population
file <- paste0(outfile.base,'-summary-prop_antibody-18plus.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_antibodyByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_antibodyByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_antibodyByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cases-18plus ----------- \n")
  e_antibody_byage_s_18plus <- summarise_e_acases_byage_c(E_antibodyByAge,
                                                        age_cat_map_18plus, 
                                                        plot.pars.basic$pop_info, 
                                                        plot.pars.basic$dates, 
                                                        plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_antibody_byage_s_18plus, file=file)
}
if(file.exists(file))
{
  e_antibody_byage_s_18plus <- readRDS(file)
}

prop_antibody_byage_s_18plus <- summarise_attackrate_byage_c(e_antibody_byage_s_18plus, age_cat_map_18plus,
																															 plot.pars.basic$pop_info, plot.pars.basic$regions)
prop_antibody_byage_s_18plus <- prop_antibody_byage_s_18plus[!is.na(age_cat),-c(1:2)]


cat("\n ----------- summarise_prop_antibody_byage_c for NY study age bands and make validation table ----------- \n")
#	summarise antibody/prop_antibody NY study age bands
file <- paste0(outfile.base,'-summary-antibody-NYstudy.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_antibodyByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_antibodyByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_antibodyByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cases-NYstudy ----------- \n")
  e_antibody_byage_s <- summarise_e_acases_byage_c(E_antibodyByAge,
                                                 age_cat_map_NYstudy, 
                                                 plot.pars.basic$pop_info, 
                                                 plot.pars.basic$dates, 
                                                 plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_antibody_byage_s, file=file)
}
if(file.exists(file))
{
  e_antibody_byage_s <- readRDS(file)
}

prop_antibody_byage_s <- summarise_attackrate_byage_c(e_antibody_byage_s, age_cat_map_NYstudy,
																									 plot.pars.basic$pop_info, plot.pars.basic$regions)
dates <- as.Date(unique(nystudy$dates))

# assumed lag 0 or 7 for delay in death reports
dateslag <- c(dates+7,dates)

artable <- make_attackrate_validation_table(prop_antibody_byage_s,prop_antibody_byage_s_18plus,plot.pars.basic$pop_info,dateslag,unique(nystudy$state),nystudy,outfile.base)
artable$lag <- difftime(dates,artable$date,units="days")
artable$study <- rep("NY study",nrow(artable))
setcolorder(artable, c(ncol(artable),1,3,ncol(artable)-1,4,2,5:(ncol(artable)-2)))


cat("\n ----------- summarise_prop_antibody_byage_c for CDC data ----------- \n")
#	summarise antibody/prop_antibody cdc study age bands
file <- paste0(outfile.base,'-summary-cases-CDCstudy.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  if(is.null(E_antibodyByAge)){
    file2 <- paste0(outfile.base,'-stanout-E_antibodyByAge-gqs.RDS')
    cat("\n read RDS:", file2)
    E_antibodyByAge <- readRDS(file2)
  }
  
  cat("\n ----------- summarise_cases-CDCageband ----------- \n")
  e_antibody_byage_s <- summarise_e_acases_byage_c(E_antibodyByAge,
                                                 age_cat_map_CDC, 
                                                 plot.pars.basic$pop_info, 
                                                 plot.pars.basic$dates, 
                                                 plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_antibody_byage_s, file=file)
}
if(file.exists(file))
{
  e_antibody_byage_s <- readRDS(file)
}
E_antibodyByAge <- NULL

prop_antibody_byage_s <- summarise_attackrate_byage_c(e_antibody_byage_s, age_cat_map_CDC,
																									 plot.pars.basic$pop_info, plot.pars.basic$regions)



# NB use over 18s summary for calculating overall prop_antibody for Florida in round 2
for(i in 1:2){
	study <- subset(cdc,round==i)
	tmp <- data.table()
	for(states in unique(study$state)){
		dates <- unique(study$dates[study$state==states])
		dates <- as.Date(dates)
		dateslag <- c(dates,dates+7)
			if(states=='FL' & i==2){
			ans <- make_attackrate_validation_table(prop_antibody_byage_s,prop_antibody_byage_s_18plus,plot.pars.basic$pop_info,dateslag,states,study,outfile.base)
		}else	{
		ans <- make_attackrate_validation_table(prop_antibody_byage_s,prop_antibody_overall_c,plot.pars.basic$pop_info,dateslag,states,study,outfile.base)
		}
		ans$lag <- difftime(dates,ans$date,units="days")
		tmp <- rbind(tmp,ans)
	}
	
	tmp <- subset(tmp,select=-c(round))
	tmp$study <- paste0("CDC round ",i)
	setcolorder(tmp, c(ncol(tmp),1,3,ncol(tmp)-1,4,2,5:(ncol(tmp)-2)))
	artable <- rbind(artable,tmp)
}

artable$notes <- ""
artable$notes[artable$loc=='FL'] <- "sampling in South Florida, with more cumulative deaths in South Florida compared to state average"
artable$notes[artable$loc=='WA'] <- "sampling in Western Washington counties with more cases reported than state average"

artable <- list(artable, format(date,  "%B %d, %Y") )

cat('\nWriting ',paste0(outfile.base,'-prop_antibody_validationtable.rds'),' ...')
saveRDS(artable, file = paste0(outfile.base,'-prop_antibody_validationtable.rds'), version = 2)

cat("\n ----------- plot estimated/observed attack rates ----------- \n")

da <- copy( artable[[1]] )

da <- subset(da, select=-c(L_est,L_obs))
da <- reshape2::melt(da, measure.vars=c('M_est','CL_est','CU_est','M_obs','CL_obs','CU_obs'))
set(da, NULL, 'value', da[, as.numeric(value)/100])	
da[, mtype:= gsub('est','Estimated',gsub('obs','Survey',gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\2',variable)))]
da[, stat:= gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\1',variable)]
da <- dcast.data.table(da, study+loc+loc_label+lag+date+age_band+mtype~stat, value.var='value')

tmp <- subset(da, age_band=='overall')
tmp[, L:= paste0(loc_label, ' (',study,'), ',as.character(date) )]

tmp[,src:='CDC']
tmp$src[tmp$study=='NY study'] <- 'NY study'
tmp <- tmp[order(-src,-loc_label,-date),]
tmp$L <- factor(tmp$L,levels=c(unique(tmp$L)))

lags <- unique(tmp$lag)
for (i in 1:length(lags)) {
  tmp_s <- tmp[lag==lags[i]]
  g <- ggplot(tmp_s) +
    geom_point(aes(y=L, x=M, colour=mtype), position=position_dodge(width=0.5)) +
    geom_errorbarh(aes(y=L, xmin=CL, xmax=CU, colour=mtype), position=position_dodge(width=0.5)) +
    scale_x_continuous(labels=scales::percent, expand=c(0,0)) +
    coord_cartesian(xlim=c(0, max(tmp$CU)*1.1)) +
    labs(x='Estimated COVID-19 seroprevalence', y='', colour='') +
    theme_bw()  +
    scale_colour_viridis_d(begin=0.2,end=0.7,alpha=1,direction=-1,option = "magma")  +
    theme(legend.position="bottom",
          legend.text = element_text(size = 14))
  ggsave(file=paste0(outfile.base, "-prop_antibody_validation_lag",lags[i],".png"),g,w=7, h=10)
}

cat(" \n -------------------------------- \n \n completed post-processing-validate-prop_antibodys.R \n \n -------------------------------- \n")
