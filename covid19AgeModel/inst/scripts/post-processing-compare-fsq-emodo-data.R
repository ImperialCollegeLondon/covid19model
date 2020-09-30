# post-processing-compare-fsq-emodo-data.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-compare-fsq-emodo-data.R \n \n -------------------------------- \n")

suppressMessages(library(rstan, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))
suppressMessages(library(gdata, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))
suppressMessages(library(tidyr, quietly = TRUE))
suppressMessages(library(EnvStats, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(stringr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))
#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['script_dir']] <- '~/git/R0t'
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
  args_dir[['job_tag']] <- '19states_stdctn_2'
}

args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-script_dir')	
  stopifnot(args_line[[3]]=='-stanModelFile')	
  stopifnot(args_line[[5]]=='-out_dir')
  stopifnot(args_line[[7]]=='-job_tag')
  args_dir <- list()
  args_dir[['script_dir']] <- args_line[[2]]
  args_dir[['stanModelFile']] <- args_line[[4]]
  args_dir[['out_dir']] <- args_line[[6]]
  args_dir[['job_tag']] <- args_line[[8]]
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir,'/comparison_emo_fsq')


cat(" \n -------------------------------- \n read data sets \n -------------------------------- \n")

#
#	make pop_info
pop_count <- read_pop_count_us(file.path(args_dir$script_dir,"usa","data","us_population_withnyc.rds"))
pop_by_age <- read_pop_count_by_age_us( file.path(args_dir$script_dir,"usa","data","us_population_withnyc.rds"))
darea <- read_us_state_areas(file.path(args_dir$script_dir,"usa","data","us_states_area_measurements.csv"))
pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
setkey(pop_info, loc_label, age.cat)
pop_by_age <- NULL
pop_count <- NULL
darea <- NULL

#
# 	read fsq data
mob_fsq <- read_foursquare_mobility(pop_info, infile_fsq=file.path(args_dir$script_dir, "usa","data",'fsq_visit_data_aug_refresh_200829.csv'))	
fsq_age_cat_map <- make_fsq_age_cat_map(mob_fsq, pop_info)
mob_fsq <- make_mobility_trends_fsq(mob_fsq, fsq_age_cat_map)	
mob_fsq <- make_decoupled_mobility_trends_into_3_parts_fsq(mob_fsq, pop_info)
mob_fsq[, week:= as.integer(strftime(date, format = "%V"))]

#
#	read Emodo data
mob_emodo <- read_emodo_cell_phone_contact_intensities(infile_emodo = '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_contacts_by_age_20200830.csv')
mob_emodo <- read_emodo_cell_phone_contact_intensities_2(infile_emodo = '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_mobility_by_age_20200831.csv')
#mob_emodo <- read_emodo_cell_phone_contact_intensities(infile_emodo = '~/contacts_and_mobility/contacts_by_age_20200729.csv')
emo_age_cat_map <- make_emodo_age_cat_map(mob_emodo, pop_info)
mob_emodo <- make_mobility_trends_emo(mob_emodo, emo_age_cat_map)
mob_emodo[, week:= as.integer(strftime(date, format = "%V"))]

#
#	reduce to pure data
mob_fsq <- unique(subset(mob_fsq, select=-c(age.cat, age.cat.label)))
mob_emodo <- unique(subset(mob_emodo, select=-c(age.cat, age.cat.label)))

#
#	remove suspicious data
mob_emodo <- subset(mob_emodo, week<=30)

cat(" \n -------------------------------- \n Foursquare only analysis \n -------------------------------- \n")

#
#	calculate avg mobility trend in calendar week that contains the rebound_date for each state

#
#	find rebound week, last complete week FSQ, and last complete week Emodo
dwks <- subset(mob_fsq, date==rebound_date, c(loc, week))
dwks[, week_type:= 'rebound_week']
tmp <- mob_fsq[, list(ndays=length(unique(date))), by=c('loc','week')]
tmp <- tmp[ndays==7, list(week=max(week)), by='loc'] 
tmp[, week_type:= 'fsq_last_week']
dwks <- rbind(dwks, tmp)
tmp <- mob_emodo[, list(ndays=length(unique(date))), by=c('loc','week')]
tmp <- tmp[ndays==7, list(week=max(week)), by='loc'] 
tmp[, week_type:= 'emodo_last_week']
dwks <- rbind(dwks, tmp)

# record last date range
fsq_last_week <- dwks[grepl('fsq_last_',week_type),unique(week)]
week_date <- unique(subset(mob_fsq,select=c(week,date)))
week_date[,date:=format(date,"%b %d")]
fsq_last_week <- week_date[week==fsq_last_week,range(date)]
emodo_last_week <- dwks[grepl('emodo_last_',week_type),unique(week)]
emodo_last_week <- week_date[week==emodo_last_week,range(date)]
emodo_date <- format(range(mob_emodo$date),"%b %d")
#
#	get data in these weeks
mob_fsq_s <- merge(dwks, mob_fsq, by=c('loc','week'))
mob_emodo_s <- merge(dwks, mob_emodo, by=c('loc','week'))

#	absolute
#	make analysis which age groups significant at rebound week
m1_rbd_fsq <- glm(mobility_trend~fsq.age.cat.label:loc-1,family = Gamma(link = "log"),data=subset(mob_fsq_s, week_type=='rebound_week'))
m1_rbd_fsq_coeff <- data.table(coef(summary(m1_rbd_fsq)),keep.rownames = TRUE)
m1_rbd_fsq_coeff[,age.cat.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
m1_rbd_fsq_coeff[,loc:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
m1_rbd_fsq_coeff[, week_type:= 'rebound_week']
m1_rbd_fsq_coeff[, data_type:= 'FSQ']
m1_rbd_fsq_coeff[, analysis_type:= 'absolute']
#	--> in all locations drop significant
#	

#
#	make analysis which age groups significant in Emodo last week
m1_elast_fsq <- glm(mobility_trend~fsq.age.cat.label:loc-1,family = Gamma(link = "log"),data=subset(mob_fsq_s, week_type=='emodo_last_week'))
m1_elast_fsq_coeff <- data.table(coef(summary(m1_elast_fsq)),keep.rownames = TRUE)
m1_elast_fsq_coeff[,age.cat.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
m1_elast_fsq_coeff[,loc:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
m1_elast_fsq_coeff[, week_type:= 'emodo_last_week']
m1_elast_fsq_coeff[, data_type:= 'FSQ']
m1_elast_fsq_coeff[, analysis_type:= 'absolute']


#
#	make analysis which age groups significant in last week
m1_last_fsq <- glm(mobility_trend~fsq.age.cat.label:loc-1,family = Gamma(link = "log"),data=subset(mob_fsq_s, week_type=='fsq_last_week'))
m1_last_fsq_coeff <- data.table(coef(summary(m1_last_fsq)),keep.rownames = TRUE)
m1_last_fsq_coeff[,age.cat.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
m1_last_fsq_coeff[,loc:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
m1_last_fsq_coeff[, week_type:= 'fsq_last_week']
m1_last_fsq_coeff[, data_type:= 'FSQ']
m1_last_fsq_coeff[, analysis_type:= 'absolute']
#	--> some locations some ages have significant drop < 1
#	--> some locations some ages have significant increase > 1



#	make analysis which age groups significant at rebound week for each state
# 	relative to baseline 35-44
locs <-  sort(unique(mob_fsq_s$loc))
m2_rbd_fsq_coeff <- vector('list',length(locs))
names(m2_rbd_fsq_coeff) <- locs
for(x in locs)
{
	tmp <- subset(mob_fsq_s, week_type=='rebound_week' & loc==x)
	tmp[, fsq.age.cat.label:=factor(fsq.age.cat.label, levels = c('35-44','18-24', '25-34', '45-54', '55-64','65+'))]
	tmp2 <- glm(mobility_trend~fsq.age.cat.label,family = Gamma(link = "log"),data=tmp)
	tmp2 <- coef(summary(tmp2))	
	m2_rbd_fsq_coeff[[x]] <- as.data.table(tmp2)
	m2_rbd_fsq_coeff[[x]][, row.names:= rownames(tmp2)]
	m2_rbd_fsq_coeff[[x]][, loc:= x]		
}
m2_rbd_fsq_coeff <- do.call('rbind',m2_rbd_fsq_coeff)
m2_rbd_fsq_coeff[, week_type:= 'rebound_week']
m2_rbd_fsq_coeff[, data_type:= 'FSQ']
m2_rbd_fsq_coeff[, analysis_type:= 'relative to 35-44']


#	make analysis which age groups significant at last week for each state
# 	relative to baseline 35-44
locs <-  unique( mob_fsq_s[week_type=='emodo_last_week', loc] )
m2_elast_fsq_coeff <- vector('list',length(locs))
names(m2_elast_fsq_coeff) <- locs
for(x in locs)
{
	tmp <- subset(mob_fsq_s, week_type=='emodo_last_week' & loc==x)
	tmp[, fsq.age.cat.label:=factor(fsq.age.cat.label, levels = c('35-44','18-24', '25-34', '45-54', '55-64','65+'))]
	tmp2 <- glm(mobility_trend~fsq.age.cat.label,family = Gamma(link = "log"),data=tmp)
	tmp2 <- coef(summary(tmp2))	
	m2_elast_fsq_coeff[[x]] <- as.data.table(tmp2)
	m2_elast_fsq_coeff[[x]][, row.names:= rownames(tmp2)]
	m2_elast_fsq_coeff[[x]][, loc:= x]		
}
m2_elast_fsq_coeff <- do.call('rbind',m2_elast_fsq_coeff)
m2_elast_fsq_coeff[, week_type:= 'emodo_last_week']
m2_elast_fsq_coeff[, data_type:= 'FSQ']
m2_elast_fsq_coeff[, analysis_type:= 'relative to 35-44']



#	make analysis which age groups significant at last week for each state
# 	relative to baseline 35-44
locs <-  sort(unique(mob_fsq_s$loc))
m2_last_fsq_coeff <- vector('list',length(locs))
names(m2_last_fsq_coeff) <- locs
for(x in locs)
{
	tmp <- subset(mob_fsq_s, week_type=='fsq_last_week' & loc==x)
	tmp[, fsq.age.cat.label:=factor(fsq.age.cat.label, levels = c('35-44','18-24', '25-34', '45-54', '55-64','65+'))]
	tmp2 <- glm(mobility_trend~fsq.age.cat.label,family = Gamma(link = "log"),data=tmp)
	tmp2 <- coef(summary(tmp2))	
	m2_last_fsq_coeff[[x]] <- as.data.table(tmp2)
	m2_last_fsq_coeff[[x]][, row.names:= rownames(tmp2)]
	m2_last_fsq_coeff[[x]][, loc:= x]		
}
m2_last_fsq_coeff <- do.call('rbind',m2_last_fsq_coeff)
m2_last_fsq_coeff[, week_type:= 'fsq_last_week']
m2_last_fsq_coeff[, data_type:= 'FSQ']
m2_last_fsq_coeff[, analysis_type:= 'relative to 35-44']


cat(" \n -------------------------------- \n Emodo only analysis \n -------------------------------- \n")


#	absolute
#	make analysis which age groups significant at rebound week
m1_rbd_emodo <- glm(mobility_trend~emo.age.label:loc-1,family = Gamma(link = "log"),data=subset(mob_emodo_s, week_type=='rebound_week'))
m1_rbd_emodo_coeff <- data.table(coef(summary(m1_rbd_emodo)),keep.rownames = TRUE)
m1_rbd_emodo_coeff[,age.cat.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
m1_rbd_emodo_coeff[,loc:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
m1_rbd_emodo_coeff[, week_type:= 'rebound_week']
m1_rbd_emodo_coeff[, data_type:= 'Emodo']
m1_rbd_emodo_coeff[, analysis_type:= 'absolute']
#	


#
#	make analysis which age groups significant in last week
m1_last_emodo <- glm(mobility_trend~emo.age.label:loc-1,family = Gamma(link = "log"),data=subset(mob_emodo_s, week_type=='emodo_last_week'))
m1_last_emodo_coeff <- data.table(coef(summary(m1_last_emodo)),keep.rownames = TRUE)
m1_last_emodo_coeff[,age.cat.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
m1_last_emodo_coeff[,loc:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
m1_last_emodo_coeff[, week_type:= 'emodo_last_week']
m1_last_emodo_coeff[, data_type:= 'Emodo']
m1_last_emodo_coeff[, analysis_type:= 'absolute']




#	make analysis which age groups significant at rebound week for each state
# 	relative to baseline 35-44
locs <-  sort(unique(mob_emodo_s$loc))
m2_rbd_emodo_coeff <- vector('list',length(locs))
names(m2_rbd_emodo_coeff) <- locs
for(x in locs)
{
	tmp <- subset(mob_emodo_s, week_type=='rebound_week' & loc==x)
	tmp[, emo.age.label:=factor(emo.age.label, levels = c('35-44','18-24', '25-34', '45-54', '55+'))]
	tmp2 <- glm(mobility_trend~emo.age.label,family = Gamma(link = "log"),data=tmp)
	tmp2 <- coef(summary(tmp2))	
	m2_rbd_emodo_coeff[[x]] <- as.data.table(tmp2)
	m2_rbd_emodo_coeff[[x]][, row.names:= rownames(tmp2)]
	m2_rbd_emodo_coeff[[x]][, loc:= x]		
}
m2_rbd_emodo_coeff <- do.call('rbind',m2_rbd_emodo_coeff)
m2_rbd_emodo_coeff[, week_type:= 'rebound_week']
m2_rbd_emodo_coeff[, data_type:= 'Emodo']
m2_rbd_emodo_coeff[, analysis_type:= 'relative to 35-44']

#	make analysis which age groups significant at last week for each state
# 	relative to baseline 35-44
locs <-  sort(unique(mob_emodo_s$loc))
m2_last_emodo_coeff <- vector('list',length(locs))
names(m2_last_emodo_coeff) <- locs
for(x in locs)
{
	tmp <- subset(mob_emodo_s, week_type=='emodo_last_week' & loc==x)
	tmp[, emo.age.label:=factor(emo.age.label, levels = c('35-44','18-24', '25-34', '45-54', '55+'))]
	tmp2 <- glm(mobility_trend~emo.age.label,family = Gamma(link = "log"),data=tmp)
	tmp2 <- coef(summary(tmp2))	
	m2_last_emodo_coeff[[x]] <- as.data.table(tmp2)
	m2_last_emodo_coeff[[x]][, row.names:= rownames(tmp2)]
	m2_last_emodo_coeff[[x]][, loc:= x]		
}
m2_last_emodo_coeff <- do.call('rbind',m2_last_emodo_coeff)
m2_last_emodo_coeff[, week_type:= 'emodo_last_week']
m2_last_emodo_coeff[, data_type:= 'Emodo']
m2_last_emodo_coeff[, analysis_type:= 'relative to 35-44']



cat(" \n -------------------------------- \n Collecting results \n -------------------------------- \n")


#
#	collect results and make plots

ans <- rbind( m1_rbd_fsq_coeff, 
	m1_elast_fsq_coeff,
	m1_last_fsq_coeff, 
	m1_rbd_emodo_coeff, 
	m1_last_emodo_coeff)
set(ans, NULL, 'age.cat.label', gsub('fsq.age.cat.label|emo.age.label','',ans$age.cat.label))
set(ans, NULL, 'loc', gsub('loc','',ans$loc))
setnames(ans, 'rn', 'row.names')
setnames(ans, colnames(ans), gsub('Pr\\(>\\|t\\|\\)','pval',gsub('Std._Error','sd',gsub(' ','_',colnames(ans)))))
ans[, updwn:=sign(Estimate)]
set(ans, which(ans$pval>0.05), 'updwn', 0L)
ans <- merge(unique(subset(pop_info, select=c(loc, loc_label))), ans, by='loc')


tmp <- rbind(m2_rbd_fsq_coeff, 
	m2_elast_fsq_coeff,
	m2_last_fsq_coeff,
	m2_rbd_emodo_coeff, 
	m2_last_emodo_coeff)
tmp[, age.cat.label:= row.names]
set(tmp, tmp[, which(age.cat.label=='(Intercept)')], 'age.cat.label', '35-44')
set(tmp, NULL, 'age.cat.label', gsub('fsq.age.cat.label|emo.age.label','',tmp$age.cat.label))
setnames(tmp, colnames(tmp), gsub('Pr\\(>\\|t\\|\\)','pval',gsub('Std._Error','sd',gsub(' ','_',colnames(tmp)))))
tmp[, updwn:=sign(Estimate)]
set(tmp, which(tmp$pval>0.05), 'updwn', 0L)
tmp <- merge(unique(subset(pop_info, select=c(loc, loc_label))), tmp, by='loc')

ans <- rbind(ans, tmp)

cat(" \n -------------------------------- \n Make FSQ plot \n -------------------------------- \n")

#
#	heatmaps FSQ data analysis -- absolute
tmp <- subset(ans, analysis_type=='absolute' & data_type=='FSQ' &  week_type!='emodo_last_week')
tmp$week_type <- factor(tmp$week_type,levels = c("rebound_week","fsq_last_week"),
                        labels=c("rebound week", "last week"))
p.fsq.abs <- ggplot(tmp) +			
  	scale_x_discrete(expand=c(0,0)) +
  	scale_y_discrete(expand=c(0,0)) +
  	geom_tile(aes(x= age.cat.label, y= reorder(loc_label, desc(loc_label)), fill=as.character(updwn))) +
  	scale_fill_manual(values = c('-1'="#D9F0D3", '0'="#5AAE61", '1'="#00441B"),
		labels=c('-1'='sig lower than baseline','0'='not sig different to baseline', '1'='sig higher than baseline')) +
  	labs(x= '', y='', fill='Mobility trend') +
  	facet_wrap(~week_type) +	
	guides(fill = guide_legend(nrow = 2, title.position = "top")) +
  	theme_bw() +
  	theme(legend.position='bottom',
		legend.justification = "left",
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
		legend.margin=margin(0,0,0,0),
		legend.box.margin=margin(0,-30,0,0),
        plot.title = element_text(size = 15, face = "bold",hjust = 0.5),
        axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank())

#
#	heatmaps FSQ data analysis -- relative
tmp <- subset(ans, analysis_type=='relative to 35-44' & data_type=='FSQ' &  week_type!='emodo_last_week')
tmp <- subset(tmp, age.cat.label!='35-44')
tmp$week_type <- factor(tmp$week_type,levels = c("rebound_week","fsq_last_week"),
                        labels=c("rebound week", "last week"))
p.fsq.rel3544 <- ggplot(tmp) +			
		scale_x_discrete(expand=c(0,0)) +
		scale_y_discrete(expand=c(0,0)) +
		geom_tile(aes(x= age.cat.label, y= reorder(loc_label, desc(loc_label)), fill=as.character(updwn))) +	
		scale_fill_manual(values = c('-1'="#E7D4E8" , '0'="#9970AB", '1'="#40004B"),
				labels=c('-1'='sig lower than 35-44','0'='not sig different to 35-44', '1'='sig higher than 35-44')) +
		labs(x= '', y='', fill='Mobility trend') +
		facet_wrap(~week_type) +
		guides(fill = guide_legend(nrow = 2, title.position = "top")) +
		theme_bw() +
		theme(legend.position='bottom',
				legend.justification = "left",
				legend.title = element_text(size = 10), 
				legend.text = element_text(size = 8),
				legend.margin=margin(0,0,0,0),
				legend.box.margin=margin(0,-30,0,0),				
				plot.title = element_text(size = 15, face = "bold",hjust = 0.5),
				axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
				axis.title.x = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				strip.background = element_blank(),
				axis.title.y=element_blank(),
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank())

p <- ggarrange(p.fsq.abs,
	p.fsq.rel3544,
	ncol=2, 
	widths=c(0.55,0.45), 
	common.legend = FALSE, 
	legend="bottom")

ggsave(file=paste0(outfile.base,'-fsq-analysis.pdf'), p, width = 250, height = 200, units = "mm")

cat(" \n -------------------------------- \n Make FSQ-Emodo plot \n -------------------------------- \n")

#	comparing FSQ + Emodo data
tmp <- unique(mob_emodo$loc)
tmp <- subset(ans, analysis_type=='absolute' & week_type!='fsq_last_week' & loc %in% tmp & !age.cat.label%in%c('55+','55-64','65+'))
set(tmp, NULL, 'week_type', 
		factor(tmp$week_type,levels = c("rebound_week","emodo_last_week"), labels=c("rebound week", "last week"))
		)
set(tmp, NULL, 'data_type', 
		factor(tmp$data_type,levels = c("FSQ","Emodo"), labels=c("Foursquare", "Emodo"))
		)		
p1 <- ggplot(tmp) +			
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  geom_tile(aes(x= age.cat.label, y= reorder(loc_label, desc(loc_label)), fill=as.character(updwn))) +
  scale_fill_manual(values = c('-1'="#D9F0D3", '0'="#5AAE61", '1'="#00441B"),
		  labels=c('-1'='sig lower than baseline','0'='not sig different to baseline', '1'='sig higher than baseline')) +
  labs(x= '', y='', fill='Mobility trend') +
  facet_grid(data_type~week_type) +	
  guides(fill = guide_legend(nrow = 2, title.position = "top")) +
  theme_bw() +  
  theme(legend.position='bottom',
		legend.justification = "left",
		legend.title = element_text(size = 10), 
		legend.text = element_text(size = 8),
		legend.margin=margin(0,0,0,0),
		legend.box.margin=margin(0,-30,0,0),
        plot.title = element_text(size = 15, face = "bold",hjust = 0.5),
        axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank())


tmp <- unique(mob_emodo$loc)
tmp <- subset(ans, analysis_type=='relative to 35-44' & week_type!='fsq_last_week' & loc %in% tmp & !age.cat.label%in%c('35-44','55+','55-64','65+'))
set(tmp, NULL, 'week_type', 
	factor(tmp$week_type,levels = c("rebound_week","emodo_last_week"), labels=c("rebound week", "last week"))
	)
set(tmp, NULL, 'data_type', 
	factor(tmp$data_type,levels = c("FSQ","Emodo"), labels=c("Foursquare", "Emodo"))
	)		
p2 <- ggplot(tmp) +			
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  geom_tile(aes(x= age.cat.label, y= reorder(loc_label, desc(loc_label)), fill=as.character(updwn))) +
  scale_fill_manual(values = c('-1'="#E7D4E8" , '0'="#9970AB", '1'="#40004B"),
		labels=c('-1'='sig lower than 35-44','0'='not sig different to 35-44', '1'='sig higher than 35-44')) +
  labs(x= '', y='', fill='Mobility trend') +
  facet_grid(data_type~week_type) +		
  guides(fill = guide_legend(nrow = 2, title.position = "top")) +
  theme_bw() +  
  theme(legend.position='bottom',
		legend.justification = "left",
		legend.title = element_text(size = 10), 
		legend.text = element_text(size = 8),
		legend.margin=margin(0,0,0,0),
		legend.box.margin=margin(0,-30,0,0),
        plot.title = element_text(size = 15, face = "bold",hjust = 0.5),
        axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p <- ggarrange(p1, 
	p2,               
	ncol=2, 
	nrow=1, 
	widths=c(0.55,0.45), 
	common.legend = FALSE,
	legend="bottom")

ggsave(file=paste0(outfile.base,'-compare-fsq-emodo-analysis.pdf'), p, width = 250, height = 140, units = "mm")


#
#	heatmap Emodo
#legend.p.emodo.rel <- get_legend(p.emodo.rel3544)
#p1 <- ggarrange(p.emodo.abs+ggtitle('absolute scale')+theme(plot.title = element_text(hjust = 0.5),legend.position='none'),
#               p.emodo.rel3544+ggtitle('relative to 35-44')+theme(plot.title = element_text(hjust = 0.5),legend.position='none'),
#               ncol=2, widths=c(0.55,0.45))
#p <- ggarrange(p1, legend.p.emodo.rel, heights = c(0.9,0.1),nrow = 2)
#ggsave(file=paste0(outfile.base,'-emodo-pval.pdf'), p, w=8, h=4)
#

cat(" \n -------------------------------- \n make rds \n -------------------------------- \n")

ans.summary <- ans[,list(sig_lower = sum(updwn==-1),
		no_diff = sum(updwn==0),
		sig_higher = sum(updwn==1),
		total=length(updwn)),
	by=c('age.cat.label','week_type', 'data_type', 'analysis_type')]
ans.summary[, sig_lower_p:= sprintf('%1.1f', sig_lower/total*100)]
ans.summary[, no_diff_p:= sprintf('%1.1f', no_diff/total*100)]
ans.summary[, sig_higher_p:= sprintf('%1.1f', sig_higher/total*100)]
for(x in c('sig_lower','no_diff','sig_higher','total'))
{
	set(ans.summary, NULL, x, as.character(ans.summary[[x]]))
}
	

# save rds
cat('\nWriting ',paste0(outfile.base,'-compare-fsq-emodo-analysis.rds'),' ...')
saveRDS(
	list(
		cnts=as.matrix(ans.summary),
		fsq_last_date=fsq_last_week,
		emodo_last_date=emodo_last_week,
		emodo_date=emodo_date
	),
	file = paste0(outfile.base,'-tables.rds'), 
	version = 2)

saveRDS(
  ans,
  file = paste0(outfile.base,'-data-for-plot.rds'), 
  version = 2)



cat(" \n -------------------------------- \n compare Emodo to FSQ trends \n -------------------------------- \n")

#
#	keep only loc_labels with Emodo data
tmp <- unique(mob_emodo$loc_label)
mob_fsq <- subset(mob_fsq, loc_label%in%tmp)

#
#	merge
setnames(mob_fsq, c('fsq.age.cat.label','mobility_trend'), c('age.cat.label','fsq_trend'))
setnames(mob_emodo, c('emo.age.label','mobility_trend'), c('age.cat.label','emo_trend'))
mob_fsq <- subset(mob_fsq, select=c(loc, loc_label, week, date, age.cat.label, fsq_trend, dip_date, rebound_date))
mob_emodo <- subset(mob_emodo, select=c(loc, loc_label, week, date, age.cat.label, emo_trend))
mob <- merge(mob_fsq, mob_emodo, by=c('loc','loc_label','week','date','age.cat.label'))

#	plot direct comparison
tmp <- reshape2::melt(mob, measure.vars=c('emo_trend','fsq_trend'))
ggplot(tmp, aes(x=date, y=value, colour=variable)) +
	scale_y_continuous(labels=scales::percent) +
	scale_x_date(expand=c(0,0)) +
    #scale_colour_viridis(discrete=TRUE,begin=0,end=.8*4/6,alpha=1,direction=-1,option='magma') +
    scale_colour_manual(
		values = c('emo_trend'='deepskyblue','fsq_trend'='deeppink4'),
		labels=c('emo_trend'='Emodo','fsq_trend'='Foursquare')) +
    labs(x='', y='mobility trend', colour='data') +
	geom_hline(yintercept=1) +
	geom_step(aes(linetype= )) +
	theme_bw() +
	facet_grid(loc_label~age.cat.label)+
  	theme(legend.position='bottom',
		legend.justification='left',
		legend.title = element_text(size = 10), 
		legend.text = element_text(size = 8),					
		strip.background= element_blank()) 
ggsave(file=paste0(outfile.base,'-ts.pdf'), w=length(unique(tmp$age.cat.label)) * 2, h=length(unique(tmp$loc_label)))


#
#	not so useful
cat(" \n -------------------------------- \n compare Emodo to FSQ trends: ranks over time \n -------------------------------- \n")
if(0)
{
	#	rank correlation by day
	tmp <- mob[, 
			list(
					age.cat.label=age.cat.label,
					fsq_trend_rank=sort(fsq_trend, index.return=TRUE)$ix,
					emo_trend_rank=sort(emo_trend, index.return=TRUE)$ix
			), 
			by=c('loc_label','date')]
	mob <- merge(mob,tmp,by=c('loc_label','date','age.cat.label'))
	
#	plot
#		olli: see comments on plot by week below
	tmp <- reshape2::melt(mob, measure.vars=c('emo_trend_rank','fsq_trend_rank'))
	ggplot(tmp, aes(x=date, y=value, colour=variable)) +		
			scale_x_date(expand=c(0,0)) +
			geom_vline(aes(xintercept=dip_date)) +
			geom_vline(aes(xintercept=rebound_date)) +
			geom_step(aes(linetype= )) +
			theme_bw() +
			facet_grid(loc_label~age.cat.label)+
			#scale_colour_viridis(discrete=TRUE,begin=0,end=.8*4/6,alpha=1,direction=-1,option='magma') +
			scale_colour_manual(
					values = c('emo_trend_rank'='deepskyblue','fsq_trend_rank'='deeppink4'),
					labels=c('emo_trend_rank'='Emodo','fsq_trend_rank'='Foursquare')) +
			labs(x='', y='mobility trend rank', colour='data') +
			theme(legend.position='bottom',
					legend.justification='left',
					legend.title = element_text(size = 10), 
					legend.text = element_text(size = 8),					
					strip.background= element_blank()) 
	ggsave(file=paste0(outfile.base,'-ts-ranks-daily.pdf'),  w=length(unique(tmp$age.cat.label)) * 2, h=length(unique(tmp$loc_label)))
	
#	rank by week
	mobw <- mob[, 
			list(				
					fsq_trend=mean(fsq_trend),
					emo_trend=mean(emo_trend)
			), 
			by=c('loc','loc_label','age.cat.label','week')]
	tmp <- mobw[, 
			list(
					age.cat.label=age.cat.label,
					fsq_trend_rank=sort(fsq_trend, index.return=TRUE)$ix,
					emo_trend_rank=sort(emo_trend, index.return=TRUE)$ix
			), 
			by=c('loc_label','week')]
	mobw <- merge(mobw,tmp,by=c('loc_label','week','age.cat.label'))
	
	
#	plot by week
#	olli: 	ok so we see that 18-24 are always ranking as lowest mobility in both data sets
#			no extremely clear pattern for other age bands
#			however 1) the mobility trends show that the other age groups are head-to-head in the Emodo data,
#				so no surprise that the ranks are highly variable
#			however 2) the main point is that there is no evidence for a large increase
#				in mobility among <35 year olds. at best the 25-34 are ranking head-to-head
	tmp <- reshape2::melt(mobw, measure.vars=c('emo_trend_rank','fsq_trend_rank'))
	ggplot(tmp, aes(x=week, y=value, colour=variable)) +		
			scale_x_continuous(expand=c(0,0)) +
			geom_step(aes(linetype= )) +
			theme_bw() +
			facet_grid(loc_label~age.cat.label)+
			#scale_colour_viridis(discrete=TRUE,begin=0,end=.8*4/6,alpha=1,direction=-1,option='magma') +
			scale_colour_manual(
					values = c('emo_trend_rank'='deepskyblue','fsq_trend_rank'='deeppink4'),
					labels=c('emo_trend_rank'='Emodo','fsq_trend_rank'='Foursquare')) +
			labs(x='calendar week', y='mobility trend rank', colour='data') +
			theme(legend.position='bottom',
					legend.justification='left',
					legend.title = element_text(size = 10), 
					legend.text = element_text(size = 8),					
					strip.background= element_blank()) 
	ggsave(file=paste0(outfile.base,'-ts-ranks-weekly.pdf'),  w=length(unique(tmp$age.cat.label)) * 2, h=length(unique(tmp$loc_label)))
	
	
	
# rank correlation
# olli: the point here is that we want to see if ages have similar ranks over time
#	from the figure by week, we know there is no strong signal
#	so let s not get into this
#	but: I don t see how the code below does the comparison that we want
	tmp <- mob[,{
				tmp <- cor.test(fsq_trend, emo_trend,method = "spearman")
				list(correlation=tmp$estimate,
						pvalue=tmp$p.value)
			}, by = c('loc_label','date','dip_date','rebound_date')]
	
#	save rds
	cat('\nWriting ',paste0(outfile.base,'-cor-mob-tables.rds'),' ...')
	saveRDS(list(tmp),
			file = paste0(outfile.base,'-cor-mob-tables.rds'), version = 2)
	
# plot
	ggplot(tmp, aes(x=date, y=correlation)) +		
			scale_x_date(expand=c(0,0)) +
			geom_vline(aes(xintercept=dip_date),col='red',linetype=1) +
			geom_vline(aes(xintercept=rebound_date),col='red',linetype=1) +
			geom_hline(aes(yintercept=0),col='red',linetype=2) +
			labs(x='', y='Spearman rank correlation coefficient') +
			geom_step()+
			theme_bw() +
			facet_grid(loc_label~.)+  
			theme(strip.background= element_blank()) 
	ggsave(file=paste0(outfile.base,'-ts-ranks-cor.pdf'), w=4, h=length(unique(tmp$loc_label)))
	
	
	ggplot(tmp, aes(x=date, y=pvalue)) +		
			scale_x_date(expand=c(0,0)) +
			geom_vline(aes(xintercept=dip_date),col='red',linetype=1) +
			geom_vline(aes(xintercept=rebound_date),col='red',linetype=1) +
			geom_hline(aes(yintercept=0.05),col='red',linetype=2) +
			labs(x='', y='p-value of Spearman rank correlation test') +
			geom_step()+
			theme_bw() +
			facet_grid(loc_label~.)+  
			theme(strip.background= element_blank()) 
	ggsave(file=paste0(outfile.base,'-ts-ranks-cor-pvalue.pdf'), w=4, h=length(unique(tmp$loc_label)))
	
}

cat(" \n -------------------------------- \n end of script \n -------------------------------- \n")