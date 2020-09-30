# post-processing-fsq-mobility-analysis.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-fsq-mobility-analysis.R \n \n -------------------------------- \n")

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

#	for dev purposes
if(0)
{	
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703j3_cmdstanv'
	args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703j3_cmdstanv-19states_devcntct_samples1500_stepsize002_J29'
	args_dir[['job_tag']] <- '19states_devcntct_samples1500_stepsize002_J29'
	args_dir[['overwrite']] <- 0
}
if(0)
{	
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200729d13_cmdstanv'
	args_dir[['out_dir']] <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200729d13_cmdstanv-4states_deaths0727_biweekly_upswing_effect'
	args_dir[['job_tag']] <- '4states_deaths0727_biweekly_upswing_effect'
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
	#args_dir[['overwrite']] <- as.integer(args_line[[10]])
} 

figure.select.loc <- "CA"

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
		args_dir$stanModelFile , "-", args_dir$job_tag)



cat(" \n -------------------------------- \n make figures: start \n -------------------------------- \n")


# load inputs for making figure
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# this script must be run after post-processing-etas.R
file <- paste0(outfile.base,'-contact_patterns_over_time.rds')
cat("\n read RDS:", file)
cnts <- readRDS(file)


if(!figure.select.loc %in% plot.pars.basic$regions)
{
	figure.select.loc <- plot.pars.basic$regions[1]
	cat("\nResetting figure.select.loc to",figure.select.loc)
}

mobility_data <- copy(plot.pars.basic$mobility_data)
death_data <- copy(plot.pars.basic$death_data)
pop_info <- copy(plot.pars.basic$pop_info)

#
#	select FSQ age categories
mobility_data <- unique(subset(mobility_data, select=-c(age.cat, age.cat.label)))

#
#	mobility trend on first weekday after rebound date
ans <- unique(subset(mobility_data, weekend=='no', select=c(loc, fsq.age.cat.label, weekend, base_dip_mobility_trend)))
setnames(ans, 'base_dip_mobility_trend', 'rebound_date_mobility_trend')
#
#	mobility trend over the past 5 working days
tmp <- unique(subset(mobility_data, weekend=='no', select=date))[, tail(date,5)]
mmc <- subset(mobility_data, date%in%tmp)
mmc <- mmc[, list(current_mobility_trend=mean(mobility_trend)), by=c('loc','fsq.age.cat.label')]
ans	<- merge(ans, mmc, by=c('loc','fsq.age.cat.label'))

#
#	cumulative percent increase since rebound date
tmp <- subset(mobility_data, date>=rebound_date)	
tmp[, mobility_multiplier_pc:= mobility_multiplier-1]
tmp <- tmp[, list(avg_mobility_multiplier_pc= mean(mobility_multiplier_pc)), by=c('loc','fsq.age.cat.label')]
ans	<- merge(ans, tmp, by=c('loc','fsq.age.cat.label'))

#
#	cumulative percent increase over the past 5 working days
tmp <- unique(subset(mobility_data, weekend=='no', select=date))[, tail(date,5)]
tmp <- subset(mobility_data, date%in%tmp)		
tmp[, mobility_multiplier_pc:= mobility_multiplier-1]
tmp <- tmp[, list(avg_mobility_multiplier_pc_lastweek= mean(mobility_multiplier_pc)), by=c('loc','fsq.age.cat.label')]
ans	<- merge(ans, tmp, by=c('loc','fsq.age.cat.label'))

#
#	make supp figure 	
ans <- reshape2::melt(ans, id.vars=c('loc','fsq.age.cat.label','weekend'))	
tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
tmp[, loc_label2:= factor(1:nrow(tmp), levels=1:nrow(tmp), labels=loc_label)]
ans <- merge(tmp, ans, by='loc')
set(ans, NULL, 'variable', ans[,factor(variable, 
						levels=c("rebound_date_mobility_trend",									
								"avg_mobility_multiplier_pc",
								"avg_mobility_multiplier_pc_lastweek",
								"current_mobility_trend"), 
						labels=c('1-week mobility trend,\nrebound date',
								'average percent mobility increase\nsince rebound date',
								'average percent mobility increase\nrelative to rebound date',
								paste0('1-week mobility trend,\n',format(max(mobility_data$date),'%b %e'))									
						))])	
tmp <- subset(ans, variable%in%c('1-week mobility trend,\nrebound date','average percent mobility increase\nrelative to rebound date'))
tmp <- merge(tmp, unique(subset(pop_info, select=c(loc, pop_total))), by='loc')
tmp_ntl <- tmp[, list(value_national= sum(pop_total*value)/sum(pop_total)), by=c('fsq.age.cat.label','variable')]

p <- ggplot(tmp, aes(colour=fsq.age.cat.label)) +
		geom_hline(data=tmp_ntl, aes(yintercept=value_national, colour=fsq.age.cat.label), linetype='dashed') +
		geom_point(aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value)) +
		geom_line(aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value, group=fsq.age.cat.label)) +
		theme_bw() +
		scale_y_continuous(label=scales::percent) +
		scale_colour_viridis(discrete=TRUE,begin=0,end=.8,alpha=1,direction=-1,option='magma') +
		labs(x='', y='', colour='age band') +
		facet_grid(.~variable, scales='free') +
		theme(legend.position='bottom',
				#panel.grid.major.y = element_blank(), 
				#panel.grid.minor.y = element_blank(),
				strip.background= element_blank()) +
		coord_flip() +
		guides(colour=guide_legend(nrow=1)) 	
file <- paste0(outfile.base, '-supp_figure_fsq_mobility_relative_rebound.png')
ggsave(p, file=file, w=15, h=10)

#
#	make national part one of figure
fsq_a <- merge(mobility_data, unique(subset(pop_info, select=c(loc, pop_total))), by='loc')
fsq_a <- fsq_a[, list(dip_date=median(dip_date),
	rebound_date=median(rebound_date),
	mobility_trend= sum(pop_total*mobility_trend)/sum(pop_total)
	), by=c('fsq.age.cat','fsq.age.cat.label','date')]
plot.mobility.trend.ntl <- ggplot(fsq_a) +
		geom_hline(yintercept = 0, color = 'black') +
		geom_vline(xintercept = c(fsq_a$dip_date[1], fsq_a$rebound_date[1]), color = 'black', linetype='dashed') +
		geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh", show.legend = FALSE) +
		scale_x_date(expand=c(0,0),date_breaks = "weeks", labels = date_format("%e %b"))+
		scale_y_continuous(labels = label_percent(suffix="%")) +
		coord_cartesian(ylim=c(min(fsq_a$mobility_trend),max(fsq_a$mobility_trend)*1.0)) +
		theme_bw() +
		labs(x= ' ', y='Mobility trend \n(national average)', colour='Age band') +
		theme( axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
				axis.title.x = element_blank(),
				axis.title.y = element_text(size=10),
				panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.background = element_blank()) +
		scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
		guides(colour=guide_legend(nrow=1)) 


#
#	make part one of figure
fsq_m <- subset(mobility_data, loc==figure.select.loc)
plot.mobility.trend.CA <- ggplot(fsq_m) +
		geom_hline(yintercept = 0, color = 'black') +
		geom_vline(xintercept = c(fsq_m$dip_date[1], fsq_m$rebound_date[1]), color = 'black', linetype='dashed') +
		geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh", show.legend = FALSE) +
		scale_x_date(expand=c(0,0),date_breaks = "weeks", labels = date_format("%e %b"))+
		scale_y_continuous(labels = label_percent(suffix="%")) +
		coord_cartesian(ylim=c(min(fsq_m$mobility_trend),max(fsq_m$mobility_trend)*1.0)) +
		theme_bw() +
		labs(x= ' ', y='Mobility trend', colour='Age band') +
		theme( axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
				axis.title.x = element_blank(),
				axis.title.y = element_text(size=10),
				panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.background = element_blank()) +
		scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
		guides(colour=guide_legend(nrow=1)) 


#
#	make part two of figure
tmp <- subset(ans, variable==paste0('1-week mobility trend,\n',format(max(mobility_data$date),'%b %e')))
tmp <- merge(tmp, unique(subset(pop_info, select=c(loc, pop_total))), by='loc')
tmp_ntl <- tmp[, list(value_national= sum(pop_total*value)/sum(pop_total)), by=c('fsq.age.cat.label','variable')]
plot.crossectional.current.trends <- ggplot(tmp) +
		geom_hline(data=tmp_ntl, aes(yintercept=value_national, colour=fsq.age.cat.label), linetype='dashed') +
		geom_point(aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value, colour=fsq.age.cat.label)) +
		geom_line(aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value, colour=fsq.age.cat.label, group=fsq.age.cat.label), show.legend = FALSE) +
		theme_bw() +
		scale_y_continuous(label=scales::percent) +
		scale_colour_viridis(discrete=TRUE,begin=0,end=.8,alpha=1,direction=-1,option='magma') +
		labs(x='', y='', colour='age band') +
		facet_grid(.~variable, scales='free') +
		theme(legend.position='bottom',
				legend.justification='left',
				legend.title = element_text(size = 10), 
				legend.text = element_text(size = 8),					
				strip.background= element_blank()) +
		coord_flip() +
		guides(colour=guide_legend(nrow=1)) 	

#
#	make part three of figure
cnts <- subset(cnts, loc==figure.select.loc)
#cnts[, label:= paste0(dates, '\n',ifelse(wend==1,'(weekend)','(weekday)'))]
tmp <- unique(cnts$dates)
tmp <- tmp[c(1,3,5,7,9)]
cnts <- subset(cnts, dates%in%tmp)
plot.contacts.CA <-		ggplot(cnts, aes(y = age_cnt, x = age_index)) + 
		geom_tile(aes(fill=cnt_intensity)) +
		scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100))+
		scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))+
		labs(y="Age of contact",x = "Age of index person",fill="Estimated contact intensity\n(posterior median)") +
		theme_bw()   +
		scale_fill_viridis(begin=0,end=1,alpha=0.8,direction=1,option="viridis",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,1)) + # same scale as exploratry plot
		guides(fill = guide_colourbar(barwidth = 5, barheight = 0.5, direction="horizontal")) +
		facet_wrap(.~dates,ncol=2,labeller=labeller(label = label_wrap_gen(width = 10))) + 
		theme(legend.position="bottom",
				axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1),
				#axis.text.y=element_text(size=8),
				axis.title=element_text(size=10),
				axis.title.x = element_text(vjust=-1),
				#strip.text = element_text(size = 16),
				legend.title = element_text(size = 8),
				legend.text = element_text(size = 8),
				strip.background = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank())	

#	make full figure
plot.bottom <- ggarrange(plot.crossectional.current.trends, plot.contacts.CA, 
	labels=c('B','C'),
	font.label=list(size=14),
	ncol=2, 
	vjust=1, 		
	widths=c(3,3)
	)
plot.overall <- ggarrange(plot.mobility.trend.ntl, plot.bottom,		
	nrow=2,
	labels='A',
	heights=c(3,6)
	)
file <- paste0(outfile.base, '-figure_fsq_mobility.png')
cat("\nWrite to file",file,"...")
ggsave(plot.overall, file=file, w=15, h=15)


cat(" \n -------------------------------- \n make figures: end \n -------------------------------- \n")


if(0)
{
	setnames(death_data, c('state','code'), c('loc_label','loc'))
	setkey(death_data, loc, date)
	tmp <- death_data[, list(date=date, c_deaths=cumsum(Deaths)), by=c('loc')]
	death_data <- merge(death_data, tmp, by=c('loc','date'))
	
	#
	#	variation in rebound time vs 10th death
	drt <- unique(subset(mobility_data, select=c(loc, loc_label, rebound_date)))
	tmp <- death_data[c_deaths>=10, list(cd10_date=min(date)), by='loc']
	drt <- merge(drt, tmp, by='loc')
	ggplot(drt, aes(x=cd10_date, y=rebound_date)) + geom_abline(slope=1, intercept=0) + geom_point() + theme_bw()	
}

cat(" \n -------------------------------- \n \n End: post-processing-fsq-mobility-analysis.R \n \n -------------------------------- \n")