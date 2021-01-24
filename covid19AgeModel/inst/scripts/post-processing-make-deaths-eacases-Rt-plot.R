# post-processing-make-deaths-eacases-Rt-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-deaths-eacases-Rt-plot.R \n \n -------------------------------- \n")

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
if(1)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015f8_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015f8_cmdstanv-40states_tau10_Oct29_Levin'
	args_dir[['job_tag']] <- '40states_tau10_Oct29_Levin'
	args_dir[['overwrite']] <- 0
}

if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
	args_dir[['job_tag']] <- '19states_stdctn_2'
	
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	stopifnot(args_line[[7]]=='-overwrite')
	stopifnot(args_line[[9]]=='-with_forecast')
	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
	args_dir[['overwrite']] <- as.integer(args_line[[8]])
	args_dir[['with_forecast']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# set colors by age 
ggplotColours <- function(n=6, h=c(0, 360) +15){
	if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
	hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
ggplotCol = ggplotColours(nrow(plot.pars.basic$dages))

cat(" \n -------------------------------- \n  summarise deaths and case samples \n -------------------------------- \n")

# get start date for plots
date.min <- min( as.Date( sapply( plot.pars.basic$dates, function(x) min(as.character(x)) ) ) )

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

# map age groups for state death data to model groupings
age_state <- map_deaths_ages( plot.pars.basic$deathByAge_data,
		plot.pars.basic$dages,
		plot.pars.basic$dc)

#
# expected cumulative deaths
file <- paste0(outfile.base,'-summary-E_deathsByAge.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{
	file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_deathsByAge <- readRDS(file2)  
	e_adeaths <- make_cum_deaths_by_age_summaries(E_deathsByAge,
			plot.pars.basic$pop_info,
			plot.pars.basic$dates,
			age_state,
			plot.pars.basic$regions)	
	cat("\nWrite ",file," ... ")
	saveRDS(e_adeaths, file=file)
}
if(file.exists(file))
{
	e_adeaths <- readRDS(file)
}

#
#	summarise Rt by age
file <- paste0(outfile.base,'-summary-Rt-age_averageover', "1", 'days.RDS')
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
	                                                 period_length = 1,
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
if(nrow(subset(Rt_byage_c, loc == 'US')) > 0)
{
  Rt_byage_c = subset(Rt_byage_c, loc != 'US')
}

#
#	summarise effectively infectious cases by age
file <- paste0(outfile.base,'-summary-eff-infectious-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
	cat("\n ----------- summarise_e_acases_eff_byage_c ----------- \n")
	file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
	cat("\n read RDS:", file2)
	E_effcasesByAge <- readRDS(file2)
	
	cat("\n ----------- summarise_e_acases_eff_byage_c ----------- \n")
	e_acases_eff_byage_c <- summarise_e_acases_eff_byage_c(E_effcasesByAge,
			age_cat_map, 
			plot.pars.basic$pop_info, 
			plot.pars.basic$dates, 
			plot.pars.basic$regions)
	cat("\nWrite ",file," ... ")
	saveRDS(e_acases_eff_byage_c, file=file)
	gc()
}
if(file.exists(file))
{
	e_acases_eff_byage_c <- readRDS(file)
}
if(nrow(subset(e_acases_eff_byage_c, loc == 'US')) > 0)
{
	e_acases_eff_byage_c = subset(e_acases_eff_byage_c, loc != 'US')
}

E_effcasesByAge <- NULL
E_deathsByAge <- NULL
RtByAge <- NULL
gc()

cat(" \n -------------------------------- \n  generating standalone plots \n -------------------------------- \n")

cat("\n ----------- plot deaths by age ----------- \n")

p1.1 <- vector('list', length(names(plot.pars.basic$deathByAge_data$A_AD)))
p1.2 <- vector('list', length(names(plot.pars.basic$deathByAge_data$A_AD)))
p_deathsbyage <- vector('list', length(names(plot.pars.basic$deathByAge_data$A_AD)))
names(p_deathsbyage) <- names(plot.pars.basic$deathByAge_data$A_AD)
### FOR STATES WITH DEATH BY AGE DATA
for(x in names(plot.pars.basic$deathByAge_data$A_AD))
{
	e_adeaths_c <- subset(e_adeaths, loc==x)
	
	#	extract observed cumulated deaths
	tmp1 <- as.data.table(subset(plot.pars.basic$deathByAge, code == x, select=-code))
	set(tmp1, NULL, 'date', as.Date(tmp1$date, format='%Y-%m-%d'))
	set(tmp1, NULL, 'age', as.character(tmp1$age))
	setnames(tmp1, c('age'), c('age_band'))
	
	# regroup age band with age > 85
	max_age = unique(tmp1$age_band)[grepl("\\+", unique(tmp1$age_band))]
	if(as.numeric(gsub("(.+)\\+", "\\1", max_age)) > 85){
		
		# age bands specified by the state
		tmp2 = subset(tmp1, !duplicated(age_band))
		tmp2 = select(tmp2, -c("date", "cum.deaths", "daily.deaths"))
		n.age = nrow(tmp2)
		tmp2[, group := 1:n.age] 
		tmp1 = merge(tmp1, tmp2, by = "age_band")
		
		# for the last category (ending with string +), set the upper boundary to 99
		tmp2[ !grepl("\\+", age_band), age_band := age_band]
		tmp2[ grepl("\\+", age_band), age_band := paste0(gsub("(.+)\\+", "\\1", age_band), "-99") ]
		# find the lower boundaries of age band specified by the state
		tmp2[, age_min := suppressWarnings(as.numeric(gsub("(.+)-.*", "\\1", age_band)))]
		stopifnot(all(!is.na(tmp2$age_min)))
		
		# max of age_min conditional on being =< 85 
		age_max = tmp2$age_min[which.max( tmp2$age_min[ which(tmp2$age_min <= 85)] ) ]
		
		# new age bands
		tmp2[, g_age_max := age_min >= age_max]
		tmp2_new = subset(tmp2, !g_age_max)
		tmp2_new = rbind(tmp2_new, 
				data.table(age_band = paste0(min(subset(tmp2, g_age_max)$age_min), "+"),
						age_min = min(subset(tmp2, g_age_max)$age_min),
						group = min(subset(tmp2, g_age_max)$group)), fill = TRUE)
		
		# aggregate the new age bands (sum number of deaths over them)
		tmp2[ , newgroup := group ]
		tmp2[(g_age_max), newgroup := min(group) ]
		tmp1 = merge(tmp1, select(tmp2, c("newgroup", "group")), by = "group")
		tmp1 = tmp1[ , list(cum.deaths = sum(cum.deaths), 
						daily.deaths = sum(daily.deaths)),
				by = list(date, newgroup)]
		setnames(tmp1, "newgroup", "group")
		
		# new age band names
		tmp1 = merge(tmp1, tmp2_new, by = "group") 
		tmp1 = select(tmp1, c(date, age_band, cum.deaths, daily.deaths))
	}
	
	#	merge
	e_adeaths_c <- merge(e_adeaths_c, tmp1, by=c('date','age_band'), all.x=TRUE)
	e_adeaths_c[, age:= factor(age_cat, levels=age_cat, labels=age_band)]
	setnames(e_adeaths_c, 'date', 'dates')
	
  	#
	#	handle if forecast period is to be included in plots
  if(!args_dir$with_forecast)
  {
		date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	  	cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))	  
		e_adeaths_c <- subset(e_adeaths_c, dates<=date.max)
	}
   
	#	make plots
	p_deathsbyage[[x]] <- plot_deaths_byage(e_adeaths_c, ggplotColours)
	p1.1[[x]] <- p_deathsbyage[[x]][[1]]
	p1.2[[x]] <- p_deathsbyage[[x]][[2]]
	xintercept <- p_deathsbyage[[x]][[3]]
	
	# save deathsbyage with legend at bottom
	p1.1[[x]] <- p1.1[[x]] + guides(fill=guide_legend(title="Age band"),
		color=guide_legend(title="Age band"),
		shape=guide_legend(title="Age band"),nrow=1) + theme_bw(base_size=22) 	+
		scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
								 limits = c(date.min, 
								 					 p1.1[[x]]$data$date[length(p1.1[[x]]$data$date)])) +
		labs(x='',y=paste0('Fit to age-specific COVID-19 \nattributable deaths')) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		theme(legend.position="right")
		
	p1.2[[x]] <- p1.2[[x]] + guides(fill=guide_legend(title="Age band"),
		color=guide_legend(title="Age band"),
		shape=guide_legend(title="Age band"),nrow=1) + theme_bw(base_size=22)	+ 
		scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
								 limits = c(date.min, 
								 					 p1.2[[x]]$data$date[length(p1.2[[x]]$data$date)])) +
		labs(x='',y=paste0('Fit to age-specific COVID-19 \nattributable deaths')) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		theme(legend.position="right")

	p_deathsbyage[[x]] <- ggarrange(p1.1[[x]], 
		p1.2[[x]],
		nrow=1,
		common.legend = TRUE,
		legend="bottom")
}

cat("\n ----------- plot Rt/effectively infectious cases by age ----------- \n")

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))	  
	Rt_byage_c <- subset(Rt_byage_c, date<=date.max)
	e_acases_eff_byage_c <- subset(e_acases_eff_byage_c, date<=date.max)
}

p_aRt <- vector('list',length(plot.pars.basic$regions))
p_eacases_eff <- vector('list',length(plot.pars.basic$regions))
for(c in plot.pars.basic$regions)
{
	p_aRt[[c]] <- plot_Rt_byage_c(Rt_byage_c, 
		"aRt", 
		ylab='Time-varying reproduction numbers', 
		c, 
		outfile.base=NULL)	+ 
		scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
								 limits = c(date.min, 
								 					 max(Rt_byage_c$date))) +
		theme_bw(base_size=14) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")

	p_eacases_eff[[c]] <- plot_par_byage_c(e_acases_eff_byage_c, 
		"e_acases_eff", 
		ylab='Number of infectious individuals',
		c,
		outfile.base=NULL) +
		scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
		scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
								 limits = c(date.min, 
								 					 max(e_acases_eff_byage_c$date))) +
		theme_bw(base_size=14) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")
}	

cat(" \n -------------------------------- \n combinining plots to panel \n -------------------------------- \n")
snames <- c('NYC','FL','CA','AZ')
date.min <- min( as.Date( sapply( plot.pars.basic$dates[snames], function(x) min(as.character(x)) ) ) )

fig2c = list()
fig2cr = list()

for(c in plot.pars.basic$regions)
{
	# report figure 2
	fig2cr[[c]] <- ggarrange(p_eacases_eff[[c]],p_aRt[[c]],legend="bottom",common.legend=TRUE,labels=c('B','C'),ncol=2,font.label=list(size=20),align="h")
	fig2c[[c]] <- plot_grid(p_deathsbyage[[c]],fig2cr[[c]],labels=c('A'),rel_widths=c(2,3),nrow=1,label_size=20,align="h",axis="l")
	title <- ggdraw() + draw_label(plot.pars.basic$region_names$loc_label[plot.pars.basic$region_names$loc==c], size = 20, fontface='bold',x = 0,hjust = 0) + theme(plot.margin = margin(0, 0, 0, 7))
	fig2c[[c]] <- plot_grid(title, fig2c[[c]], ncol=1, rel_heights=c(0.1, 1),align="vh",axis="l")
	ggsave(paste0(outfile.base,'-figure_2_panel-', c, '.png'), fig2c[[c]], w = 14, h=5)
}

# combine plots for 4 states
if(all(snames %in% plot.pars.basic$regions )){
	for (c in snames){
		p1.1[[c]] <- p1.1[[c]] + 	
								scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
																						limits = c(date.min, 
																											 p1.1[[x]]$data$date[length(p1.1[[x]]$data$date)])) +
			theme(axis.title.y = element_blank())
		p1.2[[c]] <- p1.2[[c]] + 	
								scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
																						limits = c(date.min, 
																											 p1.2[[x]]$data$date[length(p1.2[[x]]$data$date)])) +
			theme(axis.title.y = element_blank())
		p_deathsbyage[[c]] <- ggarrange(p1.1[[c]], 
																		p1.2[[c]],
																		nrow=1,
																		common.legend = TRUE,
																		legend="bottom")
		p_eacases_eff[[c]] <- p_eacases_eff[[c]]  + 
			scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
									 limits = c(date.min, 
									 					 p_eacases_eff[[x]]$data$date[length(p_eacases_eff[[x]]$data$date)])) +
			theme_bw(base_size=22) +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_blank(),legend.position="bottom")
			p_aRt[[c]] <- p_aRt[[c]] +
				scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b"), 
										 limits = c(date.min, 
										 					 p_aRt[[x]]$data$date[length(p_aRt[[x]]$data$date)])) +
				theme_bw(base_size=22) +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.y = element_blank(),legend.position="bottom")
			if(c!=snames[length(snames)]){
				p1.1[[c]] <- p1.1[[c]] + theme(axis.text.x = element_blank(),legend.position="bottom")
				p1.2[[c]] <- p1.2[[c]] + theme(axis.text.x = element_blank(),legend.position="bottom")
				p_deathsbyage[[c]] <- ggarrange(p1.1[[c]], 
																				p1.2[[c]],
																				nrow=1,
																				common.legend = TRUE,
																				legend="bottom")
				p_eacases_eff[[c]] <- p_eacases_eff[[c]]  +
					theme(axis.text.x = element_blank(),legend.text = element_blank()
								, legend.title = element_blank()) + guides(fill=guide_legend(override.aes=list(fill="white",colour="white", shape = NA,alpha = 0),nrow=1))
				p_aRt[[c]] <- p_aRt[[c]] +
					theme(axis.text.x = element_blank(),legend.text = element_blank()
								, legend.title = element_blank()) + guides(color=guide_legend(override.aes=list(fill="white",colour="white", shape = NA,alpha = 0),nrow=1))
			}
			if(c==snames[1]){
				p_deathsbyage[[c]] <- ggarrange(p1.1[[c]], 
																				p1.2[[c]],
																				nrow=1,
																				common.legend = TRUE,
																				legend="bottom") 
				p_deathsbyage[[c]] <- annotate_figure(p_deathsbyage[[c]],
												top = text_grob("Fit to age-specific COVID-19 attributable deaths",size=26))
				p_eacases_eff[[c]] <- p_eacases_eff[[c]] + guides(fill=guide_legend(override.aes=list(fill="white",colour="white", shape = NA,alpha = 0),nrow=1))
				p_eacases_eff[[c]] <- annotate_figure(p_eacases_eff[[c]],
																								 top = text_grob("Number of infectious individuals",size=26))
				p_aRt[[c]] <- p_aRt[[c]] + guides(col=guide_legend(override.aes=list(fill="white",colour="white", shape = NA,alpha = 0),nrow=1))
				p_aRt[[c]] <- annotate_figure(p_aRt[[c]],
																			top = text_grob("Time-varying reproduction numbers",size=26))
			}
		fig2cr[[c]] <- ggarrange(p_eacases_eff[[c]],p_aRt[[c]],legend="bottom",common.legend=TRUE,ncol=2,align="h")
		fig2c[[c]] <- plot_grid(p_deathsbyage[[c]],fig2cr[[c]],rel_widths=c(2,3),nrow=1,align="hv",axis="b")
		title <- ggdraw() + draw_label(plot.pars.basic$region_names$loc_label[plot.pars.basic$region_names$loc==c], size = 28, fontface='bold',x = 0,hjust = 0) + theme(plot.margin = margin(0, 0, 0, 5))
		fig2c[[c]] <- plot_grid(title, fig2c[[c]], ncol=1, rel_heights=c(0.1, 1),align="vh",axis="l")
	}
	fig2 <- plot_grid(fig2c[[snames[1]]],fig2c[[snames[2]]],fig2c[[snames[3]]],fig2c[[snames[4]]],ncol=1,rel_heights=c(1.15,1,1,1.15))
	ggsave(paste0(outfile.base,'-figure_2_panel-4-states', '.png'), fig2, w = 25, h=28)
	ggsave(paste0(outfile.base,'-figure_2_panel-4-states', '.pdf'), fig2, w = 25, h=28,dpi=500)
}
cat(" \n -------------------------------- \n \n completed post-processing-make-deaths-eacases-Rt-plot.R \n \n -------------------------------- \n")

