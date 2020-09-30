# post-processing-make-deaths-panel-plot.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-make-deaths-panel-plot.R \n \n -------------------------------- \n")

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
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200728c2_cmdstanv'
	args_dir[['out_dir']] <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200728c2_cmdstanv-30states_updatedeath2707'
	args_dir[['job_tag']] <- '30states_updatedeath2707'
	args_dir[['overwrite']] <- 0
	args_dir[['with_forecast']] <- 0
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

E_deathsByAge <- NULL

# set colors by age 
ggplotColours <- function(n=6, h=c(0, 360) +15){
	if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
	hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
ggplotCol = ggplotColours(nrow(plot.pars.basic$dages))


cat(" \n -------------------------------- \n summarise samples: start \n -------------------------------- \n")

#
# map age groups for state death data to model groupings
age_state <- map_deaths_ages(plot.pars.basic$deathByAge_data,
		plot.pars.basic$dages,
		plot.pars.basic$dc)

#
# summarise cumulated deaths
file <- paste0(outfile.base,'-summary-cum-deaths-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  	file2 <- paste0(outfile.base,'-stanout-E_deathsByAge-gqs.RDS')
  	cat("\n read RDS:", file2)
  	E_deathsByAge <- readRDS(file2)
  
	cat("\n ----------- summarise_deaths_byage_c ----------- \n")
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

E_deathsByAge <- NULL
gc()


cat(" \n -------------------------------- \n summarise samples: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n  generating parameter plots: start \n -------------------------------- \n")

#
# make data set for states with age-specific death data
df.list <- vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(df.list) <- names(plot.pars.basic$deathByAge_data$A_AD)
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
	
	#	store merged data
	df.list[[x]] <- copy(e_adeaths_c)		
}

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
	date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
	cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max))
	deaths_s <- subset(deaths_s, date<=date.max)
	for(x in names(plot.pars.basic$deathByAge_data$A_AD))
	{
		df.list[[x]] <- subset(df.list[[x]], dates<=date.max)
	}
}


#
# make overall deaths plots 
cat(" \n -------------------------------- \n generate overall death plots: start \n -------------------------------- \n")
cd_data <- copy(plot.pars.basic$death_data)
setnames(cd_data, c('state','code'), c('loc_label','loc'))
deaths.plots <- make_deathsoverall_overtime_plot( deaths_s, 
		cd_data, 		
		outfile.base)

cat(" \n -------------------------------- \n generate overall death plots: end \n -------------------------------- \n")

cat(" \n -------------------------------- \n generate three panel plots: start \n -------------------------------- \n")
p_deathsbyage <- vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p_deathsbyage) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(x in names(plot.pars.basic$deathByAge_data$A_AD))
{
  p_deathsbyage[[x]] <- plot_deaths_byage(df.list[[x]], 
                                          ggplotColours)
}

p1.1 <- vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p1.1) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD)){
  p1.1[[state]] <- p_deathsbyage[[state]][[1]]
}

p1.2 = vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p1.2) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD)){
  p1.2[[state]] <- p_deathsbyage[[state]][[2]] 
}

p1.3 = vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p1.3) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD)){
  p1.3[[state]] = ggarrange(p1.1[[state]],p1.2[[state]],nrow=1,common.legend = TRUE,legend="right")
}

xintercept.list = vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(xintercept.list) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD)){
  xintercept.list[[state]] <- p_deathsbyage[[state]][[3]]
}

p1 <- vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p1) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD))
{
	p1[[state]] <- grid.arrange( 
		deaths.plots[[state]] + 
			geom_vline(xintercept=xintercept.list[[state]], linetype=2, alpha = .7),
   		p1.3[[state]], 
		nrow = 1, 
		widths = c(1,2.2))
	file <- paste0(outfile.base, '-E_deathsByAge_panel-', state, '.png')
	cat("\nWrite to file ",file)
   	ggsave(file, p1[[state]], width=12,height=5,limitsize=FALSE)
}

p_deathsbyage <- vector('list',length(plot.pars.basic$deathByAge_data$A_AD))
names(p_deathsbyage) <- names(plot.pars.basic$deathByAge_data$A_AD)
for(state in names(plot.pars.basic$deathByAge_data$A_AD))
{
	p1.1[[state]] <- p1.1[[state]] +
		scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"),
								 limits = c(p1.1[[state]]$data$dates[1],
								 					 p1.1[[state]]$data$dates[length(p1.1[[state]]$data$dates)])) +
		theme_bw(base_size=14) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1),
					legend.position="bottom") +
    	guides(fill=guide_legend(title="Age band",nrow=1),
        	color=guide_legend(title="Age band"),
			shape=guide_legend(title="Age band"))
  	p1.2[[state]] <- p1.2[[state]] +
  		scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"),
  								 limits = c(p1.2[[state]]$data$dates[1],
  								 					 p1.2[[state]]$data$dates[length(p1.2[[state]]$data$dates)])) +
  		theme_bw(base_size=14) +
  		theme(axis.text.x = element_text(angle = 45, hjust = 1),
  					legend.position="bottom") +
  		guides(fill=guide_legend(title="Age band",nrow=1),
        	color=guide_legend(title="Age band"),
			shape=guide_legend(title="Age band"))
  	p_deathsbyage[[state]] <- ggarrange( p1.1[[state]],
		  p1.2[[state]],
		  nrow=1,
		  common.legend = TRUE,
		  legend="bottom")
  	file <- paste0(outfile.base,'-E_deathsByAge-', state, '.png')
  	ggsave(file, p_deathsbyage[[state]], w = 14, h=5)
}

cat(" \n -------------------------------- \n generate three panel plots: end \n -------------------------------- \n")
cat(" \n -------------------------------- \n \n completed post-processing-make-deaths-panel-plot.R \n \n -------------------------------- \n")
