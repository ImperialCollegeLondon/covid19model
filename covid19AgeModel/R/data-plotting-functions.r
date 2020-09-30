
`%notin%` = Negate(`%in%`)

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 viridis
#' @importFrom scales date_format label_percent
plot_demograpy = function(pop_by_age, plotdir){
  
  if (!is.na(plotdir)){
    # Calculate median age of states
    GroupedMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
      if (!is.null(sep)) {
        if (is.null(trim)) pattern <- ""
        else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\+"
        else pattern <- trim
        intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
        intervals[[length(intervals)]][2] <- 100
        intervals <- matrix(unlist(intervals), nrow = 2, byrow = FALSE)
      }
      Midpoints <- rowMeans(intervals)
      cf <- cumsum(frequencies)
      Midrow <- findInterval(max(cf)/2, cf) + 1
      L <- intervals[1, Midrow]    
      h <- diff(intervals[,Midrow]) 
      f <- frequencies[Midrow]       
      cf2 <- cf[Midrow - 1]         
      n_2 <- max(cf)/2               
      unname(L + (n_2 - cf2)/f * h)
    }
    pop_by_age <- pop_by_age %>% mutate(medage=0)
    slist <- unique(pop_by_age$code)
    for (i in slist) {
      pop_by_age$medage[pop_by_age$code==i] <- GroupedMedian(pop_by_age$pop[pop_by_age$code==i],levels(pop_by_age$age),sep='-',trim='cut')
    }
    
    totals <- subset(pop_by_age,code!='NYC') %>% 
      group_by(age) %>%
      summarise(pop:= sum(pop)) %>%
      ungroup() %>%
      mutate(popus= pop/sum(pop))  %>% select(-pop)
    
    # Compare distribution of age gps to national average
    pop_by_age <- left_join(pop_by_age,totals,by="age") %>% 
      mutate(diff=pop-popus) %>% 
      mutate(diffdir:=case_when(diff>=0~'positive',
                                TRUE~'negative')) %>%
      group_by(state) %>%
      mutate(diff.nean = mean(diff))

    # Population pyramids by state
    ggplot() +   # Fill column
      geom_bar(data=pop_by_age, aes(x = age, y = pop, fill=medage),stat = "identity",width = 1,alpha = 0.5) +   # draw the bars
      geom_bar(data=pop_by_age, aes(x = age, y = popus), width = 1,stat = "identity",color='black',size=1.1,alpha=0) +   # draw the bars
      scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="")) + coord_cartesian(ylim=c(0, 0.1)) +
      coord_flip() +
      labs(x="Age band",y = "Percent of total population") +
      facet_wrap(~forcats::fct_reorder(state, -medage), ncol=6) +
      theme_bw() + 
      scale_fill_gradient(low = "blue",
                           high = "red", space = "Lab" ) +
      theme(axis.text.y=element_text(size=10),legend.position="none",axis.text.x=element_text(size=12),strip.background = element_blank(),
            strip.text = element_text(size = 20),axis.title=element_text(size=20))
    ggsave(file=file.path(plotdir,'pop_pyramid.pdf'),height=29,width=21)
    
    # Difference vs. national average
    ggplot(subset(pop_by_age), aes(x = age, y = diff,fill=diffdir)) +   # Fill column
      geom_bar(stat = "identity", width =1) +   # draw the bars
      coord_flip() 	+
      theme_bw() + 
      scale_fill_viridis(discrete=TRUE,begin=0.1,end=1,alpha=1) +
      theme(axis.text.y=element_text(size=10),legend.position="none",axis.text.x=element_text(size=12),strip.background = element_blank(),
            strip.text = element_text(size = 20),axis.title=element_text(size=20)) +
      facet_wrap(~fct_reorder(pop_by_age$state, -pop_by_age$medage), ncol=6) +
      scale_y_continuous(limits = max(pop_by_age$diff) * c(-1,1),labels = label_percent(accuracy=1.0,suffix="")) +
      labs(x="Age band",y = "vs. national average (%)") 
    ggsave(file=file.path(plotdir,'pop_diff.pdf'),height=29,width=21)
  }
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom scales date_format label_percent
plot_decoupled_mobility_trends_2_parts_fsq <- function(mobility_data, plotdir=NA_character_, plotselect=1:4)
{
	states <- unique(mobility_data$loc)	
	states <- states[plotselect]
	plist <- vector('list', length(states))	
	names(plist) <- states
	
	for(m in 1:length(states))
	{
		fsq_m <- unique(subset(mobility_data, loc==states[m], select=-c(age.cat, age.cat.label, norm_visits, norm_visits_rate, base_visits_rate, base_mobility_trend)))
		fsq_m <- reshape2::melt(fsq_m, measure.vars=c('mobility_trend','eased_mobility_trend','mobility_multiplier'))
		set(fsq_m, NULL, 'variable', fsq_m[, factor(variable, levels=c('mobility_trend','eased_mobility_trend','mobility_multiplier'), labels=c("Mobility trend", "Eased mobility trend", "Multiplier"))])
		
		plist[[m]] <- ggplot(fsq_m) +
				geom_hline(yintercept=1, color = "black", size = 0.75) +
				geom_vline(xintercept=fsq_m$rebound_date[1], linetype='dotted') +
				geom_step(aes(x= date, y= value, colour= fsq.age.cat.label)) +
				facet_wrap(~variable, ncol = 3)+				
				scale_y_continuous(labels= label_percent(suffix="%")) +
				scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
				scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "magma") +				
				labs(x= '', y=' ', colour='Age band', title = fsq_m$loc_label[1]) +
				theme_bw() +				
				guides(colour=guide_legend(nrow=1)) +
				theme(
						plot.title = element_text(size=25, hjust = 0.5),
						legend.position="bottom",
						legend.title = element_text(size = 20), 
						legend.text = element_text(size = 16),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
						axis.text.y=element_text(size=14),
						axis.title.x=element_blank(),
						axis.title=element_text(size=24),
						strip.text = element_text(size = 20),
						panel.grid.major = element_blank(), 
						panel.grid.minor = element_blank(),
						panel.background = element_blank(), 
						strip.background = element_rect( color="black", fill="white", size=1, linetype="solid" )) 		
		if(m == length(states))
		{
			plist[[m]] <- plist[[m]] + labs(x = "Date")
		}
	}
	
	
	p <- ggpubr::ggarrange(plotlist=plist, 
						nrow=length(plotselect), 
						common.legend = TRUE, 
						legend="none") %>%
				gridExtra::grid.arrange(get_legend(plist[[1]]), heights=c(10,1))
	p <- annotate_figure(p,
				#bottom = text_grob("Date",  hjust = 0.5, size = 25, vjust = -6),
				left = text_grob("Change from the baseline", size = 25, rot = 90, vjust = 1))
	h <- 20
	cat("\n Write to file ", file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage.png")), "...")
	ggsave(p, file=file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage.png")), w = h*210/297, h = h) # DINA4 proportion	
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom scales date_format label_percent
plot_decoupled_mobility_trends_3_parts_fsq <- function(mobility_data, plotdir=NA_character_, plotselect=1:4)
{
	states <- unique(mobility_data$loc)	
	states <- states[plotselect]
	plist <- vector('list', length(states))	
	names(plist) <- states
	
	for(m in 1:length(states))
	{
		fsq_m <- unique(subset(mobility_data, loc==states[m], select=-c(age.cat, age.cat.label, norm_visits, norm_visits_rate, base_visits_rate)))
		fsq_m <- reshape2::melt(fsq_m, measure.vars=c('mobility_trend','baseline_mobility_trend','eased_mobility_trend','mobility_multiplier'))
		set(fsq_m, NULL, 'variable', fsq_m[, factor(variable, levels=c('mobility_trend','baseline_mobility_trend','eased_mobility_trend','mobility_multiplier'), labels=c("Mobility trend",'Baseline mobility trend', "Eased mobility trend", "Multiplier"))])
		
		plist[[m]] <- ggplot(fsq_m) +
			geom_hline(yintercept=1, color = "black", size = 0.75) +
			geom_vline(xintercept=fsq_m$dip_date[1], linetype='dotted') +
			geom_vline(xintercept=fsq_m$rebound_date[1], linetype='dotted') +
			geom_step(aes(x= date, y= value, colour= fsq.age.cat.label)) +
			facet_wrap(~variable, ncol = 4)+				
			scale_y_continuous(labels= label_percent(suffix="%")) +
			scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
			scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "magma") +				
			labs(x= '', y=' ', colour='Age band', title = fsq_m$loc_label[1]) +
			theme_bw() +				
			guides(colour=guide_legend(nrow=1)) +
			theme(
				plot.title = element_text(size=25, hjust = 0.5),
				legend.position="bottom",
				legend.title = element_text(size = 20), 
				legend.text = element_text(size = 16),
				axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
				axis.text.y=element_text(size=14),
				axis.title.x=element_blank(),
				axis.title=element_text(size=24),
				strip.text = element_text(size = 20),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_blank(), 
				strip.background = element_rect( color="black", fill="white", size=1, linetype="solid" )) 		
		if(m == length(states))
		{
		plist[[m]] <- plist[[m]] + labs(x = "Date")
		}
		#ggsave(plist[[m]], file=file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage",states[m],".png")), h = 4, w = 10) # DINA4 proportion	
	}
	
	
	p <- ggpubr::ggarrange(plotlist=plist, 
												 nrow=length(plotselect), 
												 common.legend = TRUE, 
												 legend="none") %>%
		gridExtra::grid.arrange(get_legend(plist[[1]]), heights=c(10,1))
	p <- annotate_figure(p,
											 #bottom = text_grob("Date",  hjust = 0.5, size = 25, vjust = -6),
											 left = text_grob("Change from the baseline", size = 25, rot = 90, vjust = 1))
	w <- 20
	cat("\n Write to file ", file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage.png")), "...")
	ggsave(p, file=file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage.png")), h = w*210/297, w = w) # DINA4 proportion
	#ggsave(p, file=file.path(plotdir, paste0("fsq_decoupled_mobility_breakpoint_byage.pdf")), h = 150, w = 15, limitsize=FALSE) # DINA4 proportion
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom scales date_format label_percent
plot_raw_mobility_data_emodo <- function(mobility_data, plotdir=NA_character_)
{	
	plotdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	if(!is.na(plotdir))
	{
		dc <- copy(mobility_data)	
		setnames(dc, 'avg_contacts', 'value')
		set(dc, NULL, 'emo.idx.age.label', dc[, factor(emo.idx.age.cat, levels=emo.idx.age.cat, labels=emo.idx.age.label)])
		set(dc, NULL, 'emo.pair.age.label', dc[, factor(emo.cont.age.cat, levels=emo.cont.age.cat, labels=emo.cont.age.label)])
		set(dc, NULL, 'emo.pair.age.label', dc[, factor(emo.pair.age.cat, levels=emo.pair.age.cat, labels=emo.pair.age.label)])
		
				
		slist <- unique(dc$loc)
		date.min <- min(dc$date)
		date.max <- max(dc$date)
		# plot marginal number of contacts from index individuals for each state
		cat("\n plot marginal number of cell phone contacts from index individuals ...")
		for (x in slist) 
		{
			#x<- 'NY'			
			dc_m <- subset(dc, loc==x & date>=date.min & date<=date.max)
			dc_m <- dc_m[, list(value=sum(value)), by=c('loc','loc_label','emo.idx.age.cat','emo.idx.age.label','date','weekend')]
			dc_mw <- unique(subset(dc_m, select=c(date, weekend)))
			y.max <- max(dc_m$value)
			ggplot(dc_m) +
					ggtitle(unique(dc_m$loc_label)) +
					geom_tile(data=dc_mw, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend = FALSE) +
					scale_fill_manual(values = c("transparent", "black")) +
					scale_x_date(expand=c(0,0), date_breaks = "weeks", labels = date_format("%e %b")) +
					geom_step(aes(x=date, y=value, colour=emo.idx.age.label), direction="vh")			+
					coord_cartesian(ylim=c(0,y.max*1.3)) +
					theme_bw() +
					labs(x= 'Date', y='Avg cell phone contacts per person per day', colour='Age band') +
					theme(legend.position=c(0.7,0.85),
							legend.title = element_text(size = 10), 
							legend.text = element_text(size = 8),
							plot.title = element_text(size = 24, face = "bold"),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
							axis.title.x = element_blank(),
							panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
							panel.background = element_blank()) +
					scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option="magma") +
					guides(colour=guide_legend(nrow=1))
			ggsave(file=file.path(plotdir, paste0('emodo_marg_index_contacts_by_age_',x,'.png')), w=14, h=4, limitsize=FALSE)		
		}
		
		# plot marginal total contacts for each state
		cat("\n plot marginal total contacts for each state ...")
		dc_w <- unique(subset(dc, select=c(date, weekend)))
		dc_m <- dc[, list(value=sum(value)), by=c('loc','loc_label','emo.idx.age.cat','emo.idx.age.label','date','weekend')]
		dc_m <- dc_m[, list(value= mean(value)), by=c('loc','loc_label','date','weekend')]
		y.max <- max(dc_m$value)
		ggplot(dc_m) +				
				geom_tile(data=dc_w, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend = FALSE) +
				scale_fill_manual(values = c("transparent", "black")) +
				scale_x_date(expand=c(0,0), date_breaks = "weeks", labels = date_format("%e %b")) +
				scale_y_continuous(expand=c(0,0)) +
				geom_step(aes(x=date, y=value, colour=loc_label), direction="vh")			+
				coord_cartesian(ylim=c(0,y.max*1.1)) +
				theme_bw() +
				labs(x= 'Date', y='Avg cell phone contacts per person per day', colour='US state') +
				theme(legend.position='bottom',
						legend.title = element_text(size = 10), 
						legend.text = element_text(size = 8),
						plot.title = element_text(size = 24, face = "bold"),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
						axis.title.x = element_blank(),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank()) +
				scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option="magma") +
				guides(colour=guide_legend(ncol=11))
		ggsave(file=file.path(plotdir, paste0('emodo_avg_index_contacts_by_state.png')), w=14, h=10, limitsize=FALSE)
		
		# plot marginal contacts for each state against national
		cat("\n plot marginal contacts for each state against national average ...")
		dc_w <- unique(subset(dc, select=c(date, weekend)))
		dc_m <- dc[, list(value=sum(value)), by=c('loc','loc_label','emo.idx.age.cat','emo.idx.age.label','date','weekend')]
		dc_m <- dc_m[, list(value= mean(value)), by=c('loc','loc_label','date','weekend')]		
		dc_n <- unique(subset(pop_info, select=c(loc, pop_total)))
		dc_n[, pop_total_p:= pop_total / sum(pop_total)]
		dc_n <- merge(dc_n, dc_m, by='loc')
		dc_n <- dc_n[, list(national_avg= sum(pop_total_p*value)), by=c('date','weekend')]
		dc_n <- merge(dc_m, dc_n, by=c('date','weekend'))
		
		y.max <- max(dc_n$value)		
		ggplot(dc_n) +				
				#geom_tile(data=dc_w, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend = FALSE) +
				#scale_fill_manual(values = c("transparent", "black")) +
				scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +
				scale_y_continuous(expand=c(0,0)) +
				geom_bar(aes(x = date, y = value, fill= loc_label), stat = "identity", colour='transparent', width = 1, alpha = 0.5, show.legend = FALSE)	+
				#geom_bar(aes(x = date, y = national_avg), stat = "identity", width = 1, color='black', fill='transparent', size=1) +   # draw the bars
				geom_step(aes(x=date, y= national_avg), colour='black', direction="mid", size=.4, show.legend = FALSE)	+
				#geom_step(aes(x=date, y= value, colour=loc_label), colour='black', direction="hv", show.legend = FALSE)	+
				scale_fill_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option="magma") +
				coord_cartesian(ylim=c(0, y.max*1.1)) +
				theme_bw() +
				labs(x= 'Date', y='Avg cell phone contacts per person per day') +
				facet_wrap(loc_label~., ncol=6) +
				theme(legend.position='bottom',
						legend.title = element_text(size = 10), 
						legend.text = element_text(size = 8),
						plot.title = element_text(size = 24, face = "bold"),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
						axis.title.x = element_blank(),
						strip.background = element_blank(),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank()) 
		ggsave(file=file.path(plotdir, paste0('emodo_avg_index_contacts_by_state_vs_nationalavg.png')), w=192, h=272, units='mm', limitsize=FALSE)		
	}
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom scales date_format label_percent
plot_raw_mobility_data_fsq <- function(mobility_data, plotdir=NA_character_)
{  
  	fsq <- mobility_data
  	
	if(!is.na(plotdir))
	{
		if(0){
			#	plot NYC
			fsq_ny <- subset(fsq, loc=="NY")
			ggplot(fsq_ny) +
					geom_tile(data=unique(subset(fsq_ny, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
					scale_fill_manual(values = c("transparent", "black")) +
					geom_step(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label), direction="vh") +
					geom_point(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label)) +
					theme_bw() +
					labs(x= 'date', y='norm_visits per capita', colour='age band') +
					facet_wrap(.~loc, scales='free') +
					scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"))
			ggsave(file=file.path(plotdir, 'fsq_200527_normvisitspc_usa_NY.pdf'), w=8, h=8)
		}
     
		if(0){ 
			#	plot by loc
			ggplot(fsq) +
					geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
					scale_fill_manual(values = c("transparent", "black")) +
					geom_step(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label), direction="vh") +
					geom_point(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label)) +
					theme_bw() +
					labs(x= 'date', y='norm_visits per capita', colour='age band') +
					facet_wrap(.~loc, scales='free', ncol=1) +
					theme(legend.position='top')+
					scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"))
			ggsave(file=file.path(plotdir, 'fsq_200527_normvisitspc_usa_allstates.pdf'), w=10, h=200, limitsize=FALSE)
		}
  
		if(0){
			#	plot by age band
			ggplot(fsq) +
					geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
					scale_fill_manual(values = c("transparent", "black")) +
					geom_step(aes(x=date, y=norm_visits_rate, colour=loc), direction="vh") +
					geom_text(aes(x=date, y=norm_visits_rate, label=loc), size=2) +
					#geom_point(aes(x=date, y=norm_visits_rate, colour=loc)) +
					theme_bw() +
					labs(x= 'date', y='norm_visits per capita', colour='state') +
					facet_wrap(.~fsq.age.cat.label, scales='free', ncol=1) +
					scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"))
			ggsave(file=file.path(plotdir, 'fsq_200527_normvisitspc_usa_allstates_by_age.pdf'), w=10, h=20, limitsize=FALSE)
		}
    
		if(0){
			fsqb <- subset(fsq, date<"2020-03-15")
			fsqbm <- fsqb[, list(norm_visits_mean= mean(norm_visits_rate)), by=c('loc','fsq.age.cat.label')]
			ggplot(fsqbm) +
					geom_bar(aes(x= loc, y= norm_visits_mean, fill=loc), stat='identity') +
					facet_grid(~fsq.age.cat.label, scales='free') +
					theme_bw() +
					coord_flip() +
					labs(x='', y='average norm_visits per capita', fill='state') 
			ggsave(file=file.path(plotdir, 'fsq_200527_normvisitspc_usa_allstates_by_age2.pdf'), w=20, h=10, limitsize=FALSE)
			
		}
    
		if(0){
			# Plots by state
			slist <- unique(fsq$loc_label)
			for (i in slist) {
				ggplot(subset(fsq,loc_label==i & date!=c("2020-02-01","2020-02-02"))) +
						ggtitle(i) +
						geom_area(aes(x=date, y=norm_visits_rate, fill=fsq.age.cat.label)) +
						theme_bw() +
						labs(x= 'Date', y='Norm_visits per capita', fill='Age band') +
						theme(legend.position=c(0.8,0.9),
								legend.title = element_text(size = 6), 
								legend.text = element_text(size = 6),
								plot.title = element_text(size = 24, face = "bold"),
								axis.text.x=element_text(angle=45, vjust = 0.3, hjust=0.3),
								panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
								panel.background = element_blank()) +
						guides(fill=guide_legend(nrow=1)) +
						scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"))
				ggsave(file=file.path(plotdir, paste0('fsq_200527_normvisitspc_usa_by_age_',i,'.pdf')), w=14, h=4, limitsize=FALSE)		
			}
		}

  	  	# Plots by state - to be combined with contact intensity plots later  		
		slist <- unique(fsq$loc)
  		for (i in slist) 
		{
  			# Raw trends as step plot
			fsq_m <- subset(fsq,loc==i)
			ggplot(fsq_m) +
					ggtitle(fsq_m$loc_label[1]) +
					scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b")) +
					geom_step(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label), direction="vh")			+
					coord_cartesian(ylim=c(0,max(fsq_m$norm_visits_rate)*1.3)) +
					theme_bw(base_size=18) +
					labs(x= 'Date', y='Visits per person per day', colour='Age band') +
				theme(legend.position=c(0.7,0.85),
							legend.title = element_text(size = 14), 
							legend.text = element_text(size = 12),
							plot.title = element_text(size = 24, face = "bold"),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
							axis.title.x = element_blank(),
							panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
							panel.background = element_blank()) +
					scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option="magma") +
					guides(colour=guide_legend(nrow=1))
			ggsave(file=file.path(plotdir, paste0('fsq_normvisitspc_usa_by_age_',i,'.png')), w=14, h=4, limitsize=FALSE)	
		}
    
    	# Projected visits per capita per day
		ggplot(fsq) +
				geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
				scale_fill_manual(values = c("transparent", "black")) +
				geom_step(aes(x=date, y=norm_visits_rate, colour=fsq.age.cat.label), direction="vh")            +
				scale_y_continuous(expand=c(0,0)) +
				theme_bw() +
				labs(x= ' ', y='Visits per person per day', colour='Age band', fill = "Weekend") +
				facet_wrap(~loc_label,ncol=6) +
				scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
				guides(colour=guide_legend(nrow=1)) +
				scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
				theme(legend.position="bottom",
						legend.title = element_text(size = 24), 
						legend.text = element_text(size = 20),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=16),
						axis.text.y=element_text(size=16),
						axis.title=element_text(size=28),
						axis.title.x = element_blank(),
						strip.text = element_text(size = 20),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank(),
						strip.background = element_rect(
								color="white", fill="white", size=1, linetype="solid"
						))
    	ggsave(file=file.path(plotdir, paste0('fsq_raw_usa_by_age_allstates','.pdf')), height=19,width=16, limitsize=FALSE)         
  	}
}

#' @export
#' @keywords internal
#' @importFrom scales date_format label_percent
#' @import tidyr forcats grid ggplot2
plot_mobility_trends_fsq <- function(mobility_data,
                                 plotdir=NA_character_)
{
	fsq <- unique(subset(mobility_data, select=-c(age.cat,age.cat.label)))
	
	
	# plot mobility trends as line plot
	ggplot(fsq) +
			geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_hline(yintercept = 0, color = 'black', size = 0.75) + 
			geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
			scale_y_continuous(breaks = seq(0.5,1.75,0.25),labels = label_percent(suffix="%")) +
			coord_cartesian(ylim=c(0.4,1.8)) +
			theme_bw() +
			labs(x= ' ', y='Mobility trends', colour='Age band', fill = "Weekend") +
			facet_wrap(~loc_label,ncol=6) +
			scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
			guides(colour=guide_legend(nrow=1)) +
			scale_x_date(expand=c(0,0),date_breaks = "months", labels = date_format("%e %b"))+
			theme(legend.position="bottom",
					legend.title = element_text(size = 20), 
					legend.text = element_text(size = 16),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
					axis.text.y=element_text(size=14),
					axis.title=element_text(size=24),
					axis.title.x = element_blank(),
					strip.text = element_text(size = 20),
					panel.grid.major = element_blank(), 
					panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_rect(
							color="white", fill="white", size=1, linetype="solid"
					))
	file <- file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_allstates','.pdf'))
	cat("\nWrite to", file)
	ggsave(file=file, height=21,width=16, limitsize=FALSE)		
	
	
	
	# mobility trends by state line plots - needed for supplement fig1
	if(1)
	{
		slist <- unique(fsq$loc)
		for (i in slist) 
		{
			fsq_m <- subset(fsq, loc==i)
			ggplot(fsq_m) +
					#geom_tile(data=unique(subset(fsqmt, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2,show.legend = FALSE) +
					#scale_fill_manual(values = c("transparent", "black")) +
					geom_hline(yintercept = 0, color = 'black') + 
					geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
					scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"))+
					scale_y_continuous(labels = label_percent(suffix="%")) +
					coord_cartesian(ylim=c(min(fsq_m$mobility_trend),max(fsq_m$mobility_trend)*1.4)) +
					theme_bw(base_size=18) +
					labs(x= ' ', y='Mobility trends', colour='Age band') +
					theme(legend.position=c(0.77,0.86),
								legend.title = element_text(size = 14), 
								legend.text = element_text(size = 12),
								plot.title = element_text(size = 24, face = "bold"),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
							axis.title.x = element_blank(),
							axis.text.y=element_text(size=12),
							panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
							panel.background = element_blank()) +
					scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
					guides(colour=guide_legend(nrow=1)) 
			file <- file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_',i,'.png'))
			cat("\nWrite to", file)
			ggsave(file=file, w=14, h=4, limitsize=FALSE)		
		}
	}
	 	
	y.max <- min(1.5, max(fsq$mobility_trend))
	y.min <- max(0.1,min(fsq$mobility_trend))	
	fsq[, plot_value:= pmin(y.max,pmax(y.min, mobility_trend))]
	ggplot(fsq) +			
			scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +
			scale_y_discrete(expand=c(0,0)) +
			geom_tile(aes(x= date, y= fsq.age.cat.label, fill=plot_value)) +
			scale_fill_viridis(begin=0, end=1, alpha=0.6, direction=1,option="magma", labels = scales::percent,
					values = scales::rescale(c(0.2,0.4,0.6,0.8,1,1.5)),breaks=seq(0.2,1.5,.2)) +
			labs(x= 'Date', y='Age', fill='Mobility trend ') +
			facet_wrap(loc_label~.,ncol=5) +			
			theme_bw() +
			theme(legend.position='bottom',
					legend.title = element_text(size = 10), 
					legend.text = element_text(size = 8),
					plot.title = element_text(size = 24, face = "bold"),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
					axis.title.x = element_blank(),
					panel.grid.major = element_blank(), 
					panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_blank()) +
			guides(fill = guide_colourbar(barheight = 0.5, barwidth = 10, direction="horizontal"))					
	file <- file.path(plotdir, paste0('fsq_mobilitytrendsheatmap_usa_by_age_allstates','.pdf'))
	cat("\nWrite to", file)
	ggsave(file=file, height=20,width=15, limitsize=FALSE)		
	
	
	# mobility trends by state heatmap for each state
	if(0)
	{
		slist <- unique(fsq$loc)
		for (x in slist) 
		{
			dc_m <- subset(fsq, loc==x)
			dc_m[, plot_value:= pmin(y.max,pmax(y.min, mobility_trend))]
			ggplot(dc_m) +			
					scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +
					scale_y_discrete(expand=c(0,0)) +
					geom_tile(aes(x= date, y= fsq.age.cat.label, fill=plot_value)) +
					scale_fill_viridis(begin=0, end=1, alpha=0.6, direction=1,option="magma", labels = scales::percent,
							values = scales::rescale(c(0.2,0.4,0.6,0.8,1,1.5)),breaks=seq(0.2,1.5,.2)) +
					labs(x= 'Date', y='Age band', fill='Mobility\ntrend') +
					facet_wrap(loc_label~.) +			
					theme_bw() +
					theme(legend.position='right',
							legend.title = element_text(size = 10), 
							legend.text = element_text(size = 8),
							plot.title = element_text(size = 24, face = "bold"),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
							axis.title.x = element_blank(),
							panel.grid.major = element_blank(), 
							panel.grid.minor = element_blank(),
							panel.background = element_blank(),
							strip.background = element_blank()) +
					guides(fill = guide_colourbar(barheight = 7, barwidth = 0.5, direction="vertical"))					
			file <- file.path(plotdir, paste0('fsq_mobilitytrendsheatmap_usa_by_age_',x,'.png'))
			cat("\nWrite to file",file)
			ggsave(file=file, w=14, h=4, limitsize=FALSE)	
		}								
	}
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom scales date_format label_percent
plot_mobility_trends_fsq_withbreakpoints <- function(mobility_data,
                                     plotdir=NA_character_)
{
  fsq <- unique(subset(mobility_data, select=-c(age.cat,age.cat.label)))
  
  
  # plot mobility trends as line plot
  ggplot(fsq) +
    geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
    scale_fill_manual(values = c("transparent", "black")) +
    geom_hline(yintercept = 0, color = 'black', size = 0.75) + 
    geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
    scale_y_continuous(breaks = seq(0.5,1.75,0.25),labels = label_percent(suffix="%")) +
    geom_vline(aes(xintercept = dip_date), color = 'black', size = 0.75, linetype = "dashed") + 
    geom_vline(aes(xintercept = rebound_date), color = 'black', size = 0.75, linetype = "dashed") + 
    coord_cartesian(ylim=c(0.4,1.8)) +
    theme_bw() +
    labs(x= ' ', y='Mobility trends', colour='Age band', fill = "Weekend") +
    facet_wrap(~loc_label,ncol=6) +
    scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
    guides(colour=guide_legend(nrow=1)) +
    scale_x_date(expand=c(0,0),date_breaks = "months", labels = date_format("%e %b"))+
    theme(legend.position="bottom",
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 16),
          axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=24),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 20),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(
            color="white", fill="white", size=1, linetype="solid"
          ))
  file <- file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_allstates_breakpoints','.pdf'))
  cat("\nWrite to", file)
  ggsave(file=file, height=21,width=16, limitsize=FALSE)
  
  # split trends into 2 pdfs
  p1 <- unique(fsq$loc)[1:28]
  p2 <- unique(fsq$loc)[29:length(unique(fsq$loc))]
  ggplot(subset(fsq, loc%in%p1)) +
  	geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
  	scale_fill_manual(values = c("transparent", "black")) +
  	geom_hline(yintercept = 0, color = 'black', size = 0.75) + 
  	geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
  	scale_y_continuous(breaks = seq(0.5,1.75,0.25),labels = label_percent(suffix="%")) +
  	geom_vline(aes(xintercept = dip_date), color = 'black', size = 0.75, linetype = "dashed") + 
  	geom_vline(aes(xintercept = rebound_date), color = 'black', size = 0.75, linetype = "dashed") + 
  	coord_cartesian(ylim=c(0.4,1.8)) +
  	theme_bw() +
  	labs(x= ' ', y='Mobility trends', colour='Age band', fill = "Weekend") +
  	facet_wrap(~loc_label,ncol=4) +
  	scale_colour_viridis_d(begin=0,end=.8,alpha=0.8,direction=-1,option='magma') +
  	guides(colour=guide_legend(nrow=1)) +
  	scale_x_date(expand=c(0,0),date_breaks = "months", labels = date_format("%e %b"))+
  	theme(legend.position="bottom",
  				legend.title = element_text(size = 24), 
  				legend.text = element_text(size = 20),
  				axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=16),
  				axis.text.y=element_text(size=16),
  				axis.title=element_text(size=28),
  				axis.title.x = element_blank(),
  				strip.text = element_text(size = 20),
  				panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  				panel.background = element_blank(),
  				strip.background = element_rect(
  					color="white", fill="white", size=1, linetype="solid"
  				))
  ggsave(file=file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_allstates_breakpoints_','p1','.pdf')), height=19,width=16, limitsize=FALSE)   
  
  ggplot(subset(fsq, loc%in%p2)) +
  	geom_tile(data=unique(subset(fsq, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
  	scale_fill_manual(values = c("transparent", "black")) +
  	geom_hline(yintercept = 0, color = 'black', size = 0.75) + 
  	geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
  	scale_y_continuous(breaks = seq(0.5,1.75,0.25),labels = label_percent(suffix="%")) +
  	geom_vline(aes(xintercept = dip_date), color = 'black', size = 0.75, linetype = "dashed") + 
  	geom_vline(aes(xintercept = rebound_date), color = 'black', size = 0.75, linetype = "dashed") + 
  	coord_cartesian(ylim=c(0.4,1.8)) +
  	theme_bw() +
  	labs(x= ' ', y='Mobility trends', colour='Age band', fill = "Weekend") +
  	facet_wrap(~loc_label,ncol=4) +
  	scale_colour_viridis_d(begin=0,end=.8,alpha=0.8,direction=-1,option='magma') +
  	guides(colour=guide_legend(nrow=1)) +
  	scale_x_date(expand=c(0,0),date_breaks = "months", labels = date_format("%e %b"))+
  	theme(legend.position="bottom",
  				legend.title = element_text(size = 24), 
  				legend.text = element_text(size = 20),
  				axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=16),
  				axis.text.y=element_text(size=16),
  				axis.title=element_text(size=28),
  				axis.title.x = element_blank(),
  				strip.text = element_text(size = 20),
  				panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  				panel.background = element_blank(),
  				strip.background = element_rect(
  					color="white", fill="white", size=1, linetype="solid"
  				))
  ggsave(file=file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_allstates_breakpoints_','p2','.pdf')), height=16,width=16, limitsize=FALSE)  
  
  # mobility trends by state line plots - needed for supplement fig1
  if(1)
  {
    slist <- unique(fsq$loc)
    for (i in slist) 
    {
      fsq_m <- subset(fsq, loc==i)
      ggplot(fsq_m) +
        #geom_tile(data=unique(subset(fsqmt, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2,show.legend = FALSE) +
        #scale_fill_manual(values = c("transparent", "black")) +
        geom_hline(yintercept = 0, color = 'black') + 
        geom_step(aes(x=date, y=mobility_trend, colour=fsq.age.cat.label), direction="vh")			+
        scale_x_date(expand=c(0,0),date_breaks = "weeks", labels = date_format("%e %b"))+
        scale_y_continuous(labels = label_percent(suffix="%")) +
        geom_vline(aes(xintercept = dip_date), color = 'black', size = 0.75, linetype = "dashed") + 
        geom_vline(aes(xintercept = rebound_date), color = 'black', size = 0.75, linetype = "dashed") + 
        coord_cartesian(ylim=c(min(fsq_m$mobility_trend),max(fsq_m$mobility_trend)*1.4)) +
        theme_bw() +
        labs(x= ' ', y='Mobility trends', colour='Age band') +
        theme(legend.position=c(0.79,0.88),
              legend.title = element_text(size = 10), 
              legend.text = element_text(size = 8),
              plot.title = element_text(size = 24, face = "bold"),
              axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
              axis.title.x = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option='magma') +
        guides(colour=guide_legend(nrow=1)) 
      file <- file.path(plotdir, paste0('fsq_mobilitytrends_usa_by_age_',i,"_breakpoints",'.png'))
      cat("\nWrite to", file)
      ggsave(file=file, w=14, h=4, limitsize=FALSE)		
    }
  }
  
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom gridExtra grid.arrange
plot_deaths_by_age_data <- function(deathByAge, pop_info, plotdir)
{
	
	# read death by age	
	deathByAge <- as.data.table(deathByAge)
	set(deathByAge, NULL, 'date', deathByAge[,as.Date(date)])
	setnames(deathByAge, c('code','age'), c('loc','age_cat'))
	
	tmp <- unique(subset(pop_info, select=c(loc,loc_label)))
	deathByAge <- merge(deathByAge, tmp, by='loc')
	
	#	remove rows with no deaths
	deathByAge <- subset(deathByAge, cum.deaths>0)
	
	#	calculate number of stacked deaths up to age band a
	deathByAge[, age_cat_from:= as.integer(gsub('\\+','',gsub('^([0-9]+)-([0-9]+)','\\1',age_cat)))]
	setkey(deathByAge, loc, date, age_cat_from)
	tmp <- deathByAge[, list( 	age_cat_from=age_cat_from,
								cum.deaths.stacked=cumsum(cum.deaths),
								cum.deaths.stacked.before=c(0, cumsum(cum.deaths[-length(cum.deaths)]))
								), by=c('loc','date')]
	#	calculate corresponding proportion of stacked deaths up to age band a					
	tmp <- tmp[, list(	date=date,
				age_cat_from=age_cat_from, 
				cum.deaths.stacked=cum.deaths.stacked,
				cum.deaths.stacked.p= cum.deaths.stacked/max(cum.deaths.stacked),
				cum.deaths.stacked.before=cum.deaths.stacked.before,
				cum.deaths.stacked.before.p= cum.deaths.stacked.before/max(cum.deaths.stacked)
				), by='loc']					
						
	deathByAge <- merge(deathByAge,tmp,by=c('loc','date','age_cat_from'))
	setkey(deathByAge, loc, date, age_cat_from)					
	
	deathByAge.first.date <- deathByAge[, min(date)]
	deathByAge.last.date <- deathByAge[, max(date)]
	deathByAge.loc_labels <- unique(deathByAge$loc_label)	
	
	deathByAge.np1 <- 4*4
	
	dps <- vector('list',length(deathByAge.loc_labels))
	for(i in seq_along(deathByAge.loc_labels) )
	{
		tmp <- subset(deathByAge, loc_label==deathByAge.loc_labels[i])[order(age_cat_from),]
		#tmp <- subset(deathByAge, loc_label=='Alaska')[order(age_cat_from),]
		tmp[, age_cat_2:= factor(age_cat_from, levels=unique(tmp$age_cat_from), labels=unique(tmp$age_cat) )]
		n.col = 1
		if(unique(tmp$loc) == "GA") n.col =2
		dps[[i]] <- ggplot(tmp, aes(x=date, ymin=cum.deaths.stacked.before.p, ymax=cum.deaths.stacked.p, fill=age_cat_2)) +
		  coord_cartesian(xlim=c(deathByAge.first.date, deathByAge.last.date), ylim=c(0,1)) +				
		  geom_ribbon(alpha = 0.8) +
		  geom_line(aes(y=cum.deaths.stacked.p), show.legend = FALSE, size = 0.5, alpha =0.5, color = "lightgrey") +
		  scale_x_date(expand=c(0,0), labels = date_format("%e %b")) +
		  scale_y_continuous(labels=scales:::percent, expand=c(0,0)) +
		  labs(x='', y='', fill='Age band') +
		  theme_bw() +
		  ggtitle(tmp$loc_label[1]) +
		  guides(fill=guide_legend(ncol=n.col)) +
		  theme(	plot.title = element_text(size=rel(1), hjust = 0.5),
		         #legend.position= 'bottom',
		         legend.position= c(0.3,0.55),
		         #legend.title=element_text(size=20),
		         legend.text=element_text(size=rel(.7)),
		         # text = element_text(size=20),
		         legend.background=element_blank(),
		         legend.key.size = unit(2, "mm"),
		         axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1))
		# remove ticks for facet which are not on the axis
		if(i %notin% c((deathByAge.np1 - 0:3), (4*4*2):(4*4*2-3),length(deathByAge.loc_labels):(length(deathByAge.loc_labels)-3)  ) ) dps[[i]] <- dps[[i]] + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
		if(i %notin% c((1 + 4*0:(4*3-1))) ) dps[[i]] <- dps[[i]] + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
	}

	p<- gridExtra::grid.arrange(	grobs=dps[1:deathByAge.np1], 
			ncol=4, 					
			left=text_grob('Cumulated reported COVID-19 deaths in percent', size=15, rot = 90),
			heights = c(1, 1, 1, 1.25),
			widths = c(1.3, 1, 1, 1))
	ggsave(file=file.path(plotdir, 'death_by_age_p1.png'), plot=p, width = 210, height = 297, units = "mm")
	p<- gridExtra::grid.arrange(	grobs=dps[(deathByAge.np1+1):(deathByAge.np1*2)], 
			ncol=4, 					
			left=text_grob('Cumulated reported COVID-19 deaths in percent', size=15, rot = 90),
			heights = c(1, 1, 1, 1.25),
			widths = c(1.3, 1, 1, 1))
	ggsave(file=file.path(plotdir, 'death_by_age_p2.png'), plot=p, width = 210, height = 297, units = "mm")	
	p<- gridExtra::grid.arrange(	grobs=dps[(deathByAge.np1*2+1):length(dps)], 
	                  ncol=4, 					
	                  left=text_grob('Cumulated reported COVID-19 deaths in percent', size=15, rot = 90),
	                  heights = c(1, 1, 1.25, 1.25),
	                  widths = c(1.3, 1, 1, 1))
	ggsave(file=file.path(plotdir, 'death_by_age_p3.png'), plot=p, width = 210, height = 297, units = "mm")	
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
plot_death_by_age_vs_deaths_overall <- function(deathByAge, death_data, pop_info, plotdir)
{
    
  # sum deaths over all age groups
  deathByAge_plot = as.data.table(deathByAge) %>%
    group_by(code, date) %>%
    summarise(cum.deaths = sum(cum.deaths)) %>%
    mutate(source = "Department of Health",
           date = as.Date(date)) %>%
    select(code, date, cum.deaths, source)
  
  # find cumulative deaths
  death_data_plot = subset(as.data.table(death_data), code %in% unique(deathByAge$code)) %>%
    group_by(code) %>%
    mutate(cum.deaths = cumsum(Deaths),
           source = "JHU or NYC Gitub Repository") %>%
    select(code, date, cum.deaths, source)
  
  tmp = rbind(deathByAge_plot, death_data_plot) %>%
    merge(select(pop_info, c("loc_label", "loc")), by.x = "code", by.y = "loc") %>%
    mutate(source = factor(source, levels = c("JHU or NYC Gitub Repository", "Department of Health")))
  
  deathByAge.first.date <- min(tmp$date)
  deathByAge.last.date <- max(tmp$date)

  p <- ggplot(tmp, aes(x=date, y = cum.deaths, col = source)) +
    coord_cartesian(xlim=c(deathByAge.first.date, deathByAge.last.date)) +				
    geom_line() +
    #geom_point(size = 0.5) +
    scale_x_date(expand=c(0,0), labels = date_format("%e %b")) +
    labs(x='', y='Cumulative Covid-19 deaths', colour='Source') +
    theme_bw() +
    facet_wrap(~loc_label, ncol = 5, scale = "free_y") +
    theme(	plot.title = element_text(size=rel(1), hjust = 0.5),
           #legend.position= 'bottom',
           legend.position= "bottom",
           #legend.title=element_text(size=20),
           legend.text=element_text(size=rel(1)),
           # text = element_text(size=20),
           legend.background=element_blank(),
           legend.key.size = unit(2, "mm"),
           axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1),
           panel.background = element_blank(), 
           strip.background = element_rect( color="white", fill="white", size=1, linetype="solid" )) +
    scale_color_manual(values = c("#999999", "#E69F00"))
  ggsave(p, file=file.path(plotdir, 'death_by_age_comp_overall.png'), width = 210, height = 297, units = "mm")
}


#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
plot_contact_matrix_polymod_countries = function(path_to_file, plotdir){
  
  polymod.tab = readRDS(path_to_file)
  
  polymod.tab$LOC_LABEL = as.factor(polymod.tab$LOC)
  levels(polymod.tab$LOC_LABEL) <- list(Belgium="BE", Germany="DE", Finland="FI", `United Kingdom`="GB",
                                        Italy = "IT", Luxembourg="LU",Netherlands="NL",Poland="PL")
  
  ggplot(polymod.tab, aes(x = part.age, y = cont.age)) +
    geom_tile(aes(fill=m)) +
    scale_y_continuous(expand = c(0,0),breaks = c(seq(0,90,10),99), labels= c(seq(0,90,10),99))+
    scale_x_continuous(expand = c(0,0),breaks = seq(0,90,10), labels= seq(0,90,10)) +
    labs(y="Age of contact",x = "Age of index person",fill="Predicted contact intensities") +
    theme_bw() +
    facet_wrap(~LOC_LABEL + TYPE,ncol=4) +
    theme(legend.position="bottom",
          axis.text.x=element_text(angle=70, vjust = 1, hjust=1,size=14),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=24),
          axis.title.x = element_text(vjust=-0.5),
          strip.text = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20, vjust=-1))  +
    scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,1)) +
    scale_color_continuous(breaks=seq(0,8,1)) +
    guides(fill = guide_colourbar(barwidth = 45, barheight = 0.5,direction="horizontal")) 
  ggsave(file=file.path(plotdir, paste0("contactmatrix_polymod",'.pdf')), height=24.5,width=21, limitsize=FALSE)
}


#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
plots_baseline_contact_matrices <- function(pop_info, dcontact, path_to_polymod_data, plotdir){
  
  # get population size/area info
  pop_size <- unique(subset(pop_info,select=c('loc_label','age.cat.label','prop_pop','pop_total','land_area_sqm')))
  
  # aggregate pop_info for US overall
  pop_info_us <- subset(pop_info,loc!='NYC') %>% 
    group_by(age.cat,age.cat.label,age.cat.from,age.cat.to) %>%
    summarise(pop:= sum(pop),land_area_sqm:=sum(land_area_sqm))	%>%
    ungroup() %>%
    mutate(pop_total=sum(pop),prop_pop= pop/pop_total,pop_sqm=pop/land_area_sqm,loc='USA',loc_label='USA') 
  pop_info_us <- data.table(pop_info_us)

  dcontact <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekday', path_to_polymod_data)
  dcontact[, type:='weekday']
  tmp <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekend', path_to_polymod_data)
  tmp[, type:='weekend']
  dcontact <- rbind(dcontact, tmp)
  
  # compare weekend/weekday
  weekday <- subset(dcontact,type=='weekday',select=-c(type))
  setnames(weekday, c('m'), c('mweekday'))
  weekend <- subset(dcontact,type=='weekend',select=-c(type))
  setnames(weekend, c('m'), c('mweekend'))
  dow <- merge(weekday,weekend,by=c('loc_label','part.age.cat','cont.age.cat','loc','cont.age.cat.label',
                                    'part.age.cat.label'))
  dow$logdiff <- log(dow$mweekend) - log(dow$mweekday)
  dow <- merge(dow,pop_size,by.x=c('loc_label','cont.age.cat.label'),by.y=c('loc_label','age.cat.label'))
  dow[,pop_sqm:= pop_total/land_area_sqm]
  
  # predict m for US total
  dcontactus <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info_us, 'weekday', path_to_polymod_data)
  dcontactus[, type:='weekday']
  tmp <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info_us, 'weekend', path_to_polymod_data)
  tmp[, type:='weekend']
  dcontactus <- rbind(dcontactus, tmp)	
  setnames(dcontactus, c('m'), c('averagem'))
  dcontactus <- subset(dcontactus,select=c('part.age.cat','cont.age.cat','type','averagem'))
  
  # calculate difference in m from national average predictions
  dcontact <- merge(dcontact,dcontactus,by=c('part.age.cat','cont.age.cat','type'))
  dcontact$diff <- dcontact$m - dcontact$averagem
  dcontact$logdiff <- log(dcontact$m) - log(dcontact$averagem)
  dcontact <- merge(dcontact,pop_size,by.x=c('loc_label','cont.age.cat.label'),by.y=c('loc_label','age.cat.label'))
  setnames(dcontact,c('prop_pop'),c('cont_pop_p'))
  dcontact[,pop_sqm:= pop_total/land_area_sqm]
  
  # calculate total contacts by index age groups
  marginal <- dcontact[,list(ma=sum(m),ma_us=sum(averagem)),by=c('part.age.cat','part.age.cat.label','type','loc','loc_label','pop_total','pop_sqm')]
  
  # labels for plots
  brks <- seq(1,18,1)
  lbls <- c(paste(seq(0,80,5),seq(4,84,5),sep="-"), "85-99")
  clabels <- c("70-74" = "Contact: 70-74", "75-79" = "Contact: 75-79", "80-84" = "Contact: 80-84","85+" = "Contact: 85+")
  ilabels <- c("85+"="Index: 85+","75-79"="Index: 75-79", "65-69" = "Index: 65-69", "55-59" = "Index: 55-59","45-49" = "Index: 45-49","35-39"="Index: 35-39","25-29"="Index: 25-29","15-19"="Index: 15-19","5-9"="Index: 5-9")
  dcontact$index.age.label <- factor(dcontact$part.age.cat.label,levels=c("85+","80-84","75-79","70-74","65-69","60-64","55-59","50-54","45-49",
                                                                          "40-44","35-39","30-34","25-29","20-24","15-19","10-14","5-9","0-4"))
  day <- unique(dcontact$type)
  for (i in day) {
    ggplot(subset(dcontact,type==i), aes(y = cont.age.cat, x = part.age.cat)) +
      geom_tile(aes(fill=m)) +
      scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100))+
      scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))+
      labs(y="Age of contact",x = "Age of index person",fill="Predicted contact intensities") +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x=element_text(angle=70, vjust = 1, hjust=1,size=14),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=24),
            axis.title.x = element_text(vjust=-0.5),
            strip.text = element_text(size = 20),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20, vjust=-1))  +
      scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,1)) +
      scale_color_continuous(breaks=seq(0,8,1)) +
      guides(fill = guide_colourbar(barwidth = 45, barheight = 0.5,direction="horizontal")) +
      facet_wrap(.~fct_reorder(loc_label,pop_sqm),ncol=6)
    ggsave(file=file.path(plotdir, paste0(i,'_baseline_cntct_matrix_usa_by_age_allstates','.pdf')), height=29.7,width=21, limitsize=FALSE)
    
    # plot log(diff) with national average
    ggplot(subset(dcontact,type==i), aes(y = cont.age.cat, x = part.age.cat)) + 
      geom_tile(aes(fill=logdiff)) +
      scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100))+
      scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))+
      labs(y="Age of contact",x = "Age of index person",fill="Log difference predicted contact intensities \n vs. national average") +
      theme_bw() + 
      theme(legend.position="bottom",
            axis.text.x=element_text(angle=70, vjust = 1, hjust=1,size=14),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=24),
            axis.title.x = element_text(vjust=-0.5),
            strip.text = element_text(size = 20),
            strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20, vjust=-1))  +
      scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0.2,0.6,0.9,1,1.5,2))) +
      guides(fill = guide_colourbar(barwidth = 45, barheight = 0.5,direction="horizontal"))		+
      facet_wrap(.~fct_reorder(loc_label,pop_sqm),ncol=6)
    ggsave(file=file.path(plotdir, paste0(i,'_logdiff_baseline_cntct_matrix_usa_by_age_allstates','.pdf')), height=29.7,width=21, limitsize=FALSE)		
    
    # plot contacts with older population by index age group
    ggplot(data=subset(dcontact,part.age.cat.label %in% c("5-9","15-19","25-29","35-39","45-49","55-59","65-69","75-79","85+") & cont.age.cat>=15 & type==i), 
           aes(x=cont_pop_p, y=m,group=reorder(loc_label,pop_sqm), color = log(pop_sqm))) +
      geom_point(size = 4) +
      scale_x_continuous(expand=c(0,0),breaks=seq(0,0.09,0.01),labels=percent) +
      theme_bw() +
      labs(x= '% of population in age band', y='Expected number of baseline contacts',col='State (by population density)') +
      theme(legend.position="right",
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14),
            axis.text.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=24),
            strip.text = element_text(size = 16),
            strip.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.spacing = unit(1, "lines")) +
      scale_colour_gradient(low = "blue", high = "red", space = "Lab", breaks = sort(unique(log(dcontact$pop_sqm))),
                            labels=unique(dcontact[order(pop_sqm)]$loc_label))  +
      guides(colour=guide_legend(ncol=1)) 	+
      facet_grid(index.age.label~cont.age.cat.label,switch="y",labeller=labeller(cont.age.cat.label=clabels,index.age.label=ilabels),
                 scales = "free_x")
    ggsave(file=file.path(plotdir, paste0(i,'_contactswith_olderpopulation','.pdf')), height=29, width=21, limitsize=FALSE)
    
    # plot expected contacts by index age group
    ggplot(data=subset(marginal, type==i),aes(x=part.age.cat,y=ma,col= log(pop_sqm))) +
      geom_step(direction="vh", size = 1.25) +
      geom_step(aes(x=part.age.cat,y = ma_us), color = 'black',direction="vh") + 
      theme_bw() +
      labs(x= 'Age of index person', y='Expected number of contacts at baseline') +
      theme(legend.position="none",
            legend.title = element_text(size = 10), 
            legend.text = element_text(size = 8),
            axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1,size=14),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=24),
            axis.title.x=element_text(vjust = -0.5),
            strip.text = element_text(size = 16),
            strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.spacing = unit(1, "lines")) +
      scale_colour_gradient(low = "blue", high = "red", space = "Lab" )  +
      #scale_x_continuous(expand=c(0,0),breaks=seq(1,18,1),labels=labs) +
      guides(colour=guide_legend(nrow=1)) 	+
      facet_wrap(.~fct_reorder(loc_label,pop_sqm),ncol=6) +
      scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))
    ggsave(file=file.path(plotdir, paste0(i,'_total_expected_contacts','.pdf')), height=29, width=21, limitsize=FALSE)
  }

  # plot log(diff) weekend vs. weekday
  ggplot(dow, aes(y = cont.age.cat, x = part.age.cat)) + 
    geom_tile(aes(fill=logdiff)) +
    scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100))+
    scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))+
    labs(y="Age of contact",x = "Age of index person",fill="Log difference predicted contact intensities \n weekend vs. weekday") +
    theme_bw() + 
    theme(legend.position="bottom",
          axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1,size=14),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=24),
          axis.title.x = element_text(vjust=-1),
          strip.text = element_text(size = 20),
          legend.title = element_text(size = 20, vjust=-1),
          legend.text = element_text(size = 20, vjust=-1),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  +
    scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(-0.3,0.04,0.4))) +
    guides(fill = guide_colourbar(barwidth = 45, barheight = 0.5,direction="horizontal"))		+
    facet_wrap(.~fct_reorder(loc_label,pop_sqm),ncol=6)
  ggsave(file=file.path(plotdir, paste0('logdiff_weekend_weekday_baseline_cntct_matrix_usa_by_age_allstates','.pdf')), height=29,width=21, limitsize=FALSE)		
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
plot_school_policy = function(path_to_file_school_intervention, plotdir)
{
  school_intervention <- as.data.table(read.csv(path_to_file_school_intervention))
  
  tmp = subset(school_intervention, !is.na(RegionName) & !is.na(Date) & RegionName != "" & !is.na(C1_School.closing))
  tmp[, date := as.Date(as.character(Date), format= "%Y%m%d")]
  tmp[, loc := gsub("US_(.+)","\\1",RegionCode)]
  tmp[, loc_label := RegionName]
  
  # remove territories
  tmp = subset(tmp, loc_label != "Virgin Islands")
  
  #
  # Fix Nevada 
  # in raw data, school were supposedly recommended to close on 2020-10-9
  # while they were ordered to close on 2020-03-16, ref http://www.doe.nv.gov/uploadedFiles/ndedoenvgov/content/home/DeclarationofEmergencyDirectiveSchools.pdf
  tmp1 = tmp[loc_label == "Nevada", C1_School.closing := ifelse(date >= as.Date("2020-03-16"), 3, 0)]
  tmp = rbind(subset(tmp, loc_label != "Nevada"), tmp1)
  

  #
  # Find number of states with school closure and max date
  tmp1 = unique(tmp[C1_School.closing != 0, list(flag = max(C1_School.closing), date = min(date)), by = c("loc")])
  nrow(subset(tmp1, flag == 1)) # recommend closing
  nrow(subset(tmp1, flag == 2 )) # require closing (only some levels or categories, eg just high school, or just public schools)
  nrow(subset(tmp1, flag == 3)) # require closing all levels
  subset(tmp1, loc == "DC")$flag
  
  #
  # Change Washington DC to District of Columbia
  tmp[loc_label == "Washington DC", loc_label:= "District of Columbia"]
  tmp[,sort(loc_label)]
  #
  # Plot
  first_date_plot = as.Date("2020-02-01")
  tmp2 = select(tmp, loc, loc_label, C1_School.closing, date)
  # find max date
  tmp1 = tmp2[, list(max_date = max(date)), by = "loc"]
  tmp2 = subset(tmp2, date < min(tmp1$max_date) & date >= first_date_plot)
  
  ggplot(subset(tmp2, C1_School.closing != 0), aes(x = date, y = reorder(loc_label, desc(loc_label)))) +
    geom_raster(aes(fill = as.factor(C1_School.closing))) +
    theme_bw() +
    labs(y = "", x = "") +
    geom_hline(yintercept = 1:length(unique(tmp2$loc_label))+0.5, col = "gray78") +
    theme(legend.position="bottom",
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          axis.text.x=element_text(size=14, angle = 70, hjust = 1),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(1, "lines")) +
    scale_fill_manual(name = "School status", values = c("coral1", "brown3", "red4"), 
                      labels = c("recommended closure", "required closing at least one level", "required closing all levels"), 
                      breaks = c(1,2,3) ) +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0.01,0)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  ggsave(file=file.path(plotdir, paste0('school_closure_status_bystate_overtime','.png')), height=13,width=10, limitsize=FALSE)		
}



#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom scales date_format label_percent
plot_school_closure_contactmatrix_UK_China = function(stan_data, path_to_file_contact_intensities_outbreak_China, path_to_file_contact_intensities_outbreak_UK)
{
  
  #
  # Contact matrices from Zhang et al. Science https://science.sciencemag.org/content/368/6498/1481
  # 
  
  contact_intensities_outbreak_China <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
  
  age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
  children_label = c("0-4", "5-9", "10-14")

  #
  # contacts to children
  tmp = subset(contact_intensities_outbreak_China, age_contact %in% children_label)
  tmp = tmp[, list(count_pre = sum(count_pre), count_post = sum(count_post) ), by = c("age_index", "city")]
  tmp[, multiplier := count_post/count_pre]
  tmp[, age_index := factor(age_index, levels = age_category)]
  tmp = reshape2::melt(tmp, id.vars = c("age_index", "city"))
  
  p1 = ggplot(subset(tmp, variable != "multiplier"), aes(x = age_index, y = value, col = variable, shape = variable)) +
    geom_point(size = 3) +
    ylab(paste0("Average number of contacts\n to age group 0-14")) +
    theme_bw() +
    xlab("from age") + 
    scale_color_manual(name = "Contacts", labels = c("pre-pandemic", "during outbreak"), values = c("#56B4E9", "#E69F00")) + 
    scale_shape_discrete(name = "Contacts", labels = c("pre-pandemic", "during outbreak")) + 
    facet_wrap(~city) +
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),
          strip.text = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(1, "lines"), 
          legend.position = c(0.85, 0.9),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) 
  
  p2 = ggplot(subset(tmp, variable == "multiplier"), aes(x = age_index, y = value)) +
    geom_point(shape = 18, fill = "black", size =4, color = "#999999") +
    ylab("Multiplier from pre-pandemic\n to during outbreak") +
    theme_bw() +
    xlab("from age") + 
    facet_wrap(~city) +
    theme( legend.title = element_text(size = 16), 
           legend.text = element_text(size = 14),
           axis.text.y=element_text(size=14),
           axis.title=element_text(size=20),
           strip.text = element_blank(),
           strip.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           panel.spacing = unit(1, "lines"),
           axis.text.x = element_text(size=14, angle = 70, vjust = 0.25, hjust=0.25))+ 
    scale_y_continuous(labels = scales::percent) 
  
  p = gridExtra::grid.arrange(
    grobs = list(p1, p2), 
    heights = c(0.6, 0.4), 
    widths = c(0.03, 1),
    layout_matrix = rbind(c(NA, 1),
                          c(2, 2)),
    left = textGrob(c("A", "B"), gp=gpar(fontsize=20), y = c(0.95, 0.45)) )
  
  ggsave(p, file = file.path(outdir, "Zhang_contacts_to_0-14.pdf"), w = 10, h = 10)
  
  
  #
  # contacts from children
  tmp = subset(contact_intensities_outbreak_China, age_index %in% children_label)
  tmp = tmp[, list(count_pre = mean(count_pre), count_post = mean(count_post) ), by = c("age_contact", "city")]
  tmp[, multiplier := count_post/count_pre]
  tmp[, age_contact := factor(age_contact, levels = age_category)]
  tmp = reshape2::melt(tmp, id.vars = c("age_contact", "city"))
  
  p1 = ggplot(subset(tmp, variable != "multiplier"), aes(x = age_contact, y = value, col = variable, shape = variable)) +
    geom_point(size = 3) +
    ylab(paste0("Average number of contacts\n from one child in 0-14")) +
    theme_bw() +
    xlab("to age") + 
    scale_color_manual(name = "Contacts", labels = c("pre-pandemic", "during outbreak"), values = c("#56B4E9", "#E69F00")) + 
    scale_shape_discrete(name = "Contacts", labels = c("pre-pandemic", "during outbreak")) + 
    facet_wrap(~city) +
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),
          strip.text = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(1, "lines"), 
          legend.position = c(0.85, 0.9),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) 
  
  p2 = ggplot(subset(tmp, variable == "multiplier"), aes(x = age_contact, y = value)) +
    geom_point(shape = 18, fill = "black", size =4, color = "#999999") +
    ylab("Multiplier from pre-pandemic\n to during outbreak") +
    theme_bw() +
    xlab("to age") + 
    facet_wrap(~city) +
    theme( legend.title = element_text(size = 16), 
           legend.text = element_text(size = 14),
           axis.text.y=element_text(size=14),
           axis.title=element_text(size=20),
           strip.text = element_blank(),
           strip.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           panel.spacing = unit(1, "lines"),
           axis.text.x = element_text(size=14, angle = 70, vjust = 0.25, hjust=0.25))+ 
    scale_y_continuous(labels = scales::percent) 
  
  p = gridExtra::grid.arrange(
    grobs = list(p1, p2), 
    heights = c(0.6, 0.4), 
    widths = c(0.04, 1),
    layout_matrix = rbind(c(NA, 1),
                          c(2, 2)),
    left = textGrob(c("A", "B"), gp=gpar(fontsize=20), y = c(0.95, 0.45)) )
  
  ggsave(p, file = file.path(outdir, "Zhang_contacts_from_0-14.pdf"), w = 10, h = 10)
  
  
}
