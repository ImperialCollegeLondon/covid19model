#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_trace_plot <- function(fit, target.pars, outfile.base, fig.type='.pdf')
{
  color_scheme_set("mix-blue-red")
  target.pars2 <- names(fit)[ grepl(paste(paste0('^',target.pars),collapse = '|'),names(fit)) ]
  if(length(target.pars2)==0 & any(grepl('\\[',target.pars)))
  {
	  target.pars2 <- target.pars
  }	    
  p <- rstan::traceplot(fit, 
		  pars= target.pars2,  
		  inc_warmup=TRUE, 
		  ncol = 1)
  cat('\n write trace plot', paste0(outfile.base, "-trace_plot",fig.type))
  ggsave(file=paste0(outfile.base, "-trace_plot",fig.type),p,w=20, h=length(target.pars2)*3,limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_parcoord_plot <- function(fit, outfile.base,fig.type='.png')
{
  color_scheme_set("darkgray")
  
  np <- nuts_params(fit)
  posterior <- as.array(fit)
  p <- mcmc_parcoord(posterior, 
                     np = np,
                     pars = names(fit)[!grepl('lp_',names(fit))])+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

  cat('\n write parallel coordinates plot', paste0(outfile.base, "-parcoord_plot",fig.type))
  ggsave(file=paste0(outfile.base, "-parcoord_plot",fig.type),p,w=length(names(fit)), h=5,limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_pairs_plot <- function(fit, target.pars, outfile.base, h, w, fig.type='.pdf', target.pars.prefix='^')
{
	color_scheme_set("mix-blue-red")		
	
	#	pair plots
	target.pars2 <- names(fit)[ grepl(paste(paste0(target.pars.prefix,target.pars),collapse = '|'),names(fit)) ]
	if(length(target.pars2)==0 & any(grepl('\\[',target.pars)))
	{
		target.pars2 <- target.pars
	}		
	tmp <- rstan::extract(fit, pars=target.pars2 )
	tmp <- unlist(tmp)	
	tmp <- matrix(tmp, ncol=length(target.pars2), byrow=FALSE)
	colnames(tmp) <- target.pars2
	
	p <- bayesplot::mcmc_pairs(tmp, diag_fun = "dens", off_diag_fun = "hex")
	cat('\n write pairs plot', paste0(outfile.base, "-pair_plot",fig.type))
	ggsave(file=paste0(outfile.base, "-pair_plot",fig.type),p,w=w, h=h,limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_pairs_plot_from_samples <- function(samples, outfile.base, h, w,fig.type='.pdf')
{
  color_scheme_set("mix-blue-red")		
  
  p <- bayesplot::mcmc_pairs(samples, diag_fun = "dens", off_diag_fun = "hex")
  cat('\n write pairs plot', paste0(outfile.base, "-pair_plot",fig.type))
  ggsave(file=paste0(outfile.base, "-pair_plot",fig.type),p,w=w, h=h,limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr 
make_state_pairs_plot <- function(fit, target.pars, outfile.base, h, w, fig.type='.pdf')
{
  color_scheme_set("mix-blue-red")		

  tmp <- rstan::extract(fit, pars=target.pars )
  tmp <- unlist(tmp)	
  tmp <- matrix(tmp, ncol=length(target.pars), byrow=FALSE)
  colnames(tmp) <- target.pars
  
  p <- bayesplot::mcmc_pairs(tmp, diag_fun = "dens", off_diag_fun = "hex")
  cat('\n write pairs plot', paste0(outfile.base, "-pair_plot",fig.type))
  ggsave(file=paste0(outfile.base, "-pair_plot",fig.type),p,w=w, h=h,limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom bayesplot mcmc_intervals
make_posterior_intervals <- function(tmp,pars,xtick=NULL,xintercept=NULL,xmin=NULL,
																		 xlab=NULL,label=0,outfile.base, logscale = 0){
	cat("\n ----------- make ", pars ," figure ----------- \n")
	if(is.null(xlab)){
		xlab <- pars
	}
	
	if(is.null(xtick)){
		xtick <- 1:ncol(tmp)
	}
	
	colnames(tmp) <- xtick
	
	gv <- bayesplot::mcmc_intervals(tmp,prob = .9) +
		geom_vline(xintercept=xintercept) +
		labs(x=pars) +
		scale_y_discrete(labels=colnames(tmp)) +
		theme_bw()
	
	if(logscale){
		gv <- gv + scale_x_log10()
	}
	
	ggsave(paste0(outfile.base,'-',pars,'.png'),gv,width=4,height=(length(xtick)/2 + 0.3))
	
	###
	
	if(!label){
		tmp <- as.matrix( tmp[, rev(seq_len(ncol(tmp)))], nrow(tmp) , ncol = ncol(tmp) )
		if(ncol(tmp) == 1) colnames(tmp) <- xtick
	}
	
	gh <- bayesplot::mcmc_intervals(tmp,prob = .9) +
		geom_vline(xintercept=xintercept) +
		labs(x=xlab) +
		coord_flip() +
		theme_bw() +
		theme(axis.text.y=element_text(size=14),
					axis.title.y=element_text(size=18,angle=90))
	
	if(label){
		gh <- gh + scale_y_discrete(labels=colnames(tmp))
	}	
	if(logscale){
		gh <- gh + scale_x_log10()
	}
	if(!is.null(xmin)){
		gh <- gh + scale_x_continuous(expand=c(0,0),limits = c(0, NA))
	}
	if(ncol(tmp)>3) ggsave(paste0(outfile.base,'-',pars,'.png'),gh,width=(length(xtick)/2 + 0.3),height=4)
	
	return(gh)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom bayesplot mcmc_intervals
make_posterior_intervals_R0 <- function(tmp,pars,xtick=NULL,xintercept=NULL,xmin=NULL,
																		 xlab=NULL,outfile.base){
	cat("\n ----------- make ", pars ," figure ----------- \n")
	if(is.null(xlab)){
		xlab <- pars
	}
	
	if(is.null(xtick)){
		xtick <- 1:ncol(tmp)
	}
	
	colnames(tmp) <- xtick
	
	###
	
	gh <- bayesplot::mcmc_intervals(tmp,prob = .9) +
		geom_vline(xintercept=xintercept) +
		labs(x=xlab) +
		coord_flip() +
		scale_y_discrete(position = "right",labels=colnames(tmp)) +
		scale_x_log10() +
		theme_bw() +
		theme(axis.title.x=element_blank(),
					axis.ticks.x=element_blank(),
					axis.title.y=element_text(size=18,angle=90),
					axis.text.y=element_text(size=14),
					axis.text.x=element_text(size=14,angle=90, vjust = 0.5, hjust=0))
	
	if(!is.null(xmin)){
		gh <- gh + scale_x_continuous(expand=c(0,0),limits = c(0, NA))
	}
	ggsave(paste0(outfile.base,'-',pars,'.png'),gh,width=4,height=(length(xtick)/2 + 0.3))
	
	return(gh)
}

#' @export
#' @keywords internal
#' @import grid ggplot2 ggpubr
make_state_parameter_summary_plot <- function(ggplot.list, outfile.base){
	cat("\n ----------- make state_parameter_summary figure ----------- \n")
	
	g <- ggarrange(plotlist=ggplot.list,ncol=1,align="v",heights=c(1.3,rep(1,length(ggplot.list)-1)))
	ggsave(paste0(outfile.base,'-model_parameters_all_states','.png'), g, w = 21, h = 29)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales rescale percent
plot_contact_patterns_over_time <- function(cnts, fsq.infile.dir, outfile.base)
{	
	cat("\n ----------- make contact_patterns_over_time figures ----------- \n")
	for(x in unique(cnts$loc))
  	{
		tmp <- subset(cnts, loc==x)
		tmp[, label:= paste0(dates, '\n',ifelse(wend==1,'(weekend)','(weekday)'))]
		cm <-		ggplot(tmp, aes(y = age_cnt, x = age_index)) + 
				geom_tile(aes(fill=cnt_intensity)) +
				scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100))+
				scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5)))+
				labs(y="Age of contact",x = "Age of index person",fill="Estimated contact intensity\n(posterior median)") +
				theme_bw()   +
				scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,1)) + # same scale as exploratry plot
				guides(fill = guide_colourbar(barwidth = 45, barheight = 0.5,direction="horizontal")) +
				facet_wrap(.~label,ncol=5,labeller=labeller(label = label_wrap_gen(width = 10))) + 
				theme(legend.position="bottom",
						axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1,size=11),
						axis.text.y=element_text(size=11),
						axis.title=element_text(size=20),
						axis.title.x = element_text(vjust=-1),
						strip.text = element_text(size = 16),
						legend.title = element_text(size = 20, vjust=-1),
						legend.text = element_text(size = 20, vjust=-1),
						strip.background = element_blank(),
						panel.grid.major = element_blank(),
						panel.grid.minor = element_blank())	
		ggsave(paste0(outfile.base,'-contact_patterns_over_time-', x, '.png'), cm, w = 14, h = 7)
		# fig 1 combine with fsq trends
		fsq <- image_read(file.path(fsq.infile.dir, paste0("fsq_normvisitspc_usa_by_age_",x, ".png")),density=2000)
		cm <- image_read(paste0(outfile.base,'-contact_patterns_over_time-', x, '.png'),density=2000)
		g <- image_append(image_scale(c(fsq, cm), "5000"), stack = TRUE)
		image_write(g, paste0(outfile.base,'-fsqtrends_contact_patterns_over_time-', x, '.png'))
  	}										
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales rescale percent
make_flow_sources_prop_onward_prop_side_by_side_plot <-function(flow, outfile.base, method='barplot')
{  
  cat("\n ----------- make flow_sources_prop_onward_prop_side_by_side_plot ----------- \n")
  for(x in unique(flow$loc))
  {
	  df <- subset(flow, loc==x & stat=='rec_prop')	  
	  setnames(df, colnames(df), gsub('source','index',colnames(df)))
	  setnames(df, colnames(df), gsub('rec','fill',colnames(df)))
	  
	  tmp <- subset(flow, loc==x & stat=='sources_prop')	  
	  setnames(tmp, colnames(tmp), gsub('source','fill',colnames(tmp)))
	  setnames(tmp, colnames(tmp), gsub('rec','index',colnames(tmp)))
	  
	  df <- rbind(df, tmp)
	  set(df, NULL, 'stat', df[, factor(stat, levels=c('sources_prop','rec_prop'), labels=c('sources of infections in age band','transmissions from age band'))])
	  if(method=='barplot')
	  {
		  g <- ggplot(df,aes(x=date, y=M, fill=fill_age_cat_label)) +
				  geom_bar(stat='identity',position = "stack") +
				  facet_grid(index_age_cat_label~stat, scales = 'free') +
				  labs(x='', y='', fill='Age band') +
				  scale_fill_viridis_d(begin=0,end=1,direction=-1) +
				  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
				  scale_y_continuous(label=scales::percent, expand=c(0,0)) +
				  theme_bw() +
				  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
						  panel.spacing = unit(1, "lines")) +
				  theme(legend.position="bottom") +
				  guides(fill = guide_legend(nrow = 1)) +
				  theme(panel.background = element_blank(), 
						  strip.background = element_blank()
				  )
	  }
	  if(method!='barplot')
	  {
		  g <- ggplot(df,aes(x=date, y=M, fill=fill_age_cat_label)) +
				  geom_area(position = "stack") +
				  facet_grid(index_age_cat_label~stat, scales = 'free') +
				  labs(x='', y='', fill='Age band') +
				  scale_fill_viridis_d(begin=0,end=1,direction=-1) +
				  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
				  scale_y_continuous(label=scales::percent, expand=c(0,0)) +
				  theme_bw() +
				  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
						  panel.spacing = unit(1, "lines")) +
				  theme(legend.position="bottom") +
				  guides(fill = guide_legend(nrow = 1)) +
				  theme(panel.background = element_blank(), 
						  strip.background = element_blank()
				  )
	  }	  
	  cat("\nWrite to file", paste0(outfile.base,'-',x,'.png'))
	  ggsave(paste0(outfile.base,'-',x,'.png'),g, width=10, height= length(unique( df$index_age_cat_label ))/1.5 + 1)	  	  
  }  
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_flow_sources_onward_side_by_side_plot <-function(flow, outfile.base, method='barplot')
{  
	cat("\n ----------- make flow_sources_onward_side_by_side_plot ----------- \n")
	for(x in unique(flow$loc))
	{
		df <- subset(flow, loc==x )	  
		setnames(df, colnames(df), gsub('source','index',colnames(df)))
		setnames(df, colnames(df), gsub('rec','fill',colnames(df)))
		set(df, NULL, 'stat', 'rec_abs')
		
		tmp <- subset(flow, loc==x)	  
		setnames(tmp, colnames(tmp), gsub('source','fill',colnames(tmp)))
		setnames(tmp, colnames(tmp), gsub('rec','index',colnames(tmp)))
		set(tmp, NULL, 'stat', 'sources_abs')
		
		df <- rbind(df, tmp)
		set(df, NULL, 'stat', df[, factor(stat, levels=c('sources_abs','rec_abs'), labels=c('sources of infections in age band','transmissions from age band'))])
		if(method=='barplot')
		{
			g <- ggplot(df,aes(x=date, y=M, fill=fill_age_cat_label)) +
					geom_bar(stat='identity', position = "stack") +
					facet_grid(index_age_cat_label~stat, scales = 'free') +
					labs(x='', y='', fill='Age band') +
					scale_fill_viridis_d(begin=0,end=1,direction=-1) +
					scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
					scale_y_continuous(expand=c(0,0)) +
					theme_bw() +
					theme(axis.text.x = element_text(angle = 45, hjust = 1), 
							panel.spacing = unit(1, "lines")) +
					theme(legend.position="bottom") +
					guides(fill = guide_legend(nrow = 1)) +
					theme(panel.background = element_blank(), 
							strip.background = element_blank()
					)
		}
		if(method!='barplot')
		{
			g <- ggplot(df,aes(x=date, y=M, fill=fill_age_cat_label)) +
					geom_area(position = "stack") +
					facet_grid(index_age_cat_label~stat, scales = 'free') +
					labs(x='', y='', fill='Age band') +
					scale_fill_viridis_d(begin=0,end=1,direction=-1) +
					scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), expand=c(0,0)) +
					scale_y_continuous(expand=c(0,0)) +
					theme_bw() +
					theme(axis.text.x = element_text(angle = 45, hjust = 1), 
							panel.spacing = unit(1, "lines")) +
					theme(legend.position="bottom") +
					guides(fill = guide_legend(nrow = 1)) +
					theme(panel.background = element_blank(), 
							strip.background = element_blank()
					)
		}	
		cat("\nWrite to file", paste0(outfile.base,'-',x,'.png'))
		ggsave(paste0(outfile.base,'-',x,'.png'),g, width=10, height= length(unique( df$index_age_cat_label ))/1.5 + 1)	  	  
	}  
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales rescale percent
make_source_onward_plot = function(flow,nrow_low_dim_age_band,outfile.base){
  
  cat("\n ----------- make source_onward figure ----------- \n")
  
  # it depends on low dimensional age bands
  ages <- c('0-9','10-19','21-34','35-49','50-64','65-79','80+')
  
  prop.flow.scale <- flow
  tmp <- prop.flow.scale[, list(sum3=sum(M)), by=c('date','region_name','age_cat_i')]
  prop.flow.scale <- merge(prop.flow.scale, tmp, by=c('date','region_name','age_cat_i'))
  prop.flow.scale[, flow3:= M/sum3]
  tmp <- prop.flow.scale[, list(sum4=sum(M)), by=c('date','region_name','age_cat_j')]
  prop.flow.scale <- merge(prop.flow.scale, tmp, by=c('date','region_name','age_cat_j'))
  prop.flow.scale[, flow4:= M/sum4]
  set(prop.flow.scale,NULL,c('sum3','sum4','M','CL','CU'),NULL)
  setnames(prop.flow.scale,c('flow3','flow4'),c('rec','scr'))
  prop.flow.scale <- reshape2::melt(prop.flow.scale, id.vars = c('date','region_name','age_cat_i','age_cat_j'),
                          variable.name = 'type', value.name = 'M')
  
  for (i in unique(flow$region_name)){
    df <- prop.flow.scale[region_name==i & type=='scr']
    setnames(df, c('age_cat_j', 'age_cat_i'),c('age_cat_fct', 'age_cat_fill'))
    df[,type:='sources of infections in age band']
    tmp <- prop.flow.scale[region_name==i & type=='rec']
    setnames(tmp, c('age_cat_j', 'age_cat_i'),c('age_cat_fill', 'age_cat_fct'))
    tmp[,type:='transmissions from age band']
    df <- rbind(df, tmp)
    
    g <- ggplot(df,aes(x=date,y=M,fill=factor( age_cat_fill,1:nrow_low_dim_age_band,ages))) +
      geom_area(position = "stack") +
      facet_grid(factor(age_cat_fct,1:nrow_low_dim_age_band,ages)~type,
                 scales = 'free') +
      labs(x='',y='', fill='Age band') +
      scale_fill_viridis_d(begin=0,end=1,direction=-1) +
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) +
      scale_y_continuous(expand = c(0,0),labels = scales::percent)+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.spacing = unit(1, "lines")) +
      theme(legend.position="bottom") +
      guides(fill = guide_legend(nrow = 1))+
      theme(panel.background = element_blank(), 
            strip.background = element_rect( color="black", fill="white", size=1, linetype="solid" ))
    
    
    ggsave(paste0(outfile.base,'-flow_sourcesonwardprop-',i,'.png'),g,width=10,height=length(ages)/1.5 + 1)
  }
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales pseudo_log_trans
make_flow_matrix_plot <- function(flow,outfile.base=NULL)
{  
  cat("\n ----------- make flow_matrix_plot ----------- \n")
  
  gs <- vector('list', length(unique(flow$loc)))
  for(x in unique(flow$loc))
  {	  
	  df <- subset(flow, loc==x)
	  max_M <- ifelse(max(df$M)>100, ceiling(max(df$M)/100)*100, ceiling(max(df$M)/10)*10)	 
	  # mid_M <- ifelse(mean(df$M)>100, ceiling(mean(df$M)/100)*100, ceiling(mean(df$M)/10)*10)
	  b_M <- 2^(seq(floor(log2(min(df$M[df$M > 0]))), ceiling(log2(max_M)),length.out = 15))
	  b_M <- unique(ifelse(b_M>=10, ceiling(b_M/10)*10, round(b_M)))
	  
	  gs[[x]] <- ggplot(df,aes(x=rec_age_cat_label, y=source_age_cat_label, fill=M)) +
			  geom_tile()+
			  labs(y='Age (sources)',x='Age (recipients)', fill='Number of transmissions\n(posterior median estimate)') +
			  # scale_fill_gradient2(trans='log10',mid="#B63679FF", limits=c(10^(-10),max_M), midpoint = mid_M,guide = "colourbar")+
			  scale_fill_viridis(option = "magma", limits=c(0,max_M), trans=scales::pseudo_log_trans(base=2),breaks=b_M) +
			  scale_x_discrete(expand = c(0,0) )+
			  scale_y_discrete(expand = c(0,0) )+
			  theme_bw() +
			  theme( axis.text.x = element_text(angle = 45, hjust = 1),
					 strip.background = element_blank(),
					 plot.title = element_text(hjust = 0.5), 
					 legend.position='bottom') +
			  facet_grid(~date) +			  
			  guides(fill = guide_colourbar(barwidth = 25, barheight = 0.5))
	  if(!is.null(outfile.base))
	  {
		  cat("\nWrite to file", paste0(outfile.base,'-matrix-',x,'.png'))
		  ggsave(paste0(outfile.base,'-matrix-',x,'.png'),gs[[x]], width=8, height=5)	  
	  }	 
  }
  gs
}
 
#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales percent
make_flow_sources_plot <- function(flow, dates, outfile.base=NULL)
{ 
	gs <- vector('list', length(unique(flow$loc)))
	for(x in unique(flow$loc))
	{		
		df <- subset(flow, loc==x )
		max.non.forecast.date <- dates[[x]][ sort(unique(df$time)) ]
		max.non.forecast.date <- tail(na.omit(max.non.forecast.date),1)		
		df <- subset(df, date==max.non.forecast.date)		
		df[, facet_col:= factor(as.integer(!rec_age_cat%in%c(1,2,3,4)))]
		max.cu <- max(df$CU)
		gs[[x]] <- ggplot(df,aes(x=source_age_cat_label, y=M, fill=rec_age_cat_label)) +
				geom_bar(stat="identity", position=position_dodge(width=.9, preserve = "single"), width= 0.8) +
				geom_errorbar(aes(ymin=CL, ymax=CU), position=position_dodge(width=.9, preserve = "single"), width=0.2) +
				labs(x='Age (sources)', y='Transmissions into age bands', fill='Age (recipients)') +
				scale_fill_viridis_d(begin=0,end=1,direction=-1) +
				scale_x_discrete(expand = c(0,0))+
				scale_y_continuous(expand = c(0,0), labels = scales::percent, limits = c(0,max.cu*1.1))+
				theme_bw() +
				facet_grid(facet_col ~ date) +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
						strip.background = element_blank(),
						strip.text.y = element_blank(),
						plot.title = element_text(hjust = 0.5),
						legend.position='bottom') +
				guides(fill = guide_legend(nrow = 1)) 
		if(!is.null(outfile.base))
		{
			cat("\nWrite to file", paste0(outfile.base,'-sources-',x,'.png'))
			ggsave(paste0(outfile.base,'-sources-',x,'.png'),gs[[x]], width=8, height=7)	  
		}	 
	}
	gs
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_prop_susceptible_byage_plot <- function(prop_susceptibleByAge,outfile.base){
  cat("\n ----------- make prop_susceptibleByAge figure ----------- \n")
  ps <- c(0.5, 0.025, 0.975)
  e_propsuc <- as.data.table( reshape2::melt( prop_susceptibleByAge ) )
  setnames(e_propsuc, 2:5, c('region','time','age_cat','e_propsuc'))
  e_propsuc <- e_propsuc[, list(q= quantile(e_propsuc, prob=ps),
                                p= c('M','CL','CU')
  ), by=c('time','region','age_cat')]
  e_propsuc <- dcast.data.table(e_propsuc, time+age_cat+region~p, value.var='q')
  e_propsuc <- merge(e_propsuc, dc, by='region')
  e_propsuc <- merge(e_propsuc, da, by=c('region_name','time'), all.x=TRUE)
  tmp <- unique(subset(e_propsuc, select=c(region_name,time,dates)))[, list(time=time,
                                                                            dates=dates[time==1]+seq.int(0,length.out=length(time))
  ), by='region_name']
  e_propsuc <- merge(subset(e_propsuc, select=-dates),tmp, by=c('region_name','time'))
  e_propsuc <- merge(e_propsuc, dages, by=c('age_cat'), all.x=TRUE)
  g <- ggplot(e_propsuc) +
    geom_line(aes(x=dates, y=M, col=age_band), stat='identity') +
    facet_wrap(~region_name, ncol=1, scale='free') +
    labs(x='days',y='expected deaths\n(posterior median by age band)')
  ggsave(paste0(outfile.base,'-e_propsuc.png'),g,width=7,height=50,limitsize=FALSE)
  saveRDS(e_propsuc, file=paste0(outfile.base,'-e_propsuc.rds'))
  
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_deathsoverall_overtime_plot <- function(deaths_s, cd_data, outfile.base=NULL)
{		  
	ans <- vector('list', length(unique(deaths_s$loc)))
	names(ans) <- unique(deaths_s$loc)
	for(x in unique(deaths_s$loc))
	{
		tmp <- subset(deaths_s, loc==x)		
		tmp2 <- subset(cd_data, loc==x)
		max.cases <- max( c(max(tmp$CU), max(tmp2$Deaths)) )
		
		ans[[x]] <- ggplot() +
				geom_bar(data= tmp2, aes(x = date, y = Deaths), fill = "coral4", stat='identity', alpha=0.5) +				
				geom_ribbon(data=tmp, aes(x = date, ymin = CL, ymax = CU), fill=alpha("deepskyblue4", 0.45)) +
				geom_line( data=tmp, aes(x = date, y=M), colour= "deepskyblue4") +
				labs(x= "", y="Daily number of deaths") +
				scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
				scale_y_continuous(labels = comma, expand=c(0,0), limits=c(0,max.cases*1.1)) +
				theme_bw() + 
				theme(axis.text.x = element_text(angle = 45, hjust = 1), 
						legend.position = "None") + 
				guides(fill=guide_legend(ncol=1))
		if(!is.null(outfile.base))
		{
			cat("\nWriting to file ", paste0(outfile.base,'-new-deaths-',x,'.png'))
			ggsave(paste0(outfile.base,'-new-deaths-',x,'.png'), ans[[x]], w = 7, h = 5)
		}
	}
	ans			
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_Rtoverall_overtime_plot <- function(rt_s, outfile.base=NULL)
{
	ans <- vector('list', length(unique(rt_s$loc)))
	for(x in unique(rt_s$loc))
	{
		tmp <- subset(rt_s, loc==x)				
		max.v <- max(7,max(tmp$CU))
		ans[[x]] <- ggplot() +			
				geom_hline(yintercept=1) +
				geom_ribbon(data=tmp, aes(x = date, ymin = CL, ymax = CU), fill=alpha("deepskyblue4", 0.45)) +
				geom_line( data=tmp, aes(x = date, y=M), colour= "deepskyblue4") +
				labs(x= "", y=expression(R[t])) +
				scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
				scale_y_continuous(labels = comma, expand=c(0,0), limits=c(0,max.v*1.1)) +
				theme_bw() + 
				theme(axis.text.x = element_text(angle = 45, hjust = 1), 
						legend.position = "None") + 
				guides(fill=guide_legend(ncol=1))
		if(!is.null(outfile.base))
		{
			cat("\nWriting to file ", paste0(outfile.base,'-Rt-',x,'.png'))
			ggsave(paste0(outfile.base,'-Rt-',x,'.png'), ans[[x]], w = 7, h = 5)
		}
	}
	ans			
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_casesoverall_overtime_plot <- function(cases_s, cd_data, outfile.base=NULL)
{	
	ans <- vector('list', length(unique(cases_s$loc)))
	for(x in unique(cases_s$loc))
	{
		tmp <- subset(cases_s, loc==x)		
		tmp2 <- subset(cd_data, loc==x)
		max.cases <- max( c(max(tmp$CU), max(tmp2$Cases)) )
		ans[[x]] <- ggplot() +
				geom_bar(data= tmp2, aes(x = date, y = Cases), fill = "coral4", stat='identity', alpha=0.5) +				
				geom_ribbon(data=tmp, aes(x = date, ymin = CL, ymax = CU), fill=alpha("deepskyblue4", 0.45)) +
				geom_line( data=tmp, aes(x = date, y=M), colour= "deepskyblue4") +
				labs(x= "", y="Daily number of infections") +
				scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
				scale_y_continuous(labels = comma, expand=c(0,0), limits=c(0,max.cases*1.1)) +
				theme_bw() + 
				theme(axis.text.x = element_text(angle = 45, hjust = 1), 
						legend.position = "None") + 
				guides(fill=guide_legend(ncol=1))
		if(!is.null(outfile.base))
		{
			cat("\nWriting to file ", paste0(outfile.base,'-new-infections-',x,'.png'))
			ggsave(paste0(outfile.base,'-new-infections-',x,'.png'), ans[[x]], w = 7, h = 5)
		}
	}
	ans			
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_Rt_byage_c <- function(parameter,parname,ylab="",c, outfile.base=NULL)
{	
	if('loc'%in%colnames(parameter))
	{
		data <- subset(parameter, loc == c)	
	}
	if('region_name'%in%colnames(parameter))
	{
		data <- subset(parameter, region_name == c)	
	}
	if('dates'%in%colnames(data))
	{
		colnames(data)[colnames(data)=='dates'] <- 'date'
	}   	
	g_aRt_c <-  ggplot(data) +
		geom_ribbon(aes(x=date, ymin = CL, ymax = CU, fill=age_band), alpha = .1,show.legend=FALSE) +
		geom_line(aes(x=date, y=M, col=age_band), stat='identity') +
		labs(x='',y=ylab) +
		scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"), 
								 limits = c(data$date[1], data$date[length(data$date)])) + 
		theme_bw() + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+ 
		scale_color_viridis_d(aesthetics = c("colour", "fill"),direction=-1) +
		guides(col = guide_legend(title="Age band",nrow=1)) +
		geom_hline(yintercept = 1, color = 'black', size = 1) 	+
		scale_y_sqrt(breaks=c(0.5,1,2^seq(1,max(max(parameter$M),1),1)))

	if(!is.null(outfile.base))
	{
		ggsave(paste0(outfile.base,'-',parname,'_byage_c-', c, '.png'), g_aRt_c, w = 10, h=5)	
	}
	
	return(g_aRt_c)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales percent
plot_Rt_less_than_one <- function(Rt_byage_less_than_one, plot_rebound_date=FALSE, rebound_date, outfile.base=NA_character_)
{	
  
	
  tmp <- data.table(y=runif(1e3 *  length(unique(Rt_byage_less_than_one$loc_label)), 0.5, 0.5 + length(unique(Rt_byage_less_than_one$age_band))),
                    loc_label=rep(unique(Rt_byage_less_than_one$loc_label),each=1e3))								
  tmp <- merge(tmp, unique(subset(Rt_byage_less_than_one, select=c(loc_label, epi_scenario_label))), by='loc_label')
  set(tmp, tmp[, which(is.na(epi_scenario_label) | epi_scenario_label=='-')], 'epi_scenario_label', 'unclear')
  set(tmp, NULL, 'epi_scenario_label', tmp[,factor(epi_scenario_label)])
  tmp[,epi_scenario_label:=factor(epi_scenario_label,
                                  levels= c("Rt<1 for all ages in last 4 weeks","Rt>1 after 4 week period of Rt<1 for all ages > 1",
                                  					"unclear",
                                  					"Rt>1 from 35-49 in last 4 weeks and no prior period with sustained Rt<1",
                                            "Rt>1 from 35-49 and other age groups in last 4 weeks and no prior period with sustained Rt<1"),
                                  labels = c("Rt<1 for all ages in last 4 weeks",
                                             "Rt>1 after 4 week period of Rt<1 \n for all ages > 1",
                                  					 "unclear",
                                             "Rt>1 from 35-49 in last 4 weeks \n and no prior period with \n sustained Rt<1",
                                  					 "Rt>1 from 35-49 and other age groups \n in last 4 weeks and no prior period \n with sustained Rt<1"))]
  p <- ggplot( Rt_byage_less_than_one) +
    scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
    scale_y_discrete(expand=c(0,0)) +
    geom_tile(aes(x= date, y= age_band, fill=value)) +
    geom_rug(data=tmp,aes(y=y, color=epi_scenario_label),sides='r',outside=TRUE, show.legend=TRUE,size=2)+
    scale_colour_manual(values=c("Rt<1 for all ages in last 4 weeks"="deepskyblue",
    														 "Rt>1 after 4 week period of Rt<1 \n for all ages > 1"="gold",
    														 "Rt>1 from 35-49 in last 4 weeks \n and no prior period with \n sustained Rt<1"="darkorchid1",
    														 "Rt>1 from 35-49 and other age groups \n in last 4 weeks and no prior period \n with sustained Rt<1"="firebrick1",
                                 "unclear"="pink"))+
	scale_fill_gradient(high = "azure1", low = "grey30", labels = scales::percent) +
	#scale_fill_gradient(high = c("lighskyblue2", "yellow", "blue"), labels = scales::percent) +
    #scale_fill_viridis(begin=0.1,end=1, alpha=1, direction=-1, option="inferno", labels = scales::percent) +
	#scale_fill_grey(labels = scales::percent) +
	#scale_fill_gradientn(colours = c("red", "yellow", "blue"), labels = scales::percent) +
   	labs(x= 'Date', y='Age bands',color='Epidemiologic classification',
   	     fill='Posterior probability that age specific \n R[t] is below 1 \n \n \n ') +
    facet_wrap(loc_label~.,ncol=5) +
    theme_bw() +
    theme(#plot.margin = margin(1, 1, 1, 1, "cm"),
		      legend.position='bottom',
          legend.box="horizontal",
          legend.direction='horizontal',
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
		      legend.text.align = 0,
		      legend.justification=c(0,0),
          plot.title = element_text(size = 10, face = "bold"),
          axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank()) +
    guides(col = guide_legend(ncol=2, byrow = TRUE, direction='vertical', title.position='top',order=1),
           fill = guide_colourbar(barheight = 0.5, barwidth = 10, direction='horizontal',title.position='top',order=2)) 
  if(plot_rebound_date){
    p = p+   geom_vline(data=rebound_date, aes(xintercept=rebound_date),color = "red")
  }
  h <- ceiling(length(unique(Rt_byage_less_than_one$loc))/5)
  
  ggsave(paste0(outfile.base,'-plot_Rt_less_than_one.png'),p, width = 5*1.7, height = h * 1.1 + 1, limitsize = FALSE)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_par_byage_c <- function(parameter,parname,ylab="",c,outfile.base=NULL)
{
	if('loc'%in%colnames(parameter))
	{
		data <- subset(parameter, loc == c)	
	}
	if('region_name'%in%colnames(parameter))
	{
		data <- subset(parameter, region_name == c)	
	}
	if('dates'%in%colnames(data))
	{
		colnames(data)[colnames(data)=='dates'] <- 'date'
	}
  	g <-  ggplot(data) +
		geom_bar(aes(x=date, y=M, fill=age_band), stat='identity') +
		labs(x='',y=ylab) +
		scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"), 
								 limits = c(data$date[1], 
								 					 data$date[length(data$date)])) + 
		theme_bw() + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom") +
		scale_fill_viridis_d(begin=0,end=1,direction=-1) +
		guides(fill = guide_legend(title="Age band",nrow=1))

	if(!is.null(outfile.base))
	{
		ggsave(paste0(outfile.base,'-',parname,'_byage_c-', c, '.png'), g, w = 10, h=5)
	}
	
	return(g)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_marginal_contacts <- function(ocnts, outfile.base=NA_character_)
{      
	n.loc <- length(unique(ocnts$loc))
	p <- ggplot(ocnts) +
			geom_hline(yintercept=c(2), linetype='dotted') +
	    geom_hline(yintercept=c(2.7), linetype='dashed') +
			geom_ribbon(aes(x=date, ymin = CL, ymax = CU), alpha = .4, show.legend=FALSE) +
			geom_line(aes(x=date, y=M), stat='identity') +
			labs(x='',y='Estimated number of contacts per day') +
			scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"), 
					limits = range(ocnts$date)) + 
			coord_cartesian(ylim=c(0,NA)) +
			theme_bw() + 
			theme(legend.position="bottom",
					axis.text.x = element_text(angle = 45, hjust=1),
					axis.text.y=element_text(size=12),
					axis.title=element_text(size=24),
					axis.title.x = element_text(vjust=-1),
					strip.text = element_text(size = 16),
					legend.title = element_text(size = 20),
					legend.text = element_text(size = 16),
					strip.background = element_blank(),) +		
			facet_wrap(~loc_label, ncol=4, scale='free')
	if(!is.na(outfile.base))
	{
		ggsave(paste0(outfile.base,'-marginal-contacts-overall-', 'all-states', '.png'), p, w = 20, h = n.loc*3/4, limitsize=FALSE)
	}
	return(p)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_marginal_contacts_by_age <- function(mcnts, outfile.base=NA_character_)
{      
	n.loc <- length(unique(mcnts$loc))
	p <- ggplot(mcnts) +
			geom_ribbon(aes(x=date, ymin = CL, ymax = CU, fill=age_band), alpha = .1, show.legend=FALSE) +
			geom_line(aes(x=date, y=M, col=age_band), stat='identity') +
			labs(x='',y='Estimated number of contacts per day') +
			scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"), 
					limits = range(mcnts$date)) + 			
			theme_bw() + 
			theme(legend.position="bottom",
					axis.text.x = element_text(angle = 45, hjust=1),
					axis.text.y=element_text(size=12),
					axis.title=element_text(size=24),
					axis.title.x = element_text(vjust=-1),
					strip.text = element_text(size = 16),
					legend.title = element_text(size = 20),
					legend.text = element_text(size = 16),
					strip.background = element_blank(),) +
			scale_color_viridis_d(aesthetics = c("colour", "fill"),direction=-1) +
			guides(col = guide_legend(title="Age band")) +			
			facet_wrap(~loc_label, ncol=4, scale='free')
	if(!is.na(outfile.base))
	{
		ggsave(paste0(outfile.base,'-marginal-contacts-by-age-', 'all-states', '.png'), p, w = 20, h = n.loc*3/4, limitsize=FALSE)
	}
	return(p)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_etabyage <- function(detas, outfile.base=NA_character_)
{      
	n.loc <- length(unique(detas$loc))
    g_eta_mtc <- ggplot(detas) +
      geom_ribbon(aes(x=dates, ymin = CL, ymax = CU, fill=age_band), alpha = .1, show.legend=FALSE) +
      geom_line(aes(x=dates, y=M, col=age_band), stat='identity') +
      labs(x='',y='Eta\n(posterior median by age band)') +
      scale_x_date(expand=c(0,0),date_breaks = "2 weeks", labels = date_format("%e %b"), 
                   limits = c(detas$dates[1], 
						      detas$dates[length(detas$dates)])) + 
      theme_bw() + 
      theme(legend.position="bottom",
            axis.text.x = element_text(angle = 45, hjust=1),
            axis.text.y=element_text(size=12),
            axis.title=element_text(size=24),
            axis.title.x = element_text(vjust=-1),
            strip.text = element_text(size = 16),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 16),
            strip.background = element_blank(),) +
      scale_color_viridis_d(aesthetics = c("colour", "fill"),direction=-1) +
      guides(col = guide_legend(title="Age band")) +
      geom_hline(yintercept = 1, color = 'black', size = 1)  +
      facet_wrap(~loc_label, ncol=2, scale='free')
	if(!is.na(outfile.base))
	{
		ggsave(paste0(outfile.base,'-eta_mtc-', 'all-states', '.png'), g_eta_mtc, w = 13, h = n.loc*3/2, limitsize=FALSE)	
	}    
    return(g_eta_mtc)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_parameter_ifr_relsusceptibility_reltransmissibility_plot <- function(log_ifr_age_base,
                                                                          log_relsusceptibility_age,
                                                                          log_reltransmissibility_age,
																		  hyperpara_ifr_age_lnmu,
																		  hyperpara_ifr_age_lnsd,
                                                                          dages){
  cat("\n ----------- make ifr_relsusceptibility_reltransmissibility figure ----------- \n")
  
  pars.dt <- data.table()
  ifr_age <- exp(log_ifr_age_base)
  ifr_age <- t(apply(ifr_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  ifr_age <- data.table(ifr_age)
  colnames(ifr_age) <- c('CL','M','CU')
  ifr_age$par <- 'infection fatality ratio'
  ifr_age$age <- dages$age_band
  pars.dt <- rbind(pars.dt,ifr_age)
  
  relsusceptibility_age <- exp(log_relsusceptibility_age)
  relsusceptibility_age <- t(apply(relsusceptibility_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  relsusceptibility_age <- data.table(relsusceptibility_age)
  colnames(relsusceptibility_age) <- c('CL','M','CU')
  relsusceptibility_age$par <- 'relative susceptibility'
  relsusceptibility_age$age <- dages$age_band
  pars.dt <- rbind(pars.dt,relsusceptibility_age)
  
  reltransmissibility_age <- exp(log_reltransmissibility_age)
  reltransmissibility_age <- t(apply(reltransmissibility_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  reltransmissibility_age <- data.table(reltransmissibility_age)
  colnames(reltransmissibility_age) <- c('CL','M','CU')
  reltransmissibility_age$par <- 'relative transmissibility'
  reltransmissibility_age$age <- dages$age_band
  pars.dt <- rbind(pars.dt,reltransmissibility_age)
  pars.dt$type <- 'posterior distribution'
  
  tmp <- data.table()
  ifr_age <- mapply(function(x,y){rnorm(x,y,n=nrow(log_ifr_age_base))},
                    x= hyperpara_ifr_age_lnmu, y= hyperpara_ifr_age_lnsd)
  ifr_age <- exp(ifr_age)
  ifr_age <- t(apply(ifr_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  ifr_age <- data.table(ifr_age)
  colnames(ifr_age) <- c('CL','M','CU')
  ifr_age$par <- 'infection fatality ratio'
  ifr_age$age <- dages$age_band
  tmp <- rbind(tmp,ifr_age)
  
  relsusceptibility_age_reduced <-  exp(mapply(function(x,y){rnorm(x,y,n=nrow(log_relsusceptibility_age))},
                                               x=c(-1.0702331, 0.3828269),y=c(0.2169696,0.1638433)))
  relsusceptibility_age <- cbind(matrix(rep(relsusceptibility_age_reduced[,1],each=3), ncol=3, byrow=TRUE),
                                 matrix(1,nrow(log_relsusceptibility_age),10),
                                 matrix(rep(relsusceptibility_age_reduced[,2],each=5), ncol=5, byrow=TRUE))
  relsusceptibility_age <- t(apply(relsusceptibility_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  relsusceptibility_age <- data.table(relsusceptibility_age)
  colnames(relsusceptibility_age) <- c('CL','M','CU')
  relsusceptibility_age$par <- 'relative susceptibility'
  relsusceptibility_age$age <- dages$age_band
  tmp <- rbind(tmp,relsusceptibility_age)
  
  sd_log_reltransmissibility_age <- rexp(nrow(log_relsusceptibility_age),10)
  reltransmissibility_age_reduced <- t(exp(mapply(function(x,y){rnorm(x,y,n=2)},
                                                      x=0,y=sd_log_reltransmissibility_age)))
  reltransmissibility_age <- cbind(matrix(rep(reltransmissibility_age_reduced[,1],each=3), ncol=3, byrow=TRUE),
                                   matrix(1,nrow(log_reltransmissibility_age),10),
                                   matrix(rep(reltransmissibility_age_reduced[,2],each=5), ncol=5, byrow=TRUE))
  reltransmissibility_age <- t(apply(reltransmissibility_age, 2, function(x)quantile(x,probs = c(0.025,0.5,0.975))))
  reltransmissibility_age <- data.table(reltransmissibility_age)
  colnames(reltransmissibility_age) <- c('CL','M','CU')
  reltransmissibility_age$par <- 'relative transmissibility'
  reltransmissibility_age$age <- dages$age_band
  tmp <- rbind(tmp,reltransmissibility_age)
  tmp$type <- 'prior distribution'
  
  pars.dt <- rbind(pars.dt,tmp)
  
  ggplot(pars.dt, aes(x=age, y=M, group=type, color=type)) + 
    # geom_line() +
    geom_point( stat="identity", position=position_dodge(0.5))+
    geom_errorbar(aes(ymin=CL, ymax=CU), width=.2,
                  position=position_dodge(0.7),
                  stat="identity")+
    labs(x="",y="",color="")+
    theme_bw()+
    facet_grid(.~par,scale='free')+
    coord_flip(expand = TRUE) +
    theme(legend.position="bottom",axis.text.y=element_text(size=6),axis.text.x=element_text(size=6),
          strip.text = element_text(size = 8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  ggsave(paste0(outfile.base,'-ifr_relsusceptibility_reltransmissibility_plot.png'),width=8,height=4)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_deaths_byage <- function(df,ggplotColours)
{  
	# set colors by age 
	ggplotCol_State = ggplotColours(length(levels(df$age)))
	# separate plot in two
	index1_levels = 1:floor(length(levels(df$age))/2)
	index2_levels = (floor(length(levels(df$age))/2)+1):length(levels(df$age))
	df.1 = subset(df, age %in% levels(df$age)[index1_levels]);
	df.2 = subset(df, age %in% levels(df$age)[index2_levels]); df.2$age = factor(df.2$age, levels = levels(df$age)[index2_levels])
	
	begin.data.1 = min(subset(df.1, !is.na(cum.deaths))$dates) 
	end.data.1 = max(subset(df.1, !is.na(cum.deaths))$dates)
	df.1 = subset(df.1, dates <= end.data.1)
	begin.data.2 = min(subset(df.2, !is.na(cum.deaths))$dates) 
	end.data.2 = max(subset(df.2, !is.na(cum.deaths))$dates)
	df.2 = subset(df.2, dates <= end.data.2)
	
	xintercept.1=as.numeric(df.1$dates[which(df.1$dates == begin.data.1)])
	xintercept.2=as.numeric(df.2$dates[which(df.2$dates == begin.data.2)])
	
	p1.1 = ggplot(df.1, aes(x = dates)) +
		geom_ribbon(aes(ymin = CL, ymax = CU, fill = age),alpha=0.2) +
		geom_line(aes(y = M, col = age)) +
		geom_point(aes(y = cum.deaths, col = age, shape = age), size = 1.5) +
		labs(x='',y=paste0('Cumulative number of deaths by age')) +
		scale_x_date(expand=c(0,0),date_breaks = "weeks", labels = date_format("%e %b"),
								 limits = c(df.1$dates[1],
								 					 df.1$dates[length(df.1$dates)])) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		theme(legend.position="right") +
		guides(fill=guide_legend(title="Age band"),
					 color=guide_legend(title="Age band"),
					 shape = guide_legend(title="Age band"))+ 
		scale_color_manual(values=ggplotCol_State, drop = FALSE)+ 
		scale_fill_manual(values=ggplotCol_State, drop = FALSE)+
		scale_shape_manual(values=c(index1_levels,index2_levels), drop = FALSE)+
		geom_vline(xintercept=xintercept.1, linetype=2, alpha = .7)

	p1.2 = ggplot(df.2, aes(x = dates)) +
		geom_ribbon(aes(ymin = CL, ymax = CU, fill = age),alpha=0.2) +
		geom_line(aes(y = M, col = age)) +
		geom_point(aes(y = cum.deaths, col = age, shape = age), size = 1.5) +
		labs(x='',y=paste0('Cumulative number of deaths by age')) +
		scale_x_date(expand=c(0,0),date_breaks = "weeks", labels = date_format("%e %b"),
								 limits = c(df.2$dates[1],
								 					 df.2$dates[length(df.2$dates)])) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		theme(legend.position="right") +
		guides(fill=guide_legend(title="Age band"),
					 color=guide_legend(title="Age band"),
					 shape=guide_legend(title="Age band"))+ 
		scale_color_manual(values=ggplotCol_State[index2_levels], drop = FALSE)+ 
		scale_fill_manual(values=ggplotCol_State[index2_levels], drop = FALSE)+ 
		scale_shape_manual(values=index2_levels, drop = FALSE)+
		geom_vline(xintercept=xintercept.2, linetype=2, alpha = .7)
	
	p <- list(p1.1, p1.2,xintercept.2)
	return(p)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_age_contact_boxplot <- function(age_specific_contacts, outfile.base){
  
  data_model <- data.frame("group" = age_specific_contacts$loc_label,
                           "x" = factor(rep(c(1, 2, 3, 4, 5), 3)),
                           "min" = age_specific_contacts$min,
                           "lower" = age_specific_contacts$L,
                           "median" = age_specific_contacts$M,
                           "upper" = age_specific_contacts$U,
                           "max" = age_specific_contacts$max)
  
  data <- data.frame("group" = rep("BICS", 5),
                     "x" = c(1, 2, 3, 4, 5),
                     "min" = c(0, 0, 0, 0, 0),
                     "lower" = c(2, 1, 2, 1, 1),
                     "median" = c(3, 2, 3, 2, 2),
                     "upper" = c(4, 4, 4, 3, 3),
                     "max" = c(7, 8, 7, 6, 6))
  
  data <- rbind(data_model, data)
  
  data$x <- factor(data$x, labels = c("[18, 25)", "[25, 35)", "[35, 45)", "[45, 65)", "[65, 100)"))
  
  outliers <- data.frame("x" = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5),
                         "y" = c(8, 9, 10, 9, 10, 8, 9, 10, 7, 8, 9, 10, 8, 10))
  mean <- data.frame("x" = c(1, 2, 3, 4, 5),
                     "mean" = c(2.98, 2.81, 3.09, 2.60, 2.04))
  
  p <-  ggplot() + 
    geom_boxplot(data = data, aes(x = x, lower = lower, upper = upper, middle = median, ymin = min, ymax = max, fill = group),
                 stat = "identity")  +
    ylab("Number of contacts") + xlab("Age") + labs(fill = "") +
    theme_bw()
  
  if(!is.na(outfile.base))
  {
    ggsave(paste0(outfile.base,'-number_contacts.png'), p, w = 7, h = 5, limitsize=FALSE)
  }
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_comparison_posterior_intervals = function(tmp, xtick, xlab, pars, model_names, xintercept, outfile.base, log_scale = 0, exp_tmp = 0){
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  if(is.null(xtick)){
    xtick = vector('list',length(tmp)) 
  }
  
  ans = vector('list',length(tmp))  
  for(i in 1:length(tmp)){
    
    tmp[[i]] = as.matrix(tmp[[i]])
    
    if(exp_tmp){
      tmp[[i]] = exp(tmp[[i]])
    }
    
    if(is.null(xtick[[i]])){
      xtick[[i]] = as.character(1:ncol(tmp[[i]]))
    }
    
    colnames(tmp[[i]]) <- xtick[[i]]
    df <- data.table( reshape2::melt(tmp[[i]]) )
    df <- df[, list(q= quantile(value, prob=ps),
                    q_label= p_labs), 
             by=c('Var2')]
    df <- dcast.data.table(df, Var2 ~ q_label, value.var='q')
    
    df[, model := as.character(model_names[i])]
    
    ans[[i]] = df
  }
  ans <- do.call('rbind',ans)
  ans = subset(ans, Var2 %in% Reduce(intersect, xtick))
  
  pd <- position_dodge(0.8) 
  gh <- ggplot(ans, aes(x=Var2, y = M, colour=model, group=model)) +
    geom_errorbar(aes(ymin=CU, ymax=CL), colour="black", width=.1, position=pd) +
    geom_point(position=pd, size=3, shape=21, fill="white") +
    coord_flip() +
    geom_hline(yintercept=xintercept) +
    labs(x=xlab) +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_text(size=18,angle=90),
          axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14, vjust = 0.5, hjust=0))
  
  if(log_scale){
    gh = gh + scale_y_log10() 
  }
  
  ggsave(paste0(outfile.base, "-comparison_maxyoungcontats",'-',pars,'.png'),gh,width=6,height=(length(xtick)/2 + 2))
  
  return(gh)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
make_parameter_summary_plot_comparison <- function(ggplot.list, pars, outfile.base){
  cat("\n ----------- make", pars, "parameter_summary figure ----------- \n")
  
  g <- ggarrange(plotlist=ggplot.list,ncol=1,align="v",heights=c(1.3,rep(1,length(ggplot.list)-1)))
  ggsave(paste0(outfile.base,"-comparison_maxyoungcontats",'-model_parameters_all_states',"pars-",pars, '.png'), g, w = 15, h = 29)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales percent
make_comparison_posterior_intervals_national_sensitivity_contacts_0_14 = function(tmp, xlab, pars, without_facet_title, model_names, Date, outfile.base, xintercept= NULL, scale_percent = 0)
  {
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  ans = vector('list',length(tmp))  
  regions = vector('list',length(tmp))  
  dates = vector('list',length(tmp))  
  for(i in 1:length(tmp)){
    
    df = tmp[[i]]
    regions[[i]] = unique(df$loc)
    dates[[i]] = as.character( unique(df$date))
  
    Model = ifelse(model_names[i] == "extrapolated_mobility_0-14", "extrapolated_mobility_0-14", "zhang_contacts")
    df[, model := Model]
    
    multiplier_cntct_school_closure = ifelse(Model == "zhang_contacts", paste0("tau ==", model_names[i]), NA)
    df[, multiplier_cntct_school_closure := multiplier_cntct_school_closure]
    
    base_model = ifelse(model_names[i] == "1", "1", "0")
    df[, base_model := base_model]
    
    ans[[i]] = df
  }
  ans <- do.call('rbind',ans)
  
  #national average
  ans = subset(ans, loc == "US")
  ans = subset(ans, age_band != "Overall")
  
  # find last date
  df = subset(ans, date == Date)
  
  # make factor
  df[, multiplier_cntct_school_closure:= factor(multiplier_cntct_school_closure, levels = unique(df$multiplier_cntct_school_closure))]
  df[, base_model:= as.factor(base_model)]
  
  gh <- 
    ggplot(subset(df, model == "zhang_contacts"), aes(x=age_band, y = M)) +
    geom_errorbar(aes(ymin=CU, ymax=CL, group = base_model), colour="black", width=.1) +
    geom_point(aes(col = base_model), size=3,fill="white") +
    coord_flip() +
    geom_hline(yintercept=xintercept) +
    theme_bw() +
    facet_grid(~multiplier_cntct_school_closure, labeller = label_parsed) + 
    theme(legend.position = "none",
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x= element_text(size = 14),
          axis.text.y=element_text(size = 12),
          axis.text.x=element_text(size=12, vjust = 0.5, hjust=0.5, angle = 30),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 20),
          strip.background = element_rect(fill="white")) +
    xlab("Age band") +
    ylab("") +
    #scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
    scale_color_manual(values = c("black", "gold")) 
  
  gl <- 
    ggplot(subset(df, model != "zhang_contacts"), aes(x=age_band, y = M)) +
    geom_errorbar(aes(ymin=CU, ymax=CL, group = base_model), colour="black", width=.1) +
    geom_point(aes(col = base_model), size=3,fill="white") +
    coord_flip() +
    geom_hline(yintercept=xintercept) +
    theme_bw() +
    facet_grid(~multiplier_cntct_school_closure, labeller = label_parsed) + 
    theme(legend.position = "none",
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_text(size = 14),
          axis.text.y=element_blank(),
          axis.text.x=element_text(size=12, vjust = 0.5, hjust=0.5, angle = 30),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10),
          strip.text = element_blank(),
          strip.background = element_rect(fill="white")) +
    #scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
    scale_color_manual(values = c("black", "gold")) +
    ylab("")
  
  if(scale_percent){
    gh <- gh +
      scale_y_continuous(labels = scales::percent)
    gl <- gl +
      scale_y_continuous(labels = scales::percent)
  }
  
  if(without_facet_title){
    gh <- gh + 
      theme(strip.text = element_text(size = 1, color = "white"),
            strip.background = element_blank())
  } 
  
  return(list(gh, gl))
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales percent
make_comparison_posterior_intervals_national_sensitivity_ifr_prior = function(tmp, xlab, pars, without_facet_title, model_names, Date, xintercept= NULL, scale_percent = 0)
{
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  ans = vector('list',length(tmp))  
  regions = vector('list',length(tmp))  
  dates = vector('list',length(tmp))  
  for(i in 1:length(tmp)){
    
    df = tmp[[i]]
    regions[[i]] = unique(df$loc)
    dates[[i]] = as.character( unique(df$date))
    
    df[, model := model_names[i]]
    
    ans[[i]] = df
  }
  ans <- do.call('rbind',ans)
  
  #national average
  ans = subset(ans, loc == "US")
  ans = subset(ans, age_band != "Overall")
  
  # find last date
  df = subset(ans, date == Date)

  gh <- 
    ggplot(df, aes(x=age_band, y = M)) +
    geom_errorbar(aes(ymin=CU, ymax=CL), colour="black", width=.1) +
    geom_point(aes(color = model),size=3,fill="white") +
    coord_flip() +
    geom_hline(yintercept=xintercept) +
    theme_bw() +
    facet_wrap(~model, labeller = label_wrap_gen(width=28)) + 
    theme(legend.position = "none",
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x= element_text(size = 14),
          axis.text.y=element_text(size = 12),
          axis.text.x=element_text(size=12, vjust = 0.5, hjust=0.5, angle = 30),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 20),
          strip.background = element_rect(fill="white")) +
    xlab("Age band") +
    ylab("") +
    #scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
    scale_color_manual(values = c("gold", "black")) 
  
  if(scale_percent){
    gh <- gh +
      scale_y_continuous(labels = scales::percent)
  }
  
  if(without_facet_title){
    gh <- gh + 
      theme(strip.text = element_text(size = 1, color = "white"),
            strip.background = element_blank())
  } 
  
  return(gh)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales rescale
#' @importFrom gridExtra grid.arrange
plot_contact_patterns_sensitivity_contacts_0_14 <- function(tmp, dates, stan_data, figure.select.loc, outfile.base, model_names)
{	
  ans = vector('list',length(tmp))  
  regions = vector('list',length(tmp))  
  date = vector('list',length(tmp))  
  for(i in 1:length(tmp)){
    
    df = tmp[[i]]
    regions[[i]] = unique(df$loc)
    
    # select one loc
    df = subset(df, loc == figure.select.loc)
    
    date[[i]] = as.character( unique(df$dates))
    
    Model = ifelse(model_names[i] == "extrapolated_mobility_0-14", "extrapolated_mobility_0-14", "zhang_contacts")
    df[, model := Model]
    
    multiplier_cntct_school_closure = ifelse(Model == "zhang_contacts", paste0("tau ==", model_names[i]), NA)
    df[, multiplier_cntct_school_closure := multiplier_cntct_school_closure]
    
    if(!is.null(df$school_closed)){
      df1 = select(df, school_closed, dates, loc)
      df1 = df1[school_closed==1, list(closure_date = min(dates), reopening_date = max(dates)), by = "loc"]
    } 
    
    df = select(df, loc, loc_label, dates, age_cnt, wend, age_index, cnt_intensity, model, multiplier_cntct_school_closure)
    
    ans[[i]] = df
  }
  ans <- do.call('rbind',ans)
  
  # keep dates that are common to all models
  ans = subset(ans, dates %in% as.Date( Reduce(intersect, date) ) )
  
  # find period where all states had school closed
  df1 = df1[, list(period_closure_start = max(closure_date), period_closure_end = min(reopening_date))]
  ans = subset(ans, dates >= df1$period_closure_start & dates < df1$period_closure_end)
  if(as.Date("2020-03-28") %in% unique(ans$dates)){ # try to set the selected time at the midpoint of BICS study
    df = subset(ans, dates == as.Date("2020-03-28"))
  } else{ # set to first date after start of school closure period 
    df = ans[, list(min_date = min(dates)), by = c("model", "multiplier_cntct_school_closure")]
    df = subset(ans, dates == max(df$min_date))
  }
  df[, label:= paste0(dates, '\n',ifelse(wend==1,'(weekend)','(weekday)'))]
  
  # set to grey any contacts nonchild-nonchild
  df[!age_cnt %in% stan_data$AGE_CHILD & !age_index %in% stan_data$AGE_CHILD , cnt_intensity := NA]
  
  df[, multiplier_cntct_school_closure:= factor(multiplier_cntct_school_closure, levels = unique(df$multiplier_cntct_school_closure))]
  
  cat("\n ----------- make contact_patterns_over_time figures ----------- \n")

  p1 = ggplot(subset(df, model == "zhang_contacts"), aes(y = age_cnt, x = age_index)) + 
    geom_tile(aes(fill=cnt_intensity)) +
    scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100)) +
    scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5))) +
    labs(y="Age of contact",x = "Age of index person",
         fill="Estimated contact intensity\n(posterior median)" ) +
    theme_bw()   +
    scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,0.5), na.value="gray86") + # same scale as exploratry plot
    facet_grid(~ multiplier_cntct_school_closure, labeller = label_parsed) + 
    guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5,direction="horizontal")) +
    theme(legend.position="none",
          axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1,size=12),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=24),
          axis.title.x =element_blank(),
          axis.title.y =element_blank(),
          strip.text = element_text(size = 20),
          legend.title = element_text(size = 20, vjust=-1),
          legend.text = element_text(size = 20, vjust=-1),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())	
  
  p2 = ggplot(subset(df, model != "zhang_contacts"), aes(y = age_cnt, x = age_index)) + 
    geom_tile(aes(fill=cnt_intensity)) +
    scale_y_continuous(expand = c(0,0),breaks = 1:19-0.5, labels= c(seq(0,85,by=5),100)) +
    scale_x_continuous(expand = c(0,0),breaks = 1:18-0.5, labels= c(seq(0,85,by=5))) +
    labs(y="Age of contact",x = "Age of index person",
         fill="Estimated contact intensity\n(posterior median)" ) +
    theme_bw()   +
    scale_fill_viridis(begin=0,end=1,alpha=0.6,direction=1,option="magma",values = scales::rescale(c(0,0.3,0.5,3,8)),breaks=seq(0,8,0.5), na.value="gray86") + # same scale as exploratry plot
    guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5,direction="horizontal")) +
    theme(legend.position = "bottom",
          axis.text.x=element_text(angle=70, vjust = 0.5, hjust=1,size=12),
          axis.text.y=element_text(size=12),
          axis.title.x = element_text(vjust=-1, size = 20),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 20),
          legend.title = element_text(size = 20, vjust=-1),
          legend.text = element_text(size = 20, vjust=-1),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())	
  
  file = paste0(outfile.base,'-contacts-0-14-', "contact_patterns", '.pdf')
  cat("Write ", file)
  pdf(file, w = 10, h = 8)
  gridExtra::grid.arrange(
    grobs = list(p1, p2),
    widths = c(0.75, 0.8, 0.75),
    heights = c(1, 0.1, 1.2),
    layout_matrix = rbind(c(1, 1, 1),
                          c(NA, NA, NA),
                          c(NA, 2, NA)),
    left = textGrob("Age of contact", gp=gpar(fontsize=20), rot = 90, y = 0.5, x= 0.4)
  )
  grid.text(c("A"), x = 0.03, 
            y = 0.97, gp = gpar(fontsize=20, fontface = "bold"))
  grid.text(c("B"), x = 0.35, 
            y = 0.54, gp = gpar(fontsize=20, fontface = "bold"))
  dev.off()

  file = paste0(outfile.base,'-contacts-0-14-', "contact_patterns", '.rds')
  cat("Write ", file)
  df = list( location = unique(df$loc_label), date = format( unique(df$date), "%B %d, %Y")  )
  saveRDS(df, file=file, version = 2)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_deaths_overall_overtime_forecast_school_reopen <- function(deaths_school_reopen_1, deaths_school_reopen_0, cd_data, dates, regions, multiplier, outfile.base=NULL)
{		  
  cd_data[loc_label == "New_York_City", loc_label := "New York City"]
  cd_data = subset(cd_data, loc_label %in% deaths_school_reopen_1$loc_label)
  
  deaths = NULL
  for(m in seq_along(regions)){
    x = regions[m]
    tmp_reopen_1 = subset(deaths_school_reopen_1, loc == x)
    tmp_reopen_0 = subset(deaths_school_reopen_0, loc == x)
    first_days_forecast = max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) ) + 1
    deaths = rbind(deaths, subset(deaths_school_reopen_1, loc == x & date < first_days_forecast))
    deaths_school_reopen_1 = rbind(subset(deaths_school_reopen_1, loc != x), subset(deaths_school_reopen_1, loc == x & date >= first_days_forecast))
    deaths_school_reopen_0 = rbind(subset(deaths_school_reopen_0, loc != x), subset(deaths_school_reopen_0, loc == x & date >= first_days_forecast))
  }
  
  deaths_school_reopen_1[, Scenario := "Kindergartens and elementary schools reopen"]
  deaths_school_reopen_0[, Scenario := "Kindergartens and elementary schools remain closed"]
  deaths_scenario = rbind(deaths_school_reopen_1, deaths_school_reopen_0)
  
  ggplot() +
    geom_bar(data= cd_data, aes(x = date, y = Deaths), fill = "coral4", stat='identity', alpha=0.5) +	
    geom_ribbon(data=deaths, aes(x = date, ymin = CL, ymax = CU), fill=alpha("deepskyblue4", 0.45)) +
    geom_line( data=deaths, aes(x = date, y=M), colour= "deepskyblue4") +
    geom_ribbon(data=deaths_scenario, aes(x = date, ymin = CL, ymax = CU, fill = Scenario), alpha = 0.5) +
    geom_line( data=deaths_scenario, aes(x = date, y=M, col = Scenario)) +
    labs(x= "", y="Daily number of deaths") +
    scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "bottom",
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=24),
          axis.title.x = element_text(vjust=-0.5),
          strip.text = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20)) + 
    facet_wrap(~loc_label, ncol =6, scales = "free_y")+
    scale_fill_manual(values = c("gold", "seagreen3")) +
    scale_color_manual(values = c("goldenrod3", "palegreen4"))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE), color = guide_legend(nrow=2,byrow=TRUE))
    
  n_region = length(unique(deaths_scenario$loc_label))
  
  plot_name = paste0(outfile.base, "-new_deaths-forecast-school-closure_multiplier", multiplier, ".png") 
  cat("write ", plot_name)
  ggsave(file = plot_name, w = 16, h = n_region*0.6)
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
plot_expected_observed_death = function(df, periodicity){
  
  ggplot(df, aes(x = M, y = daily.deaths, col = loc_label)) + 
    geom_point() +
    facet_wrap(~age_band_new, scale = "free") +
    labs(x = paste0("Expected ", periodicity, " deaths"), y = paste0("Observed ", periodicity, " deaths"), colour = "") +
    theme_bw()+ 
    geom_abline(slope = 1) +
    theme(legend.text = element_text(size = 15),
          axis.text.x=element_text(size = 10),
          axis.text.y=element_text(size = 10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          strip.background = element_blank(),
          strip.text = element_text(size = 15),
          panel.background = element_blank()) 
  file = paste0(outfile.base, "-expected_observed_", periodicity, "_deaths_ageband.png")
  cat("Write ", file)
  ggsave(file, w = 12, h = 10)
  
  ggplot(df, aes(x = M, y = daily.deaths, col = loc_label)) + 
    geom_point() +
    facet_wrap(~age_band_new, scale = "free") +
    labs(x = paste0("Expected ", periodicity, " deaths"), y = paste0("Observed ", periodicity, " deaths"), colour = "") +
    theme_bw()+ 
    geom_abline(slope = 1) +
    theme(legend.text = element_text(size = 15),
          axis.text.x=element_text(size = 10),
          axis.text.y=element_text(size = 10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          strip.background = element_blank(),
          strip.text = element_text(size = 15),
          panel.background = element_blank())  +
    scale_y_log10() + 
    scale_x_log10(limits = c(1e-4, 100)) 
  file = paste0(outfile.base, "-expected_observed_", periodicity, "_deaths_ageband_log.png")
  cat("Write ", file)
  ggsave(file, w = 12, h = 10)
}
