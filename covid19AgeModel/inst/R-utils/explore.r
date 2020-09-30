explore.manuscript.Rta.table <- function()
{
	indir <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa'
	job.id <- 'base_age_fsq_mobility_200728c2_cmdstanv-33states_updatedeath2707'
	
	infile <- file.path(indir, job.id, paste0(job.id,'-',"Ralastday.rds"))
	Ralastday <- readRDS(infile)
	
	rta.lt1 <- Ralastday[[2]]
	tmp <- which(rta.lt1$overall!='-')
	rta.lt1 <- rta.lt1[tmp,]
	
	rta.lt1.n <- apply(rta.lt1[,-c(1,2)], 2, function(x) sum(as.integer(x>0.95)))
	rta.lt1.p <- sprintf('%1.1f', rta.lt1.n / nrow(rta.lt1) * 100 )
	
	rta.ntl <- unclass(Ralastday[[1]][1,])	
	rta.ntl2 <- gsub('\\[|\\]','',gsub('-',' ',gsub('1\\.','1',gsub('0\\.','',rta.ntl))))[-1]
	rta.ntl2 <- 1+which(as.numeric(sapply(strsplit(rta.ntl2, ' '),'[',2))>100)	
	rta.ntl3 <- paste0(names(rta.ntl)[rta.ntl2], collapse=', ')
	rta.ntl4 <- paste0(as.character(rta.ntl[rta.ntl2]), collapse=', ')
}

eplore.ecases.prior <- function()
{
	n <- 1e5
	tau <- rexp(n, 0.02)
	ecases <- rexp(n, 1/tau)
	ecases <- data.table(v=ecases)
	ggplot(ecases) + geom_density(aes(x=v)) + coord_cartesian(xlim=c(0,500))
	
	
	mu <- 100
	beta <- 10^(seq(-7,0,.1))
	alpha <- mu*beta
	pgamma(700, shape=alpha, rate=beta)
	
	sdlog <- seq(0.5,4,0.1)
	plnorm(400, meanlog=log(mu), sdlog)
	
	sdlog <- seq(0.3,1,0.01)
	plnorm(250, meanlog=log(mu), sdlog)
	
	sdlog <- seq(0.3,1,0.01)
	plnorm(175, meanlog=log(90), sdlog)
	
	
	ecases <- rlnorm(n, meanlog=log(90), sdlog=0.4)
	ecases <- data.table(v=ecases)
	ggplot(ecases) + geom_density(aes(x=v)) + coord_cartesian(xlim=c(0,500))
	
}

explore.manuscript.epiclass.table <- function()
{
	infile <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200728c2_cmdstanv-33states_updatedeath2707/base_age_fsq_mobility_200728c2_cmdstanv-33states_updatedeath2707-Rtless_than_one_classification_of_locs.rds'
	ec <- as.matrix(readRDS(infile))	
	ec[is.na(ec[,2]),2] <- 'unclear'	
	ec <- ec[!( ec[,2]=='-' ),]
	ecn <- numeric(5)
	ecn[1] <- length(which(grepl('< 1',ec[,2])))	
	ecn[2] <- length(which(grepl('recent rise',ec[,2])))
	ecn[3] <- length(which(grepl('> 1',ec[,2]) & !grepl('recent rise',ec[,2]) & !grepl('and|many',ec[,2])))
	ecn[4] <- length(which(grepl('> 1',ec[,2]) & !grepl('recent rise',ec[,2]) & grepl('and|many',ec[,2])))
	ecn[5] <- length(which(grepl('unclear',ec[,2])))
	ecp <- sprintf('%1.1f', ecn / sum(ecn) * 100 )
}

explore.manuscript.onwards.table <- function()
{
	infile <- '~/Downloads/base_age_fsq_mobility_200728c2_cmdstanv-33states_updatedeath2707-flow-onward-from-age-tables-lastdate.rds'
	tow <- readRDS(infile)	
	towt <- as.matrix(tow[[2]])
	towt <- gsub('%','',towt)		
}


explore.ifr.by.age.meta.analysis <- function()
{
	infile <- '~/Box/OR_Work/2020/2020_covid/data_examples/Levin_et_al_data_benchmark.csv'
	infile2 <- '~/Box/OR_Work/2020/2020_covid/data_examples/Levin_et_al_data_tracetrack.csv'
	difr <- as.data.table(read.csv(infile, stringsAsFactor=FALSE))
	difr <- subset(difr, select=-c(Authors, AgeGroup, Source))
	tmp <- as.data.table(read.csv(infile2, stringsAsFactor=FALSE))
	tmp <- subset(tmp, select=c(Study, Median.Age, IFR2, Lower.Bound.1, Upper.Bound.1))
	setnames(tmp, 1:5, c('Study','Median_Age','IFR','ifr_ci95_low','ifr_ci95_high'))
	difr <- rbind(difr, subset(tmp, !is.na(Median_Age)))
	setnames(difr, c('ifr_ci95_low','ifr_ci95_high'), c('CL','CU'))
	set(difr, NULL, 'IFR', difr[, IFR/100])
	set(difr, NULL, 'CL', difr[, CL/100])
	set(difr, NULL, 'CU', difr[, CU/100])
	difr[, Median_Age_jitter:= Median_Age + rnorm(nrow(difr), 0, 0.5)]
	
	
	p1 <- ggplot(difr) +			
			geom_errorbar(aes(x=Median_Age_jitter, ymin=CL, ymax=CU), alpha=.5) +
			geom_point(aes(x=Median_Age_jitter, y=IFR, colour=Study), show.legend=FALSE) +
			theme_bw() +
			#scale_y_continuous(expand=c(0,0),  labels=scales::percent) +
			scale_y_log10(expand=c(0,0)) +			
			scale_x_continuous(expand=c(0,0), breaks= seq(0,100,10)) +
			coord_cartesian(xlim=c(0,100), ylim=c(1e-4,0.6)) +
			labs(x='Age (median)', y='Infection fatality ratio')
	
	p2 <- ggplot(difr) +			
			geom_errorbar(aes(x=Median_Age_jitter, ymin=CL, ymax=CU), alpha=.5) +
			geom_point(aes(x=Median_Age_jitter, y=IFR, colour=Study)) +
			theme_bw() +
			scale_y_continuous(expand=c(0,0),  labels=scales::percent) +			
			scale_x_continuous(expand=c(0,0), breaks= seq(0,100,10)) +
			coord_cartesian(xlim=c(0,100), ylim=c(0,0.6)) +
			labs(x='Age (median)', y='Infection fatality ratio')
	
	p <- ggarrange(p1, p2, 
			labels=c('A','B'),
			font.label=list(size=12),
			ncol=2, 
			vjust=1, 		
			widths=c(4,5)
			)
	ggsave(file='~/Box/OR_Work/2020/2020_covid/data_examples/Levin_et_al_IFR_estimates.png', p, w=12, h=5)		
}

explore.logifrbyage <- function()
{
	require(lognorm)
	
	indir <- '~/git/R0t'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")
	file_age_ifr <- file.path(indir,'data','ifr_age_200504.csv') 	
	ifr.by.age <- read_ifr_data_by_age(path_to_file_pop_us = file_us_population, path_to_file_ifrbyage = file_age_ifr)
	ifr.by.age <- as.data.table(ifr.by.age)
	tmp <- getParmsLognormForLowerAndUpper( ifr.by.age[,ifr_cl ], ifr.by.age[,ifr_cu], sigmaFac = qnorm(0.95))
	ifr.by.age[, ln_mu:= tmp[,1]]
	ifr.by.age[, ln_sd:= tmp[,2]]
	ifr.by.age[, ln_cl:= qlnorm(0.025, meanlog=ifr.by.age[, ln_mu], sdlog=ifr.by.age[, ln_sd], lower.tail=TRUE)]
	ifr.by.age[, ln_cu:= qlnorm(0.975, meanlog=ifr.by.age[, ln_mu], sdlog=ifr.by.age[, ln_sd], lower.tail=TRUE)]
	ifr.by.age[, ln_mean:= exp(ln_mu + ln_sd*ln_sd/2)]
	ifr.by.age[, age:= as.integer(as.character(age))]
	ifr.by.age[, age_cat_label:= paste0(5*(age-1),'-',5*age-1)]
	set(ifr.by.age, ifr.by.age[, which(age_cat_label=='85-89')], 'age_cat_label', '85+')
	
	#	check how well the lognormal model compares to Bobs estimates
	difr <- subset(ifr.by.age, select=-c(ifr_beta_alpha, ifr_beta_beta))	
	difr <- melt(difr, id.vars='age')
	difr[, type:= gsub('^([a-z]+)_([a-z]+)$','\\1',variable)]
	difr[, stat:= gsub('^([a-z]+)_([a-z]+)$','\\2',variable)]
	difr <- dcast.data.table(difr, age+type~stat)
	
	ggplot(difr, aes(x=age, colour=type)) + 
			geom_point( aes(y=mean)) +
			geom_errorbar( aes(ymin=cl, ymax=cu)) +
			scale_y_log10() +
			theme_bw()
	ggsave(file=file.path(outdir,'ifrage_200624_lognormal_fit.pdf'), w=8, h=6)
	#	that s pretty good
	
	setnames(ifr.by.age, 'age', 'age_cat')
	write.csv(ifr.by.age, file=file.path(outdir, 'ifr_age_200624.csv'))
}

explore.cellphone.data.200622 <- function()
{
	require(data.table)
	require(ggplot2)
	
	infile.cell <- '~/Box/OR_Work/2020/2020_covid/data_examples/US_nr_contacts_state_age_100m_2020_06_22.csv'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	dc <- as.data.table(read.csv(infile.cell, stringsAsFactors=FALSE))
	
	#	pre-processing
	set(dc, NULL, 'day', dc[, as.Date(day)])
	set(dc, NULL, 'STATEFP',NULL)
	#	TODO check I interpreted 'individual_age','Age_groups' correctly
	setnames(dc, c('individual_age','Age_groups','STATE_NAME','n','DOW','day'), c('idx.age.label','cont.age.label','loc_label','idx.n','type','date'))
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	tmp <- tmp[order(idx.age.label)]
	tmp[, idx.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='idx.age.label')
	tmp <- unique(subset(dc, select=c(cont.age.label)))
	tmp <- tmp[order(cont.age.label)]
	tmp[, cont.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='cont.age.label')
	setkey(dc, loc_label, date, idx.age.cat, cont.age.cat)
	dc[, flow:= paste0(idx.age.label,' -> ', cont.age.label)]
	
	
	#	plot raw data
	ggplot(dc) +
			geom_tile(data=unique(subset(dc, select=c(date, type))), aes(x = date, y = 0, height = Inf, fill = type), alpha = .2) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=avg_contacts, colour=flow), direction="vh") +
			geom_point(aes(x=date, y=avg_contacts, colour=flow)) +
			theme_bw() +
			labs(x= 'date', y='average cell-phone contacts within 100m', colour='contact types') +
			facet_grid(loc_label~paste0('index person ',idx.age.label)) 
	ggsave(file=file.path(outdir, 'emodo_200622_raw.pdf'), w=12, h=80,limitsize=FALSE)
	
	
	#	how does data at baseline tally up against the predicted number
	#	of expected contacts?
	dcb <- subset(dc, date<'2020-02-26')
	#dcb <- subset(dc, date>='2020-02-27' & date<'2020-03-05')
	dcb <- dcb[, list(avg_contacts=mean(avg_contacts)), by=c('loc_label','type','idx.age.cat','idx.age.label','cont.age.cat','cont.age.label','flow')]
	setnames(dcb, colnames(dcb), gsub('\\.age','.emo.age',colnames(dcb)) )
		
	
	# make age map
	tmp <- unique(subset(dcb, select=c(idx.emo.age.label)))
	tmp[, emo.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',idx.emo.age.label)))]
	tmp[, emo.age.upper:= as.integer(gsub('55\\+','99',gsub('^([0-9]+)-([0-9\\+]+)$','\\2',idx.emo.age.label)))]
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))	
	dages <- dages[, {								
				z <- which( age.cat.to >= tmp$emo.age.lower & age.cat.to <= tmp$emo.age.upper )
				list(emo.age.label= ifelse(length(z)>0, as.character(tmp$idx.emo.age.label[z]), NA_character_))					
			}, by=c('age.cat','age.cat.label')]
	
	
	#	read in expected contacts 
	indir <- '~/git/R0t'
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))	
	source(file.path(indir, "usa","code","utils","process-covariates.r"))	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	# predict contacts
	path_to_logpopmodel <- file.path(indir,'data','contact.model_logc_vs_logpop_200623.rds')	
	dcontact <- process_make_contact_matrix_by_country_using_logpop_model(pop_info, 'weekday', path_to_logpopmodel)	
	dcontact[, type:='weekday']
	tmp <- process_make_contact_matrix_by_country_using_logpop_model(pop_info, 'weekend', path_to_logpopmodel)
	tmp[, type:='weekend']
	dcontact <- rbind(dcontact, tmp)	
	# add emo age strat to dcontacts
	setnames(dages, c('age.cat','age.cat.label','emo.age.label'), c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('cont.age.cat','cont.age.cat.label'))
	setnames(dages, c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'), c('part.age.cat','part.age.cat.label','part.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('part.age.cat','part.age.cat.label'))
	setnames(dages, c('part.age.cat','part.age.cat.label','part.emo.age.label'), c('age.cat','age.cat.label','emo.age.label'))
	# add pop among participants to calculate weighted average	
	tmp <- subset(pop_info, select=c(loc_label, age.cat, pop))
	setnames(tmp, c('age.cat','pop'), c('part.age.cat','part.pop'))
	dcontact <- merge(dcontact, tmp, by=c('loc_label','part.age.cat'))
	tmp <- dcontact[, which(part.age.cat.label=='15-19')]
	set(dcontact, tmp, 'part.pop', dcontact[tmp, part.pop*2/5])
	tmp <- dcontact[, which(cont.age.cat.label=='15-19')]
	set(dcontact, tmp, 'm', dcontact[tmp, m*2/5])
	dcontact <- subset(dcontact, !is.na(cont.emo.age.label) & !is.na(part.emo.age.label))
	dcontact <- dcontact[, list(m=sum(m)), by=c('loc','loc_label','part.age.cat','part.age.cat.label','part.emo.age.label','part.pop','type','cont.emo.age.label')]
	dcontact <- dcontact[, list(m=sum(part.pop/sum(part.pop)*m)), by=c('loc','loc_label','type','part.emo.age.label','cont.emo.age.label')]
	setnames(dcontact, 'part.emo.age.label', 'idx.emo.age.label')
	
	# compare
	dcbc <- merge(dcb, dcontact, by=c('loc_label','type','idx.emo.age.label','cont.emo.age.label'))
	dcbc[, mult:= avg_contacts/m]
	tmp <- subset(dcbc, loc_label=='Alabama' & type=='weekday')
	ggplot(tmp, aes(x= flow, y=mult)) + 
			geom_point() + 
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	
	tmp <- melt(subset(dcbc, type=='weekday'), id.vars=c('loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) + 
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_wrap(~loc_label, ncol=6)
	ggsave(file=file.path(outdir, 'emodo_200622_correspondence_to_number_exp_contacts_Polymod_weekday_2.pdf'), w=12, h=25)
	tmp <- melt(subset(dcbc, type=='weekend'), id.vars=c('loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) + 
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_wrap(~loc_label, ncol=6)
	ggsave(file=file.path(outdir, 'emodo_200622_correspondence_to_number_exp_contacts_Polymod_weekend_2.pdf'), w=12, h=25)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult, col=loc_label)) + 
			geom_jitter(height=0.25, width=0.1) + 
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200622_correspondence_to_number_exp_contacts_Polymod_weekday.pdf'), w=12, h=15)
	write.csv(dcbc, file=file.path(outdir, 'emodo_200622_correspondence_to_number_exp_contacts_Polymod.csv'), row.names=FALSE)
}

explore.cellphone.mobility.data.200729 <- function()
{
	require(data.table)
	require(ggplot2)
	
	infile.cell <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_mobility_by_age_20200729.csv'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	dc <- as.data.table(read.csv(infile.cell, stringsAsFactors=FALSE))
	
	#	pre-processing
	set(dc, NULL, 'day', dc[, as.Date(day)])
	set(dc, NULL, 'STATEFP',NULL)	
	setnames(dc, c('p_moving','Age','abbr','NAME','day'), c('mobility','emo.age.label','loc','loc_label','date'))
	tmp <- unique(subset(dc, select=c(emo.age.label)))
	tmp <- tmp[order(emo.age.label)]
	tmp[, emo.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='emo.age.label')
	setkey(dc, src, loc, loc_label, date, emo.age.cat)
	set(dc, NULL, 'weekend', dc[, factor( format(date, "%u") %in% c(6, 7), levels=c(FALSE,TRUE), labels=c('no','yes') )])
	
	#	rm Puerto Rico
	dc <- subset(dc, !grepl('Puerto', loc_label))
	
	
	#	make trends
	min.date <- min(dc$date)
	tmp <- subset(dc, date<=min.date+6)
	tmp <- tmp[, list(base_mobility= mean(mobility)), by=c('src','loc','emo.age.cat')]
	dc <- merge(dc, tmp, by=c('src','loc','emo.age.cat'))
	
	#	calculate mobility trends
	dc[, mobility_trend:= mobility / base_mobility]
		
	#	use source >5m
	dc <- subset(dc, src=='mobility_5mile', select=-src)
	
	# 	Plots by state
	slist <- unique(dc$loc)
	plist <- vector('list', length(slist))
	names(plist) <- slist
	for (x in slist) 
	{
		dc_m <- subset(dc, loc==x)
		tmp <- unique(subset(dc_m, select=c(date, weekend)))
		plist[[x]] <- ggplot(dc_m) +
				geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
				scale_fill_manual(values = c("transparent", "black")) +
				geom_step(aes(x=date, y=mobility, colour=emo.age.label), direction="vh")            +
				scale_y_continuous(expand=c(0,0), lim=c(0,1)) +
				theme_bw() +
				labs(x= ' ', y='Mobility', colour='Age band', fill = "Weekend") +
				facet_wrap(~loc_label) +
				scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
				guides(colour=guide_legend(nrow=1)) +
				scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
				theme(legend.position="bottom",
						legend.title = element_text(size = 20), 
						legend.text = element_text(size = 16),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
						axis.text.y=element_text(size=14),
						axis.title=element_text(size=24),
						axis.title.x = element_blank(),
						strip.text = element_text(size = 20),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank(),
						strip.background = element_rect(
								color="white", fill="white", size=1, linetype="solid"
						))
	}	
	
	# plots all states	
	tmp <- unique(subset(dc, select=c(date, weekend)))
	#dc_t <- 
	p <- ggplot(dc) +
			geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=mobility, colour=emo.age.label), direction="vh")            +
			scale_y_continuous(expand=c(0,0), lim=c(0,1)) +
			theme_bw() +
			labs(x= ' ', y='Mobility', colour='Age band', fill = "Weekend") +
			facet_wrap(~loc_label, ncol=6) +
			scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
			guides(colour=guide_legend(nrow=1)) +
			scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
			theme(legend.position="bottom",
					legend.title = element_text(size = 20), 
					legend.text = element_text(size = 16),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
					axis.text.y=element_text(size=14),
					axis.title=element_text(size=24),
					axis.title.x = element_blank(),
					strip.text = element_text(size = 20),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_rect(
							color="white", fill="white", size=1, linetype="solid"
					))
	ggsave(file= file.path(outdir, '200729_emodo_mobility_data.pdf'), p, height=29.7,width=21, limitsize=FALSE)
	
	
	
	#	plot trends
	tmp <- unique(subset(dc, select=c(date, weekend)))
	p <- ggplot(dc) +
			geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=mobility_trend, colour=emo.age.label), direction="vh")            +
			scale_y_continuous(expand=c(0,0), labels=scales::percent) +
			theme_bw() +
			labs(x= ' ', y='Mobility trend', colour='Age band', fill = "Weekend") +
			facet_wrap(~loc_label, ncol=6) +
			scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
			guides(colour=guide_legend(nrow=1)) +
			scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
			theme(legend.position="bottom",
					legend.title = element_text(size = 20), 
					legend.text = element_text(size = 16),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
					axis.text.y=element_text(size=14),
					axis.title=element_text(size=24),
					axis.title.x = element_blank(),
					strip.text = element_text(size = 20),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_rect(
							color="white", fill="white", size=1, linetype="solid"
					))
	ggsave(file= file.path(outdir, '200729_emodo_mobility_trend.pdf'), p, height=29.7,width=21, limitsize=FALSE)
	

	
	
	#	expand to age strata used in model 
	#set(tmp, tmp[, which(age.cat<=3)], 'fsq.age.cat', 1L)
	#tmp <- subset(fsq_age_cat_map, select=c(fsq.age.cat, age.cat, age.cat.label))
	#fsq <- merge(fsq, tmp, by='fsq.age.cat', allow.cartesian=TRUE)

}

explore.cellphone.mobility.data.200811 <- function()
{
	require(data.table)
	require(ggplot2)
	
	infile.cell <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_mobility_by_age_20200811.csv'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	dc <- as.data.table(read.csv(infile.cell, stringsAsFactors=FALSE))
	
	#	pre-processing
	set(dc, NULL, 'day', dc[, as.Date(day)])
	set(dc, NULL, 'STATEFP',NULL)	
	setnames(dc, c('p_moving','Age','abbr','NAME','day'), c('mobility','emo.age.label','loc','loc_label','date'))
	tmp <- unique(subset(dc, select=c(emo.age.label)))
	tmp <- tmp[order(emo.age.label)]
	tmp[, emo.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='emo.age.label')
	setkey(dc, src, loc, loc_label, date, emo.age.cat)
	set(dc, NULL, 'weekend', dc[, factor( format(date, "%u") %in% c(6, 7), levels=c(FALSE,TRUE), labels=c('no','yes') )])
	
	#	rm Puerto Rico
	dc <- subset(dc, !grepl('Puerto', loc_label))
	
	
	#	make trends
	min.date <- min(dc$date)
	tmp <- subset(dc, date<=min.date+6)
	tmp <- tmp[, list(base_mobility= mean(mobility)), by=c('src','loc','emo.age.cat')]
	dc <- merge(dc, tmp, by=c('src','loc','emo.age.cat'))
	
	#	calculate mobility trends
	dc[, mobility_trend:= mobility / base_mobility]
	
	#	use source >5m
	dc <- subset(dc, src=='mobility_5mile', select=-src)
	
	# 	Plots by state
	slist <- unique(dc$loc)
	plist <- vector('list', length(slist))
	names(plist) <- slist
	for (x in slist) 
	{
		dc_m <- subset(dc, loc==x)
		tmp <- unique(subset(dc_m, select=c(date, weekend)))
		plist[[x]] <- ggplot(dc_m) +
				geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
				scale_fill_manual(values = c("transparent", "black")) +
				geom_step(aes(x=date, y=mobility, colour=emo.age.label), direction="vh")            +
				scale_y_continuous(expand=c(0,0), lim=c(0,1)) +
				theme_bw() +
				labs(x= ' ', y='Mobility', colour='Age band', fill = "Weekend") +
				facet_wrap(~loc_label) +
				scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
				guides(colour=guide_legend(nrow=1)) +
				scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
				theme(legend.position="bottom",
						legend.title = element_text(size = 20), 
						legend.text = element_text(size = 16),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
						axis.text.y=element_text(size=14),
						axis.title=element_text(size=24),
						axis.title.x = element_blank(),
						strip.text = element_text(size = 20),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank(),
						strip.background = element_rect(
								color="white", fill="white", size=1, linetype="solid"
						))
	}	
	
	# plots all states	
	tmp <- unique(subset(dc, select=c(date, weekend)))
	#dc_t <- 
	p <- ggplot(dc) +
			geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=mobility, colour=emo.age.label), direction="vh")            +
			scale_y_continuous(expand=c(0,0), lim=c(0,1)) +
			theme_bw() +
			labs(x= ' ', y='Mobility', colour='Age band', fill = "Weekend") +
			facet_wrap(~loc_label, ncol=6) +
			scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
			guides(colour=guide_legend(nrow=1)) +
			scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
			theme(legend.position="bottom",
					legend.title = element_text(size = 20), 
					legend.text = element_text(size = 16),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
					axis.text.y=element_text(size=14),
					axis.title=element_text(size=24),
					axis.title.x = element_blank(),
					strip.text = element_text(size = 20),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_rect(
							color="white", fill="white", size=1, linetype="solid"
					))
	ggsave(file= file.path(outdir, '200811_emodo_mobility_data.pdf'), p, height=29.7,width=21, limitsize=FALSE)
	
	
	
	#	plot trends
	tmp <- unique(subset(dc, select=c(date, weekend)))
	p <- ggplot(dc) +
			geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=mobility_trend, colour=emo.age.label), direction="vh")            +
			scale_y_continuous(expand=c(0,0), labels=scales::percent) +
			theme_bw() +
			labs(x= ' ', y='Mobility trend', colour='Age band', fill = "Weekend") +
			facet_wrap(~loc_label, ncol=6) +
			scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
			guides(colour=guide_legend(nrow=1)) +
			scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
			theme(legend.position="bottom",
					legend.title = element_text(size = 20), 
					legend.text = element_text(size = 16),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
					axis.text.y=element_text(size=14),
					axis.title=element_text(size=24),
					axis.title.x = element_blank(),
					strip.text = element_text(size = 20),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_rect(
							color="white", fill="white", size=1, linetype="solid"
					))
	ggsave(file= file.path(outdir, '200811_emodo_mobility_trend.pdf'), p, height=29.7,width=21, limitsize=FALSE)
	
	
	
	
	#	expand to age strata used in model 
	#set(tmp, tmp[, which(age.cat<=3)], 'fsq.age.cat', 1L)
	#tmp <- subset(fsq_age_cat_map, select=c(fsq.age.cat, age.cat, age.cat.label))
	#fsq <- merge(fsq, tmp, by='fsq.age.cat', allow.cartesian=TRUE)
	
}
	
explore.cellphone.data.200716 <- function()
{
	require(data.table)
	require(ggplot2)
	
	infile.cell <- '~/Box/OR_Work/2020/2020_covid/data_examples/US_nr_contacts_US_state_age_distinct_active_weighted_10m_2020-07-14.csv'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	dc <- as.data.table(read.csv(infile.cell, stringsAsFactors=FALSE))
	
	#	pre-processing
	set(dc, NULL, 'day', dc[, as.Date(day)])
	set(dc, NULL, 'STATEFP',NULL)	
	setnames(dc, c('individual_age','Age_groups','STATE_NAME','day'), c('idx.age.label','cont.age.label','loc_label','date'))
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	tmp <- tmp[order(idx.age.label)]
	tmp[, idx.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='idx.age.label')
	tmp <- unique(subset(dc, select=c(cont.age.label)))
	tmp <- tmp[order(cont.age.label)]
	tmp[, cont.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='cont.age.label')
	setkey(dc, loc_label, date, idx.age.cat, cont.age.cat)
	dc[, flow:= paste0(idx.age.label,' -> ', cont.age.label)]
	dc[, type:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('weekday','weekend'))]	
	dc <- subset(dc, !grepl('Puerto', loc_label))
	
	#	plot raw data
	ggplot(dc) +
			geom_tile(data=unique(subset(dc, select=c(date, type))), aes(x = date, y = 0, height = Inf, fill = type), alpha = .2) +
			scale_fill_manual(values = c("transparent", "black")) +
			geom_step(aes(x=date, y=avg_contacts, colour=flow), direction="vh") +
			geom_point(aes(x=date, y=avg_contacts, colour=flow)) +
			theme_bw() +
			labs(x= 'date', y='average cell-phone contacts within 100m', colour='contact types') +
			facet_grid(loc_label~paste0('index person ',idx.age.label), scales='free_y') 
	ggsave(file=file.path(outdir, 'emodo_200716_raw.pdf'), w=25, h=80,limitsize=FALSE)
	
	
	#	how does data at baseline tally up against the predicted number
	#	of expected contacts?
	dcb <- subset(dc, date<'2020-02-26')
	#dcb <- subset(dc, date>='2020-02-27' & date<'2020-03-05')
	dcb <- dcb[, list(avg_contacts=mean(avg_contacts)), by=c('loc_label','type','idx.age.cat','idx.age.label','cont.age.cat','cont.age.label','flow')]
	setnames(dcb, colnames(dcb), gsub('\\.age','.emo.age',colnames(dcb)) )
	
	
	# make age map
	tmp <- unique(subset(dcb, select=c(idx.emo.age.label)))
	tmp[, emo.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',idx.emo.age.label)))]
	tmp[, emo.age.upper:= as.integer(gsub('55\\+','99',gsub('^([0-9]+)-([0-9\\+]+)$','\\2',idx.emo.age.label)))]
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))	
	dages <- dages[, {								
				z <- which( age.cat.to >= tmp$emo.age.lower & age.cat.to <= tmp$emo.age.upper )
				list(emo.age.label= ifelse(length(z)>0, as.character(tmp$idx.emo.age.label[z]), NA_character_))					
			}, by=c('age.cat','age.cat.label')]
	
	
	#	read in expected contacts 
	indir <- '~/git/R0t'
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))	
	source(file.path(indir, "usa","code","utils","process-covariates.r"))	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# predict contacts
	dcontact <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekday', args$file_polymod_data)	
	dcontact[, type:='weekday']	
	tmp <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekend', args$file_polymod_data)	
	tmp[, type:='weekend']
	dcontact <- rbind(dcontact, tmp)	
		
	# add emo age strat to dcontacts
	setnames(dages, c('age.cat','age.cat.label','emo.age.label'), c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('cont.age.cat','cont.age.cat.label'))
	setnames(dages, c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'), c('part.age.cat','part.age.cat.label','part.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('part.age.cat','part.age.cat.label'))
	setnames(dages, c('part.age.cat','part.age.cat.label','part.emo.age.label'), c('age.cat','age.cat.label','emo.age.label'))
	
	# add pop among participants to calculate weighted average	
	tmp <- subset(pop_info, select=c(loc_label, age.cat, pop))
	setnames(tmp, c('age.cat','pop'), c('part.age.cat','part.pop'))
	dcontact <- merge(dcontact, tmp, by=c('loc_label','part.age.cat'))
	tmp <- dcontact[, which(part.age.cat.label=='15-19')]
	set(dcontact, tmp, 'part.pop', dcontact[tmp, part.pop*2/5])
	tmp <- dcontact[, which(cont.age.cat.label=='15-19')]
	set(dcontact, tmp, 'm', dcontact[tmp, m*2/5])
	dcontact <- subset(dcontact, !is.na(cont.emo.age.label) & !is.na(part.emo.age.label))
	dcontact <- dcontact[, list(m=sum(m)), by=c('loc','loc_label','part.age.cat','part.age.cat.label','part.emo.age.label','part.pop','type','cont.emo.age.label')]
	dcontact <- dcontact[, list(m=sum(part.pop/sum(part.pop)*m)), by=c('loc','loc_label','type','part.emo.age.label','cont.emo.age.label')]
	setnames(dcontact, 'part.emo.age.label', 'idx.emo.age.label')
	
	# compare
	dcbc <- merge(dcb, dcontact, by=c('loc_label','type','idx.emo.age.label','cont.emo.age.label'))
	dcbc[, mult:= avg_contacts/m]
	tmp <- subset(dcbc, loc_label=='Alabama' & type=='weekday')
	ggplot(tmp, aes(x= flow, y=mult)) + 
			geom_point() + 
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	
	tmp <- melt(subset(dcbc, type=='weekday'), id.vars=c('loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) +
			scale_y_log10() +
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_wrap(~loc_label, ncol=6)
	ggsave(file=file.path(outdir, 'emodo_200716_correspondence_to_number_exp_contacts_Polymod_weekday_2.pdf'), w=12, h=25)
	tmp <- melt(subset(dcbc, type=='weekend'), id.vars=c('loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) + 
			scale_y_log10() +
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_wrap(~loc_label, ncol=6)
	ggsave(file=file.path(outdir, 'emodo_200716_correspondence_to_number_exp_contacts_Polymod_weekend_2.pdf'), w=12, h=25)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200716_correspondence_to_number_exp_contacts_Polymod_weekday.pdf'), w=12, h=15)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			geom_line(aes(group=loc_label)) +
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200716_correspondence_to_number_exp_contacts_Polymod_weekday_3.pdf'), w=12, h=15)
	
	write.csv(dcbc, file=file.path(outdir, 'emodo_200716_correspondence_to_number_exp_contacts_Polymod.csv'), row.names=FALSE)
}

explore.cellphone.data.200729 <- function()
{
	require(data.table)
	require(ggplot2)
	
	infile.cell <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_contacts_by_age_20200729.csv'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'	
	dc <- as.data.table(read.csv(infile.cell, stringsAsFactors=FALSE))
	
	#	pre-processing
	set(dc, NULL, 'day', dc[, as.Date(day)])
	set(dc, NULL, 'STATEFP',NULL)	
	setnames(dc, c('individual_age','Age_groups','STATE_NAME','state_abbr','day','cnt_contacts'), c('idx.age.label','cont.age.label','loc_label','loc','date','n_contacts'))
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	tmp <- tmp[order(idx.age.label)]
	tmp[, idx.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='idx.age.label')
	tmp <- unique(subset(dc, select=c(cont.age.label)))
	tmp <- tmp[order(cont.age.label)]
	tmp[, cont.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='cont.age.label')
	setkey(dc, loc_label, date, idx.age.cat, cont.age.cat)
	dc[, flow:= paste0(idx.age.label,' -> ', cont.age.label)]
	dc[, type:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('weekday','weekend'))]	
	dc <- subset(dc, !grepl('Puerto', loc_label))
		
	#	plot raw data
	if(0)
	{
		ggplot(subset(dc, loc=='CA' & src=='10m_dactive')) +
				geom_tile(data=unique(subset(dc, select=c(date, type))), aes(x = date, y = 0, height = Inf, fill = type), alpha = .2) +
				scale_fill_manual(values = c("transparent", "black")) +
				geom_step(aes(x=date, y=avg_contacts, colour=flow), direction="vh") +
				geom_point(aes(x=date, y=avg_contacts, colour=flow)) +
				theme_bw() +
				labs(x= 'date', y='average cell-phone contacts within 100m', colour='contact types') +
				facet_grid(loc_label~paste0('index person ',idx.age.label), scales='free_y') 
		ggsave(file=file.path(outdir, 'emodo_200729_raw.pdf'), w=25, h=80,limitsize=FALSE)		
	}
	
	#	make pop_info 
	indir <- '~/git/R0t'
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))	
	source(file.path(indir, "usa","code","utils","process-covariates.r"))	
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
		
	# make age map
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	setnames(tmp, 'idx.age.label', 'idx.emo.age.label')	
	tmp[, emo.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',idx.emo.age.label)))]
	tmp[, emo.age.upper:= as.integer(gsub('55\\+','99',gsub('^([0-9]+)-([0-9\\+]+)$','\\2',idx.emo.age.label)))]
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))	
	dages <- dages[, {								
				z <- which( age.cat.to >= tmp$emo.age.lower & age.cat.to <= tmp$emo.age.upper )
				list(emo.age.label= ifelse(length(z)>0, as.character(tmp$idx.emo.age.label[z]), NA_character_))					
			}, by=c('age.cat','age.cat.label')]
	
	
	#	calculate sampling probabilities and merge back into dc
	tmp <- merge(dages, pop_info, by=c('age.cat','age.cat.label'))
	tmp <- tmp[!is.na(emo.age.label), list(pop=sum(pop)), by=c('loc','emo.age.label')]
	setnames(tmp, 'emo.age.label','idx.age.label')
	tmp <- merge(tmp, unique(subset(dc, select=c(date, loc, src, idx.age.cat, idx.age.label, d_users))), by=c('loc','idx.age.label'))
	tmp[, p.idx:= d_users/pop]
	setnames(tmp, 'pop','pop.idx')
	dc <- merge(dc, subset(tmp, select=c(src, loc, date, idx.age.cat, p.idx, pop.idx)), by=c('src','loc','date','idx.age.cat'))
	setnames(tmp, c('idx.age.cat','p.idx'), c('cont.age.cat','p.cont'))
	dc <- merge(dc, subset(tmp, select=c(src, loc, date, cont.age.cat, p.cont)), by=c('src','loc','date','cont.age.cat'))
	
	#	make sampling adjusted contact intensities
	dc[, exp_contacts:= n_contacts/p.idx/p.cont]
	dc[, exp_cintensity:= exp_contacts/pop.idx]
	
	#	read in expected contacts 
	indir <- '~/git/R0t'
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))	
	source(file.path(indir, "usa","code","utils","process-covariates.r"))	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# predict contacts
	file_polymod_data <- file.path(indir,'data','polymod_data_with_covariates_200623.rds')
	dcontact <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekday', file_polymod_data)	
	dcontact[, type:='weekday']	
	tmp <- process_make_contact_matrix_by_country_using_popp_logpopdens_model(pop_info, 'weekend', file_polymod_data)	
	tmp[, type:='weekend']
	dcontact <- rbind(dcontact, tmp)	
	
	# add emo age strat to dcontacts
	setnames(dages, c('age.cat','age.cat.label','emo.age.label'), c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('cont.age.cat','cont.age.cat.label'))
	setnames(dages, c('cont.age.cat','cont.age.cat.label','cont.emo.age.label'), c('part.age.cat','part.age.cat.label','part.emo.age.label'))
	dcontact <- merge(dcontact, dages, by=c('part.age.cat','part.age.cat.label'))
	setnames(dages, c('part.age.cat','part.age.cat.label','part.emo.age.label'), c('age.cat','age.cat.label','emo.age.label'))
	
	# add pop among participants to calculate weighted average	
	tmp <- subset(pop_info, select=c(loc_label, age.cat, pop))
	setnames(tmp, c('age.cat','pop'), c('part.age.cat','part.pop'))
	dcontact <- merge(dcontact, tmp, by=c('loc_label','part.age.cat'))
	tmp <- dcontact[, which(part.age.cat.label=='15-19')]
	set(dcontact, tmp, 'part.pop', dcontact[tmp, part.pop*2/5])
	tmp <- dcontact[, which(cont.age.cat.label=='15-19')]
	set(dcontact, tmp, 'm', dcontact[tmp, m*2/5])
	dcontact <- subset(dcontact, !is.na(cont.emo.age.label) & !is.na(part.emo.age.label))
	dcontact <- dcontact[, list(m=sum(m)), by=c('loc','loc_label','part.age.cat','part.age.cat.label','part.emo.age.label','part.pop','type','cont.emo.age.label')]
	dcontact <- dcontact[, list(m=sum(part.pop/sum(part.pop)*m)), by=c('loc','loc_label','type','part.emo.age.label','cont.emo.age.label')]
	setnames(dcontact, 'part.emo.age.label', 'idx.emo.age.label')
	
	#	how does data at baseline tally up against the predicted number
	#	of expected contacts?
	date.min <- min( dc$date )
	dcb <- subset(dc, date<=date.min+14-1)
	#dcb <- subset(dc, date>='2020-02-27' & date<'2020-03-05')
	dcb <- dcb[, 
			list(
					avg_contacts=mean(avg_contacts),
					n_contacts=mean(n_contacts),
					exp_cintensity=mean(exp_cintensity)
			), 
			by=c('src','loc_label','type','idx.age.cat','idx.age.label','cont.age.cat','cont.age.label','flow')]
	setnames(dcb, colnames(dcb), gsub('\\.age','.emo.age',colnames(dcb)) )
	
	
	#
	# compare age-age contacts
	dcbc <- merge(dcb, dcontact, by=c('loc_label','type','idx.emo.age.label','cont.emo.age.label'))
	dcbc[, mult_avgcontacts:= avg_contacts/m]
	dcbc[, mult_ncontacts:= n_contacts/m]
	dcbc[, mult_expcintensity:= exp_cintensity/m]
	
	tmp <- subset(dcbc, loc_label=='Alabama' & type=='weekday' & src=='10m_dactive')
	ggplot(tmp, aes(x= flow, y=mult_avgcontacts)) + 
			geom_point() + 
			coord_flip() + 
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	
	tmp <- melt(subset(dcbc, type=='weekday'), id.vars=c('src','loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) +
			scale_y_log10() +
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_grid(loc_label~src)
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_exp_contacts_Polymod_weekday_2.pdf'), w=12, h=100, limitsize=FALSE)
	tmp <- melt(subset(dcbc, type=='weekend'), id.vars=c('src','loc_label','type','idx.emo.age.label','cont.emo.age.label','idx.emo.age.cat','cont.emo.age.cat','loc','flow'), measure.vars=c('avg_contacts','m'))
	ggplot(tmp, aes(x= flow, y=value, colour=variable)) + 
			scale_y_log10() +
			geom_point() + 
			coord_flip() +  
			labs(x='idx age -> cntct age', y='') +
			facet_grid(loc_label~src)
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_exp_contacts_Polymod_weekend_2.pdf'), w=12, h=100, limitsize=FALSE)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_avgcontacts, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_exp_contacts_Polymod_weekday.pdf'), w=20, h=15)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_ncontacts, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			geom_line(aes(group=loc_label)) +
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='n_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_exp_ncontacts_Polymod_weekday.pdf'), w=20, h=15)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_expcintensity, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			geom_line(aes(group=loc_label)) +
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='n_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_expcintensity_Polymod_weekday.pdf'), w=20, h=15)
	
	#	understand multipliers on same scale
	tmp <- dcbc[idx.emo.age.label!='55+', list(mult_avgcontacts_mean=mean(mult_avgcontacts),
			mult_ncontacts_mean=mean(mult_ncontacts),
			mult_expcintensity_mean=mean(mult_expcintensity)
			), 
		by=c('src','type')]
	dcbc <- merge(dcbc, tmp, by=c('src','type'))
	dcbc[, mult_avgcontacts2:= mult_avgcontacts/mult_avgcontacts_mean]
	dcbc[, mult_ncontacts2:= mult_ncontacts/mult_ncontacts_mean]
	dcbc[, mult_expcintensity2:= exp_cintensity/mult_expcintensity_mean]
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_avgcontacts2, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_avgcontacts2_Polymod_weekday.pdf'), w=20, h=15)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_ncontacts2, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='n_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_ncontacts2_Polymod_weekday.pdf'), w=20, h=15)
	
	ggplot(subset(dcbc, type=='weekday'), aes(x= flow, y=mult_expcintensity2, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='n_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_correspondence_to_number_expcintensity2_Polymod_weekday.pdf'), w=20, h=15)
	
	
	
	#write.csv(dcbc, file=file.path(outdir, 'emodo_200729_correspondence_to_number_exp_contacts_Polymod.csv'), row.names=FALSE)
	
	#
	#	compare marginal contacts
	dmc <- dcontact[, list(m=sum(m)), by=c('loc','loc_label','type','idx.emo.age.label')]
	tmp <- dcb[, list(avg_contacts=sum(avg_contacts)), by=c('src','loc_label','type','idx.emo.age.cat','idx.emo.age.label')]
	dmc <- merge(dmc, tmp, by=c('loc_label','type','idx.emo.age.label'))
	dmc[, mult:= avg_contacts/m]
	
	ggplot(subset(dmc, type=='weekday'), aes(x= idx.emo.age.label, y=mult, col=loc_label)) + 
			geom_jitter(height=0, width=0.25) + 
			coord_flip() + 
			facet_grid(.~src) +
			labs(x='idx age -> cntct age', y='avg_contacts (cell) <= 02-26  / expected contacts (survey)')
	ggsave(file=file.path(outdir, 'emodo_200729_marginal_correspondence_to_number_exp_contacts_Polymod_weekday.pdf'), w=15, h=8)
	
	#
	#
	#	conclusion: in comparison to other options, 10m_dactive is ok to use
	#
	#

	
	#	make avg contacts
	dmc <- dc[, list(avg_contacts= sum(avg_contacts),
			n_contacts=as.double(sum(cnt_contacts)),
			n_users=d_users[1]), 
		by=c('loc','loc_label','idx.age.cat','idx.age.label','date','type','src')]
	dmc <- melt(dmc, measure.vars=c('avg_contacts','n_contacts'))

	#	calculate baseline	
	tmp <- subset(dmc, date< date.min+14)	
	tmp <- tmp[, list( value_baseline=mean(value) ), 
		by=c('loc_label','idx.age.label','src','variable')]

	#	make trend
	dmc <- merge(dmc, tmp, by=c('loc_label','idx.age.label','src','variable'))
	dmc[, mobility_trend:= value / value_baseline]	
	setnames(dmc, c('idx.age.cat','idx.age.label','type'), c('emo.age.cat','emo.age.label','weekend'))
	
	
	#	plot trends
	tmp <- unique(subset(dmc, select=c(date, weekend)))
	select.srcs <- unique(dmc$src)
	select.variables <- unique(dmc$variable)
	for(i in seq_along(select.srcs))
	{
		for(j in seq_along(select.variables))
		{
			p <- ggplot(subset(dmc, src==select.srcs[i] & variable==select.variables[j])) +
					geom_hline(yintercept=1, linetype='dotted') +
					geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
					scale_fill_manual(values = c("transparent", "black")) +
					geom_step(aes(x=date, y=mobility_trend, colour=emo.age.label), direction="vh")            +
					scale_y_continuous(expand=c(0,0), labels=scales::percent) +
					theme_bw() +
					labs(x= ' ', y='Mobility trend', colour='Age band', fill = "Weekend") +
					facet_wrap(~loc_label, ncol=6) +
					scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
					guides(colour=guide_legend(nrow=1)) +
					scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
					coord_cartesian(ylim=c(0,3)) +
					theme(legend.position="bottom",
							legend.title = element_text(size = 20), 
							legend.text = element_text(size = 16),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
							axis.text.y=element_text(size=14),
							axis.title=element_text(size=24),
							axis.title.x = element_blank(),
							strip.text = element_text(size = 20),
							panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
							panel.background = element_blank(),
							strip.background = element_rect(
									color="white", fill="white", size=1, linetype="solid"
							))
			ggsave(file= file.path(outdir, paste0('emodo_200729_mobility_trend_',select.variables[j],'_',select.srcs[i],'.pdf')), p, height=29.7,width=21, limitsize=FALSE)			
		}
	}

	#	plot n_users
	tmp <- unique(subset(dmc, select=c(date, weekend)))
	select.srcs <- unique(dmc$src)
	select.variables <- unique(dmc$variable)
	for(i in seq_along(select.srcs))
	{
		for(j in seq_along(select.variables))
		{
			p <- ggplot(subset(dmc, src==select.srcs[i] & variable==select.variables[j])) +
					geom_hline(yintercept=1, linetype='dotted') +
					geom_tile(data=tmp, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend=FALSE) +
					scale_fill_manual(values = c("transparent", "black")) +
					geom_step(aes(x=date, y=n_users, colour=emo.age.label), direction="vh")            +
					scale_y_continuous(expand=c(0,0)) +
					theme_bw() +
					labs(x= ' ', y='Mobility trend', colour='Age band', fill = "Weekend") +
					facet_wrap(~loc_label, ncol=6) +
					scale_colour_viridis_d(begin=0.2,end=1,alpha=1,direction=-1,option = "C") +
					guides(colour=guide_legend(nrow=1)) +
					scale_x_date(date_breaks = "months", labels = date_format("%e %b")) +
					theme(legend.position="bottom",
							legend.title = element_text(size = 20), 
							legend.text = element_text(size = 16),
							axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1,size=12),
							axis.text.y=element_text(size=14),
							axis.title=element_text(size=24),
							axis.title.x = element_blank(),
							strip.text = element_text(size = 20),
							panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
							panel.background = element_blank(),
							strip.background = element_rect(
									color="white", fill="white", size=1, linetype="solid"
							))
			ggsave(file= file.path(outdir, paste0('emodo_200729_n_users_',select.variables[j],'_',select.srcs[i],'.pdf')), p, height=29.7,width=21, limitsize=FALSE)			
		}
	}
	
	
	
	#	calculate variance in first order differences	
	dmcs <- dmc[, list(ts_sd = sd(diff(mobility_trend)),
					n_users_mean=mean(n_users)), by=c('src','variable','loc')]
	dc.select.loc <- subset(dmcs, n_users_mean>2e4)[, sort(unique(loc))]
	ggplot(dmcs, aes(x=loc, y=ts_sd, colour=interaction(src,variable))) + 
			geom_point() +
			geom_line(aes(group=interaction(src,variable)))
	
	ggplot(dmcs, aes(x=interaction(src,variable), y=ts_sd, colour=interaction(src,variable))) +
			geom_boxplot() +
			coord_flip()
	
	ggplot(dmcs, aes(x=n_users_mean, y=ts_sd, colour=interaction(src,variable))) +
			geom_point() 
	
	
	#
	#	conclusion: n_users much less variable than avg_contacts
	#	but unclear if relative trends are similar?
	#

	dmcs <- subset(dmc, weekend=='weekday' & src=='10m_dactive' & loc%in%dc.select.loc & loc!='NC')
	date.max <- max(dmcs$date)
	dmcs <- subset(dmcs, date==date.max | date=='2020-04-15')
	
	tmp <- dcast.data.table(dmcs, loc_label+loc+emo.age.cat+emo.age.label+date+n_users~variable, value.var='mobility_trend')
	ggplot(tmp, aes(x=n_contacts, y=avg_contacts, colour=loc_label)) +
			geom_abline(slope=1) +
			geom_point() +
			facet_grid(date~emo.age.label)
	ggsave(file= file.path(outdir, 'emodo_200729_comparison_ncontacts_avgcontacts_correlation.pdf'), height=8,width=10)
	
	#	n_contacts tend to be lower than avg_contacts
	
	ggplot(dmcs, aes(y=loc_label, x=mobility_trend, pch=variable, colour=emo.age.label)) +
			geom_point() +
			facet_grid(date~.)
	ggsave(file= file.path(outdir, 'emodo_200729_comparison_ncontacts_avgcontacts_on_dates.pdf'), height=8,width=8)		
}

explore.contact.matrices_1 <- function()
{
	library(MASS)
	
	indir <- '~/git/R0t'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))
	source(file.path(indir, "usa","code","utils","process-covariates.r"))
	
	infiles <- data.table(F=list.files(path=file.path(indir,'data'), pattern='polymod.tab.bin.*', full.names=TRUE))
	infiles[, TYPE:= 'anyday']
	set(infiles, which(grepl('weekday',infiles$F)), 'TYPE', 'weekday')
	set(infiles, which(grepl('weekend',infiles$F)), 'TYPE', 'weekend')
	infiles[, LOC:= gsub('^polymod.tab.bin_([A-Z]+).*','\\1', basename(F))]
	infiles[, IDX:= seq_len(nrow(infiles))]
	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# make Polymod contact matrices
	dps <- list()
	for( i in infiles$IDX)
	{		
		tmp <- read_contact_rate_matrix(pop_info, infiles$F[i])
		tmp[, LOC:= infiles$LOC[i]]
		tmp[, TYPE:= infiles$TYPE[i]]
		dps[[i]] <- tmp
	}
	dps <- do.call('rbind', dps)
	dps[, cont_pop:= m/c]
	
	tmp <- unique(subset(dps, part.age.cat==1 & TYPE=='weekday', select=c(LOC, TYPE, cont.age.cat, cont.age.cat.label, cont_pop)))
	tmp <- tmp[, list(	pop_total=sum(cont_pop),
						cont.age.cat=cont.age.cat,
						cont_pop_p= cont_pop/sum(cont_pop)
						),by='LOC']	
	dps <- merge(dps, tmp, by=c('LOC','cont.age.cat'))
			
	#	expected number of contacts
	dpsm <- dps[, list(m=sum(m),
					c=sum(cont_pop/sum(cont_pop) * c),
					pop_total= pop_total[1]
					), by=c('LOC','TYPE','part.age.cat','part.age.cat.label')]	
	ggplot(dpsm, aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex.pdf'), w=15, h=10)
	
	ggplot(subset(dpsm, TYPE=='weekday'), aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_wrap(.~part.age.cat.label, ncol=5) +
			labs(x='\ncountry',y='expected number of contacts from index person\n(posterior median)\n') +
			guides(colour = FALSE) 
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex_weekday.pdf'), w=10, h=10)
	
	#	calculate coefficient of variation in m and c
	dpsm_cov <- dpsm[, list(m_cov= sd(m)/mean(m), c_cov= sd(c)/mean(c)), by=c('TYPE','LOC')]
	#     TYPE LOC     m_cov     c_cov
 	#1: weekday  BE 0.3828274 0.3828274
 	#2: weekend  BE 0.3716048 0.3716048
 	#3:  anyday  BE 0.3665788 0.3665788
	#	--> this is identical. this makes sense: constants cancel out in the CoV. 
	#	--> so for age bands fixed, m and c have same CoV across countries
	#	
	
	ggplot(dpsm, aes(x=pop_total, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndexByPop.pdf'), w=15, h=10)
	#	nothing apparent. could do linear regression to confirm not significant predictor


	ggplot(subset(dps, TYPE=='weekday'), aes(x=log(cont_pop), y=log(c))) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(col=LOC)) + 			
			theme_bw() +
			facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
			labs(colour='countries', x='log population size of contacts', y='log contact rate')	
	ggsave(file.path(outdir,'200623_weekday_contacts_logc_vs_logpop.pdf'), w=25, h=25)
	#	--> correlation between log(pop) and log(c) is -1.
	#		this makes sense. log(Y) ~ log(U) + log(c) <=>  
	#						  log(Y) ~ log(T) + log(pop) + log(c) <=> 
	#						  log(m) ~ log(pop) + log(c)
	#	so we can use this relationship to predict log(c)
	
	ggplot(tmp, aes(x=cont_pop_p, y=m)) +
		geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
		geom_point(aes(col=LOC)) + 			
		theme_bw() +
		facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
		labs(colour='countries', x='log population size of contacts', y='log contact rate')	


	dpsfit <- dps[, {
				z<- coef(lm( log(c) ~ log(cont_pop) ))
				list(lm_formula='log(c)~log(cont_pop)', lm_intercept= z[1], lm_slope=z[2])				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	save(dps, file=file.path(outdir, '200623_contacts_estimates_polymod.rda'))
	saveRDS(dpsfit, file=file.path(outdir, '200623_contacts_logc_vs_logpop.rds'))
}

explore.contact.matrices_3 <- function()
{
	library(MASS)
	
	indir <- '~/git/R0t'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))
	source(file.path(indir, "usa","code","utils","process-covariates.r"))
	
	infiles <- data.table(F=list.files(path=file.path(indir,'data'), pattern='polymod.tab.bin.*', full.names=TRUE))
	infiles[, TYPE:= 'anyday']
	set(infiles, which(grepl('weekday',infiles$F)), 'TYPE', 'weekday')
	set(infiles, which(grepl('weekend',infiles$F)), 'TYPE', 'weekend')
	infiles[, LOC:= gsub('^polymod.tab.bin_([A-Z]+).*','\\1', basename(F))]
	infiles[, IDX:= seq_len(nrow(infiles))]
	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# make Polymod contact matrices
	dps <- list()
	for( i in infiles$IDX)
	{		
		tmp <- read_contact_rate_matrix(pop_info, infiles$F[i])
		tmp[, LOC:= infiles$LOC[i]]
		tmp[, TYPE:= infiles$TYPE[i]]
		dps[[i]] <- tmp
	}
	dps <- do.call('rbind', dps)
	dps[, cont_pop:= m/c]
	
	tmp <- unique(subset(dps, part.age.cat==1 & TYPE=='weekday', select=c(LOC, TYPE, cont.age.cat, cont.age.cat.label, cont_pop)))
	tmp <- tmp[, list(	pop_total=sum(cont_pop),
					cont.age.cat=cont.age.cat,
					cont_pop_p= cont_pop/sum(cont_pop)
			),by='LOC']	
	dps <- merge(dps, tmp, by=c('LOC','cont.age.cat'))
	
	#	add land area 
	#	https://data.worldbank.org/indicator/AG.LND.TOTL.K2?locations=EU
	#	2018
	tmp <- data.table(LOC= c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL"),
			land_area= c(30280, 349360.0, 303910.0, 241930.0, 294140.0, 2430.0, 33690.0, 306190.0)
	)
	dps <- merge(dps, tmp, by='LOC')
	dps[, cont_pop_dens:= cont_pop/land_area]
		
	#	set up factors of labels
	tmp <- unique(subset(dps, select=c(part.age.cat, part.age.cat.label)))
	tmp[, part.age.cat.label2:= factor(part.age.cat, levels=part.age.cat, labels=paste0('p',part.age.cat.label))]
	dps <- merge(dps, tmp, by=c('part.age.cat','part.age.cat.label'))
	tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label)))
	tmp[, cont.age.cat.label2:= factor(cont.age.cat, levels=cont.age.cat, labels=paste0('c',cont.age.cat.label))]
	dps <- merge(dps, tmp, by=c('cont.age.cat','cont.age.cat.label'))
	
	#	save
	saveRDS(dps, file=file.path(outdir, '200623_Polymod_data_with_covariates.rds'))
	
		
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop_p, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='prop of contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpop_p.pdf'), w=25, h=25)
	#	--> slight tendency for more contact with higher cont_pop_p
	
	
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpop.pdf'), w=25, h=25)
	#	--> slight tendency for more contact with smaller cont_pop
	#		suggests that pop density might matter?
	
	tmp <- subset(dps, TYPE=='weekday' & part.age.cat.label=='40-44' & cont.age.cat.label=='0-4')
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop/land_area, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population/land area in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpopdensity.pdf'), w=25, h=25)
	#	--> this is not as good as expected! need to dig a bit deeper
	
	
		
	m_pop_dens <- lm(log(m) ~ cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_p <- lm(log(m) ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop <- lm(log(m) ~ cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_total <- lm(log(m) ~ pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))	
	m_lpop_dens <- lm(log(m) ~ log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lpop_p <- lm(log(m) ~ log(cont_pop_p) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lpop <- lm(log(m) ~ log(cont_pop) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lpop_total <- lm(log(m) ~ log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))	
	m_mix1 <- lm(log(m) ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix1b <- lm(log(m) ~ cont_pop_p + cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix1c <- lm(log(m) ~ cont_pop_p + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix1d <- lm(log(m) ~ cont_pop_p + log(cont_pop_dens) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))	
	m_mix2 <- lm(log(m) ~ cont_pop + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix2b <- lm(log(m) ~ cont_pop + cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix2c <- lm(log(m) ~ cont_pop + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix3 <- lm(log(m) ~ cont_pop_p + pop_total + cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix3b <- lm(log(m) ~ cont_pop_p + pop_total + cont_pop + cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix3c <- lm(log(m) ~ cont_pop_p + pop_total + cont_pop + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix1 <- lm(log(m) ~ log(cont_pop_p) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix1c <- lm(log(m) ~ log(cont_pop_p) + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix2 <- lm(log(m) ~ log(cont_pop) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix2c <- lm(log(m) ~ log(cont_pop) + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix3 <- lm(log(m) ~ log(cont_pop_p) + log(pop_total) + log(cont_pop) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_lmix3c <- lm(log(m) ~ log(cont_pop_p) + pop_total + cont_pop + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	AIC(m_pop_dens, m_pop_p, m_pop, m_pop_total, m_lpop_dens, m_lpop_p, m_lpop, m_lpop_total, m_mix1, m_mix2, m_mix3, m_mix1b, m_mix2b, m_mix3b, m_mix1c, m_mix2c, m_mix3c, m_mix1d, m_lmix1, m_lmix2, m_lmix3, m_lmix1c, m_lmix2c, m_lmix3c)
	#	             df      AIC
	#m_pop_dens   326 3419.457
	#m_pop_p      326 3360.532
	#m_pop        326 3357.121 *
	#m_pop_total  326 3334.715 *
	#m_lpop_dens  326 3354.404 *
	#m_lpop_p     326 3391.471
	#m_lpop       326 3362.400 *
	#m_lpop_total 326 3349.861 *
	#m_mix1       327 3251.853 *
	#m_mix2       327 3328.553
	#m_mix3       328 3253.562 *	
	#m_mix1b      327 3351.398
	#m_mix2b      327 3340.018
	#m_mix3b      329 3247.968
	#m_mix1c      327 3291.239
	#m_mix2c      327 3215.325 **
	#m_mix3c      329 3125.238 **
	#m_mix1d      328 3155.549 ***	
	#m_lmix1      327 3284.751 *
	#m_lmix2      327 3284.751
	#m_lmix3      327 3284.751
	#m_lmix1c     327 3317.502
	#m_lmix2c     327 3232.284
	#m_lmix3c     329 3138.602 **

	summary(m_mix1)
	#cont_pop_p	1.190e+01  1.371e+00   8.684  < 2e-16 ***
	#pop_total	-2.822e-09  2.839e-10  -9.943  < 2e-16 ***
		
	m_pop_dens <- lm(log(m) ~ cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_pop_p <- lm(log(m) ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_pop <- lm(log(m) ~ cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_pop_total <- lm(log(m) ~ pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))	
	m_lpop_dens <- lm(log(m) ~ log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_lpop_p <- lm(log(m) ~ log(cont_pop_p) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_lpop <- lm(log(m) ~ log(cont_pop) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_lpop_total <- lm(log(m) ~ log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))	
	m_mix1 <- lm(log(m) ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_mix2 <- lm(log(m) ~ cont_pop + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_mix3 <- lm(log(m) ~ cont_pop_p + pop_total + cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_lmix1 <- lm(log(m) ~ log(cont_pop_p) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))
	m_lmix2 <- lm(log(m) ~ log(cont_pop) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))	
	m_lmix3 <- lm(log(m) ~ log(cont_pop_p) + log(pop_total) + log(cont_pop) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekend'))	
	AIC(m_pop_dens, m_pop_p, m_pop, m_pop_total, m_lpop_dens, m_lpop_p, m_lpop, m_lpop_total, m_mix1, m_mix2, m_mix3, m_lmix1, m_lmix2, m_lmix3)
	#df      AIC
	#m_pop_dens   326 3050.881
	#m_pop_p      326 3016.469
	#m_pop        326 2944.435 *
	#m_pop_total  326 2909.892 *
	#m_lpop_dens  326 3017.985
	#m_lpop_p     326 3046.840
	#m_lpop       326 2939.199 *
	#m_lpop_total 326 2924.341 *
	#m_mix1       327 2844.028 **
	#m_mix2       327 2900.302
	#m_mix3       328 2845.837 *
	#m_lmix1      327 2874.940 *
	#m_lmix2      327 2874.940
	#m_lmix3      327 2874.940

	summary(m_mix1)
	#cont_pop_p		9.822e+00  1.267e+00   7.753 1.34e-14 ***
	#pop_total		-3.296e-09  2.624e-10 -12.560  < 2e-16 ***
	
	require(caret)		
	train.control <- trainControl(method = "LOOCV")
	loocv_pop_p <- train( log(m) ~ log(cont_pop_p) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	print(loocv_pop_p)
	#	Linear Regression 
	#2592 samples
   	#4 predictor
	#No pre-processing
	#Resampling: Leave-One-Out Cross-Validation 
	#Summary of sample sizes: 2591, 2591, 2591, 2591, 2591, 2591, ... 
	#Resampling results:
  	#RMSE       Rsquared   MAE      
  	#0.4597928  0.8422331  0.3580529

	
	
	#	predict contact intensities with model m_mix1c	
	dtrain <- subset(dps, TYPE=='weekday')
	dtrain[, log_m:= log(m)]
	dtrain[, log_cont_pop_dens:= log(cont_pop_dens)]
	m_mix1c <- lm(log_m ~ cont_pop_p + log_cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data= dtrain)
	tmp <- as.data.table(predict(m_mix1c, interval='prediction'))
	colnames(tmp) <- c('pr_m','pr_cl','pr_cu')
	set(tmp, NULL, 'pr_m', tmp[, exp(pr_m)])
	set(tmp, NULL, 'pr_cl', tmp[, exp(pr_cl)])
	set(tmp, NULL, 'pr_cu', tmp[, exp(pr_cu)])
	dtrain <- cbind(dtrain, tmp)
	
	nrow(subset(dtrain, m<pr_cl | m>pr_cu)) / nrow(dtrain)
	#	0.0320216

	tmp <- data.table(LOC= c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL"),
				LOC_LABEL= c("Belgium", "Germany", "Finland", "UK", "Italy", "Luxembourg","Netherlands","Poland"))
	dtrain <- merge(dtrain, tmp, by='LOC')		
	ggplot(dtrain) +			
			geom_point(aes(x=LOC_LABEL, y=pr_m), colour='grey25', pch=2) +
			geom_point(aes(x=LOC_LABEL, y=m), colour='blue') +
			geom_errorbar(aes(x=LOC_LABEL, ymin=pr_cl, ymax=pr_cu), width=0.5, colour='grey25') + 
			theme_bw() +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
			labs(x='countries', y='contact intensity')
			
			geom_line(aes(x=cont_pop_p, y=log(m))) +
			geom_point(aes(col=LOC)) + 			
			labs(colour='countries', x='population size of contacts (%)', y='log contact intensity')	
	ggsave(file.path(outdir,'200623_weekday_contacts_predictions.pdf'), w=25, h=25)
	tmp <- subset(dtrain, cont.age.cat<=6 & part.age.cat<=6)
	set(tmp, NULL, 'part.age.cat.label3', tmp[, paste0('age of index person\n',part.age.cat.label)])
	set(tmp, NULL, 'cont.age.cat.label3', tmp[, paste0('age of contacted persons\n',cont.age.cat.label)])
	set(tmp, NULL, 'part.age.cat.label3', tmp[, factor(part.age.cat, levels=part.age.cat, labels=part.age.cat.label3)])
	set(tmp, NULL, 'cont.age.cat.label3', tmp[, factor(cont.age.cat, levels=cont.age.cat, labels=cont.age.cat.label3)])
	ggplot(tmp) +			
			geom_point(aes(x=LOC_LABEL, y=pr_m), colour='grey25', pch=2) +
			geom_point(aes(x=LOC_LABEL, y=m), colour='blue') +
			geom_errorbar(aes(x=LOC_LABEL, ymin=pr_cl, ymax=pr_cu), width=0.5, colour='grey25') + 
			theme_bw() +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			facet_grid(part.age.cat.label3 ~ cont.age.cat.label3) +
			labs(x='countries', y='contact intensity')
	ggsave(file.path(outdir,'200623_weekday_contacts_predictions_first6.pdf'), w=15, h=15)
	
	
	
	dpsfit2 <- dps[, {
				cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label)
				#	take out extremely influential observations
				lm.m <- lm( m ~ cont_pop_p)
				cooks <- cooks.distance(lm.m)
				print(cooks)
				cooks.extremely.influential <- which(cooks>1)
				print(cooks.extremely.influential)
				m_2 <- m
				cont_pop_p_2 <- cont_pop_p
				if(length(cooks.extremely.influential))
				{
					cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label,' influential obersvations ',tmp[cooks.extremely.influential,LOC])
					m_2 <- m[-cooks.extremely.influential]
					cont_pop_p_2 <- cont_pop_p[-cooks.extremely.influential]					
				}
				
				print(cont_pop_p_2)
				
				#	make Box Cox transformation
				tmp2 <- boxcox(m_2~cont_pop_p_2, lambda=seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
				lambda <- tmp2$x[which.max(tmp2$y)]
				if(lambda!=0)
				{
					m_2_boxcox <- (m_2^lambda-1)/lambda	
				}
				if(lambda==0)
				{
					m_2_boxcox <- log(m_2)	
				}
				
				print( m_2_boxcox )
				
				#	make linear regression
				lm.m <- lm( m_2_boxcox ~ cont_pop_p_2 )
				lm.m.coef <- coef( lm.m )
				lm.m.sig <- summary(lm.m)$coefficients[,4]  
				
				print(lm.m.coef)
				
				#	return
				list( 	lm_formula='m_2_boxcox ~ cont_pop_p_2', 
						coef1 = lm.m.coef[1],
						coef2 = lm.m.coef[2],
						sig1 = lm.m.sig[1],
						sig2 = lm.m.sig[2]
				)				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	
	
	dpsfit <- dps[, {
				z<- coef(lm( log(c) ~ log(cont_pop) ))
				list(lm_formula='log(c)~log(cont_pop)', lm_intercept= z[1], lm_slope=z[2])				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	save(dps, file=file.path(outdir, '200623_contacts_estimates_polymod.rda'))
	saveRDS(dpsfit, file=file.path(outdir, '200623_contacts_logc_vs_logpop.rds'))
}

explore.contact.matrices_2 <- function()
{
	library(MASS)
	
	indir <- '~/git/R0t'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))
	source(file.path(indir, "usa","code","utils","process-covariates.r"))
	
	infiles <- data.table(F=list.files(path=file.path(indir,'data'), pattern='polymod.tab.bin.*', full.names=TRUE))
	infiles[, TYPE:= 'anyday']
	set(infiles, which(grepl('weekday',infiles$F)), 'TYPE', 'weekday')
	set(infiles, which(grepl('weekend',infiles$F)), 'TYPE', 'weekend')
	infiles[, LOC:= gsub('^polymod.tab.bin_([A-Z]+).*','\\1', basename(F))]
	infiles[, IDX:= seq_len(nrow(infiles))]
	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# make Polymod contact matrices
	dps <- list()
	for( i in infiles$IDX)
	{		
		tmp <- read_contact_rate_matrix(pop_info, infiles$F[i])
		tmp[, LOC:= infiles$LOC[i]]
		tmp[, TYPE:= infiles$TYPE[i]]
		dps[[i]] <- tmp
	}
	dps <- do.call('rbind', dps)
	dps[, cont_pop:= m/c]
	
	tmp <- unique(subset(dps, part.age.cat==1 & TYPE=='weekday', select=c(LOC, TYPE, cont.age.cat, cont.age.cat.label, cont_pop)))
	tmp <- tmp[, list(	pop_total=sum(cont_pop),
					cont.age.cat=cont.age.cat,
					cont_pop_p= cont_pop/sum(cont_pop)
			),by='LOC']	
	dps <- merge(dps, tmp, by=c('LOC','cont.age.cat'))
	
	#	add land area 
	#	https://data.worldbank.org/indicator/AG.LND.TOTL.K2?locations=EU
	#	2018
	tmp <- data.table(LOC= c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL"),
			land_area= c(30280, 349360.0, 303910.0, 241930.0, 294140.0, 2430.0, 33690.0, 306190.0)
	)
	dps <- merge(dps, tmp, by='LOC')
	dps[, cont_pop_dens:= cont_pop/land_area]
	
	if(0)
	{
		#	calculate influential points in m ~ cont_pop_p model
		tmp <- dps[, {					
					lm.m <- lm( m ~ cont_pop_p)
					cooks <- cooks.distance(lm.m)
					list(	LOC=LOC,
							cooks=cooks)						
				}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
		dps <- merge(dps, tmp, , by=c('TYPE','LOC','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label'))		
	}
	
	#	set up factors of labels
	tmp <- unique(subset(dps, select=c(part.age.cat, part.age.cat.label)))
	tmp[, part.age.cat.label2:= factor(part.age.cat, levels=part.age.cat, labels=paste0('p',part.age.cat.label))]
	dps <- merge(dps, tmp, by=c('part.age.cat','part.age.cat.label'))
	tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label)))
	tmp[, cont.age.cat.label2:= factor(cont.age.cat, levels=cont.age.cat, labels=paste0('c',cont.age.cat.label))]
	dps <- merge(dps, tmp, by=c('cont.age.cat','cont.age.cat.label'))
	
	#	expected number of contacts
	dpsm <- dps[, list(m=sum(m),
					c=sum(cont_pop/sum(cont_pop) * c),
					pop_total= pop_total[1]
			), by=c('LOC','TYPE','part.age.cat','part.age.cat.label')]	
	ggplot(dpsm, aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex.pdf'), w=15, h=10)
	
	ggplot(subset(dpsm, TYPE=='weekday'), aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_wrap(.~part.age.cat.label, ncol=5) +
			labs(x='\ncountry',y='expected number of contacts from index person\n(posterior median)\n') +
			guides(colour = FALSE) 
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex_weekday.pdf'), w=10, h=10)
	
	#	calculate coefficient of variation in m and c
	dpsm_cov <- dpsm[, list(m_cov= sd(m)/mean(m), c_cov= sd(c)/mean(c)), by=c('TYPE','LOC')]
	#     TYPE LOC     m_cov     c_cov
	#1: weekday  BE 0.3828274 0.3828274
	#2: weekend  BE 0.3716048 0.3716048
	#3:  anyday  BE 0.3665788 0.3665788
	#	--> this is identical. this makes sense: constants cancel out in the CoV. 
	#	--> so for age bands fixed, m and c have same CoV across countries
	#	
	
	ggplot(dpsm, aes(x=pop_total, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndexByPop.pdf'), w=15, h=10)
	#	nothing apparent. could do linear regression to confirm not significant predictor
	
	
	ggplot(subset(dps, TYPE=='weekday'), aes(x=log(cont_pop), y=log(c))) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(col=LOC)) + 			
			theme_bw() +
			facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
			labs(colour='countries', x='log population size of contacts', y='log contact rate')	
	ggsave(file.path(outdir,'200623_weekday_contacts_logc_vs_logpop.pdf'), w=25, h=25)
	#	--> correlation between log(pop) and log(c) is -1.
	#		this makes sense. log(Y) ~ log(U) + log(c) <=>  
	#						  log(Y) ~ log(T) + log(pop) + log(c) <=> 
	#						  log(m) ~ log(pop) + log(c)
	#	so we can use this relationship to predict log(c)
	
	dps[, cooks_cat:= cut(cooks, breaks=c(-Inf, 0.5, 1, Inf))]
	#tmp <- subset(dps, TYPE=='weekday' & part.age.cat.label=='40-44' & cont.age.cat.label=='0-4')
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop_p, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(fill=LOC, colour=cooks_cat), pch=21, size=3, stroke=2) +
			scale_colour_manual(values=c('(-Inf,0.5]'='black','(0.5,1]'='orange','(1, Inf]'='red')) +
			theme_bw() +
			facet_wrap(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label), scales='free', ncol=18) +
			labs(colous='Cooks distance', fill='countries', x='prop of contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpopp.pdf'), w=45, h=45)
	#	--> no particular pattern with cont_pop_p
	
	
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpop.pdf'), w=25, h=25)
	#	--> slight tendency for more contact with smaller cont_pop
	#		suggests that pop density might matter?
	
	tmp <- subset(dps, TYPE=='weekday' & part.age.cat.label=='40-44' & cont.age.cat.label=='0-4')
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop/land_area, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population/land area in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpopdensity.pdf'), w=25, h=25)
	#	--> this is not as good as expected! need to dig a bit deeper
	
	
	dpsfit2 <- dps[, {
				#cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label)
				#	take out extremely influential observations
				lm.m <- lm( m ~ cont_pop_dens)
				
				lm.m.coef <- coef( lm.m )
				lm.m.sig <- summary(lm.m)$coefficients[,4]  
				
				#	return
				list( 	lm_formula='m ~ cont_pop_dens', 
						coef1 = lm.m.coef[1],
						coef2 = lm.m.coef[2],
						sig1 = lm.m.sig[1],
						sig2 = lm.m.sig[2]
				)				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	
	
	tmp <- dps[,list(m_avg=mean(m)), by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	dpsfit2 <- merge(dpsfit2, tmp, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label') )
	ggplot(dpsfit2, aes(x=m_avg, y=coef2, colour=factor(cut(sig2, breaks=c(0,0.05,2))) )) + 
			geom_point() +
			facet_grid(~TYPE) +
			theme_bw()
	subset(dpsfit2, TYPE=='weekday')
	subset(dpsfit2, TYPE=='weekday')[, table(coef2>0)]
	#	--> more pos than neg
	
	
	m_pop_dens <- lm(m ~ cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_p <- lm(m ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop <- lm(m ~ cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_total <- lm(m ~ pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix1 <- lm(m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix2 <- lm(m ~ cont_pop + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix3 <- lm(m ~ cont_pop_p + pop_total + cont_pop +part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	
	AIC(m_pop_dens, m_pop_p, m_pop, m_pop_total, m_mix1, m_mix2, m_mix3)
	#	             df      AIC
	#	m_pop_dens  326 3239.733
	#	m_pop_p     326 3203.181
	#	m_pop       326 3210.012
	#	m_pop_total 326 3208.019
	#	m_mix1      327 3170.652
	#	m_mix2      327 3209.999
	#	m_mix3      328 3165.693
	
	summary(m_pop_p)
	#	cont_pop_p  7.945379   1.357939   5.851 5.59e-09 ***
	
	require(caret)
	
	train.control <- trainControl(method = "LOOCV")
	loocv_pop_p <- train( m ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	print(loocv_pop_p)
	
	loocv_mix1 <- train( m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	
	loocv_mix3 <- train( m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	
	
	dpsfit2 <- dps[, {
				cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label)
				#	take out extremely influential observations
				lm.m <- lm( m ~ cont_pop_p)
				cooks <- cooks.distance(lm.m)
				print(cooks)
				cooks.extremely.influential <- which(cooks>1)
				print(cooks.extremely.influential)
				m_2 <- m
				cont_pop_p_2 <- cont_pop_p
				if(length(cooks.extremely.influential))
				{
					cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label,' influential obersvations ',tmp[cooks.extremely.influential,LOC])
					m_2 <- m[-cooks.extremely.influential]
					cont_pop_p_2 <- cont_pop_p[-cooks.extremely.influential]					
				}
				
				print(cont_pop_p_2)
				
				#	make Box Cox transformation
				tmp2 <- boxcox(m_2~cont_pop_p_2, lambda=seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
				lambda <- tmp2$x[which.max(tmp2$y)]
				if(lambda!=0)
				{
					m_2_boxcox <- (m_2^lambda-1)/lambda	
				}
				if(lambda==0)
				{
					m_2_boxcox <- log(m_2)	
				}
				
				print( m_2_boxcox )
				
				#	make linear regression
				lm.m <- lm( m_2_boxcox ~ cont_pop_p_2 )
				lm.m.coef <- coef( lm.m )
				lm.m.sig <- summary(lm.m)$coefficients[,4]  
				
				print(lm.m.coef)
				
				#	return
				list( 	lm_formula='m_2_boxcox ~ cont_pop_p_2', 
						coef1 = lm.m.coef[1],
						coef2 = lm.m.coef[2],
						sig1 = lm.m.sig[1],
						sig2 = lm.m.sig[2]
				)				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	
	
	dpsfit <- dps[, {
				z<- coef(lm( log(c) ~ log(cont_pop) ))
				list(lm_formula='log(c)~log(cont_pop)', lm_intercept= z[1], lm_slope=z[2])				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	save(dps, file=file.path(outdir, '200623_contacts_estimates_polymod.rda'))
	saveRDS(dpsfit, file=file.path(outdir, '200623_contacts_logc_vs_logpop.rds'))
}

explore.attack.rate.validation <- function()
{
	require(ggplot2)
	require(data.table)
	
	outfile.base <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200703m3_cmdstanv-19states_devcntct_samples1500_stepsize002_J29/base_age_fsq_mobility_200703m3_cmdstanv-19states_devcntct_samples1500_stepsize002_J29'
	infile.att.rate.v <- paste0(outfile.base, '-attackrate-validationtable.rds')
	da <- readRDS(infile.att.rate.v)
	da <- copy( da[[1]] )
	
	da <- subset(da, select=-c(L_est,L_obs))
	da <- melt(da, measure.vars=c('M_est','CL_est','CU_est','M_obs','CL_obs','CU_obs'))
	set(da, NULL, 'value', da[, as.numeric(value)/100])	
	da[, mtype:= gsub('est','estimated',gsub('obs','survey',gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\2',variable)))]
	da[, stat:= gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\1',variable)]
	da <- dcast.data.table(da, study+loc+loc_label+lag+date+age_band+mtype~stat, value.var='value')
	
	tmp <- subset(da, age_band=='overall')
	tmp[, L:= paste0(loc_label, ' (',study,'), ',as.character(date) )]
		
	ggplot(tmp) +
			geom_point(aes(y=L, x=M, colour=mtype), position=position_dodge(width=0.5)) +
			geom_errorbarh(aes(y=L, xmin=CL, xmax=CU, colour=mtype), position=position_dodge(width=0.5)) +
			scale_x_continuous(labels=scales::percent, expand=c(0,0)) +
			coord_cartesian(xlim=c(0, max(tmp$CU)*1.1)) +
			facet_grid(lag~., scales='free_y', space='free_y') +
			labs(x='cumulative attack rate', y='', colour='') +
			theme_bw() 
}

explore.fsq.mobility.data<- function()
{
	require(data.table)
	require(viridis)
	require(ggplot2)
	
	indir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	outfile.base <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200703m3_cmdstanv-19states_devcntct_samples1500_stepsize002_J29/base_age_fsq_mobility_200703m3_cmdstanv-19states_devcntct_samples1500_stepsize002_J29' 
	infile.contacts.over.time <- paste0(outfile.base,'-contact_patterns_over_time.rds')
	
		
	z <- readRDS(file.path(indir,'fsq_mobility_trends.rds'))
	names(z)[1] <- 'mobility_data'
	mobility_data <- copy(z$mobility_data)
	death_data <- copy(z$death_data)
	pop_info <- copy(z$pop_info)
	
	#	loads
	#	pop_info, death_data, mobility_data
	
	mobility_data <- unique(subset(mobility_data, select=-c(age.cat, age.cat.label)))
		
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
	
	#
	#	mobility trend on first weekday after rebound date
	ans <- unique(subset(mobility_data, weekend=='no', select=c(loc, fsq.age.cat.label, weekend, base_mobility_trend)))
	
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
	ans <- melt(ans, id.vars=c('loc','fsq.age.cat.label','weekend'))	
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	tmp[, loc_label2:= factor(1:nrow(tmp), levels=1:nrow(tmp), labels=loc_label)]
	ans <- merge(tmp, ans, by='loc')
	set(ans, NULL, 'variable', ans[,factor(variable, 
							levels=c("base_mobility_trend",									
									"avg_mobility_multiplier_pc",
									"avg_mobility_multiplier_pc_lastweek",
									"current_mobility_trend"), 
							labels=c('1-week mobility trend,\nrebound date',
									'average percent mobility increase\nsince rebound date',
									'average percent mobility increase\nrelative to rebound date',
									paste0('1-week mobility trend,\n',format(max(mobility_data$date),'%b %e'))									
									))])	
	tmp <- subset(ans, variable%in%c('1-week mobility trend,\nrebound date','average percent mobility increase\nrelative to rebound date'))
	p <- ggplot(tmp, aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value, colour=fsq.age.cat.label)) +
			geom_point() +
			geom_line(aes(group=fsq.age.cat.label)) +
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
	#	make part one of figure
	fsq_m <- subset(mobility_data, loc=='CA')
	plot.mobility.trend.CA <- ggplot(fsq_m) +
		geom_hline(yintercept = 0, color = 'black') +
		geom_vline(xintercept = fsq_m$rebound_date[1], color = 'black', linetype='dashed') +
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
	plot.crossectional.current.trends <- ggplot(tmp, aes(x=reorder(loc_label2, order(-as.integer(loc_label2))), y=value, colour=fsq.age.cat.label)) +
			geom_point() +
			geom_line(aes(group=fsq.age.cat.label), show.legend = FALSE) +
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
	cnts <- readRDS(infile.contacts.over.time)
	cnts <- subset(cnts, loc=='CA')
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
	plot.overall <- ggarrange(plot.mobility.trend.CA, plot.bottom,		
		nrow=2,
		labels='A',
		heights=c(3,6)
		)
	file <- paste0(outfile.base, '-figure_fsq_mobility.png')
	ggsave(plot.overall, file=file, w=15, h=15)
}

explore.contact.matrices <- function()
{
	library(MASS)
	
	indir <- '~/git/R0t'
	outdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	
	file_us_area <- file.path(indir,"usa","data","us_states_area_measurements.csv")
	file_us_population <- file.path(indir,"usa","data","us_population_withnyc.rds")	
	source(file.path(indir, "usa","code","utils","read-data-usa.r"))
	source(file.path(indir, "usa","code","utils","process-covariates.r"))
	
	infiles <- data.table(F=list.files(path=file.path(indir,'data'), pattern='polymod.tab.bin.*', full.names=TRUE))
	infiles[, TYPE:= 'anyday']
	set(infiles, which(grepl('weekday',infiles$F)), 'TYPE', 'weekday')
	set(infiles, which(grepl('weekend',infiles$F)), 'TYPE', 'weekend')
	infiles[, LOC:= gsub('^polymod.tab.bin_([A-Z]+).*','\\1', basename(F))]
	infiles[, IDX:= seq_len(nrow(infiles))]
	
	
	# make pop data set
	pop_count <- read_pop_count_us(path_to_file=file_us_population)	
	pop_by_age <- read_pop_count_by_age_us(path_to_file=file_us_population)
	darea <- read_us_state_areas(file_us_area)
	pop_info <- process_make_pop_info(pop_count, pop_by_age, darea)
	setkey(pop_info, loc, age.cat)
	
	# make Polymod contact matrices
	dps <- list()
	for( i in infiles$IDX)
	{		
		tmp <- read_contact_rate_matrix(pop_info, infiles$F[i])
		tmp[, LOC:= infiles$LOC[i]]
		tmp[, TYPE:= infiles$TYPE[i]]
		dps[[i]] <- tmp
	}
	dps <- do.call('rbind', dps)
	dps[, cont_pop:= m/c]
	
	tmp <- unique(subset(dps, part.age.cat==1 & TYPE=='weekday', select=c(LOC, TYPE, cont.age.cat, cont.age.cat.label, cont_pop)))
	tmp <- tmp[, list(	pop_total=sum(cont_pop),
					cont.age.cat=cont.age.cat,
					cont_pop_p= cont_pop/sum(cont_pop)
			),by='LOC']	
	dps <- merge(dps, tmp, by=c('LOC','cont.age.cat'))
	
	#	add land area 
	#	https://data.worldbank.org/indicator/AG.LND.TOTL.K2?locations=EU
	#	2018
	tmp <- data.table(LOC= c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL"),
			land_area= c(30280, 349360.0, 303910.0, 241930.0, 294140.0, 2430.0, 33690.0, 306190.0)
	)
	dps <- merge(dps, tmp, by='LOC')
	dps[, cont_pop_dens:= cont_pop/land_area]
	
	if(0)
	{
		#	calculate influential points in m ~ cont_pop_p model
		tmp <- dps[, {					
					lm.m <- lm( m ~ cont_pop_p)
					cooks <- cooks.distance(lm.m)
					list(	LOC=LOC,
							cooks=cooks)						
				}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
		dps <- merge(dps, tmp, , by=c('TYPE','LOC','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label'))		
	}
	
	#	set up factors of labels
	tmp <- unique(subset(dps, select=c(part.age.cat, part.age.cat.label)))
	tmp[, part.age.cat.label2:= factor(part.age.cat, levels=part.age.cat, labels=paste0('p',part.age.cat.label))]
	dps <- merge(dps, tmp, by=c('part.age.cat','part.age.cat.label'))
	tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label)))
	tmp[, cont.age.cat.label2:= factor(cont.age.cat, levels=cont.age.cat, labels=paste0('c',cont.age.cat.label))]
	dps <- merge(dps, tmp, by=c('cont.age.cat','cont.age.cat.label'))
	
	#	expected number of contacts
	dpsm <- dps[, list(m=sum(m),
					c=sum(cont_pop/sum(cont_pop) * c),
					pop_total= pop_total[1]
			), by=c('LOC','TYPE','part.age.cat','part.age.cat.label')]	
	ggplot(dpsm, aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex.pdf'), w=15, h=10)
	
	ggplot(subset(dpsm, TYPE=='weekday'), aes(x=LOC, y=m, colour=LOC)) + geom_point() +
			facet_wrap(.~part.age.cat.label, ncol=5) +
			labs(x='\ncountry',y='expected number of contacts from index person\n(posterior median)\n') +
			guides(colour = FALSE) 
	ggsave(file.path(outdir,'200623_contacts_expectedByIndex_weekday.pdf'), w=10, h=10)
	
	#	calculate coefficient of variation in m and c
	dpsm_cov <- dpsm[, list(m_cov= sd(m)/mean(m), c_cov= sd(c)/mean(c)), by=c('TYPE','LOC')]
	#     TYPE LOC     m_cov     c_cov
	#1: weekday  BE 0.3828274 0.3828274
	#2: weekend  BE 0.3716048 0.3716048
	#3:  anyday  BE 0.3665788 0.3665788
	#	--> this is identical. this makes sense: constants cancel out in the CoV. 
	#	--> so for age bands fixed, m and c have same CoV across countries
	#	
	
	ggplot(dpsm, aes(x=pop_total, y=m, colour=LOC)) + geom_point() +
			facet_grid(TYPE~part.age.cat.label)
	ggsave(file.path(outdir,'200623_contacts_expectedByIndexByPop.pdf'), w=15, h=10)
	#	nothing apparent. could do linear regression to confirm not significant predictor
	
	
	ggplot(subset(dps, TYPE=='weekday'), aes(x=log(cont_pop), y=log(c))) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(col=LOC)) + 			
			theme_bw() +
			facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
			labs(colour='countries', x='log population size of contacts', y='log contact rate')	
	ggsave(file.path(outdir,'200623_weekday_contacts_logc_vs_logpop.pdf'), w=25, h=25)
	#	--> correlation between log(pop) and log(c) is -1.
	#		this makes sense. log(Y) ~ log(U) + log(c) <=>  
	#						  log(Y) ~ log(T) + log(pop) + log(c) <=> 
	#						  log(m) ~ log(pop) + log(c)
	#	so we can use this relationship to predict log(c)
	
	dps[, cooks_cat:= cut(cooks, breaks=c(-Inf, 0.5, 1, Inf))]
	#tmp <- subset(dps, TYPE=='weekday' & part.age.cat.label=='40-44' & cont.age.cat.label=='0-4')
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop_p, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(fill=LOC, colour=cooks_cat), pch=21, size=3, stroke=2) +
			scale_colour_manual(values=c('(-Inf,0.5]'='black','(0.5,1]'='orange','(1, Inf]'='red')) +
			theme_bw() +
			facet_wrap(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label), scales='free', ncol=18) +
			labs(colous='Cooks distance', fill='countries', x='prop of contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpopp.pdf'), w=45, h=45)
	#	--> no particular pattern with cont_pop_p
	
	
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpop.pdf'), w=25, h=25)
	#	--> slight tendency for more contact with smaller cont_pop
	#		suggests that pop density might matter?
	
	tmp <- subset(dps, TYPE=='weekday' & part.age.cat.label=='40-44' & cont.age.cat.label=='0-4')
	ggplot(subset(dps, TYPE=='weekday'), aes(x=cont_pop/land_area, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(colour=LOC)) +			
			theme_bw() +
			facet_wrap(part.age.cat.label2 ~ cont.age.cat.label2, scales='free', ncol=18) +
			labs(colour='countries', x='contact population/land area in age band', y='m')	
	ggsave(file.path(outdir,'200623_weekday_contacts_m_vs_contpopdensity.pdf'), w=25, h=25)
	#	--> this is not as good as expected! need to dig a bit deeper
	
	
	dpsfit2 <- dps[, {
				#cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label)
				#	take out extremely influential observations
				lm.m <- lm( m ~ cont_pop_dens)
				
				lm.m.coef <- coef( lm.m )
				lm.m.sig <- summary(lm.m)$coefficients[,4]  
				
				#	return
				list( 	lm_formula='m ~ cont_pop_dens', 
						coef1 = lm.m.coef[1],
						coef2 = lm.m.coef[2],
						sig1 = lm.m.sig[1],
						sig2 = lm.m.sig[2]
				)				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	
	
	tmp <- dps[,list(m_avg=mean(m)), by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	dpsfit2 <- merge(dpsfit2, tmp, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label') )
	ggplot(dpsfit2, aes(x=m_avg, y=coef2, colour=factor(cut(sig2, breaks=c(0,0.05,2))) )) + 
			geom_point() +
			facet_grid(~TYPE) +
			theme_bw()
	subset(dpsfit2, TYPE=='weekday')
	subset(dpsfit2, TYPE=='weekday')[, table(coef2>0)]
	#	--> more pos than neg
	
	
	m_pop_dens <- lm(m ~ cont_pop_dens + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_p <- lm(m ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop <- lm(m ~ cont_pop + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_pop_total <- lm(m ~ pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix1 <- lm(m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix2 <- lm(m ~ cont_pop + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	m_mix3 <- lm(m ~ cont_pop_p + pop_total + cont_pop +part.age.cat.label2 : cont.age.cat.label2 - 1L, data=subset(dps, TYPE=='weekday'))
	
	AIC(m_pop_dens, m_pop_p, m_pop, m_pop_total, m_mix1, m_mix2, m_mix3)
	#	             df      AIC
	#	m_pop_dens  326 3239.733
	#	m_pop_p     326 3203.181
	#	m_pop       326 3210.012
	#	m_pop_total 326 3208.019
	#	m_mix1      327 3170.652
	#	m_mix2      327 3209.999
	#	m_mix3      328 3165.693
	
	summary(m_pop_p)
	#	cont_pop_p  7.945379   1.357939   5.851 5.59e-09 ***
	
	require(caret)
	
	train.control <- trainControl(method = "LOOCV")
	loocv_pop_p <- train( m ~ cont_pop_p + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	print(loocv_pop_p)
	
	loocv_mix1 <- train( m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	
	loocv_mix3 <- train( m ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, 
			data = subset(dps, TYPE=='weekday'), 
			method = "lm",
			trControl = train.control)
	
	
	dpsfit2 <- dps[, {
				cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label)
				#	take out extremely influential observations
				lm.m <- lm( m ~ cont_pop_p)
				cooks <- cooks.distance(lm.m)
				print(cooks)
				cooks.extremely.influential <- which(cooks>1)
				print(cooks.extremely.influential)
				m_2 <- m
				cont_pop_p_2 <- cont_pop_p
				if(length(cooks.extremely.influential))
				{
					cat('\nRemoving part.age.cat.label ',part.age.cat.label,' cont.age.cat.label ',cont.age.cat.label,' influential obersvations ',tmp[cooks.extremely.influential,LOC])
					m_2 <- m[-cooks.extremely.influential]
					cont_pop_p_2 <- cont_pop_p[-cooks.extremely.influential]					
				}
				
				print(cont_pop_p_2)
				
				#	make Box Cox transformation
				tmp2 <- boxcox(m_2~cont_pop_p_2, lambda=seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
				lambda <- tmp2$x[which.max(tmp2$y)]
				if(lambda!=0)
				{
					m_2_boxcox <- (m_2^lambda-1)/lambda	
				}
				if(lambda==0)
				{
					m_2_boxcox <- log(m_2)	
				}
				
				print( m_2_boxcox )
				
				#	make linear regression
				lm.m <- lm( m_2_boxcox ~ cont_pop_p_2 )
				lm.m.coef <- coef( lm.m )
				lm.m.sig <- summary(lm.m)$coefficients[,4]  
				
				print(lm.m.coef)
				
				#	return
				list( 	lm_formula='m_2_boxcox ~ cont_pop_p_2', 
						coef1 = lm.m.coef[1],
						coef2 = lm.m.coef[2],
						sig1 = lm.m.sig[1],
						sig2 = lm.m.sig[2]
				)				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	
	
	ggplot(dpsfit2, aes(x=sig2)) + geom_histogram()
	
	
	
	bc1 <- boxcoxfit(tmp$cont_pop_p, tmp$m, lambda2=FALSE)
	bc2 <- boxcoxfit(tmp$cont_pop_p, tmp$m, lambda2=TRUE)	
	lambda1 <- bc2$lambda[1]
	lambda2 <- bc2$lambda[2]
	tmp[, m.boxcox2:= (((m + lambda2)^lambda1) - 1) / lambda1]
	
	summary( lm(  (((m + lambda2)^lambda1) - 1) / lambda1 ~ cont_pop_p, data=tmp) )
	
	#	pop_p now strongly significant
	
	ggplot(tmp, aes(x=cont_pop_p, y=m)) +
			geom_smooth(method='lm', formula=y~x,colour='black', lwd=.5) +
			geom_point(aes(col=LOC)) + 			
			theme_bw() +
			facet_grid(paste0('p',part.age.cat.label) ~ paste0('c',cont.age.cat.label)) +
			labs(colour='countries', x='log population size of contacts', y='log contact rate')	
	
	
	dpsfit <- dps[, {
				z<- coef(lm( log(c) ~ log(cont_pop) ))
				list(lm_formula='log(c)~log(cont_pop)', lm_intercept= z[1], lm_slope=z[2])				
			}, by=c('TYPE','part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label')]
	save(dps, file=file.path(outdir, '200623_contacts_estimates_polymod.rda'))
	saveRDS(dpsfit, file=file.path(outdir, '200623_contacts_logc_vs_logpop.rds'))
}

explore.contact.matrices.internal.validation.logpop.model <- function()
{
	require(tidyverse)
	
	indir.code <- '~/git/R0t'
	indir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
	load(file=file.path(indir, '200623_contacts_estimates_polymod.rda'))
	dpsfit <- readRDS(file=file.path(indir, '200623_contacts_logc_vs_logpop.rds'))
	
	# predict for polymod countries

	# Get population info
	pop_info_pm <- as_tibble(dps) %>% 
			rename(	loc_label=LOC, 
					age.cat.label=cont.age.cat.label,
					age.cat=cont.age.cat,
					pop=cont_pop) %>% 
			mutate( pop=round(pop,digits=0),
					prop_pop=pop/pop_total,
					loc=loc_label,
					age.cat.from=sapply(strsplit(age.cat.label,"-"), `[`, 1),
					age.cat.to=sapply(strsplit(age.cat.label,"-"), `[`, 2),
					land_area_sqm=0,
					pop_sqm=0) %>% 
			mutate( age.cat.from:=case_when(age.cat.label=='85+'~'85',TRUE~age.cat.from),
					age.cat.to:=case_when(age.cat.label=='85+'~'99',TRUE~age.cat.to)) %>% 
			select( c(-m,-c,-part.age.cat,-part.age.cat.label,-TYPE)) %>% 
			unique() %>% 
			data.table()
	set(pop_info_pm, NULL, 'age.cat.from', as.integer(pop_info_pm$age.cat.from))
	set(pop_info_pm, NULL, 'age.cat.to', as.integer(pop_info_pm$age.cat.to))
	pop_info <- copy(pop_info_pm)
	
	# predict m for the Polymod countries using model
	source(file.path(indir.code, "usa","code","utils","process-covariates.r"))	
	path_to_logpopmodel <- file.path(indir,'200623_contacts_logc_vs_logpop.rds')	
	dcontactlpop <- process_make_contact_matrix_by_country_using_logpop_model(pop_info, 'weekday', path_to_logpopmodel)	
	dcontactlpop[, type:='weekday']
	tmp <- process_make_contact_matrix_by_country_using_logpop_model(pop_info, 'weekend', path_to_logpopmodel)
	tmp[, type:='weekend']
	dcontactlpop <- rbind(dcontactlpop, tmp)
	
	
	# m from van Kasteele method
	setnames(dps, c('m','LOC','TYPE'), c('m_vanK','loc','type'))
	setnames(dcontactlpop, 'm', 'm_lpop')
	dcontactc <- merge(dcontactlpop, dps, by=c('part.age.cat','part.age.cat.label','cont.age.cat','cont.age.cat.label','type','loc'))
	dcontactc[, diff := m_lpop - m_vanK]
	dcontactc[, logdiff := log(m_lpop) - log(m_vanK)]	
	# Create ordered labels for age group combinations
	dcontactc[, aa := paste(part.age.cat.label, cont.age.cat.label,sep=',')]
	set(dcontactc, NULL, 'aa', dcontactc[, factor(aa, levels=unique(aa), ordered=TRUE)])
	
	tmp <- dcontactc[, list( 	diff_cl= quantile(diff, 0.025),
						diff_cu= quantile(diff, 0.975),
						logdiff_cl= quantile(logdiff, 0.025),
						logdiff_cu= quantile(logdiff, 0.975)
						), by='type']
	dcontactc <- merge(dcontactc, tmp, by='type')
	setkey(dcontactc,type, part.age.cat, cont.age.cat, loc)
	dcontactc[, idx:= 1 + ((seq_len(nrow(dcontactc))-1) %% (nrow(dcontactc)/2)) ]
	
	#	make plots
	ggplot(subset(dcontactc,type=='weekday'), aes(x = cont.age.cat, y = part.age.cat)) + 
		geom_tile(aes(fill=diff)) +
		scale_x_continuous(expand=c(0,0), breaks = seq(1,18,1), labels = unique(dcontactc$cont.age.cat.label)) + 
		scale_y_continuous(expand=c(0,0), breaks = seq(1,18,1), labels = unique(dcontactc$part.age.cat.label))  + 
		labs(x="Age of contact",y = "Age of index person", fill="m_pred - m_polymod") +
		theme_bw() + 
		theme(legend.position="bottom",
					axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1),
					#axis.text.y=element_text(size=14),
					#axis.title=element_text(size=24,face="bold"),
					axis.title.x = element_text(vjust=-0.5),
					#strip.text = element_text(size = 20),
					panel.grid.major = element_blank(),
					panel.grid.minor = element_blank())  +
		scale_fill_viridis(begin=0,end=1,alpha=0.6, direction=1,values = scales::rescale(c(-6,-2,-1,-0.5,-0.1,-0.05,0,0.05,0.1,0.5,1.5,2)),breaks=seq(-8,3,1)) +
		guides(fill = guide_colourbar(barwidth = 30, barheight = 0.5, direction="horizontal"))	+
		facet_wrap(.~loc,ncol=4)
	ggsave(file=file.path(outdir, paste0('200623_contacts_internalvalidation_weekday','_exp_contacts_logpopmodel_vs_polymod','.pdf')), height=7,width=10, limitsize=FALSE)	
	
	# plot model residuals
	ggplot(subset(dcontactc,type=='weekday')) + 
		geom_point(aes(x=diff, y=aa, color=loc)) +
		theme_bw() +
		theme(	axis.text.y=element_text(size=10),
				axis.text.x=element_text(size=8,angle=60,vjust = 0.9, hjust=1),
				axis.title.x = element_text(vjust=-0.5),
				legend.position="bottom",
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_blank()) +
		scale_colour_viridis_d(begin=0,end=1,alpha=1,direction=-1) +
		labs(x="m_pred - m_polymod",y = "a,a' combination",col="state") +
		guides(fill=guide_legend(nrow=2))
	ggsave(file=file.path(outdir, paste0('200623_contacts_internalvalidation_weekday','_diff_exp_contacts_logpopmodel_vs_obs_polymod','.pdf')), height=40,width=21, limitsize=FALSE)		
	
	# log res
	ggplot(subset(dcontactc,type=='weekday')) + 
		geom_point(aes(x=logdiff, y=aa, color=loc)) +
		theme_bw() +
		theme(	axis.text.y=element_text(size=10),
				axis.text.x=element_text(size=8,angle=60,vjust = 0.9, hjust=1),
				axis.title.x = element_text(vjust=-0.5),
				legend.position="bottom",
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_blank()) +
		scale_colour_viridis_d(begin=0,end=1,alpha=1,direction=-1) +
		labs(x="log(m_pred / m_polymod)",y = "a,a' combination",col="state") +
		guides(fill=guide_legend(nrow=2))
	ggsave(file=file.path(outdir, paste0('200623_contacts_internalvalidation_weekday','_logdiff_exp_contacts_logpopmodel_vs_obs_polymod','.pdf','.pdf')), height=40,width=21, limitsize=FALSE)
	
	# log res
	ggplot(dcontactc) + 
			geom_ribbon(aes(x=idx, ymin = logdiff_cl, ymax = logdiff_cu), fill='grey70') +
			geom_point(aes(y=logdiff, x=idx, color=loc)) +
			scale_x_continuous(expand=c(0,0)) +
			scale_colour_viridis_d(begin=0,end=1,alpha=1,direction=-1) +
			facet_grid(type~.) +
			theme_bw() +
			theme(	legend.position="bottom",
					axis.text.x=element_blank(),
					axis.ticks.x=element_blank()) +			
			labs(y="log(m_pred / m_polymod)", x = "age combinations",col="location") +
			guides(colour=guide_legend(nrow=1))
	ggsave(file=file.path(outdir, paste0('200623_contacts_internalvalidation_','logdiff_exp_contacts_logpopmodel_vs_obs_polymod','.pdf')), height=7,width=10, limitsize=FALSE)
}

post.processing.emo.mob.trend <- function()
{
  cat(" \n -------------------------------- \n \n  post-processing-emo-mob-trend.R \n \n -------------------------------- \n")
  
  library(rstan)
  library(data.table)
  library(lubridate)
  library(gdata)
  library(dplyr)
  library(tidyr)
  library(EnvStats)
  library(scales)
  library(stringr)
  library(splines)
  library(gridExtra)
  library(ggpubr)
  library(bayesplot)
  library(viridis)
  library(ggplot2)
  
  if(0)
  {
    args_dir <- list()
    args_dir[['script_dir']] <- '/rds/general/user/or105/home/libs/R0t'
    args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
    args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
    args_dir[['job_tag']] <- '19states_stdctn_2'
  }
  
  
  if(0)
  {
    args_dir <- list()
    args_dir[['script_dir']] <- '~/R0t'
    args_dir[['out_dir']] <- '~/'
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
  
  outfile.base <- paste0(args_dir$out_dir,'/emo-mob-trend')
  
  file <- file.path(args_dir$script_dir, "usa","code","utils","read-data-usa.r")
  cat("\n source file:", file)
  source(file)
  
  file <- file.path(args_dir$script_dir, "usa","code","utils","process-covariates.r")
  cat("\n source file:", file)
  source(file)
  
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
  #	make mobility trend for mob_emodo_idx
  mob_emodo_idx <- read_emodo_cell_phone_contact_intensities(infile_emodo = '~/contacts_and_mobility/contacts_by_age_20200729.csv',type='idx')
  emo_age_cat_map_idx <- make_emodo_age_cat_map(mob_emodo_idx, pop_info)
  mob_emodo_idx <- make_mobility_trends_emo(mob_emodo_idx, emo_age_cat_map_idx)
  setnames(mob_emodo_idx,colnames(mob_emodo_idx), gsub('emo.','idx.',colnames(mob_emodo_idx)))
  
  # # make mobility trend for mob_emodo_cont
  # mob_emodo_cont <- read_emodo_cell_phone_contact_intensities(infile_emodo = '~/contacts_and_mobility/contacts_by_age_20200729.csv',type='cont')
  # emo_age_cat_map_cont <- make_emodo_age_cat_map(mob_emodo_cont, pop_info)
  # mob_emodo_cont <- make_mobility_trends_emo(mob_emodo_cont, emo_age_cat_map_cont)
  # setnames(mob_emodo_cont,colnames(mob_emodo_cont), gsub('emo.','cont.',colnames(mob_emodo_cont)))
  # mob_emodo_cont <- mob_emodo_cont[loc%in% unique(mob_emodo_idx$loc)]
  
  #	make mobility trend for mob_emodo
  mob_emodo <- read_emodo_cell_phone_contact_intensities_with_cont(infile_emodo = '~/contacts_and_mobility/contacts_by_age_20200729.csv')
  emo_age_cat_map <- make_emodo_age_cat_map_with_cont(mob_emodo, pop_info)
  mob_emodo <- make_mobility_trends_emo_with_cont(mob_emodo, emo_age_cat_map)
  mob_emodo[, week:= as.integer(strftime(date, format = "%V"))]
  mob_emodo[,month:=format(date,"%B")]
  mob_emodo <- mob_emodo[loc%in% unique(mob_emodo_idx$loc)]
  
  # select mobility trend
  ans <- subset(mob_emodo,select = c('cont.age.label','idx.age.label','loc_label','month','mobility_trend','date'))
  ans <- merge(ans, unique(subset(mob_emodo_idx,select = c('idx.age.label','loc_label','date','mobility_trend'))),by=c('idx.age.label','loc_label','date'))
  # ans <- merge(ans, unique(subset(mob_emodo_cont,select = c('cont.age.label','loc_label','date','mobility_trend'))),by=c('cont.age.label','loc_label','date'))
  ans <- merge(ans, unique(subset(mob_emodo_idx,select = c('idx.age.label','loc_label','date','mobility_trend'))),
               by.y=c('idx.age.label','loc_label','date'),
               by.x=c('cont.age.label','loc_label','date'))
  
  # mean over months
  ans <- ans[,list(mobility_trend.x=mean(mobility_trend.x),
                   mobility_trend.y=mean(mobility_trend.y),
                   mobility_trend=mean(mobility_trend)
  ),
  by=c('cont.age.label','idx.age.label','loc_label','month')]
  ans[, ratio:=mobility_trend.x/mobility_trend.y]
  ans[, ratio_prod:=mobility_trend.x/(mobility_trend.y*mobility_trend)]
  ans[, difference:=mobility_trend.x-(mobility_trend.y*mobility_trend)]
  ans[, month:=factor(month,levels = c("February","March","April","May","June"))]
  
  # plot contact
  cat('\n Writing plot to ',paste0(outfile.base,'-contact.pdf'),'\n')
  ggplot(ans,aes(x=loc_label,y=mobility_trend.x,fill=month))+
    geom_bar(position = "dodge", stat="identity")+
    theme_bw() +
    facet_grid(paste0('to ',cont.age.label)~paste0('from ',idx.age.label))+
    theme( axis.text.x = element_text(angle = 45, hjust = 1),
           strip.background = element_blank(),
           plot.title = element_text(hjust = 0.5), 
           legend.position='bottom') +
    scale_fill_viridis_d() +
    labs(x='location',y='contacts')+
    scale_y_continuous(trans='log2',breaks = seq(0.2,1.2,0.2),limits = c(0.2,1.2),expand = c(0,0))
  ggsave(paste0(outfile.base,'-contact.pdf'),w=10, h=8)
  
  tmp <- unique(subset(ans,select = c('idx.age.label','loc_label','month','mobility_trend.y')))
  tmp[, month:=factor(month,levels = c("February","March","April","May","June"))]
  
  cat('\n Writing plot to ',paste0(outfile.base,'-contact-idx.pdf'),'\n')
  ggplot(tmp,aes(x=loc_label,y=mobility_trend.y,fill=month))+
    geom_bar(position = "dodge", stat="identity")+
    theme_bw() +
    facet_grid(.~paste0('from ',idx.age.label))+
    theme( axis.text.x = element_text(angle = 45, hjust = 1),
           strip.background = element_blank(),
           plot.title = element_text(hjust = 0.5), 
           legend.position='bottom')+
    scale_fill_viridis_d() +
    labs(x='location',y='contacts')+
    scale_y_continuous(trans='log2',breaks = seq(0.2,1.2,0.2),limits = c(0.2,1.2),expand = c(0,0))
  ggsave(paste0(outfile.base,'-contact-idx.pdf'),w=10, h=3)
  
  # plot ratio
  cat('\n Writing plot to ',paste0(outfile.base,'-ratio.pdf'),'\n')
  ggplot(ans,aes(x=loc_label,y=ratio,fill=month))+
    geom_bar(position = "dodge", stat="identity")+
    theme_bw() +
    facet_grid(paste0('to ',cont.age.label)~paste0('from ',idx.age.label))+
    theme( axis.text.x = element_text(angle = 45, hjust = 1),
           strip.background = element_blank(),
           plot.title = element_text(hjust = 0.5), 
           legend.position='bottom')+
    scale_fill_viridis_d() +
    labs(x='location',y='ratio')+
    scale_y_continuous(trans='log2',breaks = seq(0.6,1.8,0.2),limits = c(0.6,1.8),expand = c(0,0))
  ggsave(paste0(outfile.base,'-ratio.pdf'),w=10, h=8)
  
  cat('\n Writing plot to ',paste0(outfile.base,'-ratio-prod.pdf'),'\n')
  ggplot(ans,aes(x=loc_label,y=ratio_prod,fill=month))+
    geom_bar(position = "dodge", stat="identity")+
    theme_bw() +
    facet_grid(paste0('to ',cont.age.label)~paste0('from ',idx.age.label))+
    theme( axis.text.x = element_text(angle = 45, hjust = 1),
           strip.background = element_blank(),
           plot.title = element_text(hjust = 0.5), 
           legend.position='bottom')+
    scale_fill_viridis_d() +
    labs(x='location',y='ratio')+
    scale_y_continuous(trans='log2',breaks = seq(0.9,3.6,0.4),limits = c(0.9,3.6),expand = c(0,0))
  ggsave(paste0(outfile.base,'-ratio-prod.pdf'),w=10, h=8)
  
  
  cat(" \n -------------------------------- \n \n  end : post-processing-emo-mob-trend.R \n \n -------------------------------- \n")
}