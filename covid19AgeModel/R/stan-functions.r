# require(roxygen2); roxygenize('/Users/or105/git/R0t/covid19AgeModel')

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
process_make_pop_info <- function(pop_count, pop_by_age, darea)
{
	#	make age category info
	tmp <- levels(pop_by_age$age)
	da <- data.table(age.cat= seq_along(tmp), age.cat.label= tmp)
	da[, age.cat.from:= as.integer(gsub('\\+','',gsub('([0-9]+)-([0-9]+)','\\1', age.cat.label)))]
	da[, age.cat.to:= as.integer(gsub('85\\+','99',gsub('([0-9]+)-([0-9]+)','\\2', age.cat.label)))]
		
	#	make data table holding pops by loc	
	tmp <- as.data.table(pop_by_age)
	stopifnot( c('state','age','pop')%in%colnames(tmp))
	tmp <- subset(tmp, select=c(age, state, pop))
	setnames(tmp,c('age','state','pop'),c('age.cat.label','loc_label','prop_pop'))
	da <- merge(da, tmp, by='age.cat.label')
	set(da, NULL, 'loc_label', da[, as.character(loc_label)])
	set(da, NULL, 'loc_label', da[, gsub('_',' ',loc_label)])
	
	#	add total pop counts
	tmp <- as.data.table(pop_count)
	setnames(tmp, c('Region','code','popt'), c('loc_label','loc','pop_total'))
	set(tmp, NULL, 'loc_label', tmp[, as.character(loc_label)])
	set(tmp, NULL, 'loc', tmp[, as.character(loc)])
	set(tmp, NULL, 'loc_label', tmp[, gsub('_',' ',loc_label)])
	stopifnot( all( unique(sort(da$loc_label))==unique(sort(tmp$loc_label)) ) )
	da <- merge( da, tmp, by=c('loc_label'))
	 
	#	add land area
	stopifnot( all(sort(darea$loc_label)==unique(sort(da$loc_label)))  )
	da <- merge(da, darea, by='loc_label')
	
	#	make variables
	da[, pop:= prop_pop*pop_total]
	da[, pop_sqm:= pop/land_area_sqm]
	
	da
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
#' @importFrom stats predict
process_make_contact_matrix_by_country_using_popp_logpopdens_model <- function(pop_info, dpolymod_type, path_to_polymod_data)
{		
	stopifnot(dpolymod_type%in%c('weekend','weekday','anyday'))
	
	#	read polymod data
	dps <- readRDS(path_to_polymod_data)
	dps <- subset(dps, TYPE==dpolymod_type)
	
	#	check correct dimensions
	stopifnot(all(sort(unique(dps$part.age.cat.label))==sort(unique(pop_info$age.cat.label))))
	stopifnot(all(sort(unique(dps$cont.age.cat.label))==sort(unique(pop_info$age.cat.label))))
	
	#	train model
	dtrain <- subset(dps, select=c(m, cont_pop_p, cont_pop_dens, part.age.cat.label2, cont.age.cat.label2, LOC ))
	model <- lm(log(m) ~ cont_pop_p + log(cont_pop_dens) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=dtrain)		
	
	#	make prediction data set 
	dpr <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label2,part.age.cat,part.age.cat.label2)))	
	tmp <- subset(pop_info, select=c(loc, age.cat, pop, prop_pop, land_area_sqm))
	tmp[, land_area_sqkm:= land_area_sqm*2.589]
	tmp[, cont_pop_dens:= pop/ land_area_sqkm]
	setnames(tmp, c('age.cat','prop_pop','loc'), c('cont.age.cat','cont_pop_p','LOC'))	
	dpr <- merge(dpr, tmp, by=c('cont.age.cat'), allow.cartesian=TRUE)
	set(dpr, NULL, c('cont.age.cat','part.age.cat','land_area_sqkm','land_area_sqm','pop'), NULL)
	
	#	predict
	tmp <- stats::predict(model, dpr, interval = "prediction")
	colnames(tmp) <- c('lm','lm_pr_cl','lm_pr_cu')	
	dpr <- cbind(dpr, tmp)
	set(dpr, NULL, 'm', dpr[,exp(lm)])				#	log median --> median 
	set(dpr, NULL, 'm_pr_cl', dpr[,exp(lm_pr_cl)])	#	log 2.5% --> 2.5%
	set(dpr, NULL, 'm_pr_cu', dpr[,exp(lm_pr_cu)])	#	log 97.5% --> 97.5%
	set(dpr, NULL, c('cont_pop_p','cont_pop_dens','lm','lm_pr_cl','lm_pr_cu'), NULL)
	
	#	return
	tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label,cont.age.cat.label2,part.age.cat,part.age.cat.label,part.age.cat.label2)))	
	dpr <- merge(tmp, dpr, by=c('cont.age.cat.label2','part.age.cat.label2'))
	setnames(dpr, 'LOC', 'loc')
	tmp <- unique(subset(pop_info, select=c(loc,loc_label)))
	dpr <- merge(tmp, dpr, by='loc')
	set(dpr, NULL, c('m_pr_cl','m_pr_cu','cont.age.cat.label2','part.age.cat.label2'), NULL)
	
	dpr
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
process_make_contact_matrix_by_country_using_logpop_model <- function(pop_info, dpolymod_type, path_to_logpopmodel)
{	
	stopifnot(dpolymod_type%in%c('weekend','weekday','anyday'))
	
	#	read model
	dm <- readRDS(path_to_logpopmodel)
	dm <- subset(dm, TYPE==dpolymod_type)
	
	#	check correct dimensions
	stopifnot(all(sort(unique(dm$part.age.cat.label))==sort(unique(pop_info$age.cat.label))))
	stopifnot(all(sort(unique(dm$cont.age.cat.label))==sort(unique(pop_info$age.cat.label))))
	
	#	predict m
	dm <- subset(dm, select=c(part.age.cat, part.age.cat.label, cont.age.cat, cont.age.cat.label, lm_intercept, lm_slope))
	tmp <- subset(pop_info, select=c(loc, loc_label, age.cat, age.cat.label, pop))
	setnames(tmp, c('age.cat','age.cat.label','pop'), c('cont.age.cat','cont.age.cat.label','cont.pop'))
	dm <- merge(dm, tmp, by=c('cont.age.cat','cont.age.cat.label'), allow.cartesian=TRUE)
	dm[, m:= exp(lm_intercept + (1+lm_slope)*log(cont.pop))]
	
	dm <- subset(dm, select=-c(lm_intercept, lm_slope, cont.pop))
	dm
}

#' @export
#' @keywords internal
#' @import data.table ggplot2
process_make_smoothed_logcases <- function(death_data, plot_dir=NA_character_)
{	
  	dd <- as.data.table(death_data)
	setkey(dd, state, date)
	tmp <- dd[, list(date=date,
					dateIdx= seq_along(date),
					cdeaths=cumsum(Deaths)),
			by=c('code')]
	dd <- merge(tmp, dd, by=c('code','date'))
	
	#	only smooth data from the calendar week with 10 cum deaths onwards
	dd[, week:=as.integer(strftime(date, format = "%V"))]
	tmp <- dd[cdeaths>=10, list(week=unique(week)), by='code']
	dd <- merge(dd, tmp, by=c('code','week'))
	
	#	calculate average cases by week to bypass strong day of the week effects
	#	calculate log average cases so loess derived confidence intervals are strictly pos on natural scale
	dd <- dd[, list(wc= mean(Cases)), by=c('code','state','week')]
	set(dd, which(dd$wc==0), 'wc', 1.)
	dd[, lwc:= log(wc)]
	
	#	get loess smooth with CI from t distribution 
	tmp <- dd[, 
			{
				nonzero <- which(wc!=0)
				lwc_nz <- lwc[nonzero]
				week_nz <- week[nonzero]
				tmp <- stats::loess(lwc_nz ~ week_nz, span=0.4)
				tmp <- predict(tmp, data.frame(week_nz=week), se = TRUE)
				list(week=week, lmean= tmp$fit, lsd= tmp$se.fit, tdf= tmp$df)			
			}, 
			by=c('code')]
	dd <- merge(dd, tmp, by=c('code','week'))
	dd[, lcl:= lmean + qt(0.025, tdf)*lsd]
	dd[, lcu:= lmean + qt(0.975, tdf)*lsd]
	
	#	plot	
	if(!is.na(plot_dir))
	{
		ggplot(dd) +			
				geom_ribbon(aes(x=week, ymin=lcl, ymax=lcu), fill='black', alpha=0.25) +
				geom_line(aes(x=week, y=lmean)) +
				geom_point(aes(x=week, y=lwc), colour='DeepSkyBlue') +
				facet_wrap(~state, ncol=5) +
				labs(x='calendar week', y='log reported COVID-19 cases, average by week') +
				theme_bw()
		ggsave(file.path(plot_dir,'log_reported_cases.pdf'), w=15, h=25)		
	}
	
	dd
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
process_make_contact_matrix_by_country<- function(dpolymod, pop_info, args_with_contacts_adjusted_for_age)
{	
	#	check that age buckets agree between dpolymod and pop_by_age
	tmp <- unique(subset(dpolymod, select=c(part.age.cat, part.age.cat.label)))
	stopifnot( all( sort(unique(pop_info$age.cat.label)) == sort(tmp$part.age.cat.label) ) )
	tmp <- unique(subset(dpolymod, select=c(cont.age.cat, cont.age.cat.label)))
	stopifnot( all( sort(unique(pop_info$age.cat.label)) == sort(tmp$cont.age.cat.label) ) )
	
	
	tmp <- subset(pop_info, select=c(loc, loc_label, age.cat, age.cat.label, pop, pop_sqm, prop_pop))
	tmp2 <- tmp[, list( pop_ntl=mean(pop), 
						pop_sqm_ntl= mean(pop_sqm),
						prop_pop_ntl= sum( prop_pop * pop / sum(pop) )
						), by='age.cat']
	tmp <- merge(tmp, tmp2, by='age.cat')			
	setnames(tmp, c('age.cat','age.cat.label'), c('cont.age.cat','cont.age.cat.label'))
	dc <- merge(dpolymod, tmp, by=c('cont.age.cat','cont.age.cat.label'),allow.cartesian=TRUE)
	dc[, cont.pop.polymod := m/c]	
	dc[, cont.pop.effective.loc := cont.pop.polymod * prop_pop / prop_pop_ntl]
	if(args_with_contacts_adjusted_for_age)
	{
		cat('\nAdjusting expected number of contacts by age distribution in location ...')
		dc[, m:= cont.pop.effective.loc * c]
	}
	dc <- subset(dc, select=c(loc, loc_label, cont.age.cat, cont.age.cat.label, part.age.cat, part.age.cat.label, m))
	dc
}

#' @export
#' @keywords internal
#' @import data.table 
stan_data_add_initA_array_20_54 <- function(processed_data)
{ 
	processed_data$stan_data$N_init_A <- 7
	processed_data$stan_data$init_A <- seq.int(5, by=1, len=7)
	processed_data
}

#' @export
#' @keywords internal
#' @import data.table 
stan_data_add_initA_array_5_29 <- function(processed_data)
{ 
  processed_data$stan_data$N_init_A <- 5
  processed_data$stan_data$init_A <- seq.int(2, by=1, len=5)
  processed_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_pairwise_mobility_trends <- function(processed_data,file_chi)
{ 
  tmp <- readRDS(file_chi)
  chi <- tmp[['chi']]
  map_chi <- tmp[['map_chi']]
  processed_data$stan_data$chi <- chi[map_chi,map_chi]
  processed_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom lubridate days decimal_date
#' @importFrom dplyr bind_rows
make_stan_data_core <- function(states, 
		mobility_data, 
		death_data, 
		deathByAge_data, 
		ifr.by.age, 
		serial_interval, 
		pop_info, 
		dcontact, 
		num_days_sim, 
		forecast)
{
	# states: vector
	# states you want to consider for analysis, must be in code format (i.e., for Washington, use WA)
	# death_data: data table
	# Death and case data by state name, state code, and date
	# with variables: date, Cases, Deaths, state, code 
	# deathByAge_data: list
	# deathByAge_data$deaths_data: list by state 
	# for every state a matrix with date by row and age by column.
	# the number of column is fixed to the number of contact_band. If the number of age groups < contact_band, empty columns are filled with -1
	# the first row corresponds to the first date d at which overall death were observed in the state. If first day of observed death by age < d, empty rows are filled with -1 
	# deathByAge_data$map_age: list by state
	# for every state a matrix M with contact band by row and state age groups by column
	# M_ij = 1 if j \in i
	# deathByAge_data$dataByAgestart: list by state
	# for every state integer corresponding to the first day with death by age data on the overall death scale
	# deathByAge_data$A_AD: list by state
	# for every state integer corresponding to the number of age groups provided by the state
	# ifr and population count by state name and state code
	# variables are state, code, ifr and popt
	# ifr.by.age: data table
	# mean, cl, cu of ifr by age obtained from Verity, 2020.
	# also provide hyperparameters alpha and beta of a beta distribution given the mean and confidence interval
	# serial_interval: data table
	# serial interval by days since infection
	# variables are X, days since infection, and fit, the serial interval
	# pop by age: matrix
	# rows are age band and columns are state
	# element i,j is the proportion of population of state j in age group i
	# num_days_sim: integer
	# number of days to simulate
	# equals to the number of days in death_data + forecast
	# forecast: integer
	# number of days to forecast
	
	# various distributions required for modeling
	set.seed(5678) # make reproducible 
	mean1 <- 5.1; cv1 <- 0.86; # infection to onset
	mean2 <- 18.8; cv2 <- 0.45 # onset to death
	x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
	x2 <- rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
	ecdf_death <- ecdf(x1+x2)
	
	#	disrectise to days since infection
	ifr_by_dsi <- vector('numeric',num_days_sim)
	ifr_by_dsi[1] <- ecdf_death(1.5) - ecdf_death(0)
	for(s in 2:num_days_sim)
	{
		ifr_by_dsi[s] <- ecdf_death(s+.5) - ecdf_death(s-.5)
	}
	
	set.seed(5678) # make reproducible 
	mean1 <- 5.1; cv1 <- 0.86; # infection to onset
	mean2 <- 14; cv2 <- 3.57 # onset to antibody
	x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
	x2 <- rnorm(1e6,mean2,cv2) # onset-to-antibody distribution
	ecdf_death <- ecdf(x1+x2)
	
	#	disrectise to days since infection
	iar_by_dsi <- vector('numeric',num_days_sim)
	iar_by_dsi[1] <- ecdf_death(1.5) - ecdf_death(0)
	for(s in 2:num_days_sim)
	{
	  iar_by_dsi[s] <- ecdf_death(s+.5) - ecdf_death(s-.5)
	}
	# serial intervention cut
	serial.interval.cut <- length( which(serial_interval$fit * 5e6 >= 1 ) )
	
	# contact bands
	cntct_bands <- max( dcontact$part.age.cat )    
	
	# states with death data by age
	states_ad <- which( states %in% names(deathByAge_data$A_AD) )
	# states without death data by age
	states_od <- which( !states %in% states[states_ad] )
	
	# first column, 1 if country has death by age data
	# second column, index within state with death by age data
	map_country = matrix(nrow = length(states), ncol = 2, 0)
	map_country[states_ad, 1] = 1
	map_country[states_ad, 2] = 1:length(states_ad)
	
	# map the age reported by the state to 5y age groups
	map_age = aperm(array(unlist(deathByAge_data$map_age[states[states_ad]]), dim=c(cntct_bands, cntct_bands, length(states_ad))), c(3, 1, 2))
	
	# going over each region
	dates <- list()
	reported_cases <- list()
	deaths_by_state <- list()
	stan_data <- list(
			#	constants
			M = length(states), 															  # number of states
			M_AD = length(states_ad), 								  			  # number of countries with age-specific death counts	
			N0 = 6L, 																					  # N0 = 6 to make it consistent with Rayleigh
			N = vector('integer',length(states)),						  	# days of observed data for country m. each entry must be <= N2
			N2 = as.integer(num_days_sim), 										  # days of observed data + # of days to forecast
			A = as.integer(cntct_bands), 							  			  # number of age bands
			SI_CUT = serial.interval.cut, 									  	# number of days in serial interval to consider		
			WKEND_IDX_N= vector('integer',length(states)),	  	# number of weekend indices in each location
			#	data
			pop = vector('numeric',length(states)),				  		# population counts
			popByAge = matrix(NA_integer_, as.integer(cntct_bands), length(states)),	# proportion of age bracket in population in location
			epidemicStart = vector('integer',length(states)), 	# day at which epidemic start		
			deaths = matrix(NA_integer_, num_days_sim, length(states)), 	# reported deaths						
			deathsByAge = array(NA_integer_, dim = c(num_days_sim, as.integer(cntct_bands), length(states_ad))),
			wkend_idx = matrix(NA_integer_, num_days_sim, length(states)),	# indices of 1:N2 that correspond to weekends in location m
			# death  by age data,
			map_country = map_country,								       	  # first column whether the state has death by age data, second column index country with death by age data
			A_AD = as.integer(unlist(deathByAge_data$A_AD[states[states_ad]])), # number of age category in the observed death by age data
			map_age = map_age, 			  			  			  			  	# map age observed to 5y age groups
			dataByAgestart = vector('integer', length(states_ad)), # start of the death by age data
			#	priors
			cntct_weekdays_mean = vector('list', length(states)), # mean of prior contact rates between age groups
			cntct_weekends_mean = vector('list', length(states)),  
			ifr_age = matrix(data=ifr.by.age$ifr_mean, ncol=cntct_bands, nrow=num_days_sim, byrow=TRUE), 	#	probability of death for age band a, stored N2 times     
			rev_ifr_daysSinceInfection = rev( ifr_by_dsi ), 			# probability of death s days after infection in reverse order
			rev_iar_daysSinceInfection = rev( iar_by_dsi ),  	# probability of antibody s days after infection in reverse order
			rev_serial_interval = rev( serial_interval$fit[1:serial.interval.cut] ), 			# fixed pre-calculated serial interval using empirical data from Neil in reverse order
			with_eta2 = 0
	) 		
	
	cat('\nProcessing country data... \n')
	for(m in seq_along(states)) 
	{ 
		#m <- 1	
		State <- states[m]
		State_name <- pop_info$loc_label[pop_info$loc==State]
		
		cat("\n Processing", State, "\n")
		
		#	create padded data of cases and deaths
		# start counting 30 days prior to reaching 10 deaths
		d1 <- death_data[death_data$code==State,]
		d1$t <- lubridate::decimal_date(d1$date) 
		d1 <- d1[order(d1$t),]
		date_min <- as.Date('2019-12-31') 
		if (d1$t[1] > date_min){
			print(paste(State,'In padding'))
			pad_days <- d1$date - date_min
			pad_dates <- date_min + lubridate::days(1:pad_days[[1]]-1)
			padded_data <- data.frame("state" = rep(State, pad_days),
					"date" = as.Date(pad_dates,format='%d/%m/%Y'),
					"Cases" = as.integer(rep(0, pad_days)),
					"Deaths" = as.integer(rep(0, pad_days)),
					stringsAsFactors=F)
			d1 <- dplyr::bind_rows(padded_data, d1)
		}
		index <- which(d1$Cases>0)[1]
		index1 <- which(cumsum(d1$Deaths)>=10)[1] # also 5
		index2 <- index1-30L
		print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))
		n.d1 = nrow(d1)
		d1 <- d1[index2:n.d1,]
		
		# death by age and overall death must have same number of days
		if( m %in% states_ad ){ 
			deathsByAge <- as.matrix(deathByAge_data$deaths_data[[State]])
			n.d2 = nrow(deathsByAge)
			if(n.d1 > n.d2){
				d1 = head(d1, -(n.d1-n.d2)) # remove  n.d1-n.d2 last elements 
				n.d1 = n.d2
			}
			
			if(n.d1 < n.d2) deathsByAge = deathsByAge[-((n.d1+1):n.d2),]
		}
		
		# hazard estimation
		N <- nrow(d1)
		print(sprintf("%s has %d days of data",State,N))
		forecast <- num_days_sim - N
		if(forecast < 0) {
			print(sprintf("%s: %d", State, N))
			print("ERROR!!!! increasing num_days_sim")
			num_days_sim <- N
			forecast <- num_days_sim - N
		}				
		
		# process overall death and death by age
		if( m %in% states_ad ){
			m2 <- which(m==states_ad)# bookeeping
			
			deaths_processed <- process_death(d1, is_m_in_states_od = 0, index1, index2, forecast, 
					deathsByAge, deathByAge_data$dataByAgestart[[State]], deathByAge_data$A_AD[[State]])
		}
		
		if( m %in% states_od ){
			deaths_processed <- process_death(d1, is_m_in_states_od = 1, index1, index2, forecast, 
					deathsByAge = NULL, dataByAgestart = NULL, A_AD = NULL)
		}
		
		deaths_without_forecast = deaths_processed$deaths_without_forecast
		deaths = deaths_processed$deaths
		deathsByAge = deaths_processed$deathsByAge 
		dataByAgestart = deaths_processed$dataByAgestart
		
		#	determine the index of weekend dates
		dates_with_forecast = c(d1[-N,]$date, seq(d1[N,]$date, d1[N,]$date+forecast, by ="day"))
		wkend_idx <- which(as.integer(format(dates_with_forecast, "%u"))>5)
		## store and save processed data
		# store number of reported deaths
		deaths_by_state[[State]] <- deaths_without_forecast
		
		# store number of reported cases
		reported_cases[[State]] <- as.vector(as.numeric(d1$Cases))
		
		# store dates for data from this country
		dates[[State]] <- d1$date 
		
		#	add constants
		stan_data$N[m] <- N
		stan_data$WKEND_IDX_N[m] <- length(wkend_idx)
		stan_data$wkend_idx[,m] <- c(wkend_idx, rep(0,num_days_sim-length(wkend_idx)))
		
		# add population count  
		tmp <- subset(pop_info, loc==State)[order(age.cat)]				
		stan_data$pop[m] <- tmp$pop_total[1]
		stan_data$popByAge[,m] <- tmp$prop_pop
		
		# add day at which epidemic start
		stan_data$epidemicStart[m] <- index1+1L-index2	
		
		# add deaths
		stan_data$deaths[,m] <- deaths
		
		# add death by age
		if( m %in% states_ad )
		{
			stan_data$deathsByAge[,,m2]<- deathsByAge
			stan_data$dataByAgestart[m2] <- dataByAgestart 
		}
		
		# add weekend contact matrix
		tmp <- dcast.data.table(subset(dcontact, loc==State & type=="weekend"), part.age.cat~cont.age.cat, value.var='m')
		tmp <- unname(as.matrix(tmp)[,-1])		
		stan_data$cntct_weekends_mean[[m]] <- tmp
		
		# add weekday contact matrix
		tmp <- dcast.data.table(subset(dcontact, loc==State & type=="weekday"), part.age.cat~cont.age.cat, value.var='m')
		tmp <- unname(as.matrix(tmp)[,-1])
		stan_data$cntct_weekdays_mean[[m]] <- tmp
		
		if(length(stan_data$N) == 1) 
		{
			stan_data$N <- as.array(stan_data$N)
		}
	}
	
	# transform in array single vector 
	if(stan_data$M == 1)
	{
		stan_data$pop <-  as.array(stan_data$pop)
		stan_data$epidemicStart <-  as.array(stan_data$epidemicStart)		
		stan_data$WKEND_IDX_N <-  as.array(stan_data$WKEND_IDX_N)
	}
	
	
	return(list("stan_data" = stan_data, "dates" = dates, "reported_cases"=reported_cases, "deaths_by_state" = deaths_by_state))
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
#' @importFrom dplyr rename distinct select
process_deathByAge = function(deathByAge, range_date, states, path_to_file_pop_us)
{
  # subset death by age data by selected states 
  deathByAge = subset(deathByAge, code %in% states)
  
  # name of the states and number states with death by age data
  state = unique(deathByAge$code)
  n.state = length(unique(deathByAge$code))

  # age band in the analysis: Alex's code
  pop_by_age <- readRDS(path_to_file_pop_us)
  age_bands <- dplyr::select(pop_by_age[which(!is.na(pop_by_age$code)),], -Total) %>%
    reshape2::melt(id.vars = c("Region", "code")) %>%
    dplyr::rename(age = variable, pop = value, state = Region) %>% dplyr::select(age) %>% dplyr::distinct()
  # min age and max age in every age band 
  age.cat <- sapply(strsplit(gsub("\\[|\\]|\\(|\\+", "", age_bands$age), "-"), as.numeric)
  if (is.na(age.cat[[length(age.cat)]][2])) age.cat[[length(age.cat)]][2] <- 99
  age.cat <- matrix(unlist(age.cat), ncol = 2, byrow = TRUE)
  n.age.cat = nrow(age.cat)
  age.cat = rbind(age.cat, rep(101, 2)) # ensure the last interval

  # map from age band in the analysis to age band specified by state
  map_age = list()
  # number of age band specified by state
  A_AD = list()
  # death by age data
  deaths_data = list()
  # first day with death by age data on the overall death scale
  dataByAgestart = list()
  
  for(State in state){
    
    cat("\nProcessing ",State)
  
    # subset death by age by State
    deathByAge_state = data.table( subset(deathByAge, code == State) )
    deathByAge_state$date = as.Date(deathByAge_state$date)
    first.day = min(deathByAge_state$date)
    
    if(any(na.omit(deathByAge_state$daily.deaths) < 0)) print("ERROR!!!! daily death for one or more days is < 0")
    
    # the daily death is not observed on the first day but the cum death is
    # if the period with overall death finishes before the first day with daily death by age data, ignore the state
    if(range_date[2] < (first.day + 1)  ) next 
    
    
    ## 1. map the age band in the analysis to age band specified by state
    
    # age band specified by the state
    df = data.table(age=unique(deathByAge_state$age)); n.age = nrow(df)
    # create groups
    df[, group := 1:n.age] 
    deathByAge_state = merge(deathByAge_state, df, by = "age")
    # for the last category (ending with string +), set the upper boundary to 99
    df[ grepl("\\+", age), age := paste0(gsub("(.+)\\+", "\\1", age), "-99") ]
    # find the lower boundaries of age band specified by the state
    df[, age_min := suppressWarnings(as.numeric(gsub("(.+)-.*", "\\1", age)))]
    stopifnot(all(!is.na(df$age_min)))

    # group all age bands with age >= 85 (e.g, "80-89", "90+" --> "80+")
    if(any(df$age_min > 85)){

      # max of age_min conditional on being =< 85 
      age_max = df$age_min[which.max( df$age_min[ which(df$age_min <= 85)] ) ]

      # age band with age greater than age_max 
      df[, g_age_max := age_min >= age_max]
      df_new = subset(df, !g_age_max)
      # new age bands
      df_new = rbind(df_new, 
                     data.table(age = paste0(min(subset(df, g_age_max)$age_min), "+"),
                                age_min = min(subset(df, g_age_max)$age_min),
                                group = min(subset(df, g_age_max)$group)), fill = TRUE)
      
      # aggregate the new age bands (sum number of deaths over them)
      df[ , newgroup := group ]
      df[(g_age_max), newgroup := min(group) ]
      deathByAge_state = merge(deathByAge_state, dplyr::select(df, c("newgroup", "group")), by = "group")
      deathByAge_state = deathByAge_state[ , list(cum.deaths = sum(cum.deaths), 
                                                  daily.deaths = sum(daily.deaths)),
                                           by = list(date, newgroup)]
      setnames(deathByAge_state, "newgroup", "group")

      # new age band names
      deathByAge_state = merge(deathByAge_state, df_new, by = "group") 
      deathByAge_state = dplyr::select(deathByAge_state, c(date, age, cum.deaths, daily.deaths))
      
      # age band specified by the state
      df = data.table(age=unique(deathByAge_state$age)); n.age = nrow(df)
      df[, group := 1:n.age] 
      # for the last category (ending with string +), set the lower boundary to 99
      df[ grepl("\\+", age), age := paste0(gsub("(.+)\\+", "\\1", age), "-99") ]
      # find the upper boundaries of age band specified by the state
      df[, age_min := suppressWarnings(as.numeric(gsub("(.+)-.*", "\\1", age)))]
      stopifnot(all(!is.na(df$age_min)))
      
    }
    
    df=df[order(age_min)]
    
    # find the upper boundaries of age band specified by the state
    df[, age_max := suppressWarnings(as.numeric(gsub(".*-(.+)", "\\1", age)))]
    stopifnot(all(!is.na(df$age_max)))
    
    # transform age band to factor, and ensure that they are in ascending order
    deathByAge_state$age = factor(deathByAge_state$age, levels = c(paste0(df$age_min[-n.age], "-", df$age_max[-n.age]), paste0(df$age_min[n.age], "+")))
    
    # find the 5y age band to which the age band specified by state belong
    df[, I_min := findInterval(age_min, age.cat[,1], all.inside = TRUE)]
    df[, I_max := findInterval(age_max, age.cat[,2], all.inside = TRUE)]
    
    stopifnot(all(df$minI <= df$maxI))
    stopifnot(df$minI[1] == 1 & df$maxI[n.age] == n.age)
    stopifnot(all(df$minI == sort(df$minI)) & all(df$maxI == sort(df$maxI)))
    stopifnot(all(df$age_min >=0))
    stopifnot(all(df$age_max >=0))
    
    # matrix with age band in analysis by rows and age band specified by state by columns
    # map_age_ij = 1 if j in i
    map_age[[State]] = matrix(nrow = n.age.cat, ncol = n.age.cat, c(rep(0, n.age.cat*n.age), rep(-1, n.age.cat*(n.age.cat*n.age-n.age))))
    for(i in 1:n.age){
      map_age[[State]][df$I_min[i]:df$I_max[i],i] = 1 
    }
    stopifnot( all(apply(map_age[[State]][, 1:n.age], 1, sum) == 1))
    
    
    ## 2. number of age band specified by state
    
    A_AD[[State]] = n.age
    
    
    ## 3.create death data
    
    # define period: from the first day with overall death data to the last day with death by age data
    n.date = length(range_date[1]:max(deathByAge_state$date))
    dates = data.table(date = seq.Date(range_date[1], max(deathByAge_state$date), by = "day"))
    
    # create var.deaths: cum deaths on the first day and daily deaths on the remaining days
    deathByAge_state[ date == first.day, var.deaths := cum.deaths]
    deathByAge_state[ date != first.day, var.deaths := daily.deaths]
    
    # rows are days and age band specified by state are columns
    # matrix is filled with var.deaths
    tmp = reshape2::dcast(deathByAge_state, date ~ age, value.var = "var.deaths") %>%
      merge(dates, by = "date", all.y = T)
    
    # index of the first day with death by age data 
    start_index = which(dates$date == min(deathByAge_state$date))
    
    # add -1 for missing days
    tmp[1:(start_index-1),2:(n.age+1)] = -1 
    if(n.age != n.age.cat)  tmp[,(n.age+2):(n.age.cat+1)] = -1
    
    deaths_data[[State]] = as.matrix(tmp[,-1])
    
    ## 4. first day with death by age data on the overall death scale
    
    dataByAgestart[[State]] = start_index
  }
  
  return(list(deaths_data = deaths_data, map_age = map_age, dataByAgestart = dataByAgestart, A_AD = A_AD))
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom scales percent rescale
#' @importFrom dplyr rename select
process_emodo_mobility_into_mobility_trends <- function(mobility_data, 
		dcontact, 
		pop_info 
		)
{	
	#	calculate baseline Emodo over 1 week 
	#	(this is roughly the same amount of data points as Google uses to calculate their baselines)
	
	dc <- copy(mobility_data)	
	setnames(dc, 'avg_contacts', 'value')
	#set(dc, NULL, 'emo.idx.age.label', dc[, factor(emo.idx.age.cat, levels=emo.idx.age.cat, labels=emo.idx.age.label)])
	#set(dc, NULL, 'emo.pair.age.label', dc[, factor(emo.cont.age.cat, levels=emo.cont.age.cat, labels=emo.cont.age.label)])
	#set(dc, NULL, 'emo.pair.age.label', dc[, factor(emo.pair.age.cat, levels=emo.pair.age.cat, labels=emo.pair.age.label)])
	
	
	slist <- unique(dc$loc)
	date.min <- min(dc$date)
	date.max <- max(dc$date)
	
	#	calculate baseline values and add to data
	dcb <- subset(dc, date<=date.min+7)
	dcb <- dcb[, list(base_value= mean(value)), by=c('loc','emo.pair.age.cat')]
	dc <- merge(dc, dcb, by=c('loc','emo.pair.age.cat'))
	
	#	define trend
	dc[, mobility_trend:= value/base_value]
	
	
	# 	plot baseline and actual value as lines
	cat("\n plot contacts + baseline ...")
	dc_w <- unique(subset(dc, select=c(date, weekend)))
	dc_bp <- data.table(xmin= date.min, xmax=date.min+7, ymin=-Inf, ymax=Inf)
	ps <- vector('list', length(slist))
	for (x in slist) 
	{
		dc_m <- subset(dc, loc==x)
		dc_b <- unique(subset(dc_m, select=c(loc_label, base_value, emo.pair.age.label)))		
		ggplot(dc_m) +			
				geom_rect(data=dc_bp, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill='green', alpha = .8) +
				geom_tile(data=dc_w, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend = FALSE) +
				scale_fill_manual(values = c("transparent", "black")) +
				scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +				
				geom_hline(data=dc_b, aes(yintercept=base_value, colour=emo.pair.age.label)) +
				geom_step(aes(x=date, y=value, colour=emo.pair.age.label), direction="vh") +				
				theme_bw() +
				labs(x= 'Date', y='Mobility trend', colour='Contacts') +
				facet_wrap(loc_label~emo.pair.age.label, scales='free_y', ncol=5) +
				theme(legend.position='bottom',
						legend.title = element_text(size = 10), 
						legend.text = element_text(size = 8),
						plot.title = element_text(size = 24, face = "bold"),
						axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
						axis.title.x = element_blank(),
						panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
						panel.background = element_blank(),
						strip.background = element_blank()) +
				scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option="magma") +
				guides(colour=guide_legend(ncol=12))
		ggsave(file=file.path(plotdir, paste0('emodo_mobility_baseline_by_age_',x,'.pdf')), w=14, h=14, limitsize=FALSE)					
	}
	
	
	# 	plot trends as lines
	cat("\n plot mobility trends ...")
	dc_w <- unique(subset(dc, select=c(date, weekend)))
	ps <- vector('list', length(slist))
	plotdir2 <- NA_character_
	for (x in slist) 
	{
		dc_m <- subset(dc, loc==x)
		y.max <- max(dc_m$mobility_trend)
		ps[[x]] <- ggplot(dc_m) +			
			geom_tile(data=dc_w, aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2, show.legend = FALSE) +
			scale_fill_manual(values = c("transparent", "black")) +
			scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +
			scale_y_continuous(expand=c(0,0), label=scales::percent) +
			geom_step(aes(x=date, y=mobility_trend, colour=emo.pair.age.label), direction="vh")			+
			coord_cartesian(ylim=c(0,y.max*1.1)) +
			theme_bw() +
			labs(x= 'Date', y='Mobility trend', colour='Contacts') +
			facet_wrap(loc_label~.) +
			theme(legend.position='bottom',
					legend.title = element_text(size = 10), 
					legend.text = element_text(size = 8),
					plot.title = element_text(size = 24, face = "bold"),
					axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
					axis.title.x = element_blank(),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					strip.background = element_blank()) +
			scale_colour_viridis_d(begin=0,end=.8,alpha=1,direction=-1,option="magma") +
			guides(colour=guide_legend(ncol=12))
		if(!is.na(plotdir2))
		{
			ggsave(file=file.path(plotdir2, paste0('emodo_mobility_trends_by_age_',x,'.png')), ps[[x]], w=14, h=6, limitsize=FALSE)	
		}		
	}
	g <- ggarrange(plotlist=ps, ncol = 1, nrow=length(slist), widths=1, heights=rep(1/length(slist),length(slist)))
	ggsave(file=file.path(plotdir, paste0('emodo_mobility_trends_by_age.pdf')), w=20, h=200, limitsize=FALSE)
	
	#	plot trends as heatmap
	ps <- vector('list',length(slist))
	names(ps) <- slist
	y.max <- min(3, max(dc$mobility_trend))
	y.min <- max(0.1,min(dc$mobility_trend))	
	for (x in slist) 
	{
		dc_m <- subset(dc, loc==x)
		dc_m[, plot_value:= pmin(y.max,pmax(y.min, mobility_trend))]
		ps[[x]] <- ggplot(dc_m) +			
				scale_x_date(expand=c(0,0), date_breaks = "2 weeks", labels = date_format("%e %b")) +
				scale_y_discrete(expand=c(0,0)) +
				geom_tile(aes(x= date, y= emo.pair.age.label, fill=plot_value)) +
				scale_fill_viridis(begin=0,end=1, alpha=0.6,direction=1,option="magma", labels = scales::percent,
									values = scales::rescale(c(0.1,0.33,0.4,0.5,0.66,1,1.5,2,2.5,3)),breaks=seq(0.5,3,.5)) +
				labs(x= 'Date', y='Contacts', fill='') +
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
				guides(fill = guide_colourbar(barheight = 10, barwidth = 0.5, direction="vertical")) 
	}
	g <- ggarrange(plotlist=ps, ncol = 5, nrow=11, widths=rep(1/5,5), heights=rep(1/11,11))
	ggsave(file=file.path(plotdir, paste0('emodo_mobility_trends_2_by_age.png')), w=40, h=60, limitsize=FALSE)
	
	
	
	
	fsqb[, age.lower:= as.integer(gsub('^([0-9]+)_([0-9a-z]+)$','\\1',age))]
	fsqb[, age.upper:= as.integer(gsub('plus','99',gsub('^([0-9]+)_([0-9a-z]+)$','\\2',age)))]
	# store raw baseline for plotting trends
	fsqbr <- fsqb	
	
	#	expand visit rates to our stratification
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
	setnames(dages, c('age.cat','age.cat.label','age.cat.from','age.cat.to'), c('age_cat','age_band','age.lower','age.upper'))
	tmp <- unique(subset(fsqb, select=c(age, age.lower, age.upper)))
	set(tmp, NULL, 'age', tmp[, as.character(age)])
	dages <- dages[, {								
				z <- which( age.upper >= tmp$age.lower & age.upper <= tmp$age.upper )
				list(age= ifelse(length(z)>0, as.character(tmp$age[z]), NA_character_))					
			}, by=c('age_cat','age_band')]	
	dages[, dummy:= 1L]
	dages <- merge(dages, data.table(state=unique(fsqb$state), dummy=1L), by='dummy', allow.cartesian=TRUE)
	set(dages, NULL, c('dummy'), NULL)	
	fsqb <- merge(dages, fsqb, by=c('age','state'), all.x=TRUE)
	
	#	merge base visit rates and average contacts of index persons
	stopifnot( all( as.character(sort(unique(dcontact$loc))) == sort(unique(fsqb$state)) ) )
	ctc <- subset(dcontact, select=c(loc, cont.age.cat, part.age.cat, m, type))
	setnames(ctc, c('part.age.cat','cont.age.cat','m','loc'), c('age_cat_index','age_cat_respondent','avg_contacts','state'))
	ctcm <- ctc[, list(marg_contacts= sum(avg_contacts)), by=c('state','type','age_cat_index')]	
	setnames(ctcm, 'age_cat_index', 'age_cat')
	fsqb <- merge(fsqb, ctcm, by=c('state','age_cat'),allow.cartesian=TRUE)
	
	# plotting
	if(0)
	{
		tmp <- subset(fsqb, state=='NYC')
		ggplot(tmp, aes(x=age_band)) +			
				geom_point(aes(y=base_visits_rate), colour='black') +			
				geom_point(aes(y=marg_contacts, pch=type, colour=type)) +
				theme_bw() +
				facet_wrap(.~state, ncol=1) +
				labs(x='', y='')
		#	this plot suggests to me that the UK POLYMOD contact matrix has more (realistic) detail
		#	than the FSQ data  
	}
	
	
	# scale the UK POLYMOD matrix by a constant for each state, constant is avg mobility in baseline period
	if(args_scale_contactmatrix_bystate)
	{
		cat('\nScaling contact matrix by avg baseline mobility per state ... ')
		tmp <- subset(fsqb, !is.na(base_visits_rate), select=c(state, base_visits_rate))
		tmp <- tmp[, list(base_visits_rate= mean(base_visits_rate)), by=c('state')]
		tmp[, base_visits_rate_avg:= mean(base_visits_rate)]
		tmp[, base_visits_rate_mult:= base_visits_rate/base_visits_rate_avg]
		# plotting
		if(0)	
		{
			ggplot(tmp, aes(x=state, y=base_visits_rate_mult)) + geom_bar(stat='identity') + coord_flip() + theme_bw()
		}
		fsqb <- merge(fsqb, subset(tmp, select=c(state,base_visits_rate_mult)),by=c('state'),all.x=TRUE)	  
	}
	
	# scale the UK POLYMOD matrix by a constant for each state ang each age band, constant is avg mobility in baseline period in this age group
	if(args_scale_contactmatrix_bystateandage)
	{
		cat('\nScaling contact matrix by age-specific baseline mobility per state ... ')
		#	so why don t we use the FSQ data to scale the UK POLYMOD data as follows:
		tmp <- unique(subset(fsqb, !is.na(base_visits_rate), select=c(age, state, base_visits_rate)))
		tmp2 <- tmp[, list(base_visits_rate_avg= mean(base_visits_rate)), by=c('age')]
		tmp <- merge(tmp, tmp2, by='age')
		tmp[, base_visits_rate_mult:= base_visits_rate/base_visits_rate_avg]
		fsqb <- merge(fsqb, subset(tmp, select=c(state,age,base_visits_rate_mult)),by=c('state','age'),all.x=TRUE)		 
		#	first value carried backwards? anyhow, let s just do something for now and 
		#	then maybe a sensitivity analysis
		fsqb <- fsqb[, {
					z<- which(is.na(base_visits_rate_mult))
					base_visits_rate_mult[z] <- base_visits_rate_mult[tail(z,1)+1]
					list(	base_visits_rate=base_visits_rate,
							base_visits_rate_mult=base_visits_rate_mult,
							marg_contacts=marg_contacts,
							age=age,
							age_cat=age_cat,
							age_band=age_band
					)				
				}, by=c('state','type')]
		# plotting
		if(0)
		{
			tmp <- subset(fsqb, state%in%c('NYC','CO','CT','CA','HI','DC','AL'))
			ggplot(tmp, aes(x=age_cat, y=base_visits_rate_mult)) +
					geom_step(aes(colour=state)) +
					geom_point(aes(colour=state)) +			
					theme_bw() 
			
			#	looks good I think - note I selected all the high outliers to see how much we inflate
			#	under this approach  
		}
	}
	
	#	now build the state specific contact matrices
	setnames(fsqb, 'age_cat', 'age_cat_index')
	if(args_scale_contactmatrix_bystateandage | args_scale_contactmatrix_bystate)
	{
		tmp <- subset(fsqb, select=c(state, type, age_cat_index, base_visits_rate_mult))
		ctc <- merge(ctc, tmp, by=c('state','age_cat_index','type'),allow.cartesian=TRUE)
		ctc[, avg_contacts:= avg_contacts * base_visits_rate_mult]
		ctc[, base_visits_rate_mult:= NULL]						
	}
	setnames(ctc, c('state','age_cat_index','age_cat_respondent','avg_contacts'), c('loc','part.age.cat','cont.age.cat','m'))
	ctc <- merge(subset(dcontact, select=-m), ctc, by=c('loc','part.age.cat','cont.age.cat','type'))
	
	# plotting
	if(0)
	{ 
		# run 1 scaling after the other		
		df <- subset(ctc,  state%in%c('NYC','CO','CT','CA','HI','DC','AL')) %>%
		  dplyr::rename(`original scale` = avg_contacts_old, `scaled by one scalar by state` = avg_contacts) %>%
				reshape2::melt(id.vars = c("age_cat_index","type","age_cat_respondent","state")) %>%
		  dplyr::rename(scaling = variable, avg_contacts = value)
		
		df2 <- subset(ctc,  state%in%c('NYC','CO','CT','CA','HI','DC','AL')) %>%
		  dplyr::rename(`original scale` = avg_contacts_old, `scaled by one scalar by state and age` = avg_contacts) %>%
				reshape2::melt(id.vars = c("age_cat_index","type","age_cat_respondent","state")) %>%
		  dplyr::rename(scaling = variable, avg_contacts = value) %>%
				rbind(subset(df, scaling != "original scale"))
		
		df2$scaling = factor(df2$scaling, levels = c("original scale", "scaled by one scalar by state", "scaled by one scalar by state and age"))
		
		ggplot(subset(df2, type == "weekday"), aes(x = age_cat_index, y = age_cat_respondent)) +
				geom_raster(aes(fill=sqrt(avg_contacts) )) + 
				scale_fill_gradient(low="white", high="red") +
				theme_bw() +
				facet_wrap(~state+scaling, ncol = 3)
		ggsave("~/Downloads/contact.matrix_weekday_scaled.png", w = 10, h = 15)
		
		ggplot(subset(df2, type == "weekend"), aes(x = age_cat_index, y = age_cat_respondent)) +
				geom_raster(aes(fill=sqrt(avg_contacts) )) + 
				scale_fill_gradient(low="white", high="red") +
				theme_bw() +
				facet_wrap(~state+scaling, ncol = 3)
		ggsave("~/Downloads/contact.matrix_weekend_scaled.png", w = 10, h = 15)
		
	}
	
	#	now we can build the FSQ multipliers
	fsqb <- unique(subset(fsqb, !is.na(base_visits_rate), select=c(state, age, base_visits_rate)))
	fsq <- merge(fsq, fsqb, by=c('state','age'))
	fsq[, mobility_trend:= norm_visits_rate / base_visits_rate]		
	set(dages, which(dages$age_cat %in% 1:3), "age", "18_24")	
	fsq <- merge(dages, fsq, by=c('age','state'), all.x=TRUE, allow.cartesian=TRUE)
	
	if(0)
	{
		tmp <- subset(fsq, state=="NYC")
		ggplot(tmp) +			
				scale_y_continuous(breaks=seq(0,3,0.1)) +
				#geom_tile(data=unique(subset(tmp, select=c(date, weekend))), aes(x = date, y = 0, height = Inf, fill = weekend), alpha = .2) +
				geom_hline(yintercept=1) +
				#scale_fill_manual(values = c("alpha", "black")) +
				geom_step(aes(x=date, y=mobility_trend, colour=age), direction="vh") +
				geom_point(aes(x=date, y=mobility_trend, colour=age)) +
				theme_bw() +
				labs(x= 'date', y='mobility_trend', colour='age band') +
				facet_wrap(.~state, scales='free')
		#	super cool. mobility drops to 55% among 18-24 and to 65% among 55-64	  
	}
	
	return(list(fsq=fsq, ctc=ctc, fsqbr=fsqbr))  
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
make_mobility_trends_emo <- function(mobility_data, emo_age_cat_map)
{				
	#	calculate baseline over 2 weeks 	
	min.date <- min(mobility_data$date)
	tmp <- subset(mobility_data, date<min.date+14)
	tmp <- tmp[, list(base_avg_contacts= mean(avg_contacts)), by=c('loc','emo.age.cat')]
	mobility_data <- merge(mobility_data, tmp, by=c('loc','emo.age.cat'))
	
	#	calculate mobility trends
	mobility_data[, mobility_trend:= avg_contacts / base_avg_contacts]
	
	#	expand to age strata used in model 
	tmp <- subset(emo_age_cat_map, select=c(emo.age.cat, age.cat, age.cat.label))
	set(tmp, tmp[, which(age.cat<=3)], 'emo.age.cat', 1L)
	mobility_data <- merge(mobility_data, tmp, by='emo.age.cat', allow.cartesian=TRUE)
	
	mobility_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
make_mobility_trends_fsq <- function(mobility_data, fsq_age_cat_map)
{		
	fsq <- mobility_data
	
	#	calculate baseline FSQ over 1 week 
	#	(this is roughly the same amount of data points as Google uses to calculate their baselines)
	min.date <- min(fsq$date)
	fsqb <- subset(fsq, date<=min.date+6)
	fsqb <- fsqb[, list(base_visits_rate= mean(norm_visits_rate)), by=c('loc','loc_label','fsq.age.cat','fsq.age.cat.label')]
	fsq <- merge(fsq, fsqb, by=c('loc','loc_label','fsq.age.cat','fsq.age.cat.label'))
	
	#	calculate mobility trends
	fsq[, mobility_trend:= norm_visits_rate / base_visits_rate]
	
	#	expand to age strata used in model 
	tmp <- subset(fsq_age_cat_map, select=c(fsq.age.cat, age.cat, age.cat.label))
	set(tmp, tmp[, which(age.cat<=3)], 'fsq.age.cat', 1L)
	fsq <- merge(fsq, tmp, by='fsq.age.cat', allow.cartesian=TRUE)
	
	fsq
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
make_emodo_age_cat_map <- function(mobility_data, pop_info)
{
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
	tmp <- unique(subset(mobility_data, select=c(emo.age.cat, emo.age.label)))
	tmp[, emo.age.cat.label2:= as.character(emo.age.label)]
	tmp[, emo.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',emo.age.cat.label2)))]
	tmp[, emo.age.upper:= as.integer(gsub('[0-9]+\\+','99',gsub('^([0-9\\+]+)-([0-9\\+]+)$','\\2',emo.age.cat.label2)))]
	
	dages <- dages[, {
				z <- which( age.cat.to >= tmp$emo.age.lower & age.cat.to <= tmp$emo.age.upper )
				list(emo.age.cat= ifelse(length(z)>0, tmp$emo.age.cat[z], NA_integer_))
			}, by=c('age.cat','age.cat.label')]
	tmp <- unique(subset(tmp, select=c(emo.age.cat, emo.age.label)))
	dages <- merge(dages, tmp, by= c('emo.age.cat'), all.x=TRUE)
	dages
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr
make_fsq_age_cat_map <- function(mobility_data, pop_info)
{
	dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
	tmp <- unique(subset(mobility_data, select=c(fsq.age.cat, fsq.age.cat.label)))
	tmp[, fsq.age.cat.label2:= as.character(fsq.age.cat.label)]
	tmp[, fsq.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',fsq.age.cat.label2)))]
	tmp[, fsq.age.upper:= as.integer(gsub('[0-9]+\\+','99',gsub('^([0-9\\+]+)-([0-9\\+]+)$','\\2',fsq.age.cat.label2)))]
	
	dages <- dages[, {
				z <- which( age.cat.to >= tmp$fsq.age.lower & age.cat.to <= tmp$fsq.age.upper )
				list(fsq.age.cat= ifelse(length(z)>0, tmp$fsq.age.cat[z], NA_integer_))
			}, by=c('age.cat','age.cat.label')]
	tmp <- unique(subset(tmp, select=c(fsq.age.cat, fsq.age.cat.label)))
	dages <- merge(dages, tmp, by= c('fsq.age.cat'), all.x=TRUE)
	dages
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
make_bics_age_cat_map <- function(pop_info)
{
  dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
  tmp <- data.table("bics.age.cat" = as.integer(c(1, 2, 3, 4, 5)), 
                    "bics.age.cat.label" = c("18-24", "25-34", "35-44", "45-64", "65+"))
  tmp[, bics.age.cat.label2:= as.character(bics.age.cat.label)]
  tmp[, bics.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',bics.age.cat.label2)))]
  tmp[, bics.age.upper:= as.integer(gsub('[0-9]+\\+','99',gsub('^([0-9\\+]+)-([0-9\\+]+)$','\\2',bics.age.cat.label2)))]
  
  dages <- dages[, {
    z <- which( age.cat.to >= tmp$bics.age.lower & age.cat.to <= tmp$bics.age.upper )
    list(bics.age.cat= ifelse(length(z)>0, tmp$bics.age.cat[z], NA_integer_))
  }, by=c('age.cat','age.cat.label')]
  tmp <- unique(subset(tmp, select=c(bics.age.cat, bics.age.cat.label)))
  dages <- merge(dages, tmp, by= c('bics.age.cat'), all.x=TRUE)
  dages
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
make_bics_age_cat_map_inc_children <- function(pop_info)
{
  dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
  tmp <- data.table("bics.age.cat" = as.integer(c(1, 2, 3, 4, 5, 6)), 
                    "bics.age.cat.label" = c("0-17", "18-24", "25-34", "35-44", "45-64", "65+"))
  tmp[, bics.age.cat.label2:= as.character(bics.age.cat.label)]
  tmp[, bics.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',bics.age.cat.label2)))]
  tmp[, bics.age.upper:= as.integer(gsub('[0-9]+\\+','99',gsub('^([0-9\\+]+)-([0-9\\+]+)$','\\2',bics.age.cat.label2)))]
  
  dages <- dages[, {
    z <- which( age.cat.to >= tmp$bics.age.lower & age.cat.to <= tmp$bics.age.upper )
    list(bics.age.cat= ifelse(length(z)>0, tmp$bics.age.cat[z], NA_integer_))
  }, by=c('age.cat','age.cat.label')]
  tmp <- unique(subset(tmp, select=c(bics.age.cat, bics.age.cat.label)))
  dages <- merge(dages, tmp, by= c('bics.age.cat'), all.x=TRUE)
  dages
}
#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_resnonres_constraints_on_alpha_beta <- function(processed_data, contact_tab)
{
	c1 <- max( contact_tab[[1]][['weekend']] / contact_tab[[1]][['weekday']] )
	c2 <- max( contact_tab[[1]][['weekday']] / contact_tab[[1]][['weekend']] )
	
	alpha <- seq(0,1,by=1e-4)
	beta.lower <- pmin(1,pmax(alpha*c1, (c2-1+alpha)/c2))		
	alpha.upper<- tail(alpha[which(beta.lower<1)],1)
	# ggplot( data.frame(a=alpha, b=beta.lower) ) + geom_line(aes(x=a, y=b))
	
	processed_data$stan_data$resnonres_constraints_on_alpha_beta <- c(alpha.upper, c1, c2)
	
	processed_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom dplyr mutate
stan_data_add_week_effects <- function(processed_data, plotdir=NA_character_)
{
  states <- names(processed_data$dates)	
  
  num_days_sim <- processed_data$stan_data$N2

  week_index_allstates = data.table( date = seq(min(as.Date(unlist(lapply(sapply(processed_data$dates, as.character), min)))), 
              max(as.Date(unlist(lapply(sapply(processed_data$dates, as.character), min)))) +num_days_sim, by = "day")) %>%
    dplyr::mutate(week.index =  lubridate::week(date) - min(lubridate::week(date)) + 1 )
  
  week_index = matrix(nrow = length(states), ncol = num_days_sim)
  W = max(week_index_allstates$week.index)
  
  for(m in 1:length(states)){
    min_date = min(processed_data$dates[[m]])
    
    tmp = data.table(date = seq(min_date, min_date+num_days_sim-1, by = "day")) %>%
      merge(week_index_allstates, by = "date" )
    
    week_index[m,] = tmp$week.index
  }

  processed_data$stan_data = c(processed_data$stan_data, 
                               list(
                                 W = W,
                                 week_index = week_index
                               ))
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom stats filter
make_decoupled_mobility_trends_into_2_parts_fsq <- function(mobility_data, pop_info)
{	
	ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average		
	
	# find breakpoint
	tmp <- unique(subset(mobility_data, select=c(age.cat,fsq.age.cat)))
	tmp[, w:= 1]
	set(tmp, tmp[, which(age.cat<4)], 'w', 0)
	set(tmp, tmp[, which(age.cat==4)], 'w', 2/5)
	tmp <- merge(pop_info, tmp, by='age.cat')	
	tmp <- tmp[, list(pop=sum(w*pop)), by=c('loc','fsq.age.cat')]
	tmp <- tmp[, list(fsq.age.cat=fsq.age.cat, w= pop/sum(pop)), by='loc']	
	mm <- unique(subset(mobility_data, select=-c(age.cat, age.cat.label)))
	mm <- merge(mm, tmp, by=c('loc','fsq.age.cat'))	
	mm_avg <- mm[, list(mavg=sum(w*mobility_trend)), by=c('loc','date','weekend')]	
	mm_avg <- mm_avg[, list(rebound_date= 1+date[which.min(ma(mavg))]), by='loc']	
	mm <- merge(mm, mm_avg, by='loc')
	
	# determine eased mobility value on weekdays and weekends
	mm[, is_after_rebound_date:= date>=rebound_date]		
	tmp <- mm[date<rebound_date, list(base_mobility_trend= mean(tail(mobility_trend,ifelse(weekend=='yes',4,5)))), by=c('loc','fsq.age.cat.label','weekend')]	
	mm <- merge(mm, tmp, by=c('loc','fsq.age.cat.label','weekend'))
	mm <- mm[order(loc, fsq.age.cat.label, date),]
	
	# decouple mobility trends before and after breakpoint on the multiplicative scale
	# so that MOB = exp( log(EASED_MOB) + log(MULTIPLIER) )
	mm[, eased_mobility_trend:= mobility_trend]
	tmp <- which(mm[,is_after_rebound_date])
	set(mm, tmp, 'eased_mobility_trend', mm[tmp, base_mobility_trend])
	mm[, mobility_multiplier:= 1.]
	set(mm, tmp, 'mobility_multiplier', mm[tmp, mobility_trend/base_mobility_trend])
	
	# clean up
	mm <- subset(mm, select=-c(is_after_rebound_date, w))
	tmp <- unique(subset(mobility_data, select=c(age.cat, age.cat.label, fsq.age.cat)))
	
	# scale up to model strata
	mm <- merge(mm, tmp, by='fsq.age.cat', allow.cartesian=TRUE)
	mm
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom stats filter
make_decoupled_mobility_trends_into_3_parts_fsq <- function(mobility_data, pop_info)
{	
	ma5 <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average		
	ma15 <- function(x, n = 15){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average
	
	# get average mobility trend
	tmp <- unique(subset(mobility_data, select=c(age.cat,fsq.age.cat)))
	tmp[, w:= 1]
	set(tmp, tmp[, which(age.cat<4)], 'w', 0)
	set(tmp, tmp[, which(age.cat==4)], 'w', 2/5)
	tmp <- merge(pop_info, tmp, by='age.cat')	
	tmp <- tmp[, list(pop=sum(w*pop)), by=c('loc','fsq.age.cat')]
	tmp <- tmp[, list(fsq.age.cat=fsq.age.cat, w= pop/sum(pop)), by='loc']	
	mm <- unique(subset(mobility_data, select=-c(age.cat, age.cat.label)))
	mm <- merge(mm, tmp, by=c('loc','fsq.age.cat'))	
	mm_avg <- mm[, list(mavg=sum(w*mobility_trend)), by=c('loc','date','weekend')]	
	tmp <- mm_avg[, list(	date=date, 
							ma_avg_5= ma5(mavg),
							ma_avg_15= ma15(mavg)), 
							by='loc']
	mm_avg <- merge(mm_avg, tmp, by=c('loc','date'))
	
	
	# find breakpoint: rebound time		
	mm_rb_5 <- mm_avg[!is.na(ma_avg_5), list(rebound_date_5= 1+date[which.min(ma_avg_5)]), by='loc']
	mm_rb_15 <- mm_avg[!is.na(ma_avg_15), list(rebound_date_15= 1+date[which.min(ma_avg_15)]), by='loc']
	
	# find breakpoint: dip time
	mm_dip <- mm_avg[!is.na(ma_avg_15), list(dip_date= date[ 15-1+min(which( ma_avg_15[15:length(ma_avg_15)] /  ma_avg_15[1:(length(ma_avg_15)-14)]< 0.9))]), by='loc']

	# plotting to assess
	# ma15 seems reasonable too
	# dip time early for VT, but time series IS decreasing
	if(0)
	{
		mm_avg <- merge(mm_avg, mm_rb_5, by='loc')
		mm_avg <- merge(mm_avg, mm_rb_15, by='loc')
		mm_avg <- merge(mm_avg, mm_dip, by='loc')
		p <- ggplot(mm_avg) + 
				geom_step(aes(x=date, y=mavg)) +
				geom_line(aes(x=date, y=ma_avg_5), colour='red') +
				geom_line(aes(x=date, y=ma_avg_15), colour='blue') +
				geom_vline(aes(xintercept=dip_date), colour='blue') +
				geom_vline(aes(xintercept=rebound_date_5), colour='red') +
				geom_vline(aes(xintercept=rebound_date_15), colour='blue') +
				facet_wrap(loc~., ncol=1)
		file <- file.path(args$job_dir,'fsq_ma.pdf')
		ggsave(p, file=file, w=10, h=80, limitsize=FALSE)		
	}
	
	# add dip and rebound times
	setnames( mm_rb_15, 'rebound_date_15','rebound_date')
	mm <- merge(mm, mm_rb_15, by='loc')
	mm <- merge(mm, mm_dip, by='loc')
			
	# determine eased mobility value on weekdays and weekends
	mm[, is_after_rebound_date:= date>=rebound_date]		
	mm[, is_after_dip_date:= date>=dip_date]
	mm <- mm[order(loc, fsq.age.cat.label, date),]
	tmp <- mm[date>dip_date & date<rebound_date, list(base_dip_mobility_trend= mean(tail(mobility_trend,ifelse(weekend=='yes',4,5)))), by=c('loc','fsq.age.cat.label','weekend')]	
	mm <- merge(mm, tmp, by=c('loc','fsq.age.cat.label','weekend'))
	mm <- mm[order(loc, fsq.age.cat.label, date),]
	
	# decouple mobility trends before and after breakpoint on the multiplicative scale
	# so that MOB = exp( log(BASE_MOB) + log(EASED_MOB) + log(MULTIPLIER) )
	
	# define baseline mobility trend
	# set covariate for baseline to 1 after dip
	mm[, baseline_mobility_trend:= mobility_trend]	
	tmp <- which(mm[,is_after_dip_date])
	set(mm,tmp,'baseline_mobility_trend',1)
	
	# define eased mobility
	# set covariate for eased baseline to 1 before to dip
	mm[, eased_mobility_trend:= mobility_trend]
	tmp <- which(mm[,is_after_rebound_date])
	set(mm, tmp, 'eased_mobility_trend', mm[tmp, base_dip_mobility_trend])	
	tmp <- which(mm[,!is_after_dip_date])
	set(mm,tmp,'eased_mobility_trend',1)
	
	# define upswing multiplier
	mm[, mobility_multiplier:= 1.]
	tmp <- which(mm[,is_after_rebound_date])
	set(mm, tmp, 'mobility_multiplier', mm[tmp, mobility_trend/base_dip_mobility_trend])
	
	# clean up
	mm <- subset(mm, select=-c(is_after_rebound_date, is_after_dip_date,w))
	tmp <- unique(subset(mobility_data, select=c(age.cat, age.cat.label, fsq.age.cat)))
	
	# scale up to model strata
	mm <- merge(mm, tmp, by='fsq.age.cat', allow.cartesian=TRUE)
	mm
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom stats filter
make_decoupled_mobility_trends_into_3_parts_emodo <- function(mobility_data, pop_info)
{	
  ma5 <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average		
  ma15 <- function(x, n = 15){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average
  
  # get average mobility trend
  tmp <- unique(subset(mobility_data, select=c(age.cat,emo.age.cat)))
  tmp[, w:= 1]
  set(tmp, tmp[, which(age.cat<4)], 'w', 0)
  set(tmp, tmp[, which(age.cat==4)], 'w', 2/5)
  tmp <- merge(pop_info, tmp, by='age.cat')	
  tmp <- tmp[, list(pop=sum(w*pop)), by=c('loc','emo.age.cat')]
  tmp <- tmp[, list(emo.age.cat=emo.age.cat, w= pop/sum(pop)), by='loc']	
  mm <- unique(subset(mobility_data, select=-c(age.cat, age.cat.label)))
  mm <- merge(mm, tmp, by=c('loc','emo.age.cat'))	
  mm_avg <- mm[, list(mavg=sum(w*mobility_trend)), by=c('loc','date','weekend')]	
  tmp <- mm_avg[, list(	date=date, 
                        ma_avg_5= ma5(mavg),
                        ma_avg_15= ma15(mavg)), 
                by='loc']
  mm_avg <- merge(mm_avg, tmp, by=c('loc','date'))
  
  
  # find breakpoint: rebound time		
  mm_rb_5 <- mm_avg[!is.na(ma_avg_5), list(rebound_date_5= 1+date[which.min(ma_avg_5)]), by='loc']
  mm_rb_15 <- mm_avg[!is.na(ma_avg_15), list(rebound_date_15= 1+date[which.min(ma_avg_15)]), by='loc']
  
  # find breakpoint: dip time
  mm_dip <- mm_avg[!is.na(ma_avg_15), list(dip_date= date[ 15-1+min(which( ma_avg_15[15:length(ma_avg_15)] /  ma_avg_15[1:(length(ma_avg_15)-14)]< 0.9))]), by='loc']
  
  # plotting to assess
  # ma15 seems reasonable too
  # dip time early for VT, but time series IS decreasing
  if(0)
  {
    mm_avg <- merge(mm_avg, mm_rb_5, by='loc')
    mm_avg <- merge(mm_avg, mm_rb_15, by='loc')
    mm_avg <- merge(mm_avg, mm_dip, by='loc')
    p <- ggplot(mm_avg) + 
      geom_step(aes(x=date, y=mavg)) +
      geom_line(aes(x=date, y=ma_avg_5), colour='red') +
      geom_line(aes(x=date, y=ma_avg_15), colour='blue') +
      geom_vline(aes(xintercept=dip_date), colour='blue') +
      geom_vline(aes(xintercept=rebound_date_5), colour='red') +
      geom_vline(aes(xintercept=rebound_date_15), colour='blue') +
      facet_wrap(loc~., ncol=1)
    file <- file.path(args$job_dir,'emodo_ma.pdf')
    ggsave(p, file=file, w=10, h=80, limitsize=FALSE)		
  }
  
  # add dip and rebound times
  setnames( mm_rb_15, 'rebound_date_15','rebound_date')
  mm <- merge(mm, mm_rb_15, by='loc')
  mm <- merge(mm, mm_dip, by='loc')
  
  # determine eased mobility value on weekdays and weekends
  mm[, is_after_rebound_date:= date>=rebound_date]		
  mm[, is_after_dip_date:= date>=dip_date]
  mm <- mm[order(loc, emo.age.label, date),]
  tmp <- mm[date>dip_date & date<rebound_date, list(base_dip_mobility_trend= mean(tail(mobility_trend,ifelse(weekend=='yes',4,5)))), by=c('loc','emo.age.label','weekend')]	
  mm <- merge(mm, tmp, by=c('loc','emo.age.label','weekend'))
  mm <- mm[order(loc, emo.age.label, date),]
  
  # decouple mobility trends before and after breakpoint on the multiplicative scale
  # so that MOB = exp( log(BASE_MOB) + log(EASED_MOB) + log(MULTIPLIER) )
  
  # define baseline mobility trend
  # set covariate for baseline to 1 after dip
  mm[, baseline_mobility_trend:= mobility_trend]	
  tmp <- which(mm[,is_after_dip_date])
  set(mm,tmp,'baseline_mobility_trend',1)
  
  # define eased mobility
  # set covariate for eased baseline to 1 before to dip
  mm[, eased_mobility_trend:= mobility_trend]
  tmp <- which(mm[,is_after_rebound_date])
  set(mm, tmp, 'eased_mobility_trend', mm[tmp, base_dip_mobility_trend])	
  tmp <- which(mm[,!is_after_dip_date])
  set(mm,tmp,'eased_mobility_trend',1)
  
  # define upswing multiplier
  mm[, mobility_multiplier:= 1.]
  tmp <- which(mm[,is_after_rebound_date])
  set(mm, tmp, 'mobility_multiplier', mm[tmp, mobility_trend/base_dip_mobility_trend])
  
  # clean up
  mm <- subset(mm, select=-c(is_after_rebound_date, is_after_dip_date,w))
  tmp <- unique(subset(mobility_data, select=c(age.cat, age.cat.label, emo.age.cat)))
  
  # scale up to model strata
  mm <- merge(mm, tmp, by='emo.age.cat', allow.cartesian=TRUE)
  mm
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_mobility_trends_fsq <- function(processed_data, mobility_data, log=TRUE)
{	
	states <- names(processed_data$dates)  
	num_days_sim <- processed_data$stan_data$N2    	 
	covariates_new <- array(NA_real_, dim = c(length(states), num_days_sim, as.integer(processed_data$stan_data$A)))	#	predictors for contacts by age band
	
	for(m in 1:length(states))
	{   
		fsq_m <- subset(mobility_data, loc==states[m], c(age.cat, date, weekend, mobility_trend))
		fsq_m[, weekend:= as.integer(weekend=='yes')]
		
		if(log)
		{
			cat("\nTaking log of mobility trends...")
			set(fsq_m, NULL, 'mobility_trend', fsq_m[, log(mobility_trend)])
		}
		if(!log)
		{
			cat("\nNo log of mobility trends...")
		}
		
		first.obs.date <- min(processed_data$dates[[states[m]]])
		last.obs.date <- max(fsq_m$date)
		last.date <- first.obs.date + num_days_sim - 1L
		
		fsq_m <- subset(fsq_m, date>=first.obs.date)
		
		# for forcasted data, repeat the last mobility index by weekend and weekday
		if(last.date>last.obs.date)
		{
			cat("\nFor",states[m],": padding mobility trends from ",as.character(last.obs.date+1), " until ", as.character(last.date) )
			#	determine padding days
			fsq_pad <- data.table(date= last.obs.date + seq(1, as.numeric(last.date-last.obs.date)) )
			fsq_pad[, weekend:= as.integer(as.integer(format(date, "%u"))>5)]
			
			#	determine padding values
			tmp <- unique(subset(fsq_m, date<=last.obs.date, select=c(date,weekend)))
			tmp <- tmp[, list(date= tail(sort(date), ifelse(weekend==1,4,5))), by='weekend']
			tmp <- merge(fsq_m, tmp, by=c('weekend','date'))
			tmp <- tmp[, list(mobility_trend=mean(mobility_trend)), by=c('weekend','age.cat')]
			
			#	pad
			fsq_pad <- merge(fsq_pad, tmp, by='weekend', allow.cartesian=TRUE)			
			fsq_m <- rbind(fsq_m, fsq_pad)			
		}
		if(last.date<=last.obs.date)
		{
			cat("\nFor",states[m],": found mobility trends until last date ", as.character(last.date) )
			fsq_m <- subset(fsq_m, date<=last.date)
		}				
		
		#	add to stan_data
		fsq_m <- dcast.data.table(fsq_m, date ~ age.cat, value.var='mobility_trend')
		tmp <- as.matrix( subset(fsq_m, select= -c(date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,,] <- tmp		
	}
				
	stopifnot( !any(is.na(covariates_new)) )
	processed_data$stan_data$covariates <- NULL
	processed_data$stan_data$covariates <- covariates_new
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_decoupled_mobility_trends_decline_plus_upswing_fsq <- function(processed_data, mobility_data, log=TRUE)
{
	states <- names(processed_data$dates)	
	num_days_sim <- processed_data$stan_data$N2
	covariates_new <- array(NA_real_, dim = c( length(states), 2, num_days_sim,  as.integer(processed_data$stan_data$A)) )		
	
	for(m in 1:length(states))
	{				
		fsq_m <- subset(mobility_data, loc==states[m], c(age.cat, date, weekend, eased_mobility_trend, mobility_multiplier))
		fsq_m[, weekend:= as.integer(weekend=='yes')]	
		fsq_m <- reshape2::melt(fsq_m, measure.vars=c('eased_mobility_trend','mobility_multiplier'))
		
		if(log)
		{
			cat("\nTaking log of mobility trends...")
			set(fsq_m, NULL, 'value', fsq_m[, log(value)])
		}
		if(!log)
		{
			cat("\nNo log of mobility trends...")
		}
		
		first.obs.date <- min(processed_data$dates[[states[m]]])
		last.obs.date <- max(fsq_m$date)
		last.date <- first.obs.date + num_days_sim - 1L
		
		fsq_m <- subset(fsq_m, date>=first.obs.date)
		
		# for forcasted data, repeat the last mobility index by weekend and weekday
		if(last.date>last.obs.date)
		{
			cat("\nFor",states[m],": padding mobility trends from ",as.character(last.obs.date+1), " until ", as.character(last.date) )
			#	determine padding days
			fsq_pad <- data.table(date= last.obs.date + seq(1, as.numeric(last.date-last.obs.date)) )
			fsq_pad[, weekend:= as.integer(as.integer(format(date, "%u"))>5)]
			
			#	determine padding values
			tmp <- unique(subset(fsq_m, date<=last.obs.date, select=c(date,weekend)))
			tmp <- tmp[, list(date= tail(sort(date), ifelse(weekend==1,4,5))), by='weekend']
			tmp <- merge(fsq_m, tmp, by=c('weekend','date'))
			tmp <- tmp[, list(value=mean(value)), by=c('weekend','age.cat','variable')]
			
			#	pad
			fsq_pad <- merge(fsq_pad, tmp, by='weekend', allow.cartesian=TRUE)			
			fsq_m <- rbind(fsq_m, fsq_pad)			
		}
		if(last.date<=last.obs.date)
		{
			cat("\nFor",states[m],": found mobility trends until last date ", as.character(last.date) )
			fsq_m <- subset(fsq_m, date<=last.date)
		}				
		
		#	add to stan_data
		fsq_m <- dcast.data.table(fsq_m, variable+date ~ age.cat, value.var='value')
		tmp <- as.matrix( subset(fsq_m, variable=='eased_mobility_trend', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,1,,] <- tmp
		tmp <- as.matrix( subset(fsq_m, variable=='mobility_multiplier', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,2,,] <- tmp	
	}
	
	stopifnot( !any(is.na(covariates_new)) )
	processed_data$stan_data$covariates <- NULL
	processed_data$stan_data$covariates <- covariates_new
	processed_data$stan_data$COVARIATES_N <- 2
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_school_cases <- function(processed_data, upper.bound.multiplier= 10,file_school_cases)
{		
  # data
  last_date = as.Date(max(sapply(processed_data$dates, function(x) as.character(max(x)))))
  school.cases <- data.table(read.csv(file_school_cases))
  school.cases = school.cases[,list(number_students=sum(number_students,na.rm = T),
                                                                cumulative_students=sum(cumulative_students,na.rm = T)),
                                                          by=c('loc','start','date')]
  school.cases[,cases_att_rate:=cumulative_students/number_students]
  school.cases = school.cases[, date := as.Date(date)]
  school.cases = school.cases[as.Date(date)<=last_date,]
  school.cases = school.cases[, .SD[which.max(date)], by='loc']
  setnames(school.cases, c('start','date'),c('date_start','date_end'))
	# #	data
	# school.cases <- data.table(
	# 	loc= c('FL', 'TX'),	
	# 	date_start= c('2020-09-06', '2020-08-24'),
	# 	date_end= c('2020-10-24', '2020-10-25'),
	# 	cases_att_rate= c(0.0025, 0.0034) 
	# 	)
	set(school.cases, NULL, 'date_start', school.cases[, as.Date(date_start)])
	set(school.cases, NULL, 'date_end', school.cases[, as.Date(date_end)])
	
	#	translate dates into time indices
	dates <- data.table(
		loc= names(processed_data$dates),
		date_min= as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) )),
		date_max= as.Date(sapply( processed_data$dates, function(x) max(as.character(x)) ))
		)
	school.cases <- merge(school.cases, dates, by='loc')
	school.cases[, time_start:= as.integer(date_start-date_min+1L)]
	school.cases[, time_end:= as.integer(date_end-date_min+1L)]
	
	#	checks
	stopifnot( nrow(school.cases)>0 )
	stopifnot( all(school.cases[, time_start] > 0) ) 
	stopifnot( all(school.cases[, time_end] < processed_data$stan_data$N2) )
	
	states <- names(processed_data$dates)
	
	#	make school_case_time_idx
	school_case_time_idx <- array(-1, dim = c( length(states), 2L) )
	for(m in 1:length(states))
	{
		tmp <- subset(school.cases, loc==states[m])
		if(nrow(tmp))
		{
			school_case_time_idx[m,] <- tmp[,c(time_start, time_end)]
		}		
	}
		
	#	make school_case_data
	school_case_data <- array(-1, dim = c( length(states), 4L) )
	for(m in 1:length(states))
	{
		tmp <- subset(school.cases, loc==states[m])
		if(nrow(tmp))
		{
			school_case_data[m,] <- tmp[, c(cases_att_rate, cases_att_rate/10, cases_att_rate*upper.bound.multiplier, (cases_att_rate*upper.bound.multiplier)/10)]			
		}		
	}
	
	#	add to stan_data
	processed_data$stan_data$school_case_time_idx <- school_case_time_idx
	processed_data$stan_data$school_case_data <- school_case_data	
	processed_data
}
	
#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_smoothed_logcases <- function(processed_data, smoothed_log_cases)
{
	
	states <- names(processed_data$dates)
	
	#	define date range across states
	date.min <- as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) ))
	dates <- data.table(
		loc= names(processed_data$dates),
		date_min= as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) )),
		date_max= as.Date(sapply( processed_data$dates, function(x) max(as.character(x)) ))
		)
	dates <- dates[, 
		list(	
			date= seq(date_min, date_max, 1),
			time_idx= seq_along(seq(date_min, date_max, 1))
		),
		by='loc']
	
	#	define calendar weeks
	dates[, week:=as.integer(strftime(date, format = "%V"))]
	tmp <- data.table(loc=states, epidemicStart=processed_data$stan_data$epidemicStart)
	dates <- merge(dates, tmp, by='loc')	
	
	#	subset to complete weeks
	tmp <- dates[, list(days_n= length(date)), by=c('loc','week')]
	dates <- merge(dates, subset(tmp, days_n==7), by=c('loc','week'))
	
	#	reset so weeks start at 1 in each location
	tmp <- dates[time_idx>=epidemicStart, list(week=unique(week), week_idx= unique(week)-min(week)+1L), by='loc']
	dates <- merge(dates, tmp, by=c('loc','week'))
		
	#	make vector of number of week indices per location
	smoothed_logcases_weeks_n <- vector('integer', length(states))
	for(m in 1:length(states))
	{
		tmp <- subset(dates, loc==states[m])
		smoothed_logcases_weeks_n[m] <- max(tmp$week_idx)
	} 
	
	smoothed_logcases_weeks_n_max <- max(smoothed_logcases_weeks_n)
		
	#	make matrix of week map	
	dates[, day:=as.integer(strftime(date, format = "%u"))]
	smoothed_logcases_week_map	<- array(-1, dim = c( length(states), smoothed_logcases_weeks_n_max, 7L) )
	for(m in 1:length(states))
	{
		tmp <- subset(dates, loc==states[m])
		tmp <- dcast.data.table(tmp, week_idx~day, value.var='time_idx')		
		smoothed_logcases_week_map[m, 1:nrow(tmp),] <- unname(as.matrix(tmp)[,-1])				
	}
		
	#	make array of t-distribution parameters state x time x 3 pars for likelihood of observed data
	setnames(dates, 'loc', 'code')
	smoothed_logcases_week_pars <- array(-1, dim = c( length(states), smoothed_logcases_weeks_n_max, 3L) )
	for(m in 1:length(states))
	{
		tmp <- subset(smoothed_log_cases, code==states[m])		
		tmp <- merge(tmp, unique(subset(dates, select=c(code,week))), by=c('code','week'))
		smoothed_logcases_week_pars[m, 1:nrow(tmp), ] <- unname(as.matrix(subset(tmp, select=c(lmean, lsd, tdf))))
	}
	
	#	check
	tmp <- sapply(1:length(states), function(m) max(which(smoothed_logcases_week_pars[m,,1]!= -1L)))
	stopifnot(all(tmp==smoothed_logcases_weeks_n))
	tmp <- sapply(1:length(states), function(m) max(which(smoothed_logcases_week_map[m,,1]!= -1L)))
	stopifnot(all(tmp==smoothed_logcases_weeks_n))
	
	#	add to stan_data
	processed_data$stan_data$smoothed_logcases_weeks_n_max <- smoothed_logcases_weeks_n_max
	processed_data$stan_data$smoothed_logcases_weeks_n <- smoothed_logcases_weeks_n
	processed_data$stan_data$smoothed_logcases_week_map <- smoothed_logcases_week_map
	processed_data$stan_data$smoothed_logcases_week_pars <- smoothed_logcases_week_pars
	
	processed_data
}

	
#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_seroprevalence_data = function(processed_data, spd)
{  
	states <- names(processed_data$dates)
  	pop <- processed_data$stan_data$pop
    
  	#	define date range across states
  	dates <- data.table(
    	loc= names(processed_data$dates),
    	date_min= as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) )),
    	date_max= as.Date(sapply( processed_data$dates, function(x) max(as.character(x)) ))
  		)
  	dates <- dates[, 
		list(	
			date= seq(date_min, date_max, 1),
			time_idx= seq_along(seq(date_min, date_max, 1))
		),
		by='loc']
  
  	# merge to seroprevalence study data
  	spds <- merge(dates, spd, by = c("loc", "date"))
  
  	# keep only overall count
  	spds <- subset(spds, age_band == "overall")
  
	# if sample size not known, add 1e3
	if(!'sample_size'%in%colnames(spds))
		spds[, sample_size:= 1e3]
	
	# transform prevalence estimates to prevalence numbers in sample
	set(spds, NULL, 'M_obs', spds[, M_obs*sample_size])
	set(spds, NULL, 'CL_obs', spds[, CL_obs*sample_size])
	set(spds, NULL, 'CU_obs', spds[, CU_obs*sample_size])
	
	# add number of sero-prevalence estimates of a loc
	spds <- merge(data.table(loc=states), spds, all.x=TRUE, by='loc')
	tmp <- spds[, list(
			N_surveys=length(which(!is.na(date))),
			N_surveys_g18=length(which(!is.na(date) & X18plus==1))
		), by='loc']
	spds <- merge(spds, tmp, by = "loc")
	  	
  	# create number of surveys per state vector 
	seroprevalence_surveys_n <- unique(subset(spds, select=c(loc, N_surveys)))
	seroprevalence_surveys_n <- merge(data.table(loc=states, loc_id=seq_along(states)), seroprevalence_surveys_n, by='loc', all.x=TRUE)
	seroprevalence_surveys_n <- seroprevalence_surveys_n[order(loc_id), N_surveys]
	
	# create date map
  	seroprevalence_date_map <- matrix(nrow = length(states), ncol = max(spds$N_surveys, na.rm=TRUE), -1)
	for(m in seq_along(states))
	{
		if(seroprevalence_surveys_n[m] > 0)
		{
			tmp <- subset(spds, loc == states[m])[, sort(time_idx)]
			seroprevalence_date_map[m,1:seroprevalence_surveys_n[m]] <- tmp			
		}		
	}
	
	# create data 
	seroprevalence_data <- array(dim = c(length(states),max(spds$N_surveys), 3), -1)
	for(m in seq_along(states))
	{
		if(seroprevalence_surveys_n[m] > 0)
		{
			tmp <- subset(spds, loc == states[m])
			seroprevalence_data[m,1:seroprevalence_surveys_n[m],1] <- tmp$CL_obs
			seroprevalence_data[m,1:seroprevalence_surveys_n[m],2] <- tmp$CU_obs
			seroprevalence_data[m,1:seroprevalence_surveys_n[m],3] <- tmp$sample_size				
		}		
	}
	
	# create number of 18+ surveys per state vector 
	seroprevalence_g18_surveys_n <- unique(subset(spds, select=c(loc, N_surveys_g18)))
	seroprevalence_g18_surveys_n <- merge(data.table(loc=states, loc_id=seq_along(states)), seroprevalence_g18_surveys_n, by='loc', all.x=TRUE)
	seroprevalence_g18_surveys_n <- seroprevalence_g18_surveys_n[order(loc_id), N_surveys_g18]
	
	# create date map of 18+ surveys
	seroprevalence_g18_date_map <- matrix(nrow = length(states), ncol = max(spds$N_surveys_g18, na.rm=TRUE), -1)
	for(m in seq_along(states))
	{
		if(seroprevalence_g18_surveys_n[m] > 0)
		{
			tmp <- subset(spds, loc == states[m] & X18plus==1)[, sort(time_idx)]
			seroprevalence_g18_date_map[m,1:seroprevalence_g18_surveys_n[m]] <- tmp			
		}		
	}
	
	# create index map of 18+ surveys
	seroprevalence_g18_index_map <- matrix(nrow = length(states), ncol = max(spds$N_surveys_g18, na.rm=TRUE), -1)
	for(m in seq_along(states))
	{
		if(seroprevalence_g18_surveys_n[m] > 0)
		{
			tmp <- subset(spds, loc == states[m])[, which(X18plus==1)]
			seroprevalence_g18_index_map[m,1:seroprevalence_g18_surveys_n[m]] <- tmp			
		}		
	}
		
	# register new data objects
	processed_data$stan_data$seroprevalence_surveys_n_max <- max(seroprevalence_surveys_n)
  	processed_data$stan_data$seroprevalence_surveys_n <- seroprevalence_surveys_n  	
  	processed_data$stan_data$seroprevalence_date_map <- seroprevalence_date_map
  	processed_data$stan_data$seroprevalence_data <- seroprevalence_data  
	processed_data$stan_data$seroprevalence_g18_surveys_n_max <- max(seroprevalence_g18_surveys_n)
	processed_data$stan_data$seroprevalence_g18_surveys_n <- seroprevalence_g18_surveys_n  	
	processed_data$stan_data$seroprevalence_g18_date_map <- seroprevalence_g18_date_map
	processed_data$stan_data$seroprevalence_g18_index_map <- seroprevalence_g18_index_map
	
  	return(processed_data)
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_upswing_time_effect <- function(processed_data, mobility_data, effect_weeks=2)
{
	stopifnot('rebound_date'%in%colnames(mobility_data))
	
	#	define date range across states
	date.min <- range( as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) )) )
	dates <- data.table(date= seq(date.min[1], date.min[2]+processed_data$stan_data$N2, 1))
	
	#	define calendar weeks
	dates[, week:=as.integer(strftime(date, format = "%V"))]
	
	#	define first week for time effect (is always after first rebound date)
	tmp <- dates[date==min(mobility_data$rebound_date), week]
	dates[, time_effect_id:= pmax(1, week-tmp+1L)]
	
	#	define last week with a time effect (last week before forecasts start)
	date.max.non.forecast <- max( as.Date(sapply( processed_data$dates, function(x) max(as.character(x)) )) )
	tmp <- dates[date==date.max.non.forecast, week]
	set(dates, NULL, 'week', pmin(tmp, dates[, week]))
	
	#	make effect weeks 
	set(dates, NULL, 'time_effect_id', dates[, ceiling(time_effect_id/effect_weeks)])
	
	#subset(dates, date<=date.max.non.forecast)
	#	remove last time effect that is not on full effect weeks
	tmp <- dates[date<=date.max.non.forecast, which(time_effect_id==max(time_effect_id))]
	if(length(tmp)<effect_weeks*7)
	{
		tmp <- dates[date==date.max.non.forecast, time_effect_id-1L]		
		set(dates, NULL, 'time_effect_id', pmin(tmp, dates[,time_effect_id]))
	}
	
	#	remove last time effect due to limited signal
	if(effect_weeks<3)
	{
		tmp <- max(dates$time_effect_id)		
		set(dates, NULL, 'time_effect_id', pmin(tmp-1L, dates[,time_effect_id]))		
	}
	
	#	set total number of time effects
	processed_data$stan_data$N_IMP <- max(dates$time_effect_id)
	cat('\nAdding time effects, n=',processed_data$stan_data$N_IMP)

	#	map time effects to dates for each state
	tmp <- as.Date(sapply( processed_data$dates, function(x) min(as.character(x)) ))
	upswing_timeeff_map <- data.table(date= tmp, loc= seq_along(processed_data$dates))
	upswing_timeeff_map <- upswing_timeeff_map[, 
		list(
			date=date+1:processed_data$stan_data$N2,
			time=1:processed_data$stan_data$N2
			), 
		by='loc']	
	upswing_timeeff_map <- merge(upswing_timeeff_map, dates, by='date')
	upswing_timeeff_map <- dcast.data.table(upswing_timeeff_map, time~loc, value.var='time_effect_id')
	processed_data$stan_data$upswing_timeeff_map <- unname(as.matrix(subset(upswing_timeeff_map, select=-time)))

	processed_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_decoupled_mobility_trends_3_parts_fsq <- function(processed_data, mobility_data, log=TRUE)
{
	states <- names(processed_data$dates)	
	num_days_sim <- processed_data$stan_data$N2
	covariates_new <- array(NA_real_, dim = c( length(states), 3, num_days_sim,  as.integer(processed_data$stan_data$A)) )		
	
	for(m in 1:length(states))
	{				
		fsq_m <- subset(mobility_data, loc==states[m], c(age.cat, date, weekend, baseline_mobility_trend, eased_mobility_trend, mobility_multiplier))
		fsq_m[, weekend:= as.integer(weekend=='yes')]	
		fsq_m <- reshape2::melt(fsq_m, measure.vars=c('baseline_mobility_trend','eased_mobility_trend','mobility_multiplier'))
		
		if(log)
		{
			cat("\nTaking log of mobility trends...")
			set(fsq_m, NULL, 'value', fsq_m[, log(value)])
		}
		if(!log)
		{
			cat("\nNo log of mobility trends...")
		}
		
		first.obs.date <- min(processed_data$dates[[states[m]]])
		last.obs.date <- max(fsq_m$date)
		last.date <- first.obs.date + num_days_sim - 1L
		
		fsq_m <- subset(fsq_m, date>=first.obs.date)
		
		# for forcasted data, repeat the last mobility index by weekend and weekday
		if(last.date>last.obs.date)
		{
			cat("\nFor",states[m],": padding mobility trends from ",as.character(last.obs.date+1), " until ", as.character(last.date) )
			#	determine padding days
			fsq_pad <- data.table(date= last.obs.date + seq(1, as.numeric(last.date-last.obs.date)) )
			fsq_pad[, weekend:= as.integer(as.integer(format(date, "%u"))>5)]
			
			#	determine padding values
			tmp <- unique(subset(fsq_m, date<=last.obs.date, select=c(date,weekend)))
			tmp <- tmp[, list(date= tail(sort(date), ifelse(weekend==1,4,5))), by='weekend']
			tmp <- merge(fsq_m, tmp, by=c('weekend','date'))
			tmp <- tmp[, list(value=mean(value)), by=c('weekend','age.cat','variable')]
			
			#	pad
			fsq_pad <- merge(fsq_pad, tmp, by='weekend', allow.cartesian=TRUE)			
			fsq_m <- rbind(fsq_m, fsq_pad)			
		}
		if(last.date<=last.obs.date)
		{
			cat("\nFor",states[m],": found mobility trends until last date ", as.character(last.date) )
			fsq_m <- subset(fsq_m, date<=last.date)
		}				
		
		#	add to stan_data
		fsq_m <- dcast.data.table(fsq_m, variable+date ~ age.cat, value.var='value')
		tmp <- as.matrix( subset(fsq_m, variable=='baseline_mobility_trend', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,1,,] <- tmp		
		tmp <- as.matrix( subset(fsq_m, variable=='eased_mobility_trend', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,2,,] <- tmp
		tmp <- as.matrix( subset(fsq_m, variable=='mobility_multiplier', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,3,,] <- tmp	
	}
	
	stopifnot( !any(is.na(covariates_new)) )
	processed_data$stan_data$covariates <- NULL
	processed_data$stan_data$covariates <- covariates_new
	processed_data$stan_data$COVARIATES_N <- 3
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_indicator_rebound_date <- function(processed_data, mobility_data)
{
  states <- names(processed_data$dates)	
  num_days_sim <- processed_data$stan_data$N2
  REBOUND_TIME_INDEX = vector(mode = "integer", length = processed_data$stan_data$M)
  
  for(m in 1:length(states))
  {				
    rebound_date <- unique(subset(mobility_data, loc==states[m])$rebound_date)
    
    tmp <- data.table(date = seq.Date( min(processed_data$dates[[m]]), min(processed_data$dates[[m]])+num_days_sim-1, by = "day"))
    tmp[, before_rebound_date := as.numeric(date < rebound_date)]
    
    
    REBOUND_TIME_INDEX[m] = which(tmp$date == rebound_date)
  }
  
  processed_data$stan_data$REBOUND_TIME_INDEX <- REBOUND_TIME_INDEX
  return(processed_data)
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_decoupled_mobility_trends_decline_plus_rest_fsq <- function(processed_data, mobility_data, log=TRUE)
{
	states <- names(processed_data$dates)	
	num_days_sim <- processed_data$stan_data$N2
	covariates_new <- array(NA_real_, dim = c( length(states), 2, num_days_sim,  as.integer(processed_data$stan_data$A)) )		
	
	for(m in 1:length(states))
	{				
		fsq_m <- subset(mobility_data, loc==states[m], c(age.cat, date, weekend, baseline_mobility_trend, eased_mobility_trend, mobility_multiplier))
		fsq_m[, rest_mobility_trend:= eased_mobility_trend * mobility_multiplier]
		set(fsq_m, NULL, c('eased_mobility_trend','mobility_multiplier'), NULL)
		fsq_m[, weekend:= as.integer(weekend=='yes')]	
		fsq_m <- reshape2::melt(fsq_m, measure.vars=c('baseline_mobility_trend','rest_mobility_trend'))
		
		if(log)
		{
			cat("\nTaking log of mobility trends...")
			set(fsq_m, NULL, 'value', fsq_m[, log(value)])
		}
		if(!log)
		{
			cat("\nNo log of mobility trends...")
		}
		
		first.obs.date <- min(processed_data$dates[[states[m]]])
		last.obs.date <- max(fsq_m$date)
		last.date <- first.obs.date + num_days_sim - 1L
		
		fsq_m <- subset(fsq_m, date>=first.obs.date)
		
		# for forcasted data, repeat the last mobility index by weekend and weekday
		if(last.date>last.obs.date)
		{
			cat("\nFor",states[m],": padding mobility trends from ",as.character(last.obs.date+1), " until ", as.character(last.date) )
			#	determine padding days
			fsq_pad <- data.table(date= last.obs.date + seq(1, as.numeric(last.date-last.obs.date)) )
			fsq_pad[, weekend:= as.integer(as.integer(format(date, "%u"))>5)]
			
			#	determine padding values
			tmp <- unique(subset(fsq_m, date<=last.obs.date, select=c(date,weekend)))
			tmp <- tmp[, list(date= tail(sort(date), ifelse(weekend==1,4,5))), by='weekend']
			tmp <- merge(fsq_m, tmp, by=c('weekend','date'))
			tmp <- tmp[, list(value=mean(value)), by=c('weekend','age.cat','variable')]
			
			#	pad
			fsq_pad <- merge(fsq_pad, tmp, by='weekend', allow.cartesian=TRUE)			
			fsq_m <- rbind(fsq_m, fsq_pad)			
		}
		if(last.date<=last.obs.date)
		{
			cat("\nFor",states[m],": found mobility trends until last date ", as.character(last.date) )
			fsq_m <- subset(fsq_m, date<=last.date)
		}				
		
		#	add to stan_data
		fsq_m <- dcast.data.table(fsq_m, variable+date ~ age.cat, value.var='value')
		tmp <- as.matrix( subset(fsq_m, variable=='baseline_mobility_trend', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,1,,] <- tmp		
		tmp <- as.matrix( subset(fsq_m, variable=='rest_mobility_trend', -c(variable, date)) )
		stopifnot(nrow(tmp)==num_days_sim)
		covariates_new[m,2,,] <- tmp			
	}
	
	stopifnot( !any(is.na(covariates_new)) )
	processed_data$stan_data$covariates <- NULL
	processed_data$stan_data$covariates <- covariates_new
	processed_data$stan_data$COVARIATES_N <- 2
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom stats filter
stan_data_add_google_mobility_covariates_after_breakpoint <- function(processed_data)
{
  
  ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)} # moving average
  
  states = names(processed_data$dates)
  
  num_days_sim = processed_data$stan_data$N2
  
  with_fsq_mobility<- 0
  
  P_RES = processed_data$stan_data$P_RES
  P_NONRES = processed_data$stan_data$P_NONRES
  processed_data$stan_data$P_RES = P_RES*2
  processed_data$stan_data$P_NONRES = P_NONRES*2
    
  for(m in 1:length(states)){
  
    State = states[m]
    
    min_date = min(processed_data$dates[[m]])
    dates = seq(min_date, min_date+num_days_sim-1, by = "day")
    is.weekend = weekdays(dates) %in% c("Saturday", "Sunday")
    
    # find breakpoint    
	breakpoint = dates[!is.weekend][which.min(ma(processed_data$stan_data$covariates_nonres[[m]][!is.weekend, "avg_mob"]))]		    
    
    is.after.breakpoint = seq(min_date, min_date+num_days_sim-1, by = "day") > breakpoint
        
    covariates_res = matrix(nrow = num_days_sim, ncol = 2*P_RES, 0)
    for(j in 1:P_RES){
        covariates_res[,j] = processed_data$stan_data$covariates_res[[m]][,j]
        
        eased_mobility = covariates_res[,j] 
        covariates_res[is.after.breakpoint & is.weekend,j] = median(tail(covariates_res[!is.after.breakpoint & is.weekend,j],n=2))
        covariates_res[is.after.breakpoint & !is.weekend,j] = median(tail(covariates_res[!is.after.breakpoint & !is.weekend,j],n=5))
        covariates_res[,(P_RES+j)] = eased_mobility - covariates_res[,j]
    }
      
    covariates_nonres = matrix(nrow = num_days_sim, ncol = 2*P_NONRES, 0)
    for(j in 1:P_NONRES){
        covariates_nonres[,j] = processed_data$stan_data$covariates_nonres[[m]][,j]
        
        eased_mobility = covariates_nonres[,j] 
        covariates_nonres[is.after.breakpoint & is.weekend,j] = median(tail(covariates_nonres[!is.after.breakpoint & is.weekend,j],n=2))
        covariates_nonres[is.after.breakpoint & !is.weekend,j] = median(tail(covariates_nonres[!is.after.breakpoint & !is.weekend,j],n=5))
        covariates_nonres[,(j+P_NONRES)] = eased_mobility - covariates_nonres[,j]
    }
      
    colnames(covariates_res) = c(colnames(processed_data$stan_data$covariates_res[[m]]), paste0(colnames(processed_data$stan_data$covariates_res[[m]]), "_afterbreakpoint"))
    colnames(covariates_nonres) = c(colnames(processed_data$stan_data$covariates_nonres[[m]]), paste0(colnames(processed_data$stan_data$covariates_nonres[[m]]), "_afterbreakpoint"))
      
    processed_data$stan_data$covariates_res[[m]] = covariates_res
    processed_data$stan_data$covariates_nonres[[m]] = covariates_nonres
  }
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
process_chi_emodo_mobilty <- function(mobility_data,pop_info,infile_emodo=NULL){
  
  # read emodo data
  mob_emodo_idx <- read_emodo_cell_phone_contact_intensities(infile_emodo,type='idx')
  emo_age_cat_map_idx <- make_emodo_age_cat_map(mob_emodo_idx, pop_info)
  mob_emodo_idx <- make_mobility_trends_emo(mob_emodo_idx, emo_age_cat_map_idx)
  
  # take dates after dip date
  tmp <- unique(subset(mobility_data,select = c('loc','dip_date')))
  mob_emodo_idx <- merge(mob_emodo_idx, tmp, by='loc')
  mob_emodo_idx <- mob_emodo_idx[date > dip_date]
  
  #	read pairwise emodo data
  mob_emodo <- read_emodo_cell_phone_contact_intensities_with_cont(infile_emodo)
  emo_age_cat_map <- make_emodo_age_cat_map_with_cont(mob_emodo, pop_info)
  mob_emodo <- make_mobility_trends_emo_with_cont(mob_emodo, emo_age_cat_map)
  
  # combine 
  ans <- subset(mob_emodo,select = c('date','loc','idx.age.label','cont.age.label','mobility_trend'))
  ans <- merge(ans, unique(subset(mob_emodo_idx,select = c('emo.age.label','loc','date','mobility_trend'))),
               by.x=c('idx.age.label','loc','date'),
               by.y=c('emo.age.label','loc','date'))
  ans <- unique(ans)
  
  # estimate log chi
  m <-glm(mobility_trend.x ~ idx.age.label:cont.age.label + offset(log(mobility_trend.y)) -1,
            data = ans, family = gaussian(link = "log"))
  # make table
  log_chi <- data.table(coef(summary(m)),keep.rownames = TRUE)
  log_chi[,idx.age.label:=unlist(lapply(strsplit(rn,':'),function(x){x[1]}))]
  log_chi[,cont.age.label:=unlist(lapply(strsplit(rn,':'),function(x){x[2]}))]
  
  # rename table
  log_chi[,idx.age.label:=gsub('idx.age.label','',idx.age.label)]
  log_chi[,cont.age.label:=gsub('cont.age.label','',cont.age.label)]
  set(log_chi, NULL, c('rn','t value','Pr(>|t|)'),NULL)
  setkey(log_chi, idx.age.label, cont.age.label)
  
  # transfer to matrix
  chi <- matrix(exp(log_chi$Estimate), byrow = TRUE, 
                nrow = length(unique(log_chi$idx.age.label)),
                ncol = length(unique(log_chi$idx.age.label)))
  
  # map chi
  map_chi <- unique(subset(mob_emodo_idx,select=c('emo.age.cat','age.cat')))
  setkey(map_chi,age.cat)
  
  # save
  file <- file.path(args$indir,'usa','data','chi_emodo_mobility_trends.rds')
  cat("\nWrite to file",file)	
  saveRDS(list(log_chi=log_chi,
           chi=chi,
           map_chi=map_chi$emo.age.cat), file=file)
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
#' @importFrom dplyr mutate rename select
stan_data_add_google_mobility = function(processed_data, mobility_data, with_avg_mobility_data){
  
  # mobility_data: data table
      # google mobility data by state code, date and index
      #  with variables: code, date, retail.recreation, grocery.pharmacy, parks, transitstations, workplace, residential
  # with_avg_mobility_data: binary
     # 1 if you want to pooled retail & recreation, grocery & pharmac, workplace and park instead of using them individually
     #  this is done automatically if with_res_nonres_contactmatrix_model=1
  
  states = names(processed_data$dates)
  
  num_days_sim = processed_data$stan_data$N2
  
  processed_data$stan_data = c(processed_data$stan_data, 
                               list(
                                 covariates_nonres = vector('list', length(states)),	#	predictors for non-residential contacts
                                 covariates_res = vector('list', length(states)),		# predictors for residential contacts
                                 P_RES = NA_integer_,											 		  	  # number of predictors for residential contacts		
                                 P_NONRES = NA_integer_														  # number of predictors for non-residential contacts
                               ))
  
  for(m in 1:length(states)){
    
    State = states[m]
    
    # if with_res_nonres_contactmatrix_model == 0
    if("retail.recreation" %in% colnames(mobility_data))
    {
      tmp <- mobility_data %>% 
        dplyr::select(code, date, retail.recreation, grocery.pharmacy, parks, transitstations, workplace, residential)
      
      if(with_avg_mobility_data){
        tmp$avg_mob = apply(dplyr::select(tmp, c("retail.recreation", "grocery.pharmacy", "parks", "workplace")), 1, mean)
        tmp = dplyr::select(tmp, c("code", "date", "residential", "transitstations", "avg_mob"))
      }		
    }
    
    # if with_res_nonres_contactmatrix_model == 1
    if(!"retail.recreation" %in% colnames(mobility_data))
    {
      tmp <- mobility_data %>% 
        dplyr::select(code, date, nonresidential_M, residential_M) %>%
        dplyr::rename(nonresidential=nonresidential_M, residential=residential_M)
    }
    
    # keep only from the date with death data
    tmp1 <- subset(tmp, date >= min(processed_data$dates[[State]]) & code == State)
    covariates <- tmp1 %>% dplyr::select(-c(code))
    
    N_cov = nrow(covariates)
    forecast_cov <- num_days_sim - N_cov

    # for forcasted data, repeat the last mobility index by weekend and weekday
    covariates = covariates[1:(N_cov+forecast_cov),]
    covariates[N_cov:(N_cov+forecast_cov),]$date <- seq(covariates[N_cov,]$date, covariates[N_cov,]$date+forecast_cov, by ="day")
    covariates <- covariates %>%
      dplyr::mutate(weekend:=as.integer(format(date, "%u"))>5)

    mobility_indices <- setdiff(colnames(covariates), c('date','weekend'))
    for(x in mobility_indices)
    {      
      z <- which(is.na(covariates[[x]]))			
      z2 <- which(diff(z)!=1)
      if(!length(z2)) 
        z2 <- length(z)
      z <- which(is.na(covariates[[x]]) & !covariates$weekend)
      z2 <- which(is.na(covariates[[x]]) & covariates$weekend)
      if(length(z)) 
        covariates[[x]][z] <- covariates[[x]][ dplyr::last(which(!is.na(covariates[[x]]) & !covariates$weekend)) ]
      if(length(z2)) 
        covariates[[x]][z2] <- covariates[[x]][ dplyr::last(which(!is.na(covariates[[x]]) & covariates$weekend)) ]
    }
    
    # add covariates residential
    processed_data$stan_data$covariates_res[[m]] <- as.matrix( covariates %>% dplyr::select(residential) )
    processed_data$stan_data$P_RES <- ncol( processed_data$stan_data$covariates_res[[m]] )
    
    # add covariates non-residential
    mobility_indices <- setdiff(colnames(covariates), c('date','weekend'))
    processed_data$stan_data$covariates_nonres[[m]] <- as.matrix( covariates %>% dplyr::select(setdiff(mobility_indices,'residential')) )
    processed_data$stan_data$P_NONRES <- ncol( processed_data$stan_data$covariates_nonres[[m]] )
  }
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
process_death = function(d1, is_m_in_states_od, index1, index2, forecast, deathsByAge, dataByAgestart, A_AD){
  
  State = unique(d1$code)
  
  # for states without death by age, only need to process overall death
  if(is_m_in_states_od){
    
    deaths_without_forecast <- as.vector(as.numeric(d1$Deaths))
    deaths <- c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
    
    cat(State, "does not have death by age data \n")
    
    return(list("deaths_without_forecast" = deaths_without_forecast, "deaths" = deaths))
  }
  
  # for states with death by age, need to process overall death and death by age
  if(!is_m_in_states_od){ 
    
   # dataByAgestart = deathByAge_data$dataByAgestart[[State]]; A_AD=deathByAge_data$A_AD[[State]]
    
    # if death by age start before epidemic start
    # start death by age 1 day after epidemic start
    if(dataByAgestart <= index1){ 
      deathsByAge[index1+1,1:A_AD] = apply(deathsByAge[dataByAgestart:(index1+1),1:A_AD], 2, sum)
      deathsByAge[dataByAgestart:index1,1:A_AD] = -1
      dataByAgestart = index1+1
    }
    
    # overwrite the overall deaths with the sum of the age specific deaths 
    n.d1 = nrow(d1) + index2 - 1
    dataByAgestart_adj = dataByAgestart-index2 + 1 
    deaths_without_forecast <- c(as.vector(as.numeric(d1$Deaths))[1:dataByAgestart_adj], apply(deathsByAge[(dataByAgestart+1):n.d1,1:A_AD],1,sum))
    
    # add forecast
    deathsByAge <- rbind(deathsByAge[index2:n.d1,], matrix(ncol=ncol(deathsByAge), nrow=forecast, -1))
    deaths <- c(deaths_without_forecast, rep(-1,forecast))

    cat(State, "has", length(dataByAgestart:n.d1), "days with death by age data, starting from day", dataByAgestart_adj, "\n")
    
    return(list("deaths_without_forecast" = deaths_without_forecast, "deaths" = deaths, "deathsByAge" = deathsByAge, "dataByAgestart" = dataByAgestart_adj))
  }
  
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_lognormal_prior_relsusceptibility <- function(processed_data, file_path_CI_OR)
{
  # estimated OR mean from https://science.sciencemag.org/content/sci/early/2020/05/04/science.abb8001.full.pdf
  CI_OR <- read.csv(file_path_CI_OR)
  processed_data$stan_data$mean_log_relsusceptibility_age_reduced = c(rep(CI_OR$mu[1], 2), rep(0, 3), rep(CI_OR$mu[2], 2))
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr splines 
stan_data_add_splines = function(processed_data, spline_degree, num_knots){
  
  # age continuous
  age = 0:99 
  n.age.cont = length(age)
  age_discrete = mean(c(0,4)) + seq(age[1], age[n.age.cont], 5) 
  n.age.disc = length(age_discrete)
  knots = seq(age[1], age[n.age.cont], length.out = num_knots+2)[-c(1, num_knots+2)]
  
  # matrix for taking the mean over contiuous age scale and obtaining discrete scale
  # get_age_discrete = matrix(nrow = n.age.disc, ncol = n.age.cont, rep(c(rep(1, n.age.cont/n.age.disc), rep(0, n.age.cont)), n.age.disc), byrow = T)
  # get_age_discrete = get_age_discrete/(n.age.cont/n.age.disc)
  # 
  # matrix of B-splines
  B <- t(bs(age_discrete, df= num_knots + spline_degree - 1, degree= spline_degree, intercept = TRUE))
  
  stan_data = list(
    num_knots = num_knots,
    knots = knots,
    spline_degree = spline_degree,
    B = B)
  
  processed_data$stan_data = c(processed_data$stan_data, stan_data)
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_lognormal_prior_on_ifr_by_age = function(processed_data, ifr.by.age)
{
	stopifnot(c('age_cat','ln_mu','ln_sd')%in%colnames(ifr.by.age))
	setkey(ifr.by.age, age_cat)
	processed_data$stan_data$hyperpara_ifr_age_lnmu <- ifr.by.age$ln_mu
	processed_data$stan_data$hyperpara_ifr_age_lnsd <- ifr.by.age$ln_sd
	within( processed_data$stan_data, rm(ifr_age) )	
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_beta_prior_on_ifr_by_age = function(processed_data, ifr.by.age){
  #	TODO we are assuming that ifr.by.age is ordered
  processed_data$stan_data$hyperpara_ifr_age = cbind(ifr.by.age$ifr_beta_alpha, ifr.by.age$ifr_beta_beta)
  within( processed_data$stan_data, rm(ifr_age) )	
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_logitincrements_prior_on_ifr_by_age = function(processed_data, ifr.by.age){
  #	TODO we are assuming that ifr.by.age is ordered
  processed_data$stan_data$ifr_age_mean = ifr.by.age$ifr_mean
  processed_data$stan_data$inv_sqrt_1_to_A = 1/sqrt(seq(1, processed_data$stan_data$A))
  within( processed_data$stan_data, rm(ifr_age) )	
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_GMRF_prior_on_susceptibility = function(processed_data){
  
  A = processed_data$stan_data$A
  node1=c(seq.int(1, A-1L), seq.int(1, A-2L))
  node2 =c(seq.int(2, A), seq.int(3, A))
  
  tmp <- sort(node1, index.return=TRUE)$ix
  
  processed_data$stan_data = c(processed_data$stan_data,
                               list(node1 = node1[tmp],
                                    node2 = node2[tmp]))
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_logitprior_ifrbyage = function(processed_data, logit.coef.ifr.by.age){
  
  age = 0:99 
  n.age.cont = length(age)
  age_midpoint = mean(c(0,4)) + seq(age[1], age[n.age.cont], 5) 
  
  processed_data$stan_data = c(processed_data$stan_data,
                               list(hyperpara_ifr_age = as.matrix(logit.coef.ifr.by.age), 
                                    age_midpoint = age_midpoint))

  
  
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_gpprior_betaage = function(processed_data){
  
  age = 0:99 
  n.age.cont = length(age)
  age_midpoint = mean(c(0,4)) + seq(age[1], age[n.age.cont], 5) 
  
  processed_data$stan_data = c(processed_data$stan_data,
                               list(age_midpoint = age_midpoint))
  
  
  
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_school_status <- function(processed_data, 
	path_to_file_school_intervention, 
	forecast_with_schools_reopened, 
	reopening_Date)
{
	states = names(processed_data$dates)
	num_days_sim = processed_data$stan_data$N2
	
	#
	# find school closure dates	
	dsi <- as.data.table(read.csv(path_to_file_school_intervention))
	dsi <- subset(dsi, !is.na(RegionName) & !is.na(Date) & RegionName != "" & !is.na(C1_School.closing))
	dsi[, date := as.Date(as.character(Date), format= "%Y%m%d")]
	dsi[, loc := gsub("US_(.+)","\\1",RegionCode)]
	dsi <- subset(dsi, select=c(loc, date, C1_School.closing))	  
	dsi <- dsi[C1_School.closing != 0, list(closure_date = min(date)), by = c("loc")]
	
	#
	# use NY for NYC
	tmp <- subset(dsi, loc == "NY")
	tmp[, loc := "NYC"]
	dsi <- rbind(dsi,tmp)
	
	#
	# set reopening_date  
	tmp <- sapply(sapply(processed_data$dates, as.character), max)
	tmp <- data.table(loc= names(tmp),  first.day.forecast= as.Date(tmp)+1)
	tmp[, first.day.data:= as.Date(sapply(sapply(processed_data$dates, as.character), min))]
	tmp[, last.day.forecast := first.day.data+num_days_sim-1]
	if(forecast_with_schools_reopened)
	{
		tmp[, reopening_date:= reopening_Date]	
	}
	if(!forecast_with_schools_reopened)
	{
		tmp[, reopening_date:= last.day.forecast+1]	
	}
	dsi <- merge(dsi, tmp, by='loc')  
	dsi <- merge(dsi, dsi[, list(date= seq(first.day.data, last.day.forecast, by='day')), by='loc'], by='loc')
	dsi[, school_closed:= as.integer(closure_date<=date & date<reopening_date)]
	
	# define stan data matrix
	SCHOOL_STATUS <- matrix(nrow = num_days_sim, ncol = processed_data$stan_data$M, NA_integer_)
	for(m in 1:length(states))
	{
		SCHOOL_STATUS[,m] <- subset(dsi, loc==states[m])[, school_closed]    	
	}
	
	processed_data$stan_data$SCHOOL_STATUS = SCHOOL_STATUS
	
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_school_status_EduWeek = function(processed_data, 
                                               path_to_file_school_Ox_data, 
                                               path_to_file_school_EduWeek_data, 
                                               counterfactual_scenario = 0,
                                               forecast_with_schools_reopened = 0, 
                                               default_school_reopening_date = as.Date("2020-08-24"))
{
  
  states = names(processed_data$dates)
  num_days_sim = processed_data$stan_data$N2
  
  #
  # find school closure dates	
  dsi <- as.data.table(read.csv(path_to_file_school_Ox_data))
  dsi <- subset(dsi, !is.na(RegionName) & !is.na(Date) & RegionName != "" & !is.na(C1_School.closing))
  dsi[, date := as.Date(as.character(Date), format= "%Y%m%d")]
  dsi[, loc := gsub("US_(.+)","\\1",RegionCode)]
  dsi <- subset(dsi, select=c(loc, date, C1_School.closing))	  
  dsi <- dsi[C1_School.closing != 0, list(closure_date = min(date)), by = c("loc")]
  # use NY for NYC
  tmp <- subset(dsi, loc == "NY")
  tmp[, loc := "NYC"]
  dsi <- rbind(dsi,tmp)
  
  #
  # find school re-opening dates  
  tmp <- sapply(sapply(processed_data$dates, as.character), max)
  tmp <- data.table(loc= names(tmp),  first.day.forecast= as.Date(tmp)+1)
  tmp[, first.day.data:= as.Date(sapply(sapply(processed_data$dates, as.character), min))]
  tmp[, last.day.forecast := first.day.data+num_days_sim-1]
  
  # set reopening date for states for which schools re-opened before forecast started
  ods <- load_EduWeekly_data(path_to_file_school_EduWeek_data, path_to_file_school_Ox_data)
  df_reopening_dates <- find_reopening_dates(ods, default_school_reopening_date)
  tmp = as.data.table( merge(tmp, df_reopening_dates, by = c("loc")) )
  tmp[schools_reopened_before_forecast == 1, reopening_date := reopening_date_Edu_Week]
  tmp[schools_reopened_before_forecast  == 0, reopening_date := last.day.forecast+1]
  
  # forecast scenarios
  if(forecast_with_schools_reopened){
    tmp[, reopening_date := default_school_reopening_date]	
  }
  
  if(counterfactual_scenario){
    
    tmp1 = copy(tmp)
    
    if(!forecast_with_schools_reopened){
      tmp[, reopening_date:= last.day.forecast+1]
    }
  }
  
  dsi <- merge(dsi, tmp, by='loc')  
  dsi <- merge(dsi, dsi[, list(date= seq(first.day.data, last.day.forecast, by='day')), by='loc'], by='loc')
  dsi[, school_closed := as.integer(closure_date<=date & date<reopening_date)]
  
  # define stan data matrix
  SCHOOL_STATUS <- matrix(nrow = num_days_sim, ncol = processed_data$stan_data$M, NA_integer_)
  for(m in 1:length(states))
  {
    SCHOOL_STATUS[,m] <- subset(dsi, loc==states[m])[, school_closed]    	
  }
  
  #
  # set 'elementary_school_reopening_idx' 
  reo.tab <- data.table(loc = states, last_obs_idx = sapply(processed_data$dates,length), 
                        last_obs_date = as.Date(sapply(processed_data$dates, function(x) tail(as.character(x), 1))))
  reo.idx = merge(reo.tab, tmp, by = "loc")
  reo.idx[, `:=`(reo_idx, last_obs_idx + as.integer(reopening_date - 
                                                      last_obs_date))]
  stopifnot(all(reo.idx$reo_idx <= processed_data$stan_data$N2 + 1))
  reo.idx = reo.idx[match(states, reo.idx$loc)]
  processed_data$stan_data$SCHOOL_STATUS = SCHOOL_STATUS
  processed_data$stan_data$elementary_school_reopening_idx <- reo.idx$reo_idx
  
  # save school reopening date index from the re-opening school scenario
  if(counterfactual_scenario){
    tmp1[schools_reopened_before_forecast == 0, reopening_date := first.day.forecast]
    elt_reopening_index = merge(reo.tab, tmp1, by = "loc")
    elt_reopening_index[, `:=`(elt_reopening_index, last_obs_idx + as.integer(reopening_date -last_obs_date))]
    stopifnot(all(elt_reopening_index$elt_reopening_index <= processed_data$stan_data$N2 + 1))
    processed_data$stan_data$elt_reopening_index <- elt_reopening_index$elt_reopening_index
  } else {
    processed_data$stan_data$elt_reopening_index = reo.idx$reo_idx
  }
  
  return(processed_data)
}
  

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_school_status_EduWeek_with_closure2 = function(processed_data, 
                                               path_to_file_school_Ox_data, 
                                               path_to_file_school_EduWeek_data, 
                                               counterfactual_scenario = 0,
                                               forecast_with_schools_reopened = 0, 
                                               forecast_with_schools_closure_2 = 0,
                                               default_school_reopening_date = as.Date("2020-08-24"),
                                               default_school_closure_2_date = as.Date("2021-01-01"))
{
  
  states = names(processed_data$dates)
  num_days_sim = processed_data$stan_data$N2
  
  #
  # find school closure dates	
  dsi <- as.data.table(read.csv(path_to_file_school_Ox_data))
  dsi <- subset(dsi, CountryName == 'United States')
  dsi <- subset(dsi, !is.na(RegionName) & !is.na(Date) & RegionName != "" & !is.na(C1_School.closing))
  dsi[, date := as.Date(as.character(Date), format= "%Y%m%d")]
  dsi[, loc := gsub("US_(.+)","\\1",RegionCode)]
  dsi <- subset(dsi, select=c(loc, date, C1_School.closing))	  
  dsi <- dsi[C1_School.closing != 0, list(closure_date = min(date)), by = c("loc")]
  # use NY for NYC
  tmp <- subset(dsi, loc == "NY")
  tmp[, loc := "NYC"]
  dsi <- rbind(dsi,tmp)
  
  #
  # find school re-opening dates  
  tmp <- sapply(sapply(processed_data$dates, as.character), max)
  tmp <- data.table(loc= names(tmp),  first.day.forecast= as.Date(tmp)+1)
  tmp[, first.day.data:= as.Date(sapply(sapply(processed_data$dates, as.character), min))]
  tmp[, last.day.forecast := first.day.data+num_days_sim-1]
  
  # load and cure edu weekly data
  ods <- load_EduWeekly_data(path_to_file_school_EduWeek_data, path_to_file_school_Ox_data)
  
  # set reopening date 
  df_reopening_dates <- find_reopening_dates(ods, default_school_reopening_date)
  tmp = as.data.table( merge(tmp, df_reopening_dates, by = c("loc")) )
  tmp[schools_reopened_before_forecast == 1, reopening_date := reopening_date_Edu_Week]
  tmp[schools_reopened_before_forecast  == 0, reopening_date := last.day.forecast+1]
  
  # set closure date 2
  df_closure_2_dates <- find_closure_2_dates(ods, default_school_closure_2_date, df_reopening_dates)
  tmp = as.data.table( merge(tmp, df_closure_2_dates, by = c("loc")) )
  tmp[schools_close_2_before_forecast  == 1, closure_2_date := closure_2_date_Edu_Week]
  tmp[schools_close_2_before_forecast  == 0, closure_2_date := last.day.forecast+1]
  
  # forecast scenarios
  if(forecast_with_schools_reopened){
    tmp[, reopening_date := default_school_reopening_date]	
  }
  
  if(forecast_with_schools_closure_2){
    tmp[, closure_2_date:= default_school_closure_2_date]
  }
  
  if(counterfactual_scenario){
    
    tmp1 = copy(tmp)
    
    if(!forecast_with_schools_reopened){
      tmp[, reopening_date:= last.day.forecast+1]
    }
    
  }
  
  dsi <- merge(dsi, tmp, by='loc')  
  dsi <- merge(dsi, dsi[, list(date= seq(first.day.data, last.day.forecast, by='day')), by='loc'], by='loc')
  dsi[, school_closed := as.integer((date >=closure_date & date<reopening_date) | (date >= closure_2_date))]
  
  # define stan data matrix
  SCHOOL_STATUS <- matrix(nrow = num_days_sim, ncol = processed_data$stan_data$M, NA_integer_)
  for(m in 1:length(states))
  {
    SCHOOL_STATUS[,m] <- subset(dsi, loc==states[m])[, school_closed]    	
  }
  
  #
  # set 'elementary_school_reopening_idx' 
  reo.tab <- data.table(loc = states, last_obs_idx = sapply(processed_data$dates,length), 
                        last_obs_date = as.Date(sapply(processed_data$dates, function(x) tail(as.character(x), 1))))
  reo.idx = merge(reo.tab, tmp, by = "loc")
  reo.idx[, `:=`(reo_idx, last_obs_idx + as.integer(reopening_date - 
                                                      last_obs_date))]
  stopifnot(all(reo.idx$reo_idx <= processed_data$stan_data$N2 + 1))
  reo.idx = reo.idx[match(states, reo.idx$loc)]
  processed_data$stan_data$SCHOOL_STATUS = SCHOOL_STATUS
  processed_data$stan_data$elementary_school_reopening_idx <- reo.idx$reo_idx
  
  # save school reopening date index from the re-opening school scenario
  if(counterfactual_scenario){
    tmp1[schools_reopened_before_forecast == 0, reopening_date := first.day.forecast]
    elt_reopening_index = merge(reo.tab, tmp1, by = "loc")
    elt_reopening_index[, `:=`(elt_reopening_index, last_obs_idx + as.integer(reopening_date -last_obs_date))]
    stopifnot(all(elt_reopening_index$elt_reopening_index <= processed_data$stan_data$N2 + 1))
    processed_data$stan_data$elt_reopening_index <- elt_reopening_index$elt_reopening_index
  } else {
    processed_data$stan_data$elt_reopening_index = reo.idx$reo_idx
  }
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_contact_school_closure = function(processed_data, dcontact, path_to_file_contact_intensities_outbreak_China, multiplier_cntct_school_closure, 
                                                with_average_cntnt_child_outbreak = FALSE, min_pc_contacts=0.05)
{
  contact_intensities_outbreak_zhang <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
  
  age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
  children_label = c("0-4", "5-9", "10-14")
  
  #
  # age categories as factor
  contact_intensities_outbreak_zhang[, age_index := factor(age_index, levels = age_category)]
  contact_intensities_outbreak_zhang[, age_contact := factor(age_contact, levels = age_category)]
    
  #
  # aggregate over both cities
  cnt.zhang = contact_intensities_outbreak_zhang[, list(count_post = mean(count_post)), by = c("age_index", "age_contact")]
  
  #
  # take the average over children
  if(with_average_cntnt_child_outbreak){
    cat("\nAveraging contact intensities for children aged 0-14")
    cnt.zhang[age_index %in% children_label & age_contact %in% children_label, count_post := mean(count_post)]
  }
  
  #
  # break 65+ age band into 65-69, 70-74, 75-79, 80-84, 85+
  old_age_label = "65+"
  new_age_label = c("65-69", "70-74", "75-79", "80-84", "85+")
  new_age_category = c(age_category[-length(age_category)], new_age_label)
  # age index
  df = cnt.zhang[age_index %in% old_age_label,]
  df = df[rep(seq_len(nrow(df)), each = length(new_age_label)),]
  df[,age_index := rep(new_age_label, length(age_category))]
  cnt.zhang = rbind(subset(cnt.zhang, !age_index %in% old_age_label), df)
  # age contact
  df2 = cnt.zhang[age_contact %in% old_age_label,]
  df2 = df2[rep(seq_len(nrow(df2)), each = length(new_age_label)),]
  df2[, age_contact := rep(new_age_label, length(new_age_category))]
  df2[, count_post := count_post/length(new_age_label)] # take average
  cnt.zhang = rbind(subset(cnt.zhang, !age_contact %in% old_age_label), df2)
  setnames(cnt.zhang, c('age_index','age_contact','count_post'),c('part.age.cat.label','cont.age.cat.label','m_zhang'))
  
  #
  # prepare contact matrices during school closure for each state  		
  cnt.zhang <- merge(dcontact, cnt.zhang, by=c('part.age.cat.label','cont.age.cat.label'))
  cnt.zhang[, m_pc:= min_pc_contacts*m]
  cnt.zhang[, m_model:= pmin(m ,multiplier_cntct_school_closure * pmax(m_pc, m_zhang))]
  #	on weekdays, use m_model for 0-14 -> any, any-> 0-14, otherwise m*trends
  tmp <- cnt.zhang[, which(type=='weekday' & part.age.cat>3 & cont.age.cat>3 )]
  set(cnt.zhang, tmp, 'm_model', cnt.zhang[tmp,m])
  #	on weekend days, use m_model for 0-14 -> any, any-> 0-14, otherwise m*trends
  tmp <- cnt.zhang[, which(type=='weekend' & part.age.cat>3 & cont.age.cat>3 )]
  set(cnt.zhang, tmp, 'm_model', cnt.zhang[tmp,m])

 
  states = names(processed_data$dates)
  cntct_school_closure_weekdays = vector('list', length(states))
  cntct_school_closure_weekends = vector('list', length(states))
  for(i in 1:length(states))
  {
	  tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekday"), part.age.cat~cont.age.cat, value.var='m_model')
	  tmp <- unname(as.matrix(tmp)[,-1])	  
	  cntct_school_closure_weekdays[[i]] <- tmp	  	  
	  tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekend"), part.age.cat~cont.age.cat, value.var='m_model')
	  tmp <- unname(as.matrix(tmp)[,-1])
	  cntct_school_closure_weekends[[i]] <- tmp	  
  }  
  
  processed_data$stan_data$A_CHILD = 3L
  processed_data$stan_data$AGE_CHILD = c(1L,2L,3L)
  processed_data$stan_data$cntct_school_closure_weekdays = cntct_school_closure_weekdays
  processed_data$stan_data$cntct_school_closure_weekends = cntct_school_closure_weekends

  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_contact_elementary_high_school_closure = function(processed_data, dcontact, path_to_file_contact_intensities_outbreak_China, 
                                                                multiplier_cntct_school_closure, min_pc_contacts=0.05)
{
  contact_intensities_outbreak_zhang <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
  
  age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
  
  #
  # age categories as factor
  contact_intensities_outbreak_zhang[, age_index := factor(age_index, levels = age_category)]
  contact_intensities_outbreak_zhang[, age_contact := factor(age_contact, levels = age_category)]
  
  #
  # aggregate over both cities
  cnt.zhang = contact_intensities_outbreak_zhang[, list(count_post = mean(count_post)), by = c("age_index", "age_contact")]
  
  #
  # break 65+ age band into 65-69, 70-74, 75-79, 80-84, 85+
  old_age_label = "65+"
  new_age_label = c("65-69", "70-74", "75-79", "80-84", "85+")
  new_age_category = c(age_category[-length(age_category)], new_age_label)
  # age index
  df = cnt.zhang[age_index %in% old_age_label,]
  df = df[rep(seq_len(nrow(df)), each = length(new_age_label)),]
  df[,age_index := rep(new_age_label, length(age_category))]
  cnt.zhang = rbind(subset(cnt.zhang, !age_index %in% old_age_label), df)
  # age contact
  df2 = cnt.zhang[age_contact %in% old_age_label,]
  df2 = df2[rep(seq_len(nrow(df2)), each = length(new_age_label)),]
  df2[, age_contact := rep(new_age_label, length(new_age_category))]
  df2[, count_post := count_post/length(new_age_label)] # take average
  cnt.zhang = rbind(subset(cnt.zhang, !age_contact %in% old_age_label), df2)
  setnames(cnt.zhang, c('age_index','age_contact','count_post'),c('part.age.cat.label','cont.age.cat.label','m_zhang'))
  
  #
  # prepare contact matrices during school closure for each state  		
  cnt.zhang <- merge(dcontact, cnt.zhang, by=c('part.age.cat.label','cont.age.cat.label'))
  cnt.zhang[, m_pc:= min_pc_contacts*m]
  cnt.zhang[, m_model:= pmin(m ,multiplier_cntct_school_closure * pmax(m_pc, m_zhang))]
  #	on weekdays, use m_model for 0-19 -> any, any-> 0-19, otherwise m*trends
  tmp <- cnt.zhang[, which(type=='weekday' & part.age.cat>4 & cont.age.cat>4 )]
  set(cnt.zhang, tmp, 'm_model', cnt.zhang[tmp,m])
  #	on weekend days, use m_model for 0-19 -> any, any-> 0-19, otherwise m*trends
  tmp <- cnt.zhang[, which(type=='weekend' & part.age.cat>4 & cont.age.cat>4 )]
  set(cnt.zhang, tmp, 'm_model', cnt.zhang[tmp,m])
  
  
  states = names(processed_data$dates)
  cntct_school_closure_weekdays = vector('list', length(states))
  cntct_school_closure_weekends = vector('list', length(states))
  for(i in 1:length(states))
  {
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekday"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])	  
    cntct_school_closure_weekdays[[i]] <- tmp	  	  
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekend"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])
    cntct_school_closure_weekends[[i]] <- tmp	  
  }  
  
  processed_data$stan_data$A_CHILD = 4L
  processed_data$stan_data$AGE_CHILD = c(1L,2L,3L,4L)
  processed_data$stan_data$A_ADULT = 7L
  processed_data$stan_data$AGE_ADULT = c(5L,6L,7L,8L, 9L,10L,11L)
  processed_data$stan_data$cntct_school_closure_weekdays = cntct_school_closure_weekdays
  processed_data$stan_data$cntct_school_closure_weekends = cntct_school_closure_weekends
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_contact_school_opening <- function(processed_data, 
	dcontact, 
	path_to_file_contact_intensities_outbreak_China, 
	multiplier_cntct_school_closure=1, 
	multiplier_cntct_school_opening=1, 
	min_pc_contacts=0.05, 
	reopening_date="2020-08-24")
{
	contact_intensities_outbreak_zhang <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
	
	age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
	children_label = c("0-4", "5-9", "10-14")
	
	#
	# age categories as factor
	contact_intensities_outbreak_zhang[, age_index := factor(age_index, levels = age_category)]
	contact_intensities_outbreak_zhang[, age_contact := factor(age_contact, levels = age_category)]
	
	#
	# aggregate over both cities
	cnt.zhang = contact_intensities_outbreak_zhang[, list(count_post = mean(count_post)), by = c("age_index", "age_contact")]
	
	#
	# break 65+ age band into 65-69, 70-74, 75-79, 80-84, 85+
	old_age_label = "65+"
	new_age_label = c("65-69", "70-74", "75-79", "80-84", "85+")
	new_age_category = c(age_category[-length(age_category)], new_age_label)
	# age index
	df = cnt.zhang[age_index %in% old_age_label,]
	df = df[rep(seq_len(nrow(df)), each = length(new_age_label)),]
	df[,age_index := rep(new_age_label, length(age_category))]
	cnt.zhang = rbind(subset(cnt.zhang, !age_index %in% old_age_label), df)
	# age contact
	df2 = cnt.zhang[age_contact %in% old_age_label,]
	df2 = df2[rep(seq_len(nrow(df2)), each = length(new_age_label)),]
	df2[, age_contact := rep(new_age_label, length(new_age_category))]
	df2[, count_post := count_post/length(new_age_label)] # take average
	cnt.zhang = rbind(subset(cnt.zhang, !age_contact %in% old_age_label), df2)
	setnames(cnt.zhang, c('age_index','age_contact','count_post'),c('part.age.cat.label','cont.age.cat.label','m_zhang'))
	
	#
	# prepare contact matrices during school opening forecast period for each state  		
	cnt.zhang <- merge(dcontact, cnt.zhang, by=c('part.age.cat.label','cont.age.cat.label'))
	cnt.zhang[, m_pc:= min_pc_contacts*m]
	cnt.zhang[, m_zhang2:= pmin(m , multiplier_cntct_school_closure * pmax(m_pc, m_zhang))]
	cnt.zhang[, m_model:= m]
	#	now use for both weekends and weekdays:
	#	baseline intensities for 15+ & 15+
	#	reduced baseline intensities for 0-9 -> any, any-> 0-9
	#	mixture baseline intensities for 10-14 -> any, any-> 10-14
	tmp <- cnt.zhang[, which( part.age.cat<3 | cont.age.cat<3 )]
	set(cnt.zhang, tmp, 'm_model', multiplier_cntct_school_opening * cnt.zhang[tmp,m])
	set(cnt.zhang, tmp, 'm_model', pmax(cnt.zhang[tmp,m_zhang2], cnt.zhang[tmp,m_model]))
	tmp <- cnt.zhang[, which( part.age.cat==3 | cont.age.cat==3 )]
	set(cnt.zhang, tmp, 'm_model', 2/5*pmax(cnt.zhang[tmp,m_zhang2], multiplier_cntct_school_opening*cnt.zhang[tmp,m]) + 
					3/5*cnt.zhang[tmp,m_zhang2])
	
	states = names(processed_data$dates)
	cntct_elementary_school_reopening_weekdays = vector('list', length(states))
	cntct_elementary_school_reopening_weekends = vector('list', length(states))
	for(i in 1:length(states))
	{
		tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekday"), part.age.cat~cont.age.cat, value.var='m_model')
		tmp <- unname(as.matrix(tmp)[,-1])	  
		cntct_elementary_school_reopening_weekdays[[i]] <- tmp	  	  
		tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekend"), part.age.cat~cont.age.cat, value.var='m_model')
		tmp <- unname(as.matrix(tmp)[,-1])
		cntct_elementary_school_reopening_weekends[[i]] <- tmp	  
	}  	
	processed_data$stan_data$cntct_elementary_school_reopening_weekdays = cntct_elementary_school_reopening_weekdays
	processed_data$stan_data$cntct_elementary_school_reopening_weekends = cntct_elementary_school_reopening_weekends
	
	#
	# set 'elementary_school_reopening_idx'
	reo.idx <- data.table(
			last_obs_idx = sapply( processed_data$dates, length ), 
			last_obs_date = as.Date(sapply( processed_data$dates, function(x) tail(as.character(x),1) ))
	)
	reo.idx[, reo_idx:= last_obs_idx + as.integer(as.Date(reopening_date)-last_obs_date)]
	stopifnot( all(reo.idx$reo_idx<=processed_data$stan_data$N2) )
	
	processed_data$stan_data$elementary_school_reopening_idx <- reo.idx$reo_idx
	
	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_contact_school_opening_EduWeek <- function(processed_data, 
                                                 dcontact, 
                                                 path_to_file_contact_intensities_outbreak_China, 
                                                 multiplier_cntct_school_closure=1, 
                                                 multiplier_cntct_school_opening=1, 
                                                 min_pc_contacts=0.05)
{
  contact_intensities_outbreak_zhang <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
  
  age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
  children_label = c("0-4", "5-9", "10-14")
  
  #
  # age categories as factor
  contact_intensities_outbreak_zhang[, age_index := factor(age_index, levels = age_category)]
  contact_intensities_outbreak_zhang[, age_contact := factor(age_contact, levels = age_category)]
  
  #
  # aggregate over both cities
  cnt.zhang = contact_intensities_outbreak_zhang[, list(count_post = mean(count_post)), by = c("age_index", "age_contact")]
  
  #
  # break 65+ age band into 65-69, 70-74, 75-79, 80-84, 85+
  old_age_label = "65+"
  new_age_label = c("65-69", "70-74", "75-79", "80-84", "85+")
  new_age_category = c(age_category[-length(age_category)], new_age_label)
  # age index
  df = cnt.zhang[age_index %in% old_age_label,]
  df = df[rep(seq_len(nrow(df)), each = length(new_age_label)),]
  df[,age_index := rep(new_age_label, length(age_category))]
  cnt.zhang = rbind(subset(cnt.zhang, !age_index %in% old_age_label), df)
  # age contact
  df2 = cnt.zhang[age_contact %in% old_age_label,]
  df2 = df2[rep(seq_len(nrow(df2)), each = length(new_age_label)),]
  df2[, age_contact := rep(new_age_label, length(new_age_category))]
  df2[, count_post := count_post/length(new_age_label)] # take average
  cnt.zhang = rbind(subset(cnt.zhang, !age_contact %in% old_age_label), df2)
  setnames(cnt.zhang, c('age_index','age_contact','count_post'),c('part.age.cat.label','cont.age.cat.label','m_zhang'))
  
  #
  # prepare contact matrices during school opening forecast period for each state  		
  cnt.zhang <- merge(dcontact, cnt.zhang, by=c('part.age.cat.label','cont.age.cat.label'))
  cnt.zhang[, m_pc:= min_pc_contacts*m]
  cnt.zhang[, m_zhang2:= pmin(m , multiplier_cntct_school_closure * pmax(m_pc, m_zhang))]
  cnt.zhang[, m_model:= m]
  #	now use for both weekends and weekdays:
  #	baseline intensities for 15+ & 15+
  #	reduced baseline intensities for 0-9 -> any, any-> 0-9
  #	mixture baseline intensities for 10-14 -> any, any-> 10-14
  tmp <- cnt.zhang[, which( part.age.cat<3 | cont.age.cat<3 )]
  set(cnt.zhang, tmp, 'm_model', multiplier_cntct_school_opening * cnt.zhang[tmp,m])
  set(cnt.zhang, tmp, 'm_model', pmax(cnt.zhang[tmp,m_zhang2], cnt.zhang[tmp,m_model]))
  tmp <- cnt.zhang[, which( part.age.cat==3 | cont.age.cat==3 )]
  set(cnt.zhang, tmp, 'm_model', 2/5*pmax(cnt.zhang[tmp,m_zhang2], multiplier_cntct_school_opening*cnt.zhang[tmp,m]) + 
        3/5*cnt.zhang[tmp,m_zhang2])
  
  states = names(processed_data$dates)
  cntct_elementary_school_reopening_weekdays = vector('list', length(states))
  cntct_elementary_school_reopening_weekends = vector('list', length(states))
  for(i in 1:length(states))
  {
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekday"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])	  
    cntct_elementary_school_reopening_weekdays[[i]] <- tmp	  	  
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekend"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])
    cntct_elementary_school_reopening_weekends[[i]] <- tmp	  
  }  	
  processed_data$stan_data$cntct_elementary_school_reopening_weekdays = cntct_elementary_school_reopening_weekdays
  processed_data$stan_data$cntct_elementary_school_reopening_weekends = cntct_elementary_school_reopening_weekends
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_add_contact_elementary_high_school_opening_EduWeek <- function(processed_data, 
                                                         dcontact, 
                                                         path_to_file_contact_intensities_outbreak_China, 
                                                         multiplier_cntct_school_closure=1, 
                                                         multiplier_cntct_school_opening=1, 
                                                         min_pc_contacts=0.05)
{
  contact_intensities_outbreak_zhang <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
  
  age_category = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
  
  #
  # age categories as factor
  contact_intensities_outbreak_zhang[, age_index := factor(age_index, levels = age_category)]
  contact_intensities_outbreak_zhang[, age_contact := factor(age_contact, levels = age_category)]
  
  #
  # aggregate over both cities
  cnt.zhang = contact_intensities_outbreak_zhang[, list(count_post = mean(count_post)), by = c("age_index", "age_contact")]
  
  #
  # break 65+ age band into 65-69, 70-74, 75-79, 80-84, 85+
  old_age_label = "65+"
  new_age_label = c("65-69", "70-74", "75-79", "80-84", "85+")
  new_age_category = c(age_category[-length(age_category)], new_age_label)
  # age index
  df = cnt.zhang[age_index %in% old_age_label,]
  df = df[rep(seq_len(nrow(df)), each = length(new_age_label)),]
  df[,age_index := rep(new_age_label, length(age_category))]
  cnt.zhang = rbind(subset(cnt.zhang, !age_index %in% old_age_label), df)
  # age contact
  df2 = cnt.zhang[age_contact %in% old_age_label,]
  df2 = df2[rep(seq_len(nrow(df2)), each = length(new_age_label)),]
  df2[, age_contact := rep(new_age_label, length(new_age_category))]
  df2[, count_post := count_post/length(new_age_label)] # take average
  cnt.zhang = rbind(subset(cnt.zhang, !age_contact %in% old_age_label), df2)
  setnames(cnt.zhang, c('age_index','age_contact','count_post'),c('part.age.cat.label','cont.age.cat.label','m_zhang'))
  
  #
  # prepare contact matrices during school opening forecast period for each state  		
  cnt.zhang <- merge(dcontact, cnt.zhang, by=c('part.age.cat.label','cont.age.cat.label'))
  cnt.zhang[, m_pc:= min_pc_contacts*m]
  cnt.zhang[, m_zhang2:= pmin(m , multiplier_cntct_school_closure * pmax(m_pc, m_zhang))]
  cnt.zhang[, m_model:= m]
  #	now use for both weekends and weekdays:
  #	baseline intensities for 15+ & 15+
  #	reduced baseline intensities for 0-14 -> any, any-> 0-14
  #	mixture baseline intensities for 15-19 -> any, any-> 15-19
  tmp <- cnt.zhang[, which( part.age.cat<4 | cont.age.cat<4 )]
  set(cnt.zhang, tmp, 'm_model', multiplier_cntct_school_opening * cnt.zhang[tmp,m])
  set(cnt.zhang, tmp, 'm_model', pmax(cnt.zhang[tmp,m_zhang2], cnt.zhang[tmp,m_model]))
  tmp <- cnt.zhang[, which( part.age.cat==4 | cont.age.cat==4 )]
  set(cnt.zhang, tmp, 'm_model', 4/5*pmax(cnt.zhang[tmp,m_zhang2], multiplier_cntct_school_opening*cnt.zhang[tmp,m]) + 
        1/5*cnt.zhang[tmp,m_zhang2])
  
  states = names(processed_data$dates)
  cntct_elementary_school_reopening_weekdays = vector('list', length(states))
  cntct_elementary_school_reopening_weekends = vector('list', length(states))
  for(i in 1:length(states))
  {
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekday"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])	  
    cntct_elementary_school_reopening_weekdays[[i]] <- tmp	  	  
    tmp <- dcast.data.table(subset(cnt.zhang, loc==states[i] & type=="weekend"), part.age.cat~cont.age.cat, value.var='m_model')
    tmp <- unname(as.matrix(tmp)[,-1])
    cntct_elementary_school_reopening_weekends[[i]] <- tmp	  
  }  	
  processed_data$stan_data$cntct_elementary_school_reopening_weekdays = cntct_elementary_school_reopening_weekdays
  processed_data$stan_data$cntct_elementary_school_reopening_weekends = cntct_elementary_school_reopening_weekends
  
  return(processed_data)
}


#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
add_date_to_pop_info <- function(pop_info, stan_data, dates)
{  	 
  	ans <- data.table( loc= names(dates), loc_idx= seq_along(dates), n_agegroups=unlist( stan_data$A_AD ) )
	
	#	set generic first and last dates for which overall deaths are available
	ans[, date.overall.first:= unlist(lapply(sapply(dates, as.character), "[[", 1))]
	set(ans, NULL, 'date.overall.first', ans[, as.Date(date.overall.first)+30])
	ans[, date.overall.last:= as.Date(unlist(lapply(sapply(dates, as.character), tail, 1)))]
		
	#	get start date from which onwards age specific deaths are available
	tmp <- ans[,	list( date.deathbyage.first= 	dates[[loc_idx]][ stan_data$dataByAgestart[loc_idx] ] ), by= 'loc_idx']
	ans <- merge(ans, tmp, by='loc_idx')
		
	#	update last date for which overall deaths are used
	tmp <- ans[, which(!is.na(date.deathbyage.first))]
	set(ans, tmp, 'date.overall.last', ans[tmp, date.deathbyage.first-1] )
	
	#	last date for which age specific deaths are available is always the last date
	ans[, date.deathbyage.last:= as.Date(unlist(lapply(sapply(dates, as.character), tail, 1)))]
	
	#	make ranges
	ans[, date.overall:= paste0(format(date.overall.first,"%b %d"), ' - ', format(date.overall.last,"%b %d"))]
	ans[, date.deathbyage:= paste0(format(date.deathbyage.first,"%b %d"), ' - ', format(date.deathbyage.last,"%b %d"))]
	set(ans, ans[, which(is.na(date.deathbyage.first))], 'date.deathbyage', '-' )
	
	ans <- subset(ans, select=c(loc, date.overall, date.deathbyage, n_agegroups))
  
  	
  	ans <- merge(pop_info, ans, by=c('loc'), all.x=TRUE)
  	ans  
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
stan_data_fix_beta1 <- function(processed_data, pop_info, mobility_data)
{
	# dip date
  	dip_date <- unique(subset(mobility_data,select = c('loc','dip_date')))
  	beta1 <- vector(length = dim(processed_data$stan_data$covariates)[1])
  	# loop over states
  	for (m in 1:dim(processed_data$stan_data$covariates)[1]) 
  	{		
		# baseline mobility before dip dates		
    	dip_date_local <- dip_date[loc==names(processed_data$dates)[m],dip_date]
		if(dip_date_local<processed_data$dates[[m]][1])
		{
			beta1[m] <- 1	
		}
		if(dip_date_local>=processed_data$dates[[m]][1])
		{
			dip_date_local_id <- which(processed_data$dates[[m]]==dip_date_local)	
			covariates_baseline_local <- processed_data$stan_data$covariates[m,1,1:dip_date_local_id,]
			
			# population weight
			prop_pop_local <- pop_info[loc==names(processed_data$dates)[m],]$prop_pop
			
			# solve eta=1
			f <- function(beta1)
			{
				mean(
						apply(exp(beta1 * covariates_baseline_local) * 
										matrix(rep(prop_pop_local,each=nrow(covariates_baseline_local)),nrow=nrow(covariates_baseline_local)),
								1,
								sum)
				) - 1
			}
			beta1[m] <- uniroot(f,c(-1,1))$root
		}		
  	}
  	processed_data$stan_data$beta1 <- beta1
  	return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
make_emodo_age_cat_map_with_cont <- function(mobility_data, pop_info)
{
  dages <- unique(subset(pop_info, select=c(age.cat.label, age.cat, age.cat.from, age.cat.to)))
  tmp <- unique(rbind(subset(mobility_data, select=c(idx.age.cat, idx.age.label)),
                      subset(mobility_data, select=c(cont.age.cat, cont.age.label)),
                      use.names=FALSE))
  tmp[, idx.age.cat.label2:= as.character(idx.age.label)]
  tmp[, idx.age.lower:= as.integer(gsub('\\+','',gsub('^([0-9\\+]+)-([0-9]+)$','\\1',idx.age.cat.label2)))]
  tmp[, idx.age.upper:= as.integer(gsub('[0-9]+\\+','99',gsub('^([0-9\\+]+)-([0-9\\+]+)$','\\2',idx.age.cat.label2)))]

  dages <- dages[, {
    z <- which( age.cat.to >= tmp$idx.age.lower & age.cat.to <= tmp$idx.age.upper )
    list(idx.age.cat= ifelse(length(z)>0, tmp$idx.age.cat[z], NA_integer_))
  }, by=c('age.cat','age.cat.label')]
  tmp <- unique(subset(tmp, select=c(idx.age.cat, idx.age.label)))
  dages <- merge(dages, tmp, by= c('idx.age.cat'), all.x=TRUE)
  dages
}

#' @export
#' @keywords internal
#' @import data.table tidyr stringr 
make_mobility_trends_emo_with_cont <- function(mobility_data, emo_age_cat_map)
{				
  #	calculate baseline over 2 weeks 	
  min.date <- min(mobility_data$date)
  tmp <- subset(mobility_data, date<min.date+14)
  tmp <- tmp[, list(base_avg_contacts= mean(avg_contacts)), by=c('loc','idx.age.cat','cont.age.cat')]
  mobility_data <- merge(mobility_data, tmp, by=c('loc','idx.age.cat','cont.age.cat'))
  
  #	calculate mobility trends
  mobility_data[, mobility_trend:= avg_contacts / base_avg_contacts]
  
  #	expand to age strata used in model 
  tmp <- subset(emo_age_cat_map, select=c(idx.age.cat, age.cat, age.cat.label))
  set(tmp, tmp[, which(age.cat<=3)], 'idx.age.cat', 1L)
  mobility_data <- merge(mobility_data, tmp, by='idx.age.cat', allow.cartesian=TRUE)
  setnames(mobility_data,c('age.cat', 'age.cat.label'),c('idx.age.cat2', 'idx.age.cat.label2'))
  setnames(tmp,'idx.age.cat','cont.age.cat')
  mobility_data <- merge(mobility_data, tmp, by='cont.age.cat', allow.cartesian=TRUE)
  setnames(mobility_data,c('age.cat', 'age.cat.label'),c('cont.age.cat2', 'cont.age.cat.label2'))
    
  mobility_data
}

#' @export
#' @keywords internal
#' @import data.table tidyr  
gqs_add_stan_data_for_flows <- function(stan_data,dates)
{
	#	full flows of the 18 age buckets in the model are aggregated into 7 age buckets as follows 
	reduced_age_bands_map <- c(1,1,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7)
	A_REDUCED <- max(reduced_age_bands_map)
	
	#	reduced flows: calculate starting from each Monday from N0 until last full week  
	reduced_flows_Monday_idx <- list()
	for (m in 1:ncol(stan_data$wkend_idx)) {
		tmp <- stan_data$wkend_idx[,m]
		tmp <- tmp[which(diff(tmp,1)==1)[1]]+2
		while(tmp>7L){
			tmp <- tmp - 7L
		}
		reduced_flows_Monday_idx[[m]] <- seq(from=tmp, to=stan_data$N2, by=7)
	}
	
	# Mondays need to be after N0 and up to full last week
	reduced_flows_Monday_idx <- lapply(reduced_flows_Monday_idx, function(x){x[x>stan_data$N0 & x<= stan_data$N2 - 7L + 1]})
	
	# find dimensions
	N_WEEKS_REDUCED_FLOWS <- sapply(reduced_flows_Monday_idx,length)
	NMAX_WEEKS_REDUCED_FLOWS <- max(N_WEEKS_REDUCED_FLOWS)
	
	# make into matrix
	for(m in 1:ncol(stan_data$wkend_idx)){
		if(length(reduced_flows_Monday_idx[[m]]) != max(lengths(reduced_flows_Monday_idx))){
			reduced_flows_Monday_idx[[m]] <- c(reduced_flows_Monday_idx[[m]],rep(-1,max(lengths(reduced_flows_Monday_idx))-length(reduced_flows_Monday_idx[[m]])))
		}
	}  
	reduced_flows_Monday_idx <- matrix( unlist(reduced_flows_Monday_idx), nrow=NMAX_WEEKS_REDUCED_FLOWS )
	
	#	make into data.table to find full flow days, and for printing
	dix <- as.data.table(reshape2::melt(reduced_flows_Monday_idx))
	setnames(dix, 1:3, c('time_idx','state','time'))  
	dix <- dix[time>0, list(time=time,
					date=dates[[state]][time]), by='state']
	cat('\nFor reduced flow analysis selected dates are ')
	print(dix)
	
	#	select Mondays for full flows.
	# this is supposed to be for the same Mondays across states (early and late)
	
	#	find Mondays	
	
	# deselect forecast period
	dix <- subset(dix, !is.na(date))
	# select first common Monday and last common Monday
	tmp <- dix[, list( date_min=min(date), 
					date_max=max(date)
			), by='state']
	first.common.Monday <- max(tmp$date_min)
	last.common.Monday <- min(tmp$date_max)  	
	cat("\nFor full flow analysis selected dates are", as.character(first.common.Monday), ' ', as.character(last.common.Monday))	  
	dix <- subset(dix, date==first.common.Monday | date==last.common.Monday)  
	# convert to matrix
	dix <- dcast.data.table(dix, date~state, value.var='time')
	full_flows_Monday_idx <- unname(as.matrix(subset(dix, select=-date)))
	
	#	find dimensions
	NMAX_WEEKS_FULL_FLOWS <- nrow(full_flows_Monday_idx)
	N_WEEKS_FULL_FLOWS <- apply(full_flows_Monday_idx,2,function(x){sum(x!=-1)})
	
	#	build additional stan data
	stan_data$NMAX_WEEKS_FULL_FLOWS <- NMAX_WEEKS_FULL_FLOWS
	stan_data$N_WEEKS_FULL_FLOWS <- N_WEEKS_FULL_FLOWS
	stan_data$full_flows_Monday_idx <- full_flows_Monday_idx
	stan_data$A_REDUCED <- A_REDUCED
	stan_data$NMAX_WEEKS_REDUCED_FLOWS <- NMAX_WEEKS_REDUCED_FLOWS
	stan_data$N_WEEKS_REDUCED_FLOWS <- N_WEEKS_REDUCED_FLOWS
	stan_data$reduced_age_bands_map <- reduced_age_bands_map
	stan_data$reduced_flows_Monday_idx <- reduced_flows_Monday_idx
	stan_data$n_days <- 1L
	
	cat("\nFlows aggregated across consecutive days, n_days=", stan_data$n_days)
	
	return(stan_data)  
}

#' @export
#' @keywords internal
gqs_add_stan_data_for_iar <- function(stan_data){
	num_days_sim <- stan_data$N2
	set.seed(5678) # make reproducible 
	mean1 <- 5.1; cv1 <- 0.86; # infection to onset
	mean2 <- 14; cv2 <- 3.57  # onset to antibody
	x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
	x2 <- rnorm(1e6,mean2,cv2) # onset-to-antibody distribution
	ecdf_death <- ecdf(x1+x2)
	
	#	disrectise to days since infection
	iar_by_dsi <- vector('numeric',num_days_sim)
	iar_by_dsi[1] <- ecdf_death(1.5) - ecdf_death(0)
	for(s in 2:num_days_sim)
	{
		iar_by_dsi[s] <- ecdf_death(s+.5) - ecdf_death(s-.5)
	}
	stan_data$rev_iar_daysSinceInfection = rev( iar_by_dsi )
	return(stan_data)  
}

#' @export
#' @keywords internal
gqs_add_stan_data_for_new_strain = function(stan_data,
                                            dates,
                                            rel_transmissibility_new_strain = 1.7,
                                            prop_cases_new_strain_first_day = 0.01,
                                            first_date_new_strain = as.Date('2020-12-15')){
  
  # time index of the introduction of the new strain
  timeidx_introduction_new_strain = vector(mode = 'integer', length = length(dates))
  for(m in seq_along(names(dates))){
    # m = 1
    dates_m = data.table(date = seq.Date(min(dates[[m]]), min(dates[[m]]) + stan_data$N2 - 1, by = 'day'), 
                         time_idx = 1:stan_data$N2)

    timeidx_introduction_new_strain[m] = subset(dates_m, date == first_date_new_strain)$time_idx[1]
  }
  stan_data$INTRO_NEW_STRAIN_TIME_IDX = timeidx_introduction_new_strain
  
  # total number of SARS-CoV-2 strains
  stan_data$N_STRAINS = 2
  
  # transmissibility multiplier new SARS-CoV-2 strains
  stan_data$rel_transmissibility_new_strain = rel_transmissibility_new_strain
  
  # proportion of new strain among cases on introduction day
  stan_data$prop_cases_new_strain_first_day = prop_cases_new_strain_first_day
  
  return(stan_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr  
gqs_add_stan_data_for_shield <- function(stan_data, return_age_band,shielded_age_band,shielded_contacts,shielded_date, dates)
{
  # find indices for age bands
  return_age_band = as.numeric(strsplit(return_age_band, '-')[[1]])
  shielded_age_band = as.numeric(strsplit(shielded_age_band, '-')[[1]])
  age_breaks = c(seq(0,85,5),100)
  return_age_band_index = max(which(age_breaks[1:(length(age_breaks)-1)] <= return_age_band[1])): min(which(age_breaks[2:(length(age_breaks))]  >= return_age_band[2]))
  shielded_age_band_index = max(which(age_breaks[1:(length(age_breaks)-1)] <= shielded_age_band[1])): min(which(age_breaks[2:(length(age_breaks))] >= shielded_age_band[2]))
  
  # indices for adult age groups which have no_impact_intv or impact_intv
  stan_data$no_impact_intv_idx <- c(return_age_band_index, shielded_age_band_index)
  stan_data$N_no_impact_intv_idx <-length(stan_data$no_impact_intv_idx)
  stan_data$impact_intv_idx <- setdiff(1:(length(age_breaks)-1), stan_data$no_impact_intv_idx)
  stan_data$impact_intv_idx <- setdiff(stan_data$impact_intv_idx, (1:stan_data$A_CHILD))
  stan_data$N_impact_intv_idx <-length(stan_data$impact_intv_idx)
  
  # set shielded age band contacts to shielded_contacts
  stan_data$cntct_weekdays_mean_shielded_age_band <-  copy(stan_data$cntct_weekdays_mean)
  stan_data$cntct_weekdays_mean_shielded_age_band <- lapply(stan_data$cntct_weekdays_mean_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  stan_data$cntct_weekends_mean_shielded_age_band <-  copy(stan_data$cntct_weekends_mean)
  stan_data$cntct_weekends_mean_shielded_age_band <- lapply(stan_data$cntct_weekends_mean_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  stan_data$cntct_school_closure_weekdays_shielded_age_band <-  copy(stan_data$cntct_school_closure_weekdays)
  stan_data$cntct_school_closure_weekdays_shielded_age_band <- lapply(stan_data$cntct_school_closure_weekdays_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  stan_data$cntct_school_closure_weekends_shielded_age_band <-  copy(stan_data$cntct_school_closure_weekends)
  stan_data$cntct_school_closure_weekends_shielded_age_band <- lapply(stan_data$cntct_school_closure_weekends_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  stan_data$cntct_elementary_school_reopening_weekdays_shielded_age_band <-  copy(stan_data$cntct_elementary_school_reopening_weekdays)
  stan_data$cntct_elementary_school_reopening_weekdays_shielded_age_band <- lapply(stan_data$cntct_elementary_school_reopening_weekdays_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  stan_data$cntct_elementary_school_reopening_weekends_shielded_age_band <-  copy(stan_data$cntct_elementary_school_reopening_weekends)
  stan_data$cntct_elementary_school_reopening_weekends_shielded_age_band <- lapply(stan_data$cntct_elementary_school_reopening_weekends_shielded_age_band, function(x){
    x[shielded_age_band_index, ] = min(shielded_contacts, x[shielded_age_band_index, ])
    x[,shielded_age_band_index] = min(shielded_contacts, x[shielded_age_band_index, ])
    return(x)
  })
  
  # date of shielding (start of forecast)
  stan_data$age_band_shielded_idx <- as.vector(sapply(dates,function(x)which(x==shielded_date)))

  return(stan_data)
}

#' @export
#' @keywords internal
#' @import data.table 
find_reopening_dates = function(ods, default_school_reopening_date){
  
  # states that have re-opened their schools before end of Edu data 
  tmp = ods[!School_Status %in% c("State ordered closure in effect (including states where openings are delayed)", "Full closure", 'Full closure in effect'), ]
  tmp[, schools_reopened_before_forecast := 1]
  tmp[, reopening_date_Edu_Week := min(date_EduWeek_data), by = "loc"]
  # if schools were opened at the beginning of the data set, school reopening date is fixed to the default
  tmp[reopening_date_Edu_Week == min(ods$date_EduWeek_data), reopening_date_Edu_Week := default_school_reopening_date]
  
  # states that have not re-opened their schools before end of Edu data 
  tmp1 = ods[!loc %in% tmp$loc]
  tmp1[,schools_reopened_before_forecast := 0]
  tmp1[, reopening_date_Edu_Week := default_school_reopening_date] # need to have a date format, will be overwitten
  
  tmp = rbind(tmp, tmp1)
  tmp = select(tmp, - c("School_Status", "date_EduWeek_data"))
  tmp = unique(tmp)
  
  return(tmp)
}

#' @export
#' @keywords internal
#' @import data.table 
find_closure_2_dates = function(ods, default_school_closure_2_date, df_reopening_dates){
  
  # states that have re-opened their schools before end of Edu data 
  tmp = ods[School_Status %in% c("State ordered closure in effect (including states where openings are delayed)", "Full closure", "Full closure in effect"), ]
  tmp = tmp[date_EduWeek_data > max(df_reopening_dates$reopening_date_Edu_Week)] # keep only date after school reopening
  tmp = tmp[!loc_label %in% df_reopening_dates[schools_reopened_before_forecast == F]$loc_label] # keep only state that reopened
  tmp[, schools_close_2_before_forecast := 1]
  tmp[, closure_2_date_Edu_Week := min(date_EduWeek_data), by = "loc"]
  
  # states that have not re-opened their schools before end of Edu data 
  tmp1 = ods[!loc %in% tmp$loc]
  tmp1[, schools_close_2_before_forecast := 0]
  tmp1[, closure_2_date_Edu_Week := default_school_closure_2_date] # need to have a date format, will be overwitten
  
  tmp = rbind(tmp, tmp1)
  tmp = select(tmp, - c("School_Status", "date_EduWeek_data"))
  tmp = unique(tmp)
  
  return(tmp)
}

#' @export
#' @keywords internal
#' @import data.table tidyr
aggregate_ifr_data_by_age = function(ifr.by.age, path_to_file_pop_us = GFNAME_us_population,  max_age){

ifr.by.age <- ifr.by.age %>% 
  dplyr::select(age, ifr_mean, ifr_cl, ifr_cu)
#    aggregate data on contacts by 1-year bands to data by desired age bands
pop_by_age <- readRDS(path_to_file_pop_us)
age_bands <- dplyr::select(pop_by_age[which(!is.na(pop_by_age$code)),], -Total) %>%
  reshape2::melt(id.vars = c("Region", "code")) %>% 
  dplyr::rename(age = variable, pop = value, state = Region) %>% dplyr::select(age) %>% dplyr::distinct()
age.cat <- sapply(strsplit(gsub("\\[|\\]|\\(|\\+", "", age_bands$age), "-"), as.numeric)
if (is.na(age.cat[[length(age.cat)]][2])) age.cat[[length(age.cat)]][2] <- 99
age.cat <- matrix(unlist(age.cat), ncol = 2, byrow = TRUE)

ifr.by.age <- ifr.by.age[1:which(ifr.by.age$age == max_age),] %>% # 0 to max_age
  dplyr::mutate(age.agg:= cut(age, breaks=c(age.cat[,1],100), right=FALSE, labels=seq_len(length(c(age.cat[,1],100))-1L))) %>%
  dplyr::group_by(age.agg) %>%
  dplyr::summarise(ifr_mean:= mean(ifr_mean), 
            ifr_cl:= mean(ifr_cl), 
            ifr_cu:= mean(ifr_cu)) %>%            
  dplyr::ungroup() %>%
  dplyr::rename(age= age.agg)

return(ifr.by.age)
}

#' @export
#' @keywords internal
#' @import data.table tidyr
stan_data_add_ifr_fixed_decay_rate = function(processed_data, ifr_decay_total, ifr_decay_month_start){
  
  states <- names(processed_data$dates) 
  
  dates = processed_data$dates
  
  timeidx_start_ifr_decay = vector(mode = 'integer', length = length(dates))
  
  # find time index start
  for(m in seq_along(names(dates))){
    # m = 1
    dates_m = data.table(date = dates[[m]], time_idx = 1:length(dates[[m]]))
    dates_m[, month := format(date, '%m')]
    timeidx_start_ifr_decay[m] = subset(dates_m, month == ifr_decay_month_start)$time_idx[1]
  }
  processed_data$stan_data$timeidx_start_ifr_decay = timeidx_start_ifr_decay
  
  # fix ifr total decay
  processed_data$stan_data$log_ifr_overall_upswing_effect = log(1 - ifr_decay_total)
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr
stan_data_add_ifr_var_decay_rate = function(processed_data, ifr_decay_total, ifr_decay_month_start){
  
  states <- names(processed_data$dates) 
  
  dates = processed_data$dates
  
  timeidx_start_ifr_decay = vector(mode = 'integer', length = length(dates))
  
  # find time index start
  for(m in seq_along(names(dates))){
    # m = 1
    dates_m = data.table(date = dates[[m]], time_idx = 1:length(dates[[m]]))
    dates_m[, month := format(date, '%m')]
    timeidx_start_ifr_decay[m] = subset(dates_m, month == ifr_decay_month_start)$time_idx[1]
  }
  processed_data$stan_data$timeidx_start_ifr_decay = timeidx_start_ifr_decay
  
  # fix ifr total decay
  processed_data$stan_data$lambda_log_ifr_overall_upswing_effect = -100*log(1 - ifr_decay_total)
  
  return(processed_data)
}

#' @export
#' @keywords internal
#' @import data.table
stan_data_add_rebound_mobility_zero = function(processed_data, default_rebound_mobility_zero_date){
  
  dates = processed_data$dates
  
  # find time index start
  for(m in seq_along(names(dates))){
    # m = 1
    dates_m = data.table(date = seq.Date(min(dates[[m]]), min(dates[[m]]) + processed_data$stan_data$N2 - 1, by = 'day'), time_idx = 1:processed_data$stan_data$N2)
    dates_idx_rebound_zero = dates_m[date == default_rebound_mobility_zero_date]$time_idx
    processed_data$stan_data$covariates[m,3,dates_idx_rebound_zero:processed_data$stan_data$N2,] = 0
  }

  return(processed_data)
}

