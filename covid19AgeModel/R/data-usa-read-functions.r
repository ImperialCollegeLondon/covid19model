#' @export
#' @keywords internal
#' @importFrom zoo rollmean
smooth_fn = function(x, days=3) {
  return(rollmean(x, days, align = "right"))
}

#' @export
#' @keywords internal
#' @import tidyr stringr 
#' @importFrom lubridate ymd
#' @importFrom dplyr bind_rows group_by left_join mutate rename select
read_death_data <- function(fname_jhu_death_data_padded, 
		fname_nyt_death_data_padded,
		fname_nyc_death_data_padded,
		fname_ihme_hospitalization, 		
		fname_states,
		fname_region,
		source, 
		smooth = FALSE)
{
  if (source == "jhu"){
    death_data <- readRDS(fname_jhu_death_data_padded)
  } else if (source ==  "nyt"){
    death_data <- readRDS(fname_nyt_death_data_padded)
  } else if (source == "ihme"){
    data <- read.csv(fname_ihme_hospitalization, stringsAsFactors = FALSE)
    data$date <- lubridate::ymd(data$date)
    data_subset <- dplyr::select(data, location_name, date, deaths_mean)
    states <- read.csv(fname_states, stringsAsFactors = FALSE)
    names(states) <- c("location_name", "code")
    data_subset <- dplyr::left_join(data_subset, states, by = "location_name")
    data_subset <- data_subset[which(!is.na(data_subset$code)),]
    # Choose deaths before current date 
    today <- Sys.Date() 
    death_data <- data_subset[which(data_subset$date < today),]
    death_data$daily_deaths <- as.integer(death_data$deaths_mean)
    death_data <- death_data %>%
      dplyr::group_by(code) %>%
      dplyr::mutate("cumulative_deaths" = cumsum(daily_deaths))
    death_data$daily_cases <- rep(0, length(death_data$daily_deaths))
  } else if (source == "jhu_adjusted"){
    death_data <- readRDS(fname_jhu_death_data_padded)
    nys_data <- read_death_data_v2()
    total_nys <- nys_data[which(nys_data$region == "total hospital deaths"),]
    nys_df <- data.frame("code" = rep("NY", length(total_nys$date)),
                         "date" = total_nys$date,
                         "cumulative_cases" = rep(0, length(total_nys$date)),
                          "cumulative_deaths" = total_nys$cumulative_death,
                         "daily_cases" = rep(0, length(total_nys$date)),
                         "daily_deaths" = total_nys$deaths)
    max_date <- max(nys_df$date)
    death_data <- death_data[which(death_data$code != "NY"),]
    death_data <- dplyr::bind_rows(death_data, nys_df)
    death_data <- death_data[which(death_data$date <= max_date),]
  } else if(source == "nyc"){
    death_data <- read.csv(fname_nyc_death_data_padded)
    var_date_of_interest = 'date_of_interest'
    if('DATE_OF_INTEREST' %in% names(death_data)) var_date_of_interest = 'DATE_OF_INTEREST'
    death_data <- data.table(death_data) %>%
      dplyr::mutate(code = "NYC",
             state = "New_York_City",
             date = as.Date(get(var_date_of_interest), format = "%m/%d/%y")) %>%
      dplyr::rename(Cases = CASE_COUNT, Deaths = DEATH_COUNT) %>%
      dplyr::select(Cases, Deaths, date, code, state) 
    
    dates_missing = seq.Date(as.Date("2020-02-01"), (min(death_data$date)-1), by = "days")
    death_data <- rbind(data.table(date = dates_missing,
                                   Cases = 0, Deaths = 0, code = "NYC", state = "New_York_City"), death_data)
    
    }
  
  death_data <- death_data[which(death_data$date >= lubridate::ymd("2020-02-1")),]
  
  if (smooth == TRUE){
    k = 3
    death_data$daily_deaths = c(rep(0, k-1), as.integer(smooth_fn(death_data$daily_deaths, days=k)))
  }
  
  #read regions
  regions <- read.csv(fname_region)
  if(source != "nyc") death_data <- merge(death_data, regions, by = 'code' )
  return(death_data)
}

#' @export
#' @keywords internal
#' @import data.table
read_deathByAge = function(path_to_file_DeathByAge)
{
	deathByAge = as.data.table( read.csv(path_to_file_DeathByAge) )
	deathByAge[,date := as.Date(date)]    
 	return(deathByAge)
}

#' @export
#' @keywords internal
#' @import data.table
read_emodo_cell_phone_contact_intensities_2 <- function(infile_emodo = NULL)
{
	#loc emo.age.label  loc_label       date avg_contacts n_contacts n_users weekend emo.age.cat
	if (is.null(infile_emodo))
	{
		infile_emodo <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_mobility_by_age_20200831.csv'		
	}
	
	dc <- as.data.table(read.csv(infile_emodo, stringsAsFactors=FALSE))
	
	#	rename cols	
	set(dc, NULL, c('STATEFP','src'), NULL)		
	setnames(dc, c('Age','NAME','abbr','day','n'), c('idx.age.label','loc_label','loc','date','n_users'))
	#	make date format
	set(dc, NULL, 'date', dc[, as.Date(date)])
		
	#	make weekend
	dc[, weekend:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('no','yes'))]
	
	#	make age buckets
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	tmp <- tmp[order(idx.age.label)]
	tmp[, idx.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='idx.age.label')
	setkey(dc, loc_label, date, idx.age.cat)
	setnames(dc, c('idx.age.label','idx.age.cat'), c('emo.age.label','emo.age.cat'))
	
	#	select loc with at least 2e4 users on avg	
	tmp <- dc[, list(n_users_mean=mean(n_users)), by=c('loc')]
	tmp <- subset(tmp, n_users_mean>2e4 & loc!='NC')
	dc <- merge(subset(tmp, select=loc), dc, by='loc')
	
	#	set name just for convenience
	setnames(dc, 'p_moving', 'avg_contacts')
	dc
}

#' @export
#' @keywords internal
#' @import data.table
read_emodo_cell_phone_contact_intensities <- function(infile_emodo = NULL, type='idx')
{
	if (is.null(infile_emodo))
	{		
		infile_emodo <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_contacts_by_age_20200830.csv'
	}
	
	dc <- as.data.table(read.csv(infile_emodo, stringsAsFactors=FALSE))
	
	#	rename cols	
	set(dc, NULL, 'STATEFP', NULL)	
	setnames(dc, c('individual_age','Age_groups','STATE_NAME','state_abbr','day','cnt_contacts'), c('idx.age.label','cont.age.label','loc_label','loc','date','n_contacts'))
		
	#	make date format
	set(dc, NULL, 'date', dc[, as.Date(date)])
	
	#	select active users
	dc <- subset(dc, src=='10m_dactive', select=-src)
	
	#	make total contacts of index indivivuals
	dc <- dc[, list(avg_contacts= sum(avg_contacts),
					n_contacts=as.double(sum(n_contacts)),
					n_users=d_users[1]), 
			by=c('loc','loc_label',paste0(type,'.age.label'),'date')]
		
	#	make weekend
	dc[, weekend:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('no','yes'))]
	
	#	make age buckets
	tmp <- unique(subset(dc, select=c(paste0(type,'.age.label'))))
	tmp <- tmp[order(get(paste0(type,'.age.label')))]
	tmp[, eval(quote(paste0(type,'.age.cat'))):= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by=paste0(type,'.age.label'))
	setkey(dc, loc_label, date)
	setkeyv(dc, paste0(type,'.age.label'))
	setnames(dc, c(paste0(type,'.age.label'),paste0(type,'.age.cat')), c('emo.age.label','emo.age.cat'))
				
	#	select loc with at least 2e4 users on avg	
	if(type=='idx'){
	  tmp <- dc[, list(n_users_mean=mean(n_users)), by=c('loc')]
	  tmp <- subset(tmp, n_users_mean>2e4 & loc!='NC')
	  dc <- merge(subset(tmp, select=loc), dc, by='loc') 
	}
	
	dc
}

#' @export
#' @keywords internal
#' @import data.table
read_emodo_cell_phone_data <- function(pop_info, infile_emodo = NULL)
{
	if (is.null(infile_emodo))
	{
		infile_emodo <- '~/Box/OR_Work/2020/2020_covid/data_examples/US_nr_contacts_US_state_age_distinct_active_weighted_10m_2020-07-14.csv'
	}

	dc <- as.data.table(read.csv(infile_emodo, stringsAsFactors=FALSE))
	
	#	rename cols	
	set(dc, NULL, 'STATEFP', NULL)	
	setnames(dc, c('individual_age','Age_groups','STATE_NAME','day'), c('idx.age.label','cont.age.label','loc_label','date'))
	
	#	make date format
	set(dc, NULL, 'date', dc[, as.Date(date)])
	
	#	make weekend
	dc[, weekend:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('no','yes'))]
	
	#	make age buckets
	tmp <- unique(subset(dc, select=c(idx.age.label)))
	tmp <- tmp[order(idx.age.label)]
	tmp[, idx.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='idx.age.label')
	tmp <- unique(subset(dc, select=c(cont.age.label)))
	tmp <- tmp[order(cont.age.label)]
	tmp[, cont.age.cat:= seq_len(nrow(tmp))]
	dc <- merge(dc, tmp, by='cont.age.label')
	setkey(dc, loc_label, date, idx.age.cat, cont.age.cat)
	setnames(dc, c('cont.age.label','idx.age.label','idx.age.cat','cont.age.cat'), c('emo.cont.age.label','emo.idx.age.label','emo.idx.age.cat','emo.cont.age.cat'))
	tmp <- unique(subset(dc, select=c(emo.idx.age.cat, emo.cont.age.cat)))
	tmp <- tmp[, list(emo.cont.age.cat= emo.cont.age.cat,
					emo.pair.age.cat= (emo.idx.age.cat-1)*max(tmp$emo.idx.age.cat)+emo.cont.age.cat) , 
			by='emo.idx.age.cat'  ]
	dc <- merge(dc, tmp, by=c('emo.idx.age.cat' ,'emo.cont.age.cat' ))
	dc[, emo.pair.age.label:= paste0(emo.idx.age.label,'->',emo.cont.age.label)]
	
	#	exclude Puerto Rico
	dc <- subset(dc, !grepl('Puerto', loc_label))
	
	#	TODO set Hawai Alaska NYC still awaiting data
	cat("\nWarning: making up data for NYC Alaska Hawaii")
	tmp <- subset(dc, loc_label=='New York')
	set(tmp, NULL, 'loc_label', 'New York City')
	dc <- rbind(dc, tmp)
	tmp <- subset(dc, loc_label=='Wyoming')
	set(tmp, NULL, 'loc_label', 'Alaska')
	dc <- rbind(dc, tmp)
	tmp <- subset(dc, loc_label=='California')
	set(tmp, NULL, 'loc_label', 'Hawaii')
	dc <- rbind(dc, tmp)
	
	#	check we have all states
	stopifnot( sort(unique(dc$loc_label))==sort(unique(pop_info$loc_label)) )
	dc <- merge(unique(subset(pop_info, select=c(loc,loc_label))), dc, by='loc_label')
	dc
}

#' @export
#' @keywords internal
#' @import data.table
read_foursquare_mobility <- function(pop_info, infile_fsq)
{	
	fsq <- as.data.table( read.csv(infile_fsq, stringsAsFactors = FALSE) )
	
	#	basic data processing
	setnames(fsq, c('dt'), c('date'))
	set(fsq, NULL, 'date', fsq[, as.Date(date, format='%m/%d/%Y')])
	set(fsq, NULL, 'norm_visits', fsq[, as.numeric(gsub('\\,','',norm_visits))])
	set(fsq, NULL, 'weekend', fsq[, factor( format(date, "%u") %in% c(6, 7), levels=c(FALSE,TRUE), labels=c('no','yes') )])
	setnames(fsq, 'state', 'loc')			
	
	#	handle NYC
	#	looking at the raw numbers it is likely that what is reported in NY is excluding NYC
	#	NY has 8.5m pop and NYC has 19.5m pop
	#	subset(fsq, date=='2020-05-03' & loc%in%c('NY','NYC'))
	set(fsq, NULL, 'loc', fsq[, gsub('New York, NY','NYC',loc)])		
	tmp <- dcast.data.table(subset(fsq, grepl('NY',loc)), age+date+weekend~loc, value.var='norm_visits')	
	tmp[, NY:= NY+NYC]
	tmp[, loc:='NY']
	set(tmp, NULL, 'NYC', NULL)
	setnames(tmp, 'NY', 'norm_visits')
	fsq <- rbind( subset(fsq, loc!='NY'), tmp )
	
	#	add age cats
	tmp <- unique(subset(fsq, select=age))
	setkey(tmp, age)
	tmp[, fsq.age.cat:= 1:nrow(tmp)]
	set(tmp, NULL, 'fsq.age.cat.label', tmp[, gsub('-plus','+',gsub('_','-',age))])
	set(tmp, NULL, 'fsq.age.cat.label', tmp[, factor(fsq.age.cat, levels=fsq.age.cat, labels=fsq.age.cat.label)])
	fsq <- merge(fsq, tmp, by='age')
	
	#	make per capita visits
	pop_by_age <- subset(pop_info, select=c(loc, age.cat.label, pop))
	tmp <- pop_by_age[, which(age.cat.label=='15-19')]
	set(pop_by_age, tmp, 'pop', pop_by_age[tmp, 2/5*pop])
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('15-19', '20-24'))], 'age.cat.label','18-24')
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('25-29','30-34'))], 'age.cat.label','25-34')
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('35-39','40-44'))], 'age.cat.label','35-44')
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('45-49','50-54'))], 'age.cat.label','45-54')
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('55-59','60-64'))], 'age.cat.label','55-64')	
	set(pop_by_age, pop_by_age[, which(age.cat.label%in%c('65-69','70-74','75-79','80-84','85+'))], 'age.cat.label','65+')
	pop_by_age <- pop_by_age[, list(pop=sum(pop)), by=c('loc','age.cat.label')]
	setnames(pop_by_age, c('age.cat.label'), c('fsq.age.cat.label'))	
	pop_by_age <- subset( pop_by_age, !fsq.age.cat.label%in%c('0-4','5-9','10-14') )	
	fsq <- merge(fsq, pop_by_age, by=c('loc','fsq.age.cat.label'), all.x=TRUE)
	stopifnot( nrow(subset(fsq, is.na(pop)))==0 )	
	fsq[, norm_visits_rate:= norm_visits/pop]
	
	#	exclude first two days and last day
	tmp <- range(fsq$date)	
	fsq <- subset(fsq, date>=tmp[1]+2 & date<=tmp[2]-1)
	
	#	clean up
	set(fsq, NULL, c('age','pop'), NULL)	
	set(fsq, NULL, 'weekend', fsq[,as.character(weekend)])
	tmp <- unique(subset(pop_info, select=c(loc,loc_label)))
	fsq <- merge(tmp, fsq, by='loc')	
	setkey(fsq, loc, fsq.age.cat, date)
	
	fsq
}

#' @export
#' @keywords internal
#' @import tidyr stringr
read_google_resnonrestrends<- function(fname)
{
	gm <- readRDS( file=fname )
	gm <- subset(gm, select=-c(idx,residential,nonresidential, is_weekend, week, nonresidential_CL, nonresidential_CU, residential_CL, residential_CU))
	gm
}

#' @export
#' @keywords internal
#' @import tidyr stringr 
#' @importFrom dplyr left_join
read_google_mobility <- function(fname_global_mobility_report, fname_states)
{
  states <- read.csv(fname_states, stringsAsFactors = FALSE)
  names(states) <- c("sub_region_1", "code")
  google_mobility <- read.csv(fname_global_mobility_report, stringsAsFactors = FALSE)
  google_mobility <- google_mobility[which(google_mobility$country_region_code == "US"),]
  #Remove county level data
  google_mobility <- google_mobility[which(google_mobility$sub_region_2 == ""),]
  google_mobility <- dplyr::left_join(google_mobility, states, by = c("sub_region_1"))
  # Format the google mobility data
  google_mobility$date = as.Date(google_mobility$date, format = '%Y-%m-%d')
  google_mobility[, c(6:11)] <- google_mobility[, c(6:11)]/100
  #google_mobility[, c(6:10)] <- google_mobility[, c(6:10)] * -1
  names(google_mobility) <- c("country_region_code", "country_region", "sub_region_1", "sub_region_2",
                              "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                              "workplace", "residential", "code")
  
  return(google_mobility)
}

#' @export
#' @keywords internal
#' @import tidyr stringr 
#' @importFrom lubridate ymd
#' @importFrom dplyr left_join select
read_visitdata_mobility <- function(fname_states, fname_grouped){
  states <- read.csv(fname_states, stringsAsFactors = FALSE)
  names(states) <- c("state", "code")
  data <- read.csv(fname_grouped, stringsAsFactors = FALSE)
  data$date <- lubridate::ymd(data$date)
  
  data <- data %>%
    dplyr::select(date, state, categoryName, visitIndex) %>%
    spread(key = categoryName, value = visitIndex)
  
  data <- dplyr::left_join(data, states, by = c("state"))
  data_subset <- dplyr::select(data, state, date, "Shops & Services", "Grocery", "Outdoors & Recreation", 
                        "Gas Stations", "Professional & Other Places", "Fast Food Restaurants", "Medical", code)
  names(data_subset) <- c("sub_region_1", "date", "shops.services", "grocery", "outdoors.recreation", 
                          "gasstations", "professional", "fast.food.restaurants", "medical", "code")

  
  # data has 100 as baseline - now rescale  between -1 and 1.
  # Choose to multiply all by -1 as they all seem to decrease
  data_subset[,3:9] <- -1*(data_subset[,3:9] - 100)/100
  data_subset[is.na(data_subset)] <- 0
  
  return(data_subset)
}

#' @export
#' @keywords internal
#' @import data.table
read_ifr_data_by_age <- function(path_to_file_ifrbyage)
{
	ifr.by.age <- as.data.table(read.csv(path_to_file_ifrbyage))
	return(ifr.by.age)
}

#' @export
#' @keywords internal
#' @import data.table
read_us_state_areas <- function(path_to_file)
{
	#	ref https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
	darea <- as.data.table(read.csv(path_to_file))
	setnames(darea, c('State','LandArea'), c('loc_label','land_area_sqm'))
	set(darea, NULL, 'loc_label', darea[, as.character(loc_label)])
	#	source for NYC: wikipedia
	darea <- rbind(darea, data.table(loc_label='New York City', land_area_sqm= 784), fill=TRUE)
	darea <- subset(darea, select=c(loc_label, land_area_sqm))
	darea
}

#' @export
#' @keywords internal
#' @import tidyr stringr 
#' @importFrom dplyr rename select
read_pop_count_us = function(path_to_file){
  pop_count <- readRDS(path_to_file)
  pop_count <- dplyr::select(pop_count[which(!is.na(pop_count$code)),], c("Region", "code", "Total")) %>%
    dplyr::rename(popt = Total) 
  
  return(pop_count)
}

#' @export
#' @keywords internal
#' @import tidyr stringr  
#' @importFrom dplyr group_by inner_join mutate rename select summarise
read_pop_count_by_age_us = function(path_to_file){
	pop_by_age <- readRDS(path_to_file)
	pop_by_age <- dplyr::select(pop_by_age[which(!is.na(pop_by_age$code)),], -Total) %>%
		reshape2::melt(id.vars = c("Region", "code")) %>%
	  dplyr::rename(age = variable, pop = value, state = Region)
	  
	pop_by_age <- pop_by_age %>%
	  dplyr::group_by(state) %>%
	  dplyr::summarise(pop_sum:= sum(pop))	%>%
	  dplyr::inner_join(pop_by_age) %>%
	  dplyr::mutate(pop= pop/pop_sum) 	
	
	return(pop_by_age)
}

#' @export
#' @keywords internal
#' @import data.table
read_contact_rate_matrix <- function(pop_info, path_to_file_contact)
{	
	#	estimates from Melodie Monod using INLA as in van Kassteele AOAS 2017
	load(path_to_file_contact)
	dp <- as.data.table(polymod.tab)
	
	#	add proportion of population in each age band for participants and their contacts
	tmp <- unique(subset(dp, select=c(cont.age, T)))
	tmp[, cont.pop.prop := T/sum(tmp$T)]
	tmp[, T:=NULL]
	dp <- merge(dp, tmp, by='cont.age')
	setnames(tmp, c('cont.age','cont.pop.prop'), c('part.age','part.pop.prop'))
	dp <- merge(dp, tmp, by='part.age')
	
	#	make age map
	da <- unique(subset(pop_info, select=c(age.cat, age.cat.label, age.cat.from, age.cat.to)))
	cat('\nAggregating elements in contact rate matrix to age bands', da$age.cat.label, '...')
	setnames(da, c('age.cat','age.cat.label','age.cat.from','age.cat.to'), c('age.cat.agg','age.cat.agg.label','age.cat.agg.from','age.cat.agg.to'))
	da <- da[, list(age.cat=seq.int(age.cat.agg.from, age.cat.agg.to, 1)), by=c('age.cat.agg','age.cat.agg.label')]
	
	#	add age map
	setnames(da, c('age.cat','age.cat.agg','age.cat.agg.label'), c('part.age','part.age.cat.agg','part.age.cat.agg.label'))
	dp <- merge(da, dp, by='part.age')
	setnames(da, c('part.age','part.age.cat.agg','part.age.cat.agg.label'), c('cont.age','cont.age.cat.agg','cont.age.cat.agg.label'))
	dp <- merge(da, dp, by='cont.age')
	
	#	aggregate by 
	#	taking weighted mean for contact rates
	#	summing expected number of contacts, with weight by index person			
	tmp <- dp[, {
				w <- part.pop.prop/sum(part.pop.prop)*sqrt(length(part.pop.prop)) * cont.pop.prop/sum(cont.pop.prop)*sqrt(length(cont.pop.prop))
				list(c= sum(w*c))
			}, by=c('part.age.cat.agg','part.age.cat.agg.label','cont.age.cat.agg','cont.age.cat.agg.label')]
	dp <- dp[, {					
				list(m= sum(m))
			}, by=c('part.age','part.pop.prop','part.age.cat.agg','part.age.cat.agg.label','cont.age.cat.agg','cont.age.cat.agg.label')]
	dp <- dp[, {			
				w <- part.pop.prop/sum(part.pop.prop)
				list(m= sum(w*m))
			}, by=c('part.age.cat.agg','part.age.cat.agg.label','cont.age.cat.agg','cont.age.cat.agg.label')]
	dp <- merge(dp, tmp, by=c('part.age.cat.agg','part.age.cat.agg.label','cont.age.cat.agg','cont.age.cat.agg.label'))		
	setnames(dp, colnames(dp), gsub('\\.agg','',colnames(dp)))
		
	dp
}

#' @export
#' @keywords internal
#' @import data.table
read_emodo_cell_phone_contact_intensities_with_cont <- function(infile_emodo = NULL)
{
  if (is.null(infile_emodo))
  {
    infile_emodo <- '~/Box/OR_Work/2020/2020_covid/data_examples/emodo_contacts_by_age_20200729.csv'
  }

  dc <- as.data.table(read.csv(infile_emodo, stringsAsFactors=FALSE))

  #	rename cols
  set(dc, NULL, 'STATEFP', NULL)
  setnames(dc, c('individual_age','Age_groups','STATE_NAME','state_abbr','day','cnt_contacts'), c('idx.age.label','cont.age.label','loc_label','loc','date','n_contacts'))

  #	make date format
  set(dc, NULL, 'date', dc[, as.Date(date)])

  #	select active users
  dc <- subset(dc, src=='10m_dactive', select=-src)
  setkey(dc,idx.age.label ,cont.age.label,loc,date)

  #	make weekend
  dc[, weekend:= factor(!format(date, "%u") %in% c(6, 7), levels=c(TRUE,FALSE), labels=c('no','yes'))]

  #	make age buckets
  tmp <- unique(subset(dc, select=c(idx.age.label)))
  tmp <- tmp[order(idx.age.label)]
  tmp[, idx.age.cat:= seq_len(nrow(tmp))]
  dc <- merge(dc, tmp, by='idx.age.label')
  setnames(tmp,colnames(tmp),gsub('idx.','cont.',colnames(tmp)))
  dc <- merge(dc, tmp, by='cont.age.label')
  setkey(dc, loc_label, date, idx.age.cat,cont.age.label)

  dc
}

#' @export
#' @keywords internal
#' @import data.table tidyr
load_EduWeekly_data = function(path_to_file_school_EduWeek_data, path_to_file_school_Ox_data) {
  
  data = as.data.table( read.csv(path_to_file_school_EduWeek_data))
  data = select(data, State, State_Abbr, Status, date)
  data[, date:= as.Date(as.character(date))]
  
  # the encoding changed from the 5th october
  ENCODING_CHANGE_DATE1 <- as.Date("2020-10-05", format="%Y-%m-%d")
  ENCODING_CHANGE_DATE2 <- as.Date("2020-11-25", format="%Y-%m-%d")
  
  data[, new_encoding1:= date>=ENCODING_CHANGE_DATE1]
  data[, new_encoding2:= date>=ENCODING_CHANGE_DATE2]
  
  #
  # Remove territories
  data = subset(data, !State %in% c("Puerto Rico"))
  
  #
  # Use NY for NYC
  tmp = subset(data, State_Abbr == "NY")
  tmp[, State_Abbr := "NYC"]
  tmp[, State := "New York City"]
  data = rbind(data, tmp)
  
  #
  # Clean
  setnames(data, c("State", "State_Abbr", "Status"), c("loc_label", "loc", "School_Status"))
  data[, School_Status := factor(School_Status, levels = c("State-ordered in-person instruction available part-time or full-time*",
                                                           "State ordered closure in effect (including states where openings are delayed)",
                                                           "State-ordered regional closure in effect",
                                                           "Varies by school/district/dependent on local health authorities",
                                                           "Only hybrid or remote instruction allowed",
                                                           "No order",
                                                           "Partial closure",
                                                           "Full closure",
                                                           "Ordered open",
                                                           "No order in effect",
                                                           "Partial closure in effect",
                                                           "Full closure in effect"))]
  stopifnot(sum(is.na(data))==0)
  
  #
  # Correct Status for Maine 
  data[date %in% as.Date(c("2020-08-19", "2020-08-21", "2020-08-28")) & loc_label == "Maine", School_Status := "State ordered closure in effect (including states where openings are delayed)" ]
  # "However, K-12 schools and universities remain closed as of 20th August," so we should indeed keep schools closed for that week.
  
  #
  # Correct Status for New York (state).
  data[date %in% as.Date(c("2020-08-19", "2020-08-21", "2020-08-28")) & loc_label == "New York", School_Status := "State ordered closure in effect (including states where openings are delayed)"]
  # No New York schools opened before 29th September. Source: https://edition.cnn.com/2020/09/27/us/nyc-schools-reopening-latest/index.html
  
  #
  # sanity check, compare to Oxford data
  check_EduWeekly_data(data, path_to_file_school_Ox_data) 
  
  #
  # Clean
  setnames(data, "date", "date_EduWeek_data")
  data = select(data, -new_encoding1, -new_encoding2)
  
  return(data)
}

#' @export
#' @keywords internal
#' @import data.table tidyr 
check_EduWeekly_data = function(Edu_data, path_to_file_school_Ox_data) {
  
  #
  # load Oxford data
  school_intervention <- as.data.table(read.csv(path_to_file_school_Ox_data))
  tmp = subset(school_intervention, !is.na(RegionName) & !is.na(Date) & RegionName != "" & !is.na(C1_School.closing))
  tmp[, date := as.Date(as.character(Date), format= "%Y%m%d")]
  tmp[, loc := gsub("US_(.+)","\\1",RegionCode)]
  tmp[, loc_label := RegionName]
  
  # remove territories and other countried
  tmp = subset(tmp, !loc_label %in% c("Virgin Islands", "Wales", "Scotland", "Puerto Rico", "Northern Ireland", "England"))
  
  # Change Washington DC to District of Columbia
  tmp[loc_label == "Washington DC", loc_label:= "District of Columbia"]
  
  # Create  School status
  tmp1 = subset(tmp, C1_School.closing == 3)
  tmp1[, School_Status := ifelse(C1_Flag == 0, "3T", "3G")]
  tmp2 = subset(tmp, C1_School.closing == 2)
  tmp2[, School_Status := ifelse(C1_Flag == 0, "2T", "2G")]
  tmp1 = rbind(tmp1, tmp2)
  tmp2 = subset(tmp, C1_School.closing == 1)
  tmp2[, School_Status := ifelse(C1_Flag == 0, "1T", "1G")]
  tmp1 = rbind(tmp1, tmp2)
  tmp2 = subset(tmp, C1_School.closing == 0)
  tmp2[, School_Status := "No measure"]
  tmp = rbind(tmp1, tmp2)
  
  # Clean
  tmp = dplyr::select(tmp, loc, loc_label, School_Status, date)
  
  #
  # Correct Status for Michigan 
  tmp[(date %in% c(as.Date("2020-08-19"), as.Date("2020-08-21"), as.Date("2020-08-28"))) & loc_label == "Michigan", School_Status := "3T"]
  # Gov. Gretchen Whitmer said in-person learning can only occur in a region that's in at least phase four of her reopening plan. 
  # The state legislature passed a measure that says districts must reconfirm their plans on how to deliver instruction every 30 days. 
  # Districts that reopen for in-person instruction must prioritize K-5 students.
  
  #
  # Correct Status for Virginia
  tmp[date %in% c(as.Date("2020-08-19"), as.Date("2020-08-21")) & loc_label == "Virginia", School_Status := "2T"]
  # The state is currently requiring that in-person instruction be accompanied by strict social distancing measures, which means many 
  # districts will have to blend remote and face-to-face learning. But Gov. Ralph Northam also said Virginia won't reopen schools if infection 
  #rates start to rise, as is happening in some regions of the state.
  
  #
  # Correct status for Kentucky
  tmp[date %in% c(as.Date("2020-11-25")) & loc_label == "Kentucky", School_Status := "3G"]
  # "Kentucky schools and death of 15-year-old girl prompts statewide school closures": https://www.wsws.org/en/articles/2020/11/28/scho-n28.html
  # "Beshear stops all in-person K-12 classes in Kentucky...": https://www.kentucky.com/news/local/education/article247273244.html
  # Effective from 23rd Nov with possibility of schools reopining in low-infection rate counties 7 Dec
  
  # Check if EdWeekly is cured
  setnames(tmp, "School_Status", "School_Status_Ox")
  tmp1 = merge(tmp, Edu_data, by = c("loc", "loc_label", "date"))
  
  tmp1[, disagreement := ifelse( School_Status_Ox == "3G" & !School_Status %in% c("State ordered closure in effect (including states where openings are delayed)", "Full closure", "Full closure in effect"), 1, 0)]
  
  if(sum(tmp1$disagreement) != 0) {
    mismatches <- tmp1 %>% dplyr::filter(disagreement==1)
    cat(sprintf("The EduWeek and Oxford datasets diasagree for the following %d rows:\n", nrow(mismatches)))
    print(mismatches %>% select(loc_label, date, School_Status_Ox, School_Status))
    stop("Disagreements between Eduweek and Oxford school closing datasets")
  }
}

#' @export
#' @keywords internal
#' @import data.table
read_seroprevalence_data <- function(path_to_cdc_survey, path_to_cross_sectional_national_survey, with_cross_sectional_national = 0)
{
	#path_to_cdc_survey <- args$file_cdc_survey; path_to_cross_sectional_national_survey <- args$file_cross_sectional_national; with_cross_sectional_national <- 0
	spd <- as.data.table( read.csv(path_to_cdc_survey, stringsAsFactor=FALSE) )
	if(with_cross_sectional_national)
	{
		tmp1 <- as.data.table( read.csv( path_to_cross_sectional_national_survey, stringsAsFactor=FALSE ) )
		tmp1[, round := 1]
		tmp1[, age_band := "overall"]
		spd = rbind(spd, tmp1)
	}
	spd[, date := as.Date(date_end, format = "%d/%m/%Y")]
	spd[, date_start := as.Date(date_start, format = "%d/%m/%Y")]
	spd[, date_end := as.Date(date_end, format = "%d/%m/%Y")]
	spd[is.na(X18plus), X18plus := 0]
	set(spd, NULL, 'M_obs', spd[, M_obs/100])
	set(spd, NULL, 'CL_obs', spd[, CL_obs/100])
	set(spd, NULL, 'CU_obs', spd[, CU_obs/100])	
	setnames(spd, "state", "loc")
	spd <- subset(spd, !is.na(date) & !is.na(M_obs) & !is.na(CL_obs) & !is.na(CU_obs) )
	spd
}
