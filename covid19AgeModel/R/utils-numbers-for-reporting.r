
#' @export
#' @keywords internal
#' @import data.table
make_tables_for_paper <- function(pop_info, stan_data, dates, deathByAge_data, death_data,max_jhu, 
		max_ny,mobility_data, states,outfile.base=NA_character_)
{
	# Produces two rds
	
	# 1. "dates_data"  dates with overall death and death by age, max NYC and JYT date, max predicted date, rebound date....
	table_date <- unique(subset(pop_info,select=c('loc','loc_label')))
	setnames(table_date, c('loc','loc_label'), c('code','state'))
	regions <- names(dates)
	
	deathByAge$date = as.Date(deathByAge$date)
	
	# with death by age data
	regions_a = names(deathByAge_data$A_AD)
	dates_a = dates[names(dates) %in% regions_a]
	# match index
	dates_a = dates_a[regions_a]
	dataByAgestart = stan_data$dataByAgestart[match(regions_a, regions[stan_data$map_country[,2]]) ]
	A_AD = stan_data$A_AD[match(regions_a, regions[stan_data$map_country[,2]]) ]
	
	# NB: we add 30 to the first date because the first day included in the likelihood is the 31th day (epidemicStart) 
	tmp_a <- data.table(code = regions_a,
			first_day_overall = as.Date(unlist(lapply(lapply(dates_a, as.character), "[[", 1))) + 30,
			last_day_overall = as.Date(  unlist( sapply(1:length(regions_a), function(i) lapply(dates_a, as.character)[[i]][dataByAgestart[i]] ) ) ) -1,
			first_day_age = as.Date(  unlist( sapply(1:length(regions_a), function(i) lapply(dates_a, as.character)[[i]][dataByAgestart[i]] ) ) ),
			last_day_age = as.Date(  unlist( sapply(1:length(regions_a), function(i) lapply(dates_a, as.character)[[i]][length(dates_a[[i]])]  ) ) ),
			n_agegroups =  A_AD)
	tmp_a[, date.overall := paste0( format(first_day_overall, "%B %d, %Y") , " - ", format(last_day_overall, "%B %d, %Y"))]
	tmp_a[, date.deathbyage := paste0( format(first_day_age, "%B %d, %Y"), " - ", format(last_day_age, "%B %d, %Y"))]
	tmp_a[, number_days := last_day_age- first_day_overall+1 ]
	
	tmp_a = select(tmp_a, code, date.overall, date.deathbyage, n_agegroups, number_days)
	
	# without death by age data
	regions_o = regions[!regions %in% regions_a]
	if(length(regions_o)>0){
		dates_o = dates[
				names(dates) %in% regions_o]
		dates_o = dates_o[regions_o]
		tmp_o <- data.table( code = regions_o,
				first_day_overall = as.Date(unlist(lapply(lapply(dates_o, as.character), "[[", 1))) + 30, 
				last_day_overall = as.Date(  unlist( sapply(1:length(regions_o), function(i) lapply(dates_o, as.character)[[i]][length(dates_o[[i]])]  ) ) ) )
		tmp_a[, date.overall := paste0( format(first_day_overall, "%B %d, %Y") , " - ", format(last_day_overall, "%B %d, %Y"))]
		tmp_a[, date.deathbyage := "-"]
		tmp_a[, number_days := last_day_age- last_day_overall+1 ]
		tmp_a[, n_agegroups := "-" ]
		
		tmp = rbind(tmp_a, tmp_o)
	}else{
		tmp = tmp_a
	}
	
	table_date <- merge(table_date, select(tmp, -number_days), by = "code", all.x=TRUE)  
	
	# find first date with data 
	mindate=subset(deathByAge) %>% group_by(code) %>% summarise(mindate = format(min(date), "%B %d, %Y"))
	table_date = merge(table_date, mindate, by = "code", all.x=TRUE)
	table_date <- subset(table_date, select=-code)
	table_date[is.na(table_date)]= "-"
	table_date <- table_date[order(state)]
	
	# last date with death by age across states
	last_day_death_by_age = format( max(as.Date(  unlist( sapply(1:length(regions_a), function(i) lapply(dates_a, as.character)[[i]][length(dates_a[[i]])]  ) ) ) )   , "%B %d, %Y")
	
	# last_day - JHU
	tmp_jhu <- format( c(min(death_data$date), max_jhu)   , "%B %d, %Y")
	# last_day - NYT
	tmp_nyt <- format( c(min(death_data$date), max_ny)   , "%B %d, %Y")
	
	# last_day with foursquare data and number of days predicted
	last_day_fsq = format(max(mobility_data$date), "%B %d, %Y")
	last_7days_fqs = paste0( format(max(mobility_data$date) - 7 +1 , "%B %d, %Y") , " - ", format(max(mobility_data$date) , "%B %d, %Y") )
	last_sunday = (max(mobility_data$date) - 0:7)[which(weekdays(max(mobility_data$date) - 0:7) == "Sunday")] 
	last_week_fqs = paste0( format( last_sunday - 6 , "%B %d, %Y") , " - ", format( last_sunday , "%B %d, %Y") )
	
	
	
	# number of observational point
	num_obs_days = sum(tmp$number_days)
	num_obs_days = format(round(as.numeric(num_obs_days), 1), big.mark=",")
	
	# number of days to simulate
	N2 = stan_data$N2
	
	## number of states with death by age data
	
	num.statesd_woDCNYC = sum(unique(deathByAge$code) %notin% c("DC", "NYC"))
	states_byage_included = paste0(num.statesd_woDCNYC, " US states")
	if("NYC" %in% states & "DC" %notin% states) states_byage_included = paste0(states_byage_included, " and New York City")
	if("DC" %in% states & "NYC" %notin% states) states_byage_included = paste0(states_byage_included, " and the District of Columbia")
	if("DC" %in% states & "NYC" %in% states) states_byage_included = paste0(states_byage_included, ", the District of Columbia and New York City")
	
	## number of states in the analysis and names
	num.states_woDCNYC = sum(states %notin% c("DC", "NYC"))
	states_included = paste0(num.states_woDCNYC, " US states")
	if("NYC" %in% states & "DC" %notin% states) states_included = paste0(states_included, " and New York City")
	if("DC" %in% states & "NYC" %notin% states) states_included = paste0(states_included, " and the District of Columbia")
	if("DC" %in% states & "NYC" %in% states) states_included = paste0(states_included, ", the District of Columbia and New York City")
	
	
	## range of rebound
	rebound_range = format( range(mobility_data$rebound_date), "%B %d, %Y")
	rebound_range = paste0(rebound_range[1], " to ", rebound_range[2])
	
	# abbreviation of the first and last state
	first_last_state = c(states[order(states)][1], states[order(states)][length(states)])
	
	dates_data = list(dates.death.available = table_date, 
			n.countries.deathbyage = stan_data$M_AD, 
			last.day.jhu = tmp_jhu, 
			last.day.nyt = tmp_nyt, 
			last.date.fsq = list(last_day_fsq = last_day_fsq, last_7days_fqs= last_7days_fqs, last_week_fqs = last_week_fqs), 
			n.days.simulate = N2, 
			states_included = states_included, 
			dates.rebound = rebound_range,
			last_day_death_by_age = last_day_death_by_age,
			states_byage_included = states_byage_included, 
			num_obs_days = num_obs_days,
			first_last_state = first_last_state)
	
	
	# 2. "median_age" median age groups in Utah and Maine
	tmp_national = pop_info %>%
			group_by(age.cat.label) %>%
			summarise(pop = sum(pop)) %>%
			ungroup() 
	median_age_us = tmp_national$age.cat.label[which(cumsum(tmp_national$pop)/sum(tmp_national$pop) > 0.5)[1]]
	
	tmp_state = pop_info %>%
			group_by(loc_label) %>%
			summarise(median_age.cat.label = age.cat.label[which(cumsum(pop)/sum(pop) > 0.5)[1]],
					median_age.cat = age.cat[which(cumsum(pop)/sum(pop) > 0.5)[1]])
	
	tmp_state_min = subset(tmp_state, median_age.cat == min(median_age.cat) )
	tmp_state_max = subset(tmp_state, median_age.cat == max(median_age.cat))
	
	median_age = list( median_age_us,
			paste( tmp_state_min$loc_label , collapse = ", "), unique(tmp_state_min$median_age.cat.label),
			paste( tmp_state_max$loc_label , collapse = ", "), unique(tmp_state_max$median_age.cat.label)
	)
	
	# 3. states where school reopening and number of days observed
	idx_oc_school_reopened = sapply(1:stan_data$M, function(m)  stan_data$SCHOOL_STATUS[stan_data$N[m],m] != 1 )
	loc_school_reopened = names(dates)[idx_oc_school_reopened]
	n_loc_total = length(idx_oc_school_reopened)
	n_loc_school_reopened = length(loc_school_reopened)
	n_days_school_reopened = 0
	if(sum(idx_oc_school_reopened) != 0) n_days_school_reopened = sum(sapply((1:stan_data$M)[idx_oc_school_reopened], function(m) length(stan_data$elementary_school_reopening_idx[m]:stan_data$N[m]) ))
	n_days_school_reopened_pretty = format(n_days_school_reopened, big.mark=",")
	
	loc_where_school_reopened = list(list(n_loc_school_reopened, n_loc_total),
	                                 n_days_school_reopened_pretty)
	
	
	if(!is.na(outfile.base))
	{
		cat('\nWriting ',paste0(outfile.base,'-dates_data.rds'),' ...')
		saveRDS(dates_data, file = paste0(outfile.base,'-dates_data.rds'), version = 2)
		
		cat('\nWriting ',paste0(outfile.base,'-median_age.rds'),' ...')
		saveRDS(median_age, file = paste0(outfile.base,'-median_age.rds'), version = 2)
		
		cat('\nWriting ',paste0(outfile.base,'-loc_where_school_reopened.rds'),' ...')
		saveRDS(loc_where_school_reopened, file = paste0(outfile.base,'-loc_where_school_reopened.rds'), version = 2)
		
	}
	
}
