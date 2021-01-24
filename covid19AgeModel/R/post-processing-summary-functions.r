#' @export
#' @keywords internal
#' @import data.table
add_pop_info_age_school_children <- function(pop_info)
{
  pop_info[, age.cat.new := as.character(age.cat)]
  
  tmp1 = pop_info[age.cat.label == "10-14", ]
  tmp1[, age.cat.label := "10-11"]
  tmp1[, prop_pop := prop_pop * 2 / 5]
  tmp1[, pop := round(pop * 2 / 5)]
  tmp1[, age.cat.from := 10]
  tmp1[, age.cat.to := 11]
  tmp1[, age.cat.new := "3_child"]
  
  tmp2 = pop_info[age.cat.label == "10-14", ]
  tmp2[, age.cat.label := "12-14"]
  tmp2[, prop_pop := prop_pop * 3 / 5]
  tmp2[, pop := round(pop * 3 / 5)]
  tmp2[, age.cat.from := 12]
  tmp2[, age.cat.to := 14]
  
  pop_info = rbind(pop_info[age.cat.label != "10-14", ], tmp1, tmp2)
  
  return(pop_info)
}

#' @export
#' @keywords internal
#' @import data.table
add_pop_info_age_school_children_teen <- function(pop_info)
{
  pop_info[, age.cat.new := as.character(age.cat)]
  
  tmp1 = pop_info[age.cat.label == "15-19", ]
  tmp1[, age.cat.label := "15-18"]
  tmp1[, prop_pop := prop_pop * 4 / 5]
  tmp1[, pop := round(pop * 4 / 5)]
  tmp1[, age.cat.from := 15]
  tmp1[, age.cat.to := 18]
  tmp1[, age.cat.new := "4_teen"]
  
  tmp2 = pop_info[age.cat.label == "15-19", ]
  tmp2[, age.cat.label := "19-19"]
  tmp2[, prop_pop := prop_pop * 1 / 5]
  tmp2[, pop := round(pop * 1 / 5)]
  tmp2[, age.cat.from := 19]
  tmp2[, age.cat.to := 19]
  
  pop_info = rbind(pop_info[age.cat.label != "15-19", ], tmp1, tmp2)
  
  return(pop_info)
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_7_school_chidren <- function(pop_info)
{
  da <- unique(subset(pop_info, select=c(age.cat, age.cat.new, age.cat.label, age.cat.from, age.cat.to)))
  da[, age.cat2:= NA_integer_]
  set(da, da[, which(age.cat.to<=11)], 'age.cat2', 1L)
  set(da, da[, which(age.cat.from>11 & age.cat.to<=19)], 'age.cat2', 2L)
  set(da, da[, which(age.cat.from>19 & age.cat.to<=34)], 'age.cat2', 3L)
  set(da, da[, which(age.cat.from>34 & age.cat.to<=49)], 'age.cat2', 4L)
  set(da, da[, which(age.cat.from>49 & age.cat.to<=64)], 'age.cat2', 5L)
  set(da, da[, which(age.cat.from>64 & age.cat.to<=79)], 'age.cat2', 6L)
  set(da, da[, which(age.cat.from>79 )], 'age.cat2', 7L)
  set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
  tmp <- data.table(age.cat2= 1:7, age.cat2.label=c('0-11','12-19','20-34','35-49','50-64','65-79','80+'))
  da <- merge(da, tmp, by='age.cat2')
  
  da[, multiplier := 1]
  da[age.cat.new == "3_child", multiplier := 2/5]
  da[age.cat.new == "3", multiplier := 3/5]
  
  da
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_7_school_chidren_teen <- function(pop_info)
{
  da <- unique(subset(pop_info, select=c(age.cat, age.cat.new, age.cat.label, age.cat.from, age.cat.to)))
  da[, age.cat2:= NA_integer_]
  set(da, da[, which(age.cat.to<=9)], 'age.cat2', 1L)
  set(da, da[, which(age.cat.from>9 & age.cat.to<=18)], 'age.cat2', 2L)
  set(da, da[, which(age.cat.from>18 & age.cat.to<=34)], 'age.cat2', 3L)
  set(da, da[, which(age.cat.from>34 & age.cat.to<=49)], 'age.cat2', 4L)
  set(da, da[, which(age.cat.from>49 & age.cat.to<=64)], 'age.cat2', 5L)
  set(da, da[, which(age.cat.from>64 & age.cat.to<=79)], 'age.cat2', 6L)
  set(da, da[, which(age.cat.from>79 )], 'age.cat2', 7L)
  set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
  tmp <- data.table(age.cat2= 1:7, age.cat2.label=c('0-9','10-18','19-34','35-49','50-64','65-79','80+'))
  da <- merge(da, tmp, by='age.cat2')
  
  da[, multiplier := 1]
  da[age.cat.new == "4_teen", multiplier := 4/5]
  da[age.cat.new == "4", multiplier := 1/5]
  
  da
}


#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_7 <- function(pop_info)
{
  da <- unique(subset(pop_info, select=c(age.cat, age.cat.label, age.cat.from, age.cat.to)))
  da[, age.cat2:= NA_integer_]
  set(da, da[, which(age.cat.to<=9)], 'age.cat2', 1L)
  set(da, da[, which(age.cat.from>9 & age.cat.to<=19)], 'age.cat2', 2L)
  set(da, da[, which(age.cat.from>19 & age.cat.to<=34)], 'age.cat2', 3L)
  set(da, da[, which(age.cat.from>34 & age.cat.to<=49)], 'age.cat2', 4L)
  set(da, da[, which(age.cat.from>49 & age.cat.to<=64)], 'age.cat2', 5L)
  set(da, da[, which(age.cat.from>64 & age.cat.to<=79)], 'age.cat2', 6L)
  set(da, da[, which(age.cat.from>79 )], 'age.cat2', 7L)
  set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
  tmp <- data.table(age.cat2= 1:7, age.cat2.label=c('0-9','10-19','20-34','35-49','50-64','65-79','80+'))
  da <- merge(da, tmp, by='age.cat2')
  da
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_6 <- function(pop_info)
{
	da <- unique(subset(pop_info, select=c(age.cat, age.cat.label, age.cat.from, age.cat.to)))
	da[, age.cat2:= NA_integer_]
	set(da, da[, which(age.cat.to<=44)], 'age.cat2', 1L)
	set(da, da[, which(age.cat.from>44 & age.cat.to<=54)], 'age.cat2', 2L)
	set(da, da[, which(age.cat.from>54 & age.cat.to<=64)], 'age.cat2', 3L)
	set(da, da[, which(age.cat.from>64 & age.cat.to<=74)], 'age.cat2', 4L)
	set(da, da[, which(age.cat.from>74 & age.cat.to<=84)], 'age.cat2', 5L)
	set(da, da[, which(age.cat.from>84 )], 'age.cat2', 6L)
	set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
	tmp <- data.table(age.cat2= 1:6, age.cat2.label=c('0-44','45-54','55-64','65-74','75-84','85+'))
	da <- merge(da, tmp, by='age.cat2')
	da
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_adult <- function(pop_info)
{
	da <- unique(subset(pop_info, select=c(age.cat, age.cat.label, age.cat.from, age.cat.to)))
	da[, age.cat2:= NA_integer_]
	set(da, da[, which(age.cat.to<=19)], 'age.cat2', 1L)
	set(da, da[, which(age.cat.from>19 & age.cat.to<=49)], 'age.cat2', 2L)
	set(da, da[, which(age.cat.from>49 )], 'age.cat2', 3L)
	set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
	tmp <- data.table(age.cat2= 1:3, age.cat2.label=c('0-19','20-49','50+'))
	da <- merge(da, tmp, by='age.cat2')
	da
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cat_map_middle <- function(pop_info)
{
	da <- unique(subset(pop_info, select=c(age.cat, age.cat.label, age.cat.from, age.cat.to)))
	da[, age.cat2:= NA_integer_]
	set(da, da[, which(age.cat.to<=29)], 'age.cat2', 1L)
	set(da, da[, which(age.cat.from>29 & age.cat.to<=49)], 'age.cat2', 2L)
	set(da, da[, which(age.cat.from>49 )], 'age.cat2', 3L)
	set(da, NULL, c('age.cat.from','age.cat.to'), NULL)
	tmp <- data.table(age.cat2= 1:3, age.cat2.label=c('0-29','30-49','50+'))
	da <- merge(da, tmp, by='age.cat2')
	da
}

#' @export
#' @keywords internal
#' @import data.table
map_deaths_ages <- function(deathByAge_data,dages,dc){
	df <- NULL
	for(State in names(deathByAge_data$A_AD)){
		n.age_state = deathByAge_data$A_AD[[State]]
		death_data_state_r = deathByAge_data$deaths_data[[State]][,1:n.age_state]
		age_state = data.table( age_state = 1:n.age_state, age = colnames(death_data_state_r))
		map_age_state = data.table(deathByAge_data$map_age[[State]][1:nrow(dages),1:n.age_state]) 
		age_state_index = c()
		for(a in 1:n.age_state){
			index = which(as.numeric(as.matrix(map_age_state)[,a]) == 1)
			age_state_index = c(age_state_index, rep(a, length(index[1]:index[length(index)])))
		}
		df_age = dages; df_age$age_state = age_state_index; df_age$age = colnames(death_data_state_r)[age_state_index]; df_age$state = State
		df <- rbind(df,df_age)
	}
	df <- merge(df,dc,by.x=c('state'),by.y=c('region_name'))
	return(df)
}

#' @export
#' @keywords internal
#' @import data.table
make_cum_deaths_by_age_summaries <- function(E_deathsByAge, pop_info, dates, age_state,regions)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_deathsByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  E_deathsByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    
    tmp <- subset(age_state[state==regions[m]], select=c('age_state', 'age_cat'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat')
    
    # 	aggregate by age groups c
    dt <- dt[, list(value=sum(value)), by=c('age_state','time','iteration')]	
    
    # 	cumsum
    setkey(dt, iteration, age_state, time)
    dt <- dt[, list(time=time, value=cumsum(value)), by=c('iteration','age_state')]	
    
    #	summarise		
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c('time','age_state')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dt <- merge(dt, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- merge(unique(subset(age_state,select=c('age_state','age','state'))), ans, 
               by.x=c('age_state','state'), by.y=c('age_state','loc'))
  setnames(ans,c('age_state','age','state','date'), c('age_cat','age_band','loc','date'))
  ans <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + time + date ~ q_label, value.var='q')
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
make_daily_deaths_by_age_summaries <- function(E_deathsByAge, pop_info, dates, age_state,regions)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_deathsByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  E_deathsByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    
    tmp <- subset(age_state[state==regions[m]], select=c('age_state', 'age_cat'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat')
    
    # 	aggregate by age groups c
    dt <- dt[, list(value=sum(value)), by=c('age_state','time','iteration')]	
    
    #	summarise		
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c('time','age_state')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dt <- merge(dt, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- merge(unique(subset(age_state,select=c('age_state','age','state'))), ans, 
               by.x=c('age_state','state'), by.y=c('age_state','loc'))
  setnames(ans,c('age_state','age','state','date'), c('age_cat','age_band','loc','date'))
  ans <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + time + date ~ q_label, value.var='q')
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
make_weekly_deaths_by_age_summaries <- function(E_deathsByAge, pop_info, dates, age_state,regions)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_deathsByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  E_deathsByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    
    tmp <- subset(age_state[state==regions[m]], select=c('age_state', 'age_cat'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat')
    
    # 	aggregate by age groups c
    dt <- dt[, list(value=sum(value)), by=c('age_state','time','iteration')]	
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, week_idx := strftime(date, format = "%V")]
    dt <- merge(dt, tmp, by='time')
    
    # 	aggregate by week
    dt <- dt[, list(value=sum(value), date = date[1]), by=c('age_state','week_idx','iteration')]	
    
    #	summarise		
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c('date','age_state')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- merge(unique(subset(age_state,select=c('age_state','age','state'))), ans, 
               by.x=c('age_state','state'), by.y=c('age_state','loc'))
  setnames(ans,c('age_state','age','state','date'), c('age_cat','age_band','loc','date'))
  ans <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q')
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
find_observed_death_fourageband = function(e_adeaths, periodicity)
{
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
    
    # aggregrated by periodicity
    if(periodicity == "weekly"){
      tmp1[, week_idx := strftime(date, format = "%V")]
      tmp1 = tmp1[, list(cum.deaths = sum(cum.deaths), daily.deaths = sum(daily.deaths), date = date[1]), 
                  by = c("week_idx", "age_band")]
    }
    #	merge
    e_adeaths_c <- merge(e_adeaths_c, tmp1, by=c('date','age_band'), all.x=TRUE)
    e_adeaths_c[, age:= factor(age_cat, levels=age_cat, labels=age_band)]
    setnames(e_adeaths_c, 'date', 'dates')
    
    # Aggregate by larger age band
    e_adeaths_c[, age_to := gsub(".*-(.+)", "\\1", age_band)]
    e_adeaths_c[grepl("\\+", age_to), age_to := gsub("(.+)\\+", "\\1", age_band)]
    e_adeaths_c[, age_to := as.numeric(age_to)]
    e_adeaths_c[age_to < 100, age_band_new := "65+"]
    e_adeaths_c[age_to < 65, age_band_new := "50-64"]
    e_adeaths_c[age_to < 50, age_band_new := "25-49"]
    e_adeaths_c[age_to < 25, age_band_new := "0-24"]
    
    stopifnot(sum(is.na(e_adeaths_c$age_band_new)) == 0)
    
    e_adeaths_c = e_adeaths_c[, list(M = sum(M), CL = sum(CL), CU = sum(CU), cum.deaths = sum(cum.deaths), daily.deaths = sum(daily.deaths)), 
                              by = c("dates", "age_band_new", "loc_label", "loc")]
    #	store merged data
    df.list[[x]] <- copy(e_adeaths_c)		
  }
  df.list <- do.call('rbind',df.list)
}

#' @export
#' @keywords internal
#' @import data.table
make_contact_intensities_over_time_summaries <- function(pop_info,stan_data,impact_intv,age_cat_map,regions,dates,
                                                         apply_eta_to_index_and_cnt, plot.pars.trmspars, with_contact_intensities_zhang)
{	
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')	
	
	stopifnot( dim(impact_intv)[2]==length(regions) )
	stopifnot( dim(impact_intv)[3]==stan_data$N2 )
	stopifnot( apply_eta_to_index_and_cnt )
	
	pc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
	pc <- subset(pc, loc %in% regions)
	pc <- merge(pc, age_cat_map, by='age.cat')
	pc <- pc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
	pc <- pc[, list(age.cat2=age.cat2, 
	                pop=pop,
	                pop_prop=pop/sum(pop)), 
	         by=c('loc')]
	setnames(pc, 'age.cat2','age_index2')
	
	#	do calculations per state to minimize memory footprint	
	# setup all list elements to avoid rebuilding the entire list internally	
	cnts <- vector('list',length(regions))	
	for(m in seq_along(regions))
	{
		#m<- 1
		cat('\nProcessing state',m,' ',regions[m])	  
		
		# convert detas to data.table		
		deta <- as.data.table( reshape2::melt( impact_intv[,m,,] ) )
		setnames(deta, 1:4, c('iteration','time','age.cat','eta'))
		
		#	add weekend index
		tmp <- data.table(time=stan_data$wkend_idx[,m][ stan_data$wkend_idx[,m]>0 ], wend=1)
		deta <- merge(deta, tmp, by='time', all.x=TRUE)
		set(deta, deta[, which(is.na(wend))], 'wend', 0)
				
		#	convert baseline contact matrices to data.table
		cnt <- as.data.table(reshape2::melt( stan_data$cntct_weekends_mean[[m]] ))
		cnt[, wend:= 1]
		tmp <- as.data.table(reshape2::melt( stan_data$cntct_weekdays_mean[[m]] ))
		tmp[, wend:= 0]
		cnt <- rbind(cnt, tmp)
		setnames(cnt, 1:3, c('age_index','age_cnt','cnt_intensity'))
		
		if(with_contact_intensities_zhang){
		  #	convert school closure contact matrices to data.table
		  cnt_zhang <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekends[[m]] ))
		  cnt_zhang[, wend:= 1]
		  tmp <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekdays[[m]] ))
		  tmp[, wend:= 0]
		  cnt_zhang <- rbind(cnt_zhang, tmp)
		  setnames(cnt_zhang, 1:3, c('age_index','age_cnt','cnt_intensity_zhang'))
		  
		  cnt <- merge(cnt, cnt_zhang, by = c('age_index','age_cnt', "wend"))
		}
		
		#	add further stats for aggregating now (cheap merge)					
		tmp <- subset(pop_info, loc==regions[m], select=c(age.cat, pop))
		tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat, age.cat2)), by='age.cat')
		tmp <- tmp[, list(age.cat=age.cat, prop_pop2= pop/sum(pop)), by='age.cat2']
		setnames(tmp, c('age.cat','age.cat2'), c('age_index','age_index2'))
		cnt <- merge(cnt, tmp, by='age_index')
		
		# time units
		time_units <- seq_len( dim(impact_intv)[3] )
		dates_with_forecast = seq.Date( min(dates[[m]]), min(dates[[m]]) + max(time_units)-1, by = "day") 
		
		# add school closure status
		if(with_contact_intensities_zhang){
		  tmp_school_status = data.table(school_closed = stan_data$SCHOOL_STATUS[1:length(dates_with_forecast),m], dates = dates_with_forecast)
		  tmp_school_status = tmp_school_status[school_closed == 1, list(closure_date = min(dates), reopening_date = max(dates)+1)]
		}

		# find dates
		tmp_dates <- data.table(time=seq_along(dates_with_forecast), dates=dates_with_forecast)
		
	 	#	process by day to keep scalable
		cnt_ts <- vector('list',length(time_units))
		for(x in time_units)
		{
		  
		  deta_t <- subset(deta, time==x, select=-time)
		  
		  # 	merging eta with cols of base contact matrix		
		  setnames(deta_t, c('age.cat','eta'), c('age_cnt','eta_cnt'))
		  cnt_t <- merge(deta_t, cnt, by=c('age_cnt','wend'),allow.cartesian=TRUE)
		  set(cnt_t, NULL, 'wend',NULL)
		  
		  
		  date = tmp_dates$dates[tmp_dates$time == x]
		  
		  if(with_contact_intensities_zhang){
		    # school closure
		    if(date >= tmp_school_status$closure_date & date < tmp_school_status$reopening_date){
		      cnt_t[, cnt_intensity := cnt_intensity_zhang]
		    }
		    
		    # school re-opening
		    if(date >= tmp_school_status$reopening_date && !is.null(plot.pars.trmspars$elt_school_intv_effect)){
		      elt_school_intv_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$elt_school_intv_effect ) )
		      setnames(elt_school_intv_effect_tab, 1:2, c('iteration','elt_school_intv_effect'))
		      elt_school_intv_effect_median <- elt_school_intv_effect_tab[, list(elt_school_intv_effect=median(elt_school_intv_effect))]
		      cnt_t[age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * elt_school_intv_effect_median$elt_school_intv_effect]
		    }
		    
		    if(date >= tmp_school_status$reopening_date && !is.null(plot.pars.trmspars$impact_intv_children_effect) && !is.null(plot.pars.trmspars$impact_intv_onlychildren_effect)){
		      impact_intv_children_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_children_effect ) )
		      setnames(impact_intv_children_effect_tab, 1:2, c('iteration','impact_intv_children_effect'))
		      impact_intv_children_effect_median <- impact_intv_children_effect_tab[, list(impact_intv_children_effect=median(impact_intv_children_effect))]
		      
		      impact_intv_onlychildren_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_onlychildren_effect ) )
		      setnames(impact_intv_onlychildren_effect_tab, 1:2, c('iteration','impact_intv_onlychildren_effect'))
		      impact_intv_onlychildren_effect_median <- impact_intv_onlychildren_effect_tab[, list(impact_intv_onlychildren_effect=median(impact_intv_onlychildren_effect))]
		      
		      cnt_t[age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * impact_intv_children_effect_median$impact_intv_children_effect]
		      cnt_t[age_index %in% stan_data$AGE_CHILD & age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * impact_intv_onlychildren_effect_median$impact_intv_onlychildren_effect^2]
		    }
		    
		  }

		  if(with_contact_intensities_zhang){
		    cnt_t[age_cnt %in% stan_data$AGE_CHILD, eta_cnt := 1.0] 
		  }
		  
			#	calculate marginal contact intensity
			cnt_t <- cnt_t[, list(age_index2=age_index2[1],
					prop_pop2=prop_pop2[1],
					mcnt_intensity= sum(cnt_intensity*eta_cnt)), 
				by=c('iteration','age_index')]
			
			# 	merging eta with indices (smaller merge, much faster after the last step)		
			setnames(deta_t, c('age_cnt','eta_cnt'), c('age_index','eta_index'))
			set(deta_t,NULL,'wend',NULL)
			cnt_t <- merge(deta_t, cnt_t, by=c('iteration','age_index'))
			if(with_contact_intensities_zhang){
			  cnt_t[age_index %in% stan_data$AGE_CHILD, eta_index := 1.0]
			}
			set(cnt_t, NULL,'mcnt_intensity',cnt_t[,mcnt_intensity*eta_index])
			set(cnt_t, NULL,'eta_index',NULL)
						
			#	aggregate						
			cnt_t <- cnt_t[, list(mcnt_intensity=sum(prop_pop2*mcnt_intensity)), by=c('age_index2','iteration')]
			
			#	calculate overall contact intensity	and save as age.cat 0		
			tmp <- merge(cnt_t, subset(pc, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
			tmp <- tmp[, list(mcnt_intensity= sum(mcnt_intensity*pop_prop)), by='iteration']
			tmp[, age_index2:=0]
			cnt_t <- rbind(cnt_t,tmp)
			
			#	summarise
			cnt_t <- cnt_t[, list(q= quantile(mcnt_intensity, prob=ps),
					q_label=p_labs), 
				by=c('age_index2')]	
		
			#	add time and save 
			cnt_t[, time:= x]
			cnt_ts[[x]] <- copy(cnt_t)				
		}
		cnt_ts <- do.call('rbind',cnt_ts)
				
		#	add loc and dates
		cnt_ts[, loc:= regions[m]]			
		tmp <- unique(subset(cnt_ts, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		cnt_ts <- merge(cnt_ts, tmp, by='time')
		cnts[[m]] <- copy(cnt_ts)	  
	}
	cnts <- do.call('rbind',cnts)
	
	# make human readable labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	cnts <- merge(cnts, tmp, by='loc')
	setnames(cnts, 'age_index2','age.cat2')
	tmp <- unique(subset(age_cat_map,select=c(age.cat2,age.cat2.label)))
	cnts <- merge(cnts, tmp, by='age.cat2', all.x=TRUE)
	
	# dcast
	cnts <- dcast.data.table(cnts, loc + loc_label + time + date + age.cat2 + age.cat2.label ~ q_label, value.var='q')
	setnames(cnts, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	
	cnts	
}

#' @export
#' @keywords internal
#' @import data.table
make_contact_intensities_across_states_summaries <- function(regions, mobility_data, dates, pop_info, 
                                                             stan_data, impact_intv, age_cat_map, 
                                                             apply_eta_to_index_and_cnt, 
                                                             plot.pars.trmspars, 
                                                             with_contact_intensities_zhang,
                                                             date)
  {
  
  ps <- c(0.5, 0.025, 0.975, 0.25, 0.75, 0, 1)
  p_labs <- c('M','CL','CU', 'L', 'U', 'min', 'max')	
  
  stopifnot( dim(impact_intv)[2]==length(regions))
  stopifnot( dim(impact_intv)[3]==stan_data$N2 )
  stopifnot( apply_eta_to_index_and_cnt )
  
  # Add weighting to age_cat_map to only chose 18 - 100.
  if (is.na(age_cat_map[1,1])){
  weights = data.table("age.cat" = c(1:18),
                       "weights" = c(0, 0, 0, 2/5, rep(1, 14)))
  } else {
    weights = data.table("age.cat" = c(1:18),
                         "weights" = rep(1, 18))
  }
  

  pc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  pc <- merge(pc, age_cat_map, by='age.cat')
  pc <- merge(pc, weights, by = "age.cat")
  # Weights so only have 18 + 
  pc <- pc[, list(pop=sum(pop*weights)), by=c('loc','bics.age.cat')]	
  # Only choose 18+ and regions in study
  pc <- pc[which(pc$loc %in% regions & ! is.na(pc$bics.age.cat)),]
  
  # Calculate total number of individuals 18+ in USA regions inc in study
  total_pop_18_plus <- sum(pc$pop)
  pc <- pc[, list(bics.age.cat=bics.age.cat, 
                  pop=pop,
                  pop_prop_state = pop/sum(pop),
                  pop_prop_usa = pop/total_pop_18_plus), 
           by=c('loc')]
  setnames(pc, 'bics.age.cat','bics_age_index')
  
  # Calculate the proportion of the usa in each age group
  pc_usa <- pc[, list(total_age = sum(pop)), by = c("bics_age_index")]
  pc <- merge(pc, pc_usa)
  pc$pop_prop_usa_age <- pc$pop/pc$total_age

  cnts_state <- vector('list', length(regions))	
  cnts_usa <- vector('list', length(regions))	
  prp_less_four <- vector('list', length(regions))	
  
  for (m in 1:length(regions)){
    
    cat('\nProcessing state',m,' ',regions[m])	 
    
    dip_date <- mobility_data$dip_date[which(mobility_data$loc == regions[m])][1] 
    
    intensity_date <- date
    intensity_date_idx <- which(dates[[m]] == intensity_date)

    # convert detas to data.table		
    deta <- as.data.table( reshape2::melt( impact_intv[,m,,] ) )
    setnames(deta, 1:4, c('iteration','time','age.cat','eta'))
    
    #	add weekend index
    tmp <- data.table(time=stan_data$wkend_idx[,m][ stan_data$wkend_idx[,m]>0 ], wend=1)
    deta <- merge(deta, tmp, by='time', all.x=TRUE)
    set(deta, deta[, which(is.na(wend))], 'wend', 0)
    
    #	convert baseline contact matrices to data.table
    cnt <- as.data.table(reshape2::melt( stan_data$cntct_weekends_mean[[m]] ))
    cnt[, wend:= 1]
    tmp <- as.data.table(reshape2::melt( stan_data$cntct_weekdays_mean[[m]] ))
    tmp[, wend:= 0]
    cnt <- rbind(cnt, tmp)
    setnames(cnt, 1:3, c('age_index','age_cnt','cnt_intensity'))
    
    if(with_contact_intensities_zhang){
      #	convert school closure contact matrices to data.table
      cnt_zhang <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekends[[m]] ))
      cnt_zhang[, wend:= 1]
      tmp <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekdays[[m]] ))
      tmp[, wend:= 0]
      cnt_zhang <- rbind(cnt_zhang, tmp)
      setnames(cnt_zhang, 1:3, c('age_index','age_cnt','cnt_intensity_zhang'))
      
      cnt <- merge(cnt, cnt_zhang, by = c('age_index','age_cnt', "wend"))
    }
    
    # find intensity date 
    tmp <- data.table(time=seq_along(dates[[m]]), dates=dates[[m]])
    intensity_date = tmp$dates[tmp$time == intensity_date_idx]
    
    # if schools are closed, use contact intensities zhang
    if(with_contact_intensities_zhang){
      tmp = data.table(school_closed = stan_data$SCHOOL_STATUS[1:length(dates[[m]]),m], dates = dates[[m]])
      tmp = tmp[school_closed == 1, list(closure_date = min(dates), reopening_date = max(dates))]
      
      # school closure
      if(intensity_date >= tmp$closure_date & intensity_date < tmp$reopening_date){
        cnt[, cnt_intensity := cnt_intensity_zhang]
      }
      
      # school re-opening
      if(intensity_date >= tmp$reopening_date && !is.null(plot.pars.trmspars$elt_school_intv_effect)){
        elt_school_intv_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$elt_school_intv_effect ) )
        setnames(elt_school_intv_effect_tab, 1:2, c('iteration','elt_school_intv_effect'))
        elt_school_intv_effect_median <- elt_school_intv_effect_tab[, list(elt_school_intv_effect=median(elt_school_intv_effect))]
        cnt[age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * elt_school_intv_effect_median$elt_school_intv_effect]
      }
      if(intensity_date >= tmp$reopening_date && !is.null(plot.pars.trmspars$impact_intv_onlychildren_effect)&& !is.null(plot.pars.trmspars$impact_intv_children_effect)){
        impact_intv_children_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_children_effect ) )
        setnames(impact_intv_children_effect_tab, 1:2, c('iteration','impact_intv_children_effect'))
        impact_intv_children_effect_median <- impact_intv_children_effect_tab[, list(impact_intv_children_effect=median(impact_intv_children_effect))]
        
        impact_intv_onlychildren_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_onlychildren_effect ) )
        setnames(impact_intv_onlychildren_effect_tab, 1:2, c('iteration','impact_intv_onlychildren_effect'))
        impact_intv_onlychildren_effect_median <- impact_intv_onlychildren_effect_tab[, list(impact_intv_onlychildren_effect=median(impact_intv_onlychildren_effect))]
        
        cnt[age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * impact_intv_children_effect_median$impact_intv_children_effect]
        cnt[age_index %in% stan_data$AGE_CHILD & age_cnt %in% stan_data$AGE_CHILD, cnt_intensity := cnt_intensity * impact_intv_onlychildren_effect_median$impact_intv_onlychildren_effect^2]
      }
      
    }
    
    #	add further stats for aggregating now (cheap merge)					
    tmp <- subset(pop_info, loc==regions[m], select=c(age.cat, pop))
    tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat,bics.age.cat)), by='age.cat')
    tmp <- tmp[, list(age.cat=age.cat, prop_pop2= pop/sum(pop)), by='bics.age.cat']
    setnames(tmp, c('age.cat','bics.age.cat'), c('age_index','bics_age_index'))
    cnt <- merge(cnt, tmp, by='age_index')
    
    #--------------------------------------------------------------------------------------------------
    
    deta_t <- subset(deta, time==intensity_date_idx, select=-time)
    
    # 	merging eta with cols of base contact matrix		
    setnames(deta_t, c('age.cat','eta'), c('age_cnt','eta_cnt'))
    cnt_t <- merge(deta_t, cnt, by=c('age_cnt','wend'),allow.cartesian=TRUE)
    set(cnt_t, NULL, 'wend',NULL)
    
    # 	merging eta with indices (smaller merge, much faster after the last step)		
    setnames(deta_t, c('age_cnt','eta_cnt'), c('age_index','eta_index'))
    set(deta_t,NULL,'wend',NULL)
    cnt_t <- merge(deta_t, cnt_t, by=c('iteration','age_index'))
    
    # if contact goes or comes from children, do not apply mobility multiplier
    if(with_contact_intensities_zhang){
      cnt_t[age_cnt %in% stan_data$AGE_CHILD, eta_cnt := 1.0]
      cnt_t[age_index %in% stan_data$AGE_CHILD, eta_index := 1.0]
    }
    
    #	calculate marginal contact intensity
    cnt_t <- cnt_t[, list(bics_age_index=bics_age_index[1],
                          prop_pop2=prop_pop2[1],
                          mcnt_intensity= sum(cnt_intensity*eta_cnt)), 
                   by=c('iteration','age_index')]
    
    # 	merging eta with indices (smaller merge, much faster after the last step)		
    cnt_t <- merge(deta_t, cnt_t, by=c('iteration','age_index'))
    set(cnt_t, NULL,'mcnt_intensity',cnt_t[,mcnt_intensity*eta_index])
    set(cnt_t, NULL,'eta_index',NULL)
    
    #	aggregate						
    cnt_t <- cnt_t[, list(mcnt_intensity=sum(prop_pop2*mcnt_intensity)), by=c('bics_age_index','iteration')]
    # remove bics_age_index NAs
    cnt_t <- cnt_t[which(!is.na(cnt_t$bics_age_index)),]
    
    #	calculate overall contact intensity at state level and save as age.cat 0		
    tmp <- merge(cnt_t, subset(pc, loc==regions[m], c(bics_age_index, pop_prop_state)), by=c('bics_age_index'))
    tmp <- tmp[, list(mcnt_intensity= sum(mcnt_intensity*pop_prop_state)), by='iteration']
    tmp[, bics_age_index:=0]
    cnt_t_state <- rbind(cnt_t,tmp)
    
    tmp_sorted <- sort(tmp$mcnt_intensity)
    idx <- which(tmp_sorted > 4)[1]
    prp <- (idx - 1)/ length(tmp_sorted)
    prp_less_four[[m]] <- data.table("loc" = regions[m],
                                     "prop" = prp)
    
    #	summarise
    cnt_t_state <- cnt_t_state[, list(q= quantile(mcnt_intensity, prob=ps),
                          q_label=p_labs), 
                   by=c('bics_age_index')]	

    cnt_t_state[, loc:=regions[m]]
    cnt_t_state[, dip_date:=dip_date]
    cnt_t_state[, contact_intensity_date:=intensity_date]
    
    #	add time and save 
    cnts_state[[m]]	<- cnt_t_state
    
    #	calculate overall contact intensity at state level and save as age.cat 0		
    tmp <- merge(cnt_t, subset(pc, loc==regions[m], c(bics_age_index, pop_prop_usa, pop_prop_usa_age)), by=c('bics_age_index'))
    tmp_ages <- tmp[, list(mcnt_intensity = mcnt_intensity*pop_prop_usa_age),  by=c('iteration', 'bics_age_index')]
    tmp_total <- tmp[, list(mcnt_intensity = sum(mcnt_intensity*pop_prop_usa)),  by=c('iteration')]
    tmp_total[, bics_age_index:=0]
    cnt_t_usa <- rbind(tmp_ages,tmp_total)
    
    cnt_t_usa[, loc:=regions[m]]
    
    #	add time and save 
    cnts_usa[[m]]	<- cnt_t_usa
    
  }
  
  prp_less_four <- do.call('rbind', prp_less_four)
  prp_less_four$prop[is.na(prp_less_four$prop)] <- 1
  
  cnts_state <- do.call('rbind',cnts_state)
  # make human readable labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  cnts_state <- merge(cnts_state, tmp, by='loc')
  cnts_state <- merge(cnts_state, prp_less_four, by = 'loc')
  
  cnts_usa <- do.call('rbind',cnts_usa)
  tmp <- cnts_usa[, list(mcnt_intensity = sum(mcnt_intensity)), by=c('iteration', 'bics_age_index')]
  
  tmp_all_age <- tmp[which(tmp$bics_age_index == 0),]
  tmp_sorted <- sort(tmp_all_age$mcnt_intensity)
  idx <- which(tmp_sorted > 4)[1]
  prop_four_usa <- (idx - 1)/ length(tmp_sorted)
  if (is.na(prop_four_usa)){
    prop_four_usa <- 1
  }
  
  weighted_usa <- tmp[, list(q= quantile(mcnt_intensity, prob=ps),
                             q_label=p_labs),
                      by=c('bics_age_index')]	
  weighted_usa[, loc_label:= "United States"]
  weighted_usa[, loc:= "US"]
  weighted_usa[, prop:= prop_four_usa]
  
  data <- rbind(cnts_state, weighted_usa, fill=TRUE)
  # Zero prop for non age combined data as not calculated
  data$prop[which(data$bics_age_index != 0)] <- NA
  
  tmp <- unique(subset(age_cat_map, select=c(bics.age.cat, bics.age.cat.label)))
  setnames(tmp, c('bics.age.cat', 'bics.age.cat.label'), c('bics_age_index', 'bics_age_cat_label'))
  data <- merge(data, tmp, by='bics_age_index', all.x=TRUE)
  
  # dcast
  cnts <- dcast.data.table(data, loc + loc_label + dip_date + contact_intensity_date + bics_age_cat_label + prop ~ q_label, value.var='q')
  
  proposed.states = c('District of Columbia', "New York City", unique(cnts$loc_label) )
  selected.states = proposed.states[proposed.states %in% cnts$loc_label][1:2]
  
  cnts <- cnts[which(cnts$loc_label %in% c("United States", selected.states)),]
  cnts	
  }

#' @export
#' @keywords internal
#' @import data.table
make_contact_patterns_over_time_summaries <- function(pop_info,stan_data,impact_intv,regions,dates,
                                                      apply_eta_to_index_and_cnt, 
                                                      with_contact_intensities_zhang,
                                                      plot.pars.trmspars,
                                                      mobility_data)
{
	cat("\n ----------- make contact_patterns_over_time data.table ----------- \n")
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')

	#	do calculations per state to minimise memory footprint	
	# setup all list elements to avoid rebuilding the entire list internally	
	cnts <- vector('list',length(regions))
	for(m in seq_along(regions))
	{
		#m<- 1
		cat('\nProcessing state',m,' ',regions[m])	  
		
		# convert detas to data.table
		stopifnot( dim(impact_intv)[2]==length(regions) )
		deta <- as.data.table( reshape2::melt( impact_intv[,m,,] ) )
		setnames(deta, 1:4, c('iteration','time','age.cat','eta'))
		
		#	add weekend index
		tmp <- data.table(time=stan_data$wkend_idx[,m][ stan_data$wkend_idx[,m]>0 ], wend=1)
		deta <- merge(deta, tmp, by='time', all.x=TRUE)
		set(deta, deta[, which(is.na(wend))], 'wend', 0)
		
		#	determine dates that are to be plotted (much faster at this stage)
		tmp <- data.table(time=seq_along(dates[[m]]), dates=dates[[m]])
		tmp[, week:= as.integer(strftime(dates, format='%V'))]
		tmp[, day:= strftime(dates, format='%A')]
		tmp <- subset(tmp, day%in%c('Wednesday','Saturday'))
		tmp <- merge(tmp, tmp[, list(complete_week= as.integer(length(day)==2)), by='week'], by='week')
		tmp <- subset(tmp, complete_week==1)		
		week.select <- round(seq(min(tmp$week), by= ( max(tmp$week) - min(tmp$week) - 3L ) / 3, length.out=4))
		week.select <- c(week.select, max(tmp$week))		
		tmp <- subset(tmp, week%in%week.select)
		
		deta <- merge(deta, tmp, by='time')
		set(deta, NULL, c('day','week','complete_week'), NULL)		

		#	we can take medians now, not before
		deta <- deta[, list(eta=median(eta)), by=c('age.cat','time','wend')]
		
		#	convert baseline contact matrices to data.table
		cnt <- as.data.table(reshape2::melt( stan_data$cntct_weekends_mean[[m]] ))
		cnt[, wend:= 1]
		tmp <- as.data.table(reshape2::melt( stan_data$cntct_weekdays_mean[[m]] ))
		tmp[, wend:= 0]
		cnt <- rbind(cnt, tmp)
		setnames(cnt, 1:3, c('age_index','age_cnt','cnt_intensity'))
		
		if(with_contact_intensities_zhang){
		  #	convert school closure contact matrices to data.table
		  cnt_zhang <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekends[[m]] ))
		  cnt_zhang[, wend:= 1]
		  tmp <- as.data.table(reshape2::melt( stan_data$cntct_school_closure_weekdays[[m]] ))
		  tmp[, wend:= 0]
		  cnt_zhang <- rbind(cnt_zhang, tmp)
		  setnames(cnt_zhang, 1:3, c('age_index','age_cnt','cnt_intensity_zhang'))

		  cnt <- merge(cnt, cnt_zhang, by = c('age_index','age_cnt', "wend"))
		}
		
		# 	merging eta with rows and cols of base contact matrix		
		setnames(deta, c('age.cat','eta'), c('age_index','eta_index'))
		cnt <- merge(deta, cnt, by=c('age_index','wend'),allow.cartesian=TRUE)
		setnames(deta, c('age_index','eta_index'), c('age_cnt','eta_cnt'))
		cnt <- merge(deta, cnt, by=c('age_cnt','time','wend'))
		
		#	add loc and dates
		cnt[, loc:= regions[m]]	
		tmp <- data.table(time=seq_along(dates[[m]]), dates=dates[[m]])
		cnt <- merge(cnt, tmp, by='time')
		
		# add school closure status
		tmp = data.table(school_closed = stan_data$SCHOOL_STATUS[1:length(dates[[m]]),m], dates = dates[[m]])
		tmp[, school_reopened := cumsum(c(1,diff(school_closed)!=0))]
		tmp[, school_reopened := school_reopened == 3]
		cnt <- merge(cnt, tmp, by='dates')
		
		# during school closure
		if(with_contact_intensities_zhang){
		  # use zhang contact intensities w
		  cnt[school_closed == 1, cnt_intensity := cnt_intensity_zhang]
		  # do not apply mobility multiplier if contact comes or goes to children
		  cnt[age_cnt %in% stan_data$AGE_CHILD, eta_cnt := 1 ]
		  cnt[age_index %in% stan_data$AGE_CHILD, eta_index := 1 ]
		}
		
		# after school closure
		if(with_contact_intensities_zhang && !is.null(plot.pars.trmspars$elt_school_intv_effect)){
		  elt_school_intv_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$elt_school_intv_effect ) )
		  setnames(elt_school_intv_effect_tab, 1:2, c('iteration','elt_school_intv_effect'))
		  elt_school_intv_effect_median <- elt_school_intv_effect_tab[, list(elt_school_intv_effect=median(elt_school_intv_effect))]
		  cnt[school_reopened == 1 & (age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD), cnt_intensity := cnt_intensity * elt_school_intv_effect_median$elt_school_intv_effect]
		}
		
		if(with_contact_intensities_zhang && !is.null(plot.pars.trmspars$impact_intv_children_effect) && !is.null(plot.pars.trmspars$impact_intv_onlychildren_effect)){
		  impact_intv_children_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_children_effect ) )
		  setnames(impact_intv_children_effect_tab, 1:2, c('iteration','impact_intv_children_effect'))
		  impact_intv_children_effect_median <- impact_intv_children_effect_tab[, list(impact_intv_children_effect=median(impact_intv_children_effect))]
		  
		  impact_intv_onlychildren_effect_tab = as.data.table( reshape2::melt( plot.pars.trmspars$impact_intv_onlychildren_effect ) )
		  setnames(impact_intv_onlychildren_effect_tab, 1:2, c('iteration','impact_intv_onlychildren_effect'))
		  impact_intv_onlychildren_effect_median <- impact_intv_onlychildren_effect_tab[, list(impact_intv_onlychildren_effect=median(impact_intv_onlychildren_effect))]
		  
		  cnt[school_reopened == 1 & (age_index %in% stan_data$AGE_CHILD | age_cnt %in% stan_data$AGE_CHILD), cnt_intensity := cnt_intensity * impact_intv_children_effect_median$impact_intv_children_effect]
		  cnt[school_reopened == 1 & (age_index %in% stan_data$AGE_CHILD & age_cnt %in% stan_data$AGE_CHILD), cnt_intensity := cnt_intensity * impact_intv_onlychildren_effect_median$impact_intv_onlychildren_effect^2]
		}
		
		cat('\nMultiplying eta to index individuals ...')
		cnt[, cnt_intensity:= cnt_intensity*eta_index]		
		if(apply_eta_to_index_and_cnt)
		{
		  cat('\nMultiplying eta to contacted individuals ...')
		  cnt[, cnt_intensity:= cnt_intensity*eta_cnt]
		}
		set(cnt, NULL, c('eta_index','eta_cnt'), NULL)

		# build new data object only after summarised
		cnts[[m]] <- copy(cnt)	  
	}
	cnts <- do.call('rbind',cnts)
	
	# make human readable labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	cnts <- merge(cnts, tmp, by='loc')
	cnts	
}

#' @export
#' @keywords internal
#' @import data.table
make_etabyage_summaries <- function(pop_info,impact_intv,regions,age_cat_map,dates)
{
	cat("\n ----------- make eta by age data.table ----------- \n")
	
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')

	# make age cat map	  
	da <- subset(age_cat_map, select=c(age.cat,age.cat2))	
	
	#	do calculations per state to minimise memory footprint
	
	# setup all list elements to avoid rebuilding the entire list internally	
	detas <- vector('list',length(regions))
	for(m in seq_along(regions))
	{
		#m<- 1
		cat('\nProcessing state',m,' ',regions[m])	  
		
		# convert detas to data.table
		stopifnot( dim(impact_intv)[2]==length(regions) )
		deta <- as.data.table( reshape2::melt( impact_intv[,m,,] ) )
		setnames(deta, 1:4, c('iteration','time','age.cat','eta'))
		
		#	add reporting age.cats now (much faster in low dim)
		deta <- merge(deta, da, by='age.cat')
		
		#	add prop of pop in age.cat (much faster in low dim)
		tmp <- subset(pop_info, loc==regions[m], c(age.cat, prop_pop))
		deta <- merge(deta, tmp, by= 'age.cat')
		
		#	work out prop_pop/sum(prop_pop) (much faster in low dim)
		tmp <- unique(subset(deta, select=c(age.cat, age.cat2, prop_pop)))
		tmp <- tmp[, list( age.cat=age.cat,
											 prop_pop2= prop_pop/sum(prop_pop)  
		), by='age.cat2']
		deta <- merge(deta, tmp, by= c('age.cat','age.cat2'))
		set(deta, NULL, 'prop_pop', NULL)
		
		#	aggregate to age.cat2
		deta <- deta[, list(eta=sum(prop_pop2*eta)), by=c('age.cat2','time','iteration')]	
		
		# 	summarise
		deta <- deta[, list(qs= quantile(eta, prob=ps), qlab=p_labs), by=c('time','age.cat2')]
		deta <- dcast.data.table(deta, time+age.cat2~qlab, value.var='qs')
		
		#	add loc and dates
		deta[, loc:= regions[m]]		
		tmp <- unique(subset(deta, select=c(time)))
		tmp[, dates:= dates[[m]][1] + time - 1L]
		deta <- merge(deta, tmp, by='time')
		
		# build new data object only after summarised
		detas[[m]] <- copy(deta)	  
	}
	detas <- do.call('rbind',detas)	
	
	# make human readable labels
	tmp <- unique(subset(age_cat_map, select=c(age.cat2, age.cat2.label)))
	detas <- merge(detas, tmp, by='age.cat2')
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	detas <- merge(detas, tmp, by='loc')
	setnames(detas, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
		
	return(detas)
}

#' @export
#' @keywords internal
#' @import data.table
make_parPerTimePerLoc_less_than_threshold <- function(samples, dates, regions, pop_info, threshold)
{	
	stopifnot( dim(samples)[3]==length(regions) )
	
  p_loc <- unique(subset(pop_info, select=c(loc,pop_total)))
  p_loc <- subset(p_loc, loc %in% regions)
  p_loc <- p_loc[, pop_prop:=pop_total/sum(pop_total)]
  
	ans <- vector('list',length(regions)) 
	ans_overall <- vector('list',length(regions)) 
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		ds_m <- as.data.table( reshape2::melt( samples[,,m] ) )
		setnames(ds_m, 1:3, c('iteration','time','value'))
		
		# save time mapping for region m
		tmp1 <- unique(subset(ds_m, select=time))
		tmp1[, date:= dates[[m]][tmp1$time[1]] + tmp1$time - tmp1$time[1]]
		
		#	to compute national average: add prop_pop, time and save 	
		tmp <- ds_m[, pop_prop := subset(p_loc, loc==regions[m])$pop_prop]
		# add dates: we need to do here because the time index are different by state
		tmp <- merge(tmp, tmp1, by = 'time')
		tmp[, loc:= regions[m]]
		ans_overall[[m]] = copy(tmp)
		
		# summarise
		ds_m <- ds_m[, list(value= mean(value<threshold)), by=c('time')]		
		
		#	add loc 
		ds_m[, loc:= regions[m]]
		
		# add time index and date
		ds_m <- merge(ds_m, tmp1, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(ds_m)
	}
	ans <- do.call('rbind',ans)
	ans_overall <- do.call('rbind',ans_overall)
	
	## National average
	# keep date that are common to every region
	tmp = unique(select(ans_overall, loc, date))
	dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
	ds_m <- subset(ans_overall, date %in% as.Date(dates_common))
	#	summarise
	ds_m <- ds_m[, list(value= sum(value*pop_prop)), by=c('date','iteration')]
	ds_m <- ds_m[, list(value= mean(value<threshold)), by=c('date')]	
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')	
	ans <- select(ans, -time)
	ds_m[,loc:= "US"]
	ds_m[,loc_label := "United-States"]
	ans <- rbind(ans, ds_m)
	
	ans 
}

#' @export
#' @keywords internal
#' @import data.table
make_ifroverall_summaries <- function(casesByAge, deathsByAge, dates, regions, pop_info)
{
	cat("\n ----------- make_ifroverall_summaries ----------- \n")
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	last.common.date <- as.Date( min( sapply(dates, function(x) as.character(tail(x,1))) ) )
	time.idx <- sapply(dates, function(x) which(x==last.common.date) )
	
	stopifnot( dim(casesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		#	select data from large table
		dc_m <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
		setnames(dc_m, 1:4, c('iteration','time','age_cat','cases'))
		tmp <- as.data.table( reshape2::melt( deathsByAge[,m,,] ) )
		setnames(tmp, 1:4, c('iteration','time','age_cat','deaths'))
		
		#	calculate overall new cases per day
		#	calculate overall new deaths per day
		dc_m <- dc_m[, list(cases=sum(cases)), by=c('iteration','time')]
		tmp <- tmp[, list(deaths=sum(deaths)), by=c('iteration','time')]
		
		#	calculate overall cumulated cases until day
		#	calculate overall cumulated deaths until day
		setkey(dc_m, iteration, time)
		setkey(tmp, iteration, time)
		dc_m <- dc_m[, list(time=time, cum_cases=cumsum(cases)), by=c('iteration')]
		tmp <- tmp[, list(time=time, cum_deaths=cumsum(deaths)), by=c('iteration')]
		
		#	select endpoint
		dc_m <- subset(dc_m, time==time.idx[m])
		tmp <- subset(tmp, time==time.idx[m])
		
		#	merge
		dc_m <- merge(dc_m, tmp, by=c('iteration','time'))
		
		#	define IFR and summarise
		dc_m[, IFR:= cum_deaths/cum_cases]
		dc_m <- dc_m[, list( 	q= quantile(IFR, prob=ps),
						q_label=p_labs), 
				by=c('time')]		
		
		#	add loc 
		dc_m[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dc_m, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dc_m <- merge(dc_m, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dc_m)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
	ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_ifrbyage_us_summaries <- function(age_cat_map,casesByAge, deathsByAge, dates, regions, pop_info)
{
	cat("\n ----------- make_ifroverall_summaries ----------- \n")
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	last.common.date <- as.Date( min( sapply(dates, function(x) as.character(tail(x,1))) ) )
	time.idx <- sapply(dates, function(x) which(x==last.common.date) )
	
	stopifnot( dim(casesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))
	ans_c <- array(0,dim=c(length(regions),dim(casesByAge)[1],length(unique(age_cat_map$age.cat2))))
	ans_d <- array(0,dim=c(length(regions),dim(casesByAge)[1],length(unique(age_cat_map$age.cat2))))
	#	loop over locations to get cum. cases/deaths by new age cat
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dc_m <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
		setnames(dc_m, 1:4, c('iteration','time','age_cat','cases'))
		dd_m <- as.data.table( reshape2::melt( deathsByAge[,m,,] ) )
		setnames(dd_m, 1:4, c('iteration','time','age_cat','deaths'))
		
		# aggregate by new age cat
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		dc_m <- merge(dc_m, tmp, by='age_cat')
		dd_m <- merge(dd_m, tmp, by='age_cat')
		
		dc_m <- dc_m[, list(cases=sum(cases)), by=c('age_cat2','time','iteration')]	
		dd_m <- dd_m[, list(deaths=sum(deaths)), by=c('age_cat2','time','iteration')]	
		
		#	calculate overall cumulated cases/deaths until day
		setkey(dc_m, iteration, time, age_cat2)
		setkey(dd_m, iteration, time, age_cat2)
		dc_m <- dc_m[, list(time=time, cum_cases=cumsum(cases)), by=c('iteration','age_cat2')]
		dd_m <- dd_m[, list(time=time, cum_deaths=cumsum(deaths)), by=c('iteration','age_cat2')]
		
		#	keep for max date only
		dc_m <- subset(dc_m, time==time.idx[m])
		dd_m <- subset(dd_m, time==time.idx[m])
		
		ans_c[m,,] <- as.matrix(dcast.data.table(dc_m, iteration  ~ age_cat2, value.var='cum_cases')[,-1])
		ans_d[m,,] <- as.matrix(dcast.data.table(dd_m, iteration  ~ age_cat2, value.var='cum_deaths')[,-1])
	}
	
	# melt and merge the cases/deaths for all states
	dc_m <- as.data.table( reshape2::melt( ans_c[,,] ) )
	setnames(dc_m, 1:4, c('loc','iteration','age.cat2','cum_cases'))
	dd_m <- as.data.table( reshape2::melt( ans_d[,,] ) )
	setnames(dd_m, 1:4, c('loc','iteration','age.cat2','cum_deaths'))
	
	dc_m <- merge(dc_m, dd_m, by=c('loc','iteration','age.cat2'))
	dc_m[, loc:= regions[dc_m$loc]]
	
	#	calc prop of US pop in age.cat across states (just for regions in model)
	tmp <- subset(pop_info, loc %in% regions, select=c(loc,age.cat,pop,pop_total)) # remove pop_total
	tmp <- merge(tmp, age_cat_map, by='age.cat')
	
	# aggregate population by age group c and get proportions across states
	tmp2 <- tmp[,list(pop_total_a=sum(pop)
	), by=c('age.cat2')]
	tmp <- tmp[,list(age.cat=age.cat,
									 pop=sum(pop),
									 pop_total=sum(pop_total)
	), by=c('loc','age.cat2')]
	
	tmp <- unique(subset(tmp,select=c(loc,age.cat2,pop,pop_total)))
	tmp <- merge(tmp,tmp2,by=c('age.cat2'))
	tmp[,prop:=pop/pop_total_a]

	dc_m <- merge(dc_m,tmp,by=c('loc','age.cat2'))
	
	# calculate weighted sum of cases/deaths
	dc_m <- dc_m[,list(weighted_cum_cases=sum(cum_cases*prop),
										 weighted_cum_deaths=sum(cum_deaths*prop)
	),by=c('iteration','age.cat2')]
	
	#	define IFR and summarise
	dc_m[, IFR:= weighted_cum_deaths/weighted_cum_cases]
	dc_m <- dc_m[, list(q= quantile(IFR, prob=ps),
											q_label=p_labs), 
							 by=c('age.cat2')]		
	
	#	make human readable loc labels
	tmp <- unique(subset(age_cat_map, select=c(age.cat2, age.cat2.label)))
	dc_m <- merge(dc_m, tmp, by='age.cat2')
	setnames(dc_m,'age.cat2.label','age_band')
	ans <- dcast.data.table(dc_m,  age_band ~ q_label, value.var='q')
	ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_casesoverall_summaries <- function(casesByAge, dates, regions, pop_info)
{
  cat("\n ----------- make_casesoverall_summaries ----------- \n")
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(casesByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dc_m <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
    setnames(dc_m, 1:4, c('iteration','time','age_cat','value'))
    
    #	calculate overall new cases per day
    dc_m <- dc_m[, list(value=sum(value)), by=c('iteration','time')]
    dc_m <- dc_m[, list( 	q= quantile(value, prob=ps),
                          q_label=p_labs), 
                 by=c('time')]		
    
    #	add loc 
    dc_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dc_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dc_m <- merge(dc_m, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dc_m)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
  ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_casesoverall_c_summaries <- function(casesByAge, dates, regions, pop_info)
{
  cat("\n ----------- make_casesoverall_summaries ----------- \n")
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(casesByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dc_m <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
    setnames(dc_m, 1:4, c('iteration','time','age_cat','value'))
    
    #	calculate overall new cases per day
    dc_m <- dc_m[, list(value=sum(value)), by=c('iteration','time')]
	
	#	take cumsum
	setkey(dc_m, iteration, time)
    dc_m <- dc_m[, list(value_c=cumsum(value),
                        time=time), by=c('iteration')]
		
	#	summarise	
    dc_m <- dc_m[, list(q= quantile(value_c, prob=ps),q_label=p_labs), by=c('time')]		
    
    #	add loc 
    dc_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dc_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dc_m <- merge(dc_m, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dc_m)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
  ans  	
}



#' @export
#' @keywords internal
#' @import data.table
make_attackrate_school_summaries <- function(casesByAge, dates, regions, observed_attackrate_school, pop_info)
{
  cat("\n ----------- make_attackrate_time_summaries ----------- \n")
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  results = c()
  
  for (region in observed_attackrate_school$loc) {
    dt <- as.data.table( reshape2::melt(casesByAge[,which( regions == region ),,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    dt = dt[age_cat%in% 2:4,]
    dt[age_cat==4, value:= value * 0.8]
    
    # 	aggregate Rt 
    dt <- dt[, list(value=sum(value)), by=c('time','iteration')]	
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[which( regions == region )]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dt <- merge(dt, tmp, by='time')
    dt = dt[date <= as.Date(observed_attackrate_school[loc==region,]$duration_end) & date >= as.Date(observed_attackrate_school[loc==region,]$duration_start)]
    # 	cumsum
    setkey(dt, iteration, time)
    dt <- dt[, list(date=date, value=cumsum(value)), by=c('iteration')]	
    
    #	summarise		
    dt <- dt[, list( q= quantile(value, prob=ps),q_label=p_labs), by=c('date')]		
    
    #	add loc 
    dt[, loc:= region]
    
    ans <- copy(dt)
    
    #	make human readable loc labels
    tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
    ans <- merge(ans, tmp, by='loc')
    
    ans <- dcast.data.table(ans, loc + loc_label + date ~ q_label, value.var='q')
    
    region_pop_info = pop_info[age.cat%in%2:4 & loc==region,]
    region_pop_info[age.cat==4, pop := 0.8 * pop]
    
    ans[,pop:=sum(region_pop_info$pop)]
    ans[,CL:=CL/pop]
    ans[,M:=M/pop]
    ans[,CU:=CU/pop]
    
    results = c(results,
                paste0(round(ans[nrow(ans),]$M,4)*100, '% ( ', round(ans[nrow(ans),]$CL,4)*100, '% - ', round(ans[nrow(ans),]$CU,4)*100, '% ) '))
    
  }
  return(results)

}

#' @export
#' @keywords internal
#' @import data.table
make_deathsoverall_summaries <- function(deathsByAge, dates, regions, pop_info)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(deathsByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dc_m <- as.data.table( reshape2::melt( deathsByAge[,m,,] ) )
    setnames(dc_m, 1:4, c('iteration','time','age_cat','value'))
    
    #	calculate overall new cases per day
    dc_m <- dc_m[, list(value=sum(value)), by=c('iteration','time')]
    dc_m <- dc_m[, list( 	q= quantile(value, prob=ps),
                          q_label=p_labs), 
                 by=c('time')]		
    
    #	add loc 
    dc_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dc_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dc_m <- merge(dc_m, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dc_m)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
  ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_var_by_strain_summaries <- function(varByStrain, pop_info, dates, regions, ds)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(varByStrain)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    ds_m <- as.data.table( reshape2::melt( varByStrain[,m,,] ) )
    setnames(ds_m, 1:4, c('iteration','strain_cat','time','value'))
    
    #	summarise
    ds_m <- ds_m[, list(q = quantile(value, prob=ps), q_label=p_labs), by=c('time','strain_cat')]		
    
    #	add loc 
    ds_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(ds_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    ds_m <- merge(ds_m, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(ds_m)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- merge(ans, ds, by = 'strain_cat')
  ans <- dcast.data.table(ans, loc + loc_label + time + date + strain_cat + strain_cat_label ~ q_label, value.var='q')
  ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_var_overall_summaries <- function(varOverall, pop_info, dates, regions)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(varOverall)[3]==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    d_m <- as.data.table( reshape2::melt( varOverall[,,m] ) )
    setnames(d_m, 1:3, c('iteration','time','value'))
    
    #	summarise
    d_m <- d_m[, list(q = quantile(value, prob=ps), q_label=p_labs), by=c('time')]		
    
    #	add loc 
    d_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(d_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    d_m <- merge(d_m, tmp, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(d_m)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
  ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_deathsoverall_summaries_with_national_average <- function(deathsByAge, dates, regions, pop_info)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(deathsByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions)) 
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dc_m <- as.data.table( reshape2::melt( deathsByAge[,m,,] ) )
    setnames(dc_m, 1:4, c('iteration','time','age_cat','value'))
    
    #	calculate overall new deaths per day
    dc_m <- dc_m[, list(value=sum(value)), by=c('iteration','time')]
    
    #	add loc 
    dc_m[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dc_m, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dc_m <- merge(dc_m, tmp, by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dc_m)
    
    # summarise
    dc_m = dc_m[, list(q = quantile(value, prob=ps), q_label=p_labs), by=c('loc', 'date')]		
    
    # build new data object only after summarised
    ans[[m]] <- copy(dc_m)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #	summarise national avg
  ans_overall <- ans_overall[,list(value=sum(value)), by=c('date','iteration')]
  dt <- ans_overall[, list(q= quantile(value, prob=ps),
                           q_label=p_labs), 
                    by=c('date')]	
  dt[, loc := "US"]
  dt[, loc_label := "United States"]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  
  # add to national avg
  ans <- rbind(ans, dt)
  ans <- dcast.data.table(ans, loc + loc_label + date ~ q_label, value.var='q')
  ans  	
}

#' @export
#' @keywords internal
#' @import data.table
make_flow_summaries <- function(in_flow, time_idx, dates, regions, pop_info)
{
  
  cat("\n ----------- make_flow_summaries ----------- \n")
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  flow <- as.data.table( reshape2::melt( in_flow ) )
  setnames(flow, 1:6, c('iteration','time','source_age_cat','rec_age_cat','loc','flow'))
  flow <- subset(flow, !is.na(flow))
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
	  #m <- 7	  
	  cat('\nProcessing state',m,' ',regions[m])
	  flow_m <- subset(flow, loc==m, select=-loc)
	  
	  #	calculate flows in numbers
	  flow_s <- flow_m[, list(	q= quantile(flow, prob=ps),
	  							q_label=p_labs), 
							by=c('time','source_age_cat','rec_age_cat')]
	  flow_s[, stat:= 'flow_abs']			
	  
	  #	calculate flows in %
	  tmp <- flow_m[, list(	source_age_cat=source_age_cat,
					  		rec_age_cat=rec_age_cat,
							flow=flow/sum(flow)), 
						by=c('iteration','time')]					      
	  tmp <- tmp[, list( 	q= quantile(flow, prob=ps,na.rm=TRUE),
					  		q_label=p_labs), 
			  			by=c('time','source_age_cat','rec_age_cat')]								
	  tmp[, stat:= 'flow_prop']			
	  flow_s <- rbind(flow_s, tmp)
	  
	  #	calculate sources in %
	  # possible that there are no flows into one age cat, so use na.rm=TRUE
	  tmp <- flow_m[, list(	source_age_cat=source_age_cat,
					  		flow=flow/sum(flow)), 
						by=c('iteration','time','rec_age_cat')]					
	  tmp <- tmp[, list( 	q= quantile(flow, prob=ps, na.rm=TRUE),
							q_label=p_labs), 
						by=c('time','source_age_cat','rec_age_cat')]								
	  tmp[, stat:= 'sources_prop']			
	  flow_s <- rbind(flow_s, tmp)
	  
	  #	calculate onward transmissions in %
	  # possible that there are no flows from one age cat, so use na.rm=TRUE
	  tmp <- flow_m[, list(	rec_age_cat=rec_age_cat,
					  		flow=flow/sum(flow)), 
			  			by=c('iteration','time','source_age_cat')]					
	  tmp <- tmp[, list( 	q= quantile(flow, prob=ps, na.rm=TRUE),
					  q_label=p_labs), 
			  by=c('time','source_age_cat','rec_age_cat')]								
	  tmp[, stat:= 'rec_prop']			
	  flow_s <- rbind(flow_s, tmp)
	  	
	  #	add loc 
	  flow_s[, loc:= regions[m]]
	  
	  # add time index and date
	  tmp2 <- time_idx[,m][ time_idx[,m]>0 ]
	  tmp <- unique(subset(flow_s, time<=length(tmp2), select=c(time)))	  
	  flow_s <- subset(flow_s, time<=length(tmp2))
	  tmp[, time_idx:= tmp2]
	  tmp[, date:= dates[[m]][tmp$time_idx[1]] + tmp$time_idx - tmp$time_idx[1]]
	  flow_s <- merge(flow_s, tmp, by='time')
	  flow_s[, time:=NULL]
	  setnames(flow_s, 'time_idx','time')
	  
	  # build new data object only after summarised
	  ans[[m]] <- copy(flow_s)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- dcast.data.table(ans, stat + loc + loc_label + time + date + source_age_cat + rec_age_cat ~ q_label, value.var='q')
  ans  
}

#' @export
#' @keywords internal
#' @import data.table
summarise_e_acases_eff_byage_c <- function(effcasesByAge, age_cat_map, pop_info, dates, regions){
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(effcasesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	ans_overall <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt( effcasesByAge[,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value'))
		
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','time','iteration')]	
		
		# 	cumsum
		setkey(dt, iteration, age_cat2, time)
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		#	to compute national average
		ans_overall[[m]] = copy(dt)
		
		#	summarise		
		dt <- dt[, list(q= quantile(value, prob=ps),
										q_label=p_labs), 
						 by=c('loc','time','date','age_cat2')]		
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	ans_overall <- do.call('rbind',ans_overall)

	#	summarise national av
	ans_overall <- ans_overall[,list(value=sum(value)), by=c('age_cat2','date','iteration')]
	dt <- ans_overall[, list(q= quantile(value, prob=ps),
													 q_label=p_labs), 
										by=c('date','age_cat2')]	
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	dt[,loc:= "US"]
	dt[,loc_label := "United-States"]
	ans <- merge(ans, dt,by=c('loc','date','age_cat2','q','q_label','loc_label'),all=T)
	ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
	
	ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + time + date ~ q_label, value.var='q')
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans	
}

#' @export
#' @keywords internal
io_saveRDS <- function(obj, work_dir, out_dir, base_name, check_if_saved_n = 0)
{
	cat('\nSave to file ',file.path(out_dir, base_name),'...')	
	tmp <- check_if_saved_n
	repeat
	{
		#   comp_9 = xzfile(tmp, compression = 9)
		# 	saveRDS(fit.gqs, comp_9)	
		tryCatch(
			{
				saveRDS(obj, file=file.path(work_dir, base_name))
				if(work_dir!=out_dir)
				{
					file.copy(file.path(work_dir, base_name), 
							file.path(out_dir, base_name), 
							overwrite = TRUE, 
							recursive = FALSE,
							copy.mode = TRUE, 
							copy.date = TRUE
					)							
				}
			}, error = function(err) { warning(err) } )
		if(check_if_saved_n<1)
			break
		check_if_saved <- try(readRDS(file=file.path(out_dir, base_name)))		
		if(!'try-error'%in%class(check_if_saved))
			break	
		tmp <- tmp-1
		if(tmp<=0)
		{
			stop('Failed to save ',check_if_saved_n,' times')
		}
	}	
}


#' @export
#' @keywords internal
#' @import data.table
summarise_e_acases_eff_midage <- function(E_effcasesByAge, age_cat_map_mid, pc, pop_info, dates, regions){
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		pcm <- subset(pc,loc==regions[m])
		tmp <- pcm[, list(loc=loc[1], age_index2=0, pop= sum(pop), pop_prop=sum(pop_prop))]
		pcm <- rbind(pcm, tmp)
				
		dt <- as.data.table( reshape2::melt( E_effcasesByAge[,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value'))
		
		tmp <- subset(age_cat_map_mid, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','time','iteration')]	
		tmp <- dt[, list(age_cat2=0, value=sum(value)), by=c('time','iteration')]	
		dt <- rbind(dt, tmp)
		
		# divide cases by population in age group
		dt <- merge(dt,pcm,by.x='age_cat2',by.y='age_index2')
		dt[,ratio:=value/pop]
		set(dt, NULL, c('pop','pop_prop'), NULL)
		
		#	melt ratio and value
		setnames(dt, 'value', 'E_effcases')
		setnames(dt, 'ratio', 'R_effcases')
		dt <- melt(dt, id.vars=c('age_cat2','time','iteration','loc'), measure.vars=c('E_effcases','R_effcases'))
		
		#	summarise		
		dt <- dt[, list(
				q= quantile(value, prob=ps),
				q_label=p_labs
			), 
			by=c('time','age_cat2','variable')]		
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- merge(unique(subset(age_cat_map_mid,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'), all.y=TRUE)
	set(ans, ans[, which(is.na(age.cat2.label))], 'age.cat2.label', 'Overall')
	
	ans <- dcast.data.table(ans, variable + loc + loc_label + age.cat2 + age.cat2.label + time + date ~ q_label, value.var='q')
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans	
}

#' @export
#' @keywords internal
#' @import data.table
summarise_e_acases_byage_c <- function(casesByAge, age_cat_map, pop_info, dates, regions)
{	
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(casesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value'))
		
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate Rt by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','time','iteration')]	
		
		# 	cumsum
		setkey(dt, iteration, age_cat2, time)
		dt <- dt[, list(time=time, value=cumsum(value)), by=c('iteration','age_cat2')]	
				
		#	summarise		
		dt <- dt[, list( 	q= quantile(value, prob=ps),
						q_label=p_labs), 
						by=c('time','age_cat2')]		
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
	
	ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + time + date ~ q_label, value.var='q')
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans	
}

#' @export
#' @keywords internal
#' @import data.table
summarise_e_newcases_byage_c <- function(casesByAge, age_cat_map, pop_info, dates, regions)
{	
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(casesByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt( casesByAge[,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value'))
		
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','time','iteration')]	
		
		#	summarise		
		dt <- dt[, list(q= quantile(value, prob=ps),
										q_label=p_labs), 
						 by=c('time','age_cat2')]		
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
	
	ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + time + date ~ q_label, value.var='q')
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans	
}

#' @export
#' @keywords internal
#' @import data.table
make_age_cases_overtime_plot <- function(E_casesByAge,regions,pop_info,dates){

	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(E_casesByAge)[2]==length(regions) )
	
	age_bands <- unique(subset(pop_info,select=c('age.cat.label','age.cat','age.cat.from','age.cat.to')))
	age_bands[,midpoint:= (age.cat.from + age.cat.to)/2]
	setnames(age_bands,'age.cat','age_cat')
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		e_acases <- as.data.table( reshape2::melt( E_casesByAge[,m,,] ) )
		setnames(e_acases, 1:4, c('iterations','time','age_cat','e_acases'))
		e_acases <- merge(e_acases,subset(age_bands,select=c('age_cat','midpoint')),by='age_cat')
		
		e_acases[, mf := e_acases*midpoint]
		e_acases_age <- e_acases[, mean_age:=sum(mf)/sum(e_acases), by=c('time','iterations')]	

		#	summarise		
		dt <- e_acases_age[, list(q= quantile(mean_age, prob=ps),
															q_label=p_labs), by=c('time')]
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')

	ans <- dcast.data.table(ans, loc + loc_label + time + date ~ q_label, value.var='q')
	ans	
}


#' @export
#' @keywords internal
#' @import data.table
summarise_attackrate_byage_c <- function(e_acases_byage_c, age_cat_map, pop_info,  regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( length(unique(e_acases_byage_c$loc))==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- e_acases_byage_c[loc==regions[m],c('age_cat','date','M','CL','CU','time')]
    
    #	merge pop_info
    tmp <- subset(pop_info, loc==regions[m], select=c( age.cat, pop, pop_total))
    tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat2, age.cat)), by='age.cat')
    tmp <- tmp[,list(pop=sum(pop)),by='age.cat2']
    setnames(tmp, 'age.cat2', 'age_cat')
    dt <- merge(dt, tmp, by='age_cat')
    
    #	calculate attack rate
    dt[, M:= M/pop]
    dt[, CL:=CL/pop]
    dt[, CU:=CU/pop]
    dt[, pop:=NULL]

    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  
  ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat'), by.x=c('age.cat2'))
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))

  return(ans)
}

#' @export
#' @keywords internal
#' @import data.table
summarise_attackrate_overall_c <- function(e_acases_overall_c, pop_info, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( length(unique(e_acases_overall_c$loc))==length(regions) )
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- e_acases_overall_c[loc==regions[m],c('date','M','CL','CU','time')]
    #	merge pop_info
    tmp <- subset(pop_info, loc==regions[m], select=c( age.cat, pop_total))
    dt[, pop_total:=unique(tmp$pop_total)]
    
    #	calculate attack rate
    dt[, M:= M/pop_total]
    dt[, CL:=CL/pop_total]
    dt[, CU:=CU/pop_total]
    dt[, pop_total:=NULL]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  
  return(ans)
}

#' @export
#' @keywords internal
#' @import data.table
summarise_lambda_byage_c <- function(lambdaByAge, age_cat_map, pop_info, dates, regions)
{
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(lambdaByAge)[2]==length(regions) )
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt( lambdaByAge[,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value'))
		
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','time','iteration')]	
				
		#	summarise		
		dt <- dt[, list( 	q= quantile(value, prob=ps),
						q_label=p_labs), 
				by=c('time','age_cat2')]		
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		dt <- merge(dt, tmp, by='time')
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
	
	ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + time + date ~ q_label, value.var='q')
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans
}

#' @export
#' @keywords internal
#' @import data.table
summarise_mcontacts_byage_c <- function(contacts_ByAge, age_cat_map, date_range=NULL, pop_info, dates, regions)
{
	ps <- c(0.5, 0.025, 0.975, 0.25, 0.75, 0, 1)
	p_labs <- c('M','CL','CU', 'L', 'U', 'min', 'max')	

		stopifnot( dim(contacts_ByAge)[2]==length(regions) )
		
		# proportion of population by age. i.e., sum_m p_age$pop_prop | age = 1
		p_age <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
		p_age <- subset(p_age, loc %in% regions)
		p_age <- merge(p_age, age_cat_map, by='age.cat')
		p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
		tmp <- p_age[,list(pop = sum(pop)),
								 by=c('loc')]
		tmp[, age.cat2 := 0]
		p_age = rbind(p_age, tmp)
		p_age <- p_age[, list(loc=loc, 
													pop=pop,
													pop_prop=pop/sum(pop)),
									 by=c('age.cat2')]
		setnames(p_age, 'age.cat2','age_cat2')
		
		# proportion of population by loc, i.e., sum_a p_loc$pop_prop | loc = 1
		p_loc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
		p_loc <- subset(p_loc, loc %in% regions)
		p_loc <- merge(p_loc, age_cat_map, by='age.cat')
		p_loc <- p_loc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
		p_loc <- p_loc[, list(age.cat2=age.cat2, 
													pop=pop,
													pop_prop=pop/sum(pop)), 
									 by=c('loc')]
		setnames(p_loc, 'age.cat2','age_cat2')
		
		# proportion of population by age and loc, i.e., sum_a sum_m p_ageloc$pop_prop = 1
		p_ageloc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
		p_ageloc <- subset(p_ageloc, loc %in% regions)
		p_ageloc <- merge(p_ageloc, age_cat_map, by='age.cat')
		p_ageloc <- p_ageloc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
		p_ageloc <- p_ageloc[, pop_prop_t:=pop/sum(p_ageloc$pop), ]
		setnames(p_ageloc, 'age.cat2','age_cat2')
		
		ans <- vector('list',length(regions)) 
		ans_overall <- vector('list',length(regions))
		#	loop over locations for speed. very hard to take quantiles on such large data
		for(m in seq_along(regions))
		{
			#m <- 1	  
			cat('\nProcessing state',m,' ',regions[m])
			
			dt <- as.data.table( reshape2::melt( contacts_ByAge[,m,,] ) )
			setnames(dt, 1:4, c('iteration','time','age_cat','value'))
			
			# add time index and date before summarising to subset to bics survey dates
			tmp <- unique(subset(dt, select=time))
			tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
			dt <- merge(dt, tmp, by='time')
			
			if(!is.null(date_range)){
				dt <- subset(dt,date>=min(date_range) & date<max(date_range))
			}
			
			#	add further stats for aggregating now (cheap merge)					
			tmp <- subset(pop_info, loc==regions[m], select=c(age.cat, pop))
			tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat, age.cat2)), by='age.cat')
			tmp <- tmp[, list(age.cat=age.cat, prop_pop2= pop/sum(pop)), by='age.cat2']
			setnames(tmp, c('age.cat','age.cat2'), c('age_cat','age_cat2'))
			dt <- merge(dt, tmp, by='age_cat')
			
			# marginal contact intensity
			dt <- dt[, list(age_cat2=age_cat2[1],
											prop_pop2=prop_pop2[1],
											value= value), 
							 by=c('iteration','age_cat','time')]
			
			#	aggregate by age cat2						
			dt <- dt[, list(value=sum(prop_pop2*value)), by=c('age_cat2','iteration','time')]

			
			# add time index and date before summarising to subset to bics survey dates
			tmp2<- unique(subset(dt, select=time))
			tmp2[, date:= dates[[m]][tmp2$time[1]] + tmp2$time - tmp2$time[1]]
			#dt <- merge(dt, tmp2, by='time')
			
			#	to compute national average: add prop_pop by age, time and save 	
			# save time mapping for region m
			tmp <- merge(dt, subset(p_age, loc==regions[m], c(age_cat2, pop_prop)), by=c('age_cat2'))
			tmp <- merge(tmp, subset(p_ageloc, loc==regions[m], c(age_cat2, pop_prop_t)), by=c('age_cat2'))
			# add dates: we need to do here because the time index are different by state
			tmp <- merge(tmp, tmp2, by = 'time')
			tmp[, loc:= regions[m]]
			ans_overall[[m]] = copy(tmp)
			
			#	calculate overall and save as age.cat 0		
			tmp <- merge(dt, subset(p_loc, loc==regions[m], c(age_cat2, pop_prop)), by=c('age_cat2'))
			tmp <- tmp[, list(value= sum(value*pop_prop)), by= c('iteration', 'time')]
			tmp[, age_cat2:=0]
			dt <- merge(dt,tmp,by=c('iteration','time','age_cat2','value'),all=T)
			
			#	summarise		
			dt <- dt[, list(q= quantile(value, prob=ps),
											q_label=p_labs), 
							 by=c('age_cat2')]		
			
			#	add loc 
			dt[, loc:= regions[m]]
			
			# build new data object only after summarised
			ans[[m]] <- copy(dt)
		}
		ans <- do.call('rbind',ans)
		ans_overall <- do.call('rbind',ans_overall)
		
		#
		# National average
		cat('\nProcessing United-States')
		# keep date that are common to every region
		tmp = unique(select(ans_overall, loc, date))
		dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
		dt <- subset(ans_overall, date %in% as.Date(dates_common))
		#	calculate overall across age and state and save as age.cat 0		
		tmp <- dt[, list(value= sum(value*pop_prop_t)), by= c('iteration', 'date')]
		tmp[, age_cat2:=0]
		#	calculate overall across state by age
		dt <- dt[, list(value= sum(value*pop_prop)), by=c('age_cat2','date','iteration')]
		dt <- merge(dt,tmp,by=c('iteration','date','age_cat2','value'),all=T)
		#	summarise
		dt <- dt[, list(q= quantile(value, prob=ps),
										q_label= p_labs), 
						 by=c('age_cat2')]
		
		#	make human readable loc labels
		tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
		ans <- merge(ans, tmp, by='loc')
		dt[,loc:= "US"]
		dt[,loc_label := "United-States"]
		ans <- merge(ans, dt,by=c('loc','loc_label','age_cat2','q','q_label'),all=T)
		
		ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
											 data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
								 ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
		
		ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label ~ q_label, value.var='q')
		setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
		ans
	}


#' @export
#' @keywords internal
#' @import data.table
make_exact_epidemiologic_scenarios_from_Rta <- function(Rt_byage_less_than_one,cutoff,mobility_data)
{
  # separate dt by weekends and weekdays
  Rt_byage_less_than_one[,date.type:=weekdays(date)]
  Rt_byage_less_than_one[,week:=floor_date(date,'week', week_start = 1)]
  Rt_byage_less_than_one_weekday <- Rt_byage_less_than_one[date.type %in% c('Monday','Tuesday','Wednesday','Thursday','Friday')]
  Rt_byage_less_than_one_weekend <- Rt_byage_less_than_one[date.type %in% c('Saturday','Sunday')]
  Rt_byage_less_than_one_weekday <-   Rt_byage_less_than_one_weekday[,list(greater_than_cutoff=sum(value>cutoff),
                                               total=length(value)),by=c('loc_label','age_band','week')]
  Rt_byage_less_than_one_weekday[,prop_greater_than_cutoff:=greater_than_cutoff/total]
  Rt_byage_less_than_one_weekend <-   Rt_byage_less_than_one_weekend[,list(greater_than_cutoff=sum(value>cutoff),
                                                total=length(value)),by=c('loc_label','age_band','week')]
  Rt_byage_less_than_one_weekend[,prop_greater_than_cutoff:=greater_than_cutoff/total]
  
  # rebound date
  rebound_date <- unique(subset(mobility_data, select=c('loc_label','rebound_date')))
  rebound_date <- rbind(rebound_date,
                        data.table(loc_label='United-States',rebound_date=max(rebound_date$rebound_date)))
  clusters <- vector()
  for (state in unique(Rt_byage_less_than_one$loc_label)){
    dt_weekday <- Rt_byage_less_than_one_weekday[loc_label==state]
    dt_weekend <- Rt_byage_less_than_one_weekend[loc_label==state]
    last4weeks <- sort(unique(dt_weekday[total==5,week]),decreasing = TRUE)[4]
    rebound_date_state <- rebound_date[loc_label==state,rebound_date]
    controled_week <- dt_weekday[total==5, list(CONTROL=all(prop_greater_than_cutoff==1)), by='week']
    dt_weekday = dt_weekday[week >= rebound_date_state]
    dt_weekend = dt_weekend[week >= rebound_date_state]

    if(
      # weekdays have more Rt < 1 than weekends
      mean(dt_weekday$prop_greater_than_cutoff) - mean(dt_weekend$prop_greater_than_cutoff)>0.1
    ){
      clusters <- c(clusters, "unclear")
    }else if(
      # any week sustained control among all groups
      any(controled_week$CONTROL)
    ){
      cont_controled <- ave(controled_week$CONTROL, cumsum(!controled_week$CONTROL), FUN = cumsum)[nrow(controled_week)]
      if(
        # controled at the end
        cont_controled >= 1
      ){
        clusters <- c(clusters, "Rt<1 for all ages in last 4 weeks")
      }else{
        clusters <- c(clusters, "Rt>1 after 4 week period of Rt<1 for all ages")
      }
      # no weeks sustained control among all groups
    }else{
      # the age group has all weekdays Rt > 1
      tmp = dt_weekday[total==5 & week >= last4weeks, list(SPREAD=any(prop_greater_than_cutoff < 1)), by='age_band']
      if(nrow(tmp[SPREAD==TRUE]) > 1){
        clusters <- c(clusters, "Rt>1 for 35-49 and other age groups in last 4 weeks and no prior period with sustained Rt<1")
      }else if(nrow(tmp[SPREAD==TRUE]) == 1) {
        if(tmp[SPREAD==TRUE, age_band] == '35-49'){
          clusters <- c(clusters, "Rt>1 for 35-49 in last 4 weeks and no prior period with sustained Rt<1")
        }else{
          clusters <- c(clusters, NA_real_)
        }
      
      # tmp1 = dt_weekday[total==5 & week >= rebound_date_state + 7 & age_band=='35-49', list(SPREAD=all(prop_greater_than_cutoff ==0)), by='age_band']
      # tmp2 = dt_weekday[total==5 & week >= rebound_date_state + 7& age_band!='35-49', list(SPREAD=any(prop_greater_than_cutoff ==0)), by='age_band']
      # # '35-49' with all weekdays Rt > 1
      # if(nrow(tmp1[SPREAD==TRUE]) ==1){
      #   if(nrow(tmp2[SPREAD==TRUE]) ==0){
      #     clusters <- c(clusters, "sustained reproductive numbers > 1 in adults")
      #   }else{
      #     clusters <- c(clusters, "sustained reproductive numbers > 1 across many age groups") 
      #   }
      }else{
        clusters <- c(clusters, 'unclear')
      }
    }
  }
  
  return(list(clusters,unique(Rt_byage_less_than_one$loc_label),rebound_date))
}

#' @export
#' @keywords internal
#' @import data.table
summarise_total_flow_byage_c <- function(flows, time_idx, age_cat_map, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(flows)[5]==length(regions) )
  
  # proportion of population by age. i.e., sum_m p_age$pop_prop | age = 1
  p_age <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_age <- subset(p_age, loc %in% regions)
  p_age <- merge(p_age, age_cat_map, by='age.cat')
  p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  tmp <- p_age[,list(pop = sum(pop)),
               by=c('loc')]
  tmp[, age.cat2 := 0]
  p_age = rbind(p_age, tmp)
  p_age <- p_age[, list(loc=loc, 
                        pop=pop,
                        pop_prop=pop/sum(pop)),
                 by=c('age.cat2')]
  setnames(p_age, 'age.cat2','age_index2')
  
  # proportion of population by loc, i.e., sum_a p_loc$pop_prop | loc = 1
  p_loc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_loc <- subset(p_loc, loc %in% regions)
  p_loc <- merge(p_loc, age_cat_map, by='age.cat')
  p_loc <- p_loc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_loc <- p_loc[, list(age.cat2=age.cat2, 
                        pop=pop,
                        pop_prop=pop/sum(pop)), 
                 by=c('loc')]
  setnames(p_loc, 'age.cat2','age_index2')
  
  # proportion of population by age and loc, i.e., sum_a sum_m p_ageloc$pop_prop = 1
  p_ageloc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_ageloc <- subset(p_ageloc, loc %in% regions)
  p_ageloc <- merge(p_ageloc, age_cat_map, by='age.cat')
  p_ageloc <- p_ageloc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_ageloc <- p_ageloc[, pop_prop_t:=pop/sum(p_ageloc$pop), ]
  setnames(p_ageloc, 'age.cat2','age_index2')
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( flows[,,,,m] ) )
    setnames(dt, 1:5, c('iteration','time','source_age_cat','rec_age_cat','value'))
    
    # take total flows per source 
    dt = dt[, list(value = sum(value)),
            by = c('iteration','time','source_age_cat')]
    setnames(dt, 'source_age_cat','age_index2')
    
    # save time mapping for region m
    tmp1 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp2 <- unique(subset(dt, time<=length(tmp1), select=c(time)))	  
    dt <- subset(dt, time<=length(tmp1))
    tmp2[, time_idx:= tmp1]
    tmp2[, date:= dates[[m]][tmp2$time_idx[1]] + tmp2$time_idx - tmp2$time_idx[1]]
    
    #	to compute national average: add prop_pop by age, time and save 	
    tmp <- merge(dt, subset(p_age, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- merge(tmp, subset(p_ageloc, loc==regions[m], c(age_index2, pop_prop_t)), by=c('age_index2'))
    # add dates: we need to do here because the time index are different by state
    tmp <- merge(tmp, tmp2, by = 'time')
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    #	calculate overall and save as age.cat 0		
    tmp <- merge(dt, subset(p_loc, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- tmp[, list(value= sum(value*pop_prop)), by= c('iteration', 'time')]
    tmp[, age_index2:=0]
    dt <- rbind(dt,tmp)
    
    #	summarise
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label= p_labs), 
             by=c('time','age_index2')]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    dt <- merge(dt, tmp2, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #
  # National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  dt <- subset(ans_overall, date %in% as.Date(dates_common))
  #	calculate overall across age and state and save as age.cat 0		
  tmp <- dt[, list(value= sum(value*pop_prop_t)), by= c('iteration', 'date')]
  tmp[, age_index2:=0]
  #	calculate overall across state by age
  dt <- dt[, list(value= sum(value*pop_prop)), by=c('age_index2','date','iteration')]
  dt <- rbind(dt,tmp)
  #	summarise
  dt <- dt[, list(q= quantile(value, prob=ps),
                  q_label= p_labs), 
           by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ans <- select(ans, -time_idx)
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q')
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_prop_flow_byage_c <- function(flows, time_idx, age_cat_map, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(flows)[5]==length(regions) )
  
  # proportion of population by age. i.e., sum_m p_age$pop_prop | age = 1
  p_age <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_age <- subset(p_age, loc %in% regions)
  p_age <- merge(p_age, age_cat_map, by='age.cat')
  p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  tmp <- p_age[,list(pop = sum(pop)),
               by=c('loc')]
  tmp[, age.cat2 := 0]
  p_age = rbind(p_age, tmp)
  p_age <- p_age[, list(loc=loc, 
                        pop=pop,
                        pop_prop=pop/sum(pop)),
                 by=c('age.cat2')]
  setnames(p_age, 'age.cat2','age_index2')
  
  # proportion of population by loc, i.e., sum_a p_loc$pop_prop | loc = 1
  p_loc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_loc <- subset(p_loc, loc %in% regions)
  p_loc <- merge(p_loc, age_cat_map, by='age.cat')
  p_loc <- p_loc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_loc <- p_loc[, list(age.cat2=age.cat2, 
                        pop=pop,
                        pop_prop=pop/sum(pop)), 
                 by=c('loc')]
  setnames(p_loc, 'age.cat2','age_index2')
  
  # proportion of population by age and loc, i.e., sum_a sum_m p_ageloc$pop_prop = 1
  p_ageloc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_ageloc <- subset(p_ageloc, loc %in% regions)
  p_ageloc <- merge(p_ageloc, age_cat_map, by='age.cat')
  p_ageloc <- p_ageloc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_ageloc <- p_ageloc[, pop_prop_t:=pop/sum(p_ageloc$pop), ]
  setnames(p_ageloc, 'age.cat2','age_index2')
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( flows[,,,,m] ) )
    setnames(dt, 1:5, c('iteration','time','source_age_cat','rec_age_cat','value'))
    
    # take total of flows per source age group
    dt = dt[, list(value = sum(value)),
            by = c('iteration','time','source_age_cat')]
    setnames(dt, 'source_age_cat','age_index2')
    
    # save time mapping for region m
    tmp1 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp2 <- unique(subset(dt, time<=length(tmp1), select=c(time)))	  
    dt <- subset(dt, time<=length(tmp1))
    tmp2[, time_idx:= tmp1]
    tmp2[, date:= dates[[m]][tmp2$time_idx[1]] + tmp2$time_idx - tmp2$time_idx[1]]
    
    #	to compute national average: add prop_pop, time and save 	
    tmp <- merge(dt, subset(p_age, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- merge(tmp, subset(p_ageloc, loc==regions[m], c(age_index2, pop_prop_t)), by=c('age_index2'))
    # add dates: we need to do here because the time index are different by state
    tmp <- merge(tmp, tmp2, by = 'time')
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    #	calculate overall and save as age.cat 0		
    tmp <- merge(dt, subset(p_loc, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- tmp[, list(value= sum(value*pop_prop)), by= c('iteration', 'time')]
    tmp[, age_index2:=0]
    dt <- rbind(dt,tmp)
    
    # take proportion of total flows
    # overall: needs to compute total flows in a first step to find overall prop flow
    tmp = subset(dt, age_index2 !=0)
    tmp = tmp[, list(value_total = sum(value)),
              by = c('iteration','time')]
    tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","time"))
    dt1 = tmp1[age_index2 == 0, list(value = value/value_total,
                             age_index2 = age_index2),
       by = c('iteration','time')]
    # by age
    dt2 = dt[age_index2 != 0, list(value = value/sum(value),
                                  age_index2 = age_index2),
            by = c('iteration','time')]
    dt = rbind(dt1, dt2)
    
    #	summarise
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label= p_labs), 
             by=c('time','age_index2')]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    dt <- merge(dt, tmp2, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #
  # National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  dt <- subset(ans_overall, date %in% as.Date(dates_common))
  #	calculate overall across age and state and save as age.cat 0		
  tmp <- dt[, list(value= sum(value*pop_prop_t)), by= c('iteration', 'date')]
  tmp[, age_index2:=0]
  #	calculate overall across state by age
  dt <- dt[, list(value= sum(value*pop_prop)), by=c('age_index2','date','iteration')]
  dt <- rbind(dt,tmp)
  # take proportion of total flows
  tmp = subset(dt, age_index2 !=0)
  tmp = tmp[, list(value_total = sum(value)),
            by = c('iteration','date')]
  tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","date"))
  dt1 = tmp1[age_index2 == 0, list(value = value/value_total,
                                   age_index2 = age_index2),
             by = c('iteration','date')]
  dt2 = dt[age_index2 != 0, list(value = value/sum(value),
                                 age_index2 = age_index2),
           by = c('iteration','date')]
  dt = rbind(dt1, dt2)
  #	summarise
  dt <- dt[, list(q= quantile(value, prob=ps),
                  q_label= p_labs), 
           by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ans <- select(ans, -time_idx)
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q')
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_cumprop_flow_byage_c <- function(flows, time_idx, age_cat_map, cutdate, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(flows)[5]==length(regions) )
  
  # proportion of population by age. i.e., sum_m p_age$pop_prop | age = 1
  p_age <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_age <- subset(p_age, loc %in% regions)
  p_age <- merge(p_age, age_cat_map, by='age.cat')
  p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  tmp <- p_age[,list(pop = sum(pop)),
               by=c('loc')]
  tmp[, age.cat2 := 0]
  p_age = rbind(p_age, tmp)
  p_age <- p_age[, list(loc=loc, 
                        pop=pop,
                        pop_prop=pop/sum(pop)),
                 by=c('age.cat2')]
  setnames(p_age, 'age.cat2','age_index2')
  
  # proportion of population by loc, i.e., sum_a p_loc$pop_prop | loc = 1
  p_loc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_loc <- subset(p_loc, loc %in% regions)
  p_loc <- merge(p_loc, age_cat_map, by='age.cat')
  p_loc <- p_loc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_loc <- p_loc[, list(age.cat2=age.cat2, 
                        pop=pop,
                        pop_prop=pop/sum(pop)), 
                 by=c('loc')]
  setnames(p_loc, 'age.cat2','age_index2')
  
  # proportion of population by age and loc, i.e., sum_a sum_m p_ageloc$pop_prop = 1
  p_ageloc <- unique(subset(pop_info, select=c(loc,age.cat,pop)))
  p_ageloc <- subset(p_ageloc, loc %in% regions)
  p_ageloc <- merge(p_ageloc, age_cat_map, by='age.cat')
  p_ageloc <- p_ageloc[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_ageloc <- p_ageloc[, pop_prop_t:=pop/sum(p_ageloc$pop), ]
  setnames(p_ageloc, 'age.cat2','age_index2')
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( flows[,,,,m] ) )
    setnames(dt, 1:5, c('iteration','time','source_age_cat','rec_age_cat','value'))
    
    # take total of flows per source age group
    dt = dt[, list(value = sum(value)),
            by = c('iteration','time','source_age_cat')]
    setnames(dt, 'source_age_cat','age_index2')
    
    # save time mapping for region m
    tmp1 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp2 <- unique(subset(dt, time<=length(tmp1), select=c(time)))	  
    dt <- subset(dt, time<=length(tmp1))
    tmp2[, time_idx:= tmp1]
    tmp2[, date:= dates[[m]][tmp2$time_idx[1]] + tmp2$time_idx - tmp2$time_idx[1]]

    # add dates: we need to do here because the time index are different by state
    dt <- merge(dt, tmp2, by = 'time')
    dt[, loc:= regions[m]]
    
    # calculate cumulative proportions after a specified date only
    if(!is.null(cutdate)){
    	dt <- subset(dt,date>=cutdate)
    }
    	
    # take cum of the flows over time
    dt = dt[, list(value = cumsum(value),
                     time = time,
    							 	 date = date),
              by = c('iteration','age_index2')]
    
    #	to compute national average: add prop_pop, time and save 	
    tmp <- merge(dt, subset(p_age, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- merge(tmp, subset(p_ageloc, loc==regions[m], c(age_index2, pop_prop_t)), by=c('age_index2'))
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    #	calculate overall and save as age.cat 0		
    tmp <- merge(dt, subset(p_loc, loc==regions[m], c(age_index2, pop_prop)), by=c('age_index2'))
    tmp <- tmp[, list(value= sum(value*pop_prop)), by= c('iteration', 'time','date')]
    tmp[, age_index2:=0]
    dt <- merge(dt,tmp,by=c('iteration','age_index2','value','time','date'),all=T)
    
    # take proportion of total flows
    # overall: needs to compute total flows in a first step to find overall prop flow
    tmp = subset(dt, age_index2 !=0)
    tmp = tmp[, list(value_total = sum(value)),
              by = c('iteration','time')]
    tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","time"))
    dt1 = tmp1[age_index2 == 0, list(value = value/value_total,
                                     age_index2 = age_index2),
               by = c('iteration','time')]
    # by age
    dt2 = dt[age_index2 != 0, list(value = value/sum(value),
                                   age_index2 = age_index2),
             by = c('iteration','time')]
    dt = rbind(dt1, dt2)
    
    #	summarise
    dt <- dt[, list(q= quantile(value, prob=ps),
                    q_label= p_labs), 
             by=c('time','age_index2')]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    dt <- merge(dt, tmp2, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #
  # National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  dt <- subset(ans_overall, date %in% as.Date(dates_common))
  #	calculate overall across age and state and save as age.cat 0		
  tmp <- dt[, list(value= sum(value*pop_prop_t)), by= c('iteration', 'date')]
  tmp[, age_index2:=0]
  #	calculate overall across state by age
  dt <- dt[, list(value= sum(value*pop_prop)), by=c('age_index2','date','iteration')]
  dt <- rbind(dt,tmp)
  # take proportion of total flows
  tmp = subset(dt, age_index2 !=0)
  tmp = tmp[, list(value_total = sum(value)),
            by = c('iteration','date')]
  tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","date"))
  dt1 = tmp1[age_index2 == 0, list(value = value/value_total,
                                   age_index2 = age_index2),
             by = c('iteration','date')]
  dt2 = dt[age_index2 != 0, list(value = value/sum(value),
                                 age_index2 = age_index2),
           by = c('iteration','date')]
  dt = rbind(dt1, dt2)
  #	summarise
  dt <- dt[, list(q= quantile(value, prob=ps),
                  q_label= p_labs), 
           by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ans <- select(ans, -time_idx)
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q')
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
make_flow_forecast_school_reopen_summaries = function(in_flow_school_reopen1, in_flow_school_reopen0, time_idx, dates, regions, pop_info)
{
  
  cat("\n ----------- make_flow_summaries ----------- \n")
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  flow_school_reopen1 <- as.data.table( reshape2::melt( in_flow_school_reopen1 ) )
  setnames(flow_school_reopen1, 1:6, c('iteration','time','source_age_cat','rec_age_cat','loc','flow_school_reopen0'))
  flow_school_reopen1 <- subset(flow_school_reopen1, !is.na(flow_school_reopen0))
  
  flow_school_reopen0 <- as.data.table( reshape2::melt( in_flow_school_reopen0 ) )
  setnames(flow_school_reopen0, 1:6, c('iteration','time','source_age_cat','rec_age_cat','loc','flow_school_reopen1'))
  flow_school_reopen0 <- subset(flow_school_reopen0, !is.na(flow_school_reopen1))
  
  # avoid merging, indices are the same
  flow = cbind(flow_school_reopen1, select(flow_school_reopen0, flow_school_reopen1))
  
  ans <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 7	  
    cat('\nProcessing state',m,' ',regions[m])
    flow_m <- subset(flow, loc==m, select=-loc)
    
    #	calculate flows in numbers
    flow_s = flow_m
    flow_s[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1+ 1e-16) / (flow_school_reopen0+ 1e-16)]
    flow_s[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    flow_s <- flow_s[, list( q_school_reopen1 = quantile(flow_school_reopen1, prob=ps,na.rm=TRUE),
                             q_school_reopen0 = quantile(flow_school_reopen0, prob=ps,na.rm=TRUE),
                             q_school_reopen1_reopen0_ratio = quantile(flow_school_reopen1_reopen0_ratio, prob=ps),
                             q_school_reopen1_reopen0_diff = quantile(flow_school_reopen1_reopen0_diff, prob=ps),
                             q_label=p_labs), 
                     by=c('time','source_age_cat','rec_age_cat')]
    flow_s[, stat:= 'flow_abs']			
    
    #	calculate flows in %
    tmp <- flow_m[, list(	source_age_cat=source_age_cat,
                          rec_age_cat=rec_age_cat,
                          flow_school_reopen1=flow_school_reopen1/sum(flow_school_reopen1),
                          flow_school_reopen0=flow_school_reopen0/sum(flow_school_reopen0)), 
                  by=c('iteration','time')]		
    tmp[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1+ 1e-16) / (flow_school_reopen0+ 1e-16)]
    tmp[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    tmp <- tmp[, list( 	q_school_reopen1 = quantile(flow_school_reopen1, prob=ps,na.rm=TRUE),
                        q_school_reopen0 = quantile(flow_school_reopen0, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_ratio = quantile(flow_school_reopen1_reopen0_ratio, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_diff = quantile(flow_school_reopen1_reopen0_diff, prob=ps,na.rm=TRUE),
                        q_label=p_labs), 
               by=c('time','source_age_cat','rec_age_cat')]								
    tmp[, stat:= 'flow_prop']			
    flow_s <- rbind(flow_s, tmp)
    
    #	calculate sources in %
    # possible that there are no flows into one age cat, so use na.rm=TRUE
    tmp <- flow_m[, list(	source_age_cat=source_age_cat,
                          flow_school_reopen1=flow_school_reopen1/sum(flow_school_reopen1),
                          flow_school_reopen0=flow_school_reopen0/sum(flow_school_reopen0)), 
                  by=c('iteration','time','rec_age_cat')]		
    tmp[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1+ 1e-16) / (flow_school_reopen0+ 1e-16)]
    tmp[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    tmp <- tmp[, list( 	q_school_reopen1 = quantile(flow_school_reopen1, prob=ps,na.rm=TRUE),
                        q_school_reopen0 = quantile(flow_school_reopen0, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_ratio = quantile(flow_school_reopen1_reopen0_ratio, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_diff = quantile(flow_school_reopen1_reopen0_diff, prob=ps,na.rm=TRUE),
                        q_label=p_labs), 
               by=c('time','source_age_cat','rec_age_cat')]								
    tmp[, stat:= 'sources_prop']			
    flow_s <- rbind(flow_s, tmp)
    
    #	calculate onward transmissions in %
    # possible that there are no flows from one age cat, so use na.rm=TRUE
    tmp <- flow_m[, list(	rec_age_cat=rec_age_cat,
                          flow_school_reopen1=flow_school_reopen1/sum(flow_school_reopen1),
                          flow_school_reopen0=flow_school_reopen0/sum(flow_school_reopen0)), 
                  by=c('iteration','time','source_age_cat')]					
    tmp[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1+ 1e-16) / (flow_school_reopen0+ 1e-16)]
    tmp[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    tmp <- tmp[, list( 	q_school_reopen1 = quantile(flow_school_reopen1, prob=ps,na.rm=TRUE),
                        q_school_reopen0 = quantile(flow_school_reopen0, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_ratio = quantile(flow_school_reopen1_reopen0_ratio, prob=ps,na.rm=TRUE),
                        q_school_reopen1_reopen0_diff = quantile(flow_school_reopen1_reopen0_diff, prob=ps,na.rm=TRUE),
                        q_label=p_labs), 
               by=c('time','source_age_cat','rec_age_cat')]								
    tmp[, stat:= 'rec_prop']			
    flow_s <- rbind(flow_s, tmp)
    
    #	add loc 
    flow_s[, loc:= regions[m]]
    
    # add time index and date
    tmp2 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp <- unique(subset(flow_s, time<=length(tmp2), select=c(time)))	  
    flow_s <- subset(flow_s, time<=length(tmp2))
    tmp[, time_idx:= tmp2]
    tmp[, date:= dates[[m]][tmp$time_idx[1]] + tmp$time_idx - tmp$time_idx[1]]
    flow_s <- merge(flow_s, tmp, by='time')
    flow_s[, time:=NULL]
    setnames(flow_s, 'time_idx','time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(flow_s)
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  tmp_school_reopen1 <- dcast.data.table(ans, stat + loc + loc_label + time + date + source_age_cat + rec_age_cat ~ q_label, value.var='q_school_reopen1')
  tmp_school_reopen1[, variable := "school_reopen1"]
  tmp_school_reopen0 <- dcast.data.table(ans, stat + loc + loc_label + time + date + source_age_cat + rec_age_cat ~ q_label, value.var='q_school_reopen0')
  tmp_school_reopen0[, variable := "school_reopen0"]
  tmp_school_reopen1_reopen0_ratio <- dcast.data.table(ans, stat + loc + loc_label + time + date + source_age_cat + rec_age_cat ~ q_label, value.var='q_school_reopen1_reopen0_ratio')
  tmp_school_reopen1_reopen0_ratio[, variable := "school_reopen1_reopen0_ratio"]
  tmp_school_reopen1_reopen0_diff <- dcast.data.table(ans, stat + loc + loc_label + time + date + source_age_cat + rec_age_cat ~ q_label, value.var='q_school_reopen1_reopen0_diff')
  tmp_school_reopen1_reopen0_diff[, variable := "school_reopen1_reopen0_diff"]
  
  tmp = rbind(tmp_school_reopen1, tmp_school_reopen0, tmp_school_reopen1_reopen0_ratio, tmp_school_reopen1_reopen0_diff)
  
  return(tmp)
}

#' @export
#' @keywords internal
#' @import data.table
summarise_cumprop_flow_byage_forecast_school_reopen_K5 <- function(in_flow_school_reopen1, in_flow_school_reopen0, start_date, time_idx, age_cat_map, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(in_flow_school_reopen0)[5]==length(regions) )
  stopifnot( dim(in_flow_school_reopen1)[5]==length(regions) )
  
  p_age <- unique(subset(pop_info, select=c(loc,age.cat.new,pop)))
  p_age <- subset(p_age, loc %in% regions)
  p_age <- merge(p_age, age_cat_map, by='age.cat.new')
  p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_loc <- p_age[, list(age.cat2=age.cat2, 
                        pop=pop,
                        pop_prop_loc=pop/sum(pop)),
                 by=c('loc')]
  p_age <- p_age[, list(loc=loc, 
                        pop=pop,
                        pop_prop_age=pop/sum(pop)),
                 by=c('age.cat2')]
  p_ageloc <- p_age[, list(age.cat2=age.cat2, 
                        loc = loc,
                        pop=pop,
                        pop_prop_ageloc=pop/sum(pop))]
  p_age_loc = merge(merge(p_age, p_loc, by = c("loc", "age.cat2", "pop")), p_ageloc, by = c("loc", "age.cat2", "pop"))
  setnames(p_age_loc, 'age.cat2','age_index2')
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( in_flow_school_reopen1[,,,,m] ) )
    setnames(dt, 1:5, c('iteration','time','source_age_cat','rec_age_cat','flow_school_reopen1'))
    dt2 <- as.data.table( reshape2::melt( in_flow_school_reopen0[,,,,m] ) )
    setnames(dt2, 1:5, c('iteration','time','source_age_cat','rec_age_cat','flow_school_reopen0'))
    dt = cbind(dt, select(dt2, flow_school_reopen0))
    
    # take total of flows per source age group
    dt = dt[, list(flow_school_reopen1 = sum(flow_school_reopen1),
                   flow_school_reopen0 = sum(flow_school_reopen0)),
            by = c('iteration','time','source_age_cat')]
    setnames(dt, 'source_age_cat','age_index2')
    
    # take cum of the flows over time
    dt = dt[, list(flow_school_reopen1 = cumsum(flow_school_reopen1),
                   flow_school_reopen0 = cumsum(flow_school_reopen0),
                   time = time),
            by = c('iteration','age_index2')]
    
    # aggregate by new age 
    dt2 = dt[age_index2 == 2,]
    setnames(dt2, c("flow_school_reopen1", "flow_school_reopen0"), c("flow_school_reopen1_1019", "flow_school_reopen0_1019"))
    dt = merge(dt, select(dt2, -age_index2), by = c("iteration", "time"), all.x = T)
    dt[age_index2 == 1, flow_school_reopen1 := flow_school_reopen1 + 2/10 * flow_school_reopen1_1019]
    dt[age_index2 == 1, flow_school_reopen0 := flow_school_reopen0 + 2/10 * flow_school_reopen0_1019]
    dt[age_index2 == 2, flow_school_reopen1 := 8/10 * flow_school_reopen1_1019]
    dt[age_index2 == 2, flow_school_reopen0 := 8/10 * flow_school_reopen0_1019]
    dt = select(dt, -flow_school_reopen1_1019, -flow_school_reopen0_1019)
    
    # save time mapping for region m
    tmp1 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp2 <- unique(subset(dt, time<=length(tmp1), select=c(time)))	  
    dt <- subset(dt, time<=length(tmp1))
    tmp2[, time_idx:= tmp1]
    tmp2[, date:= dates[[m]][tmp2$time_idx[1]] + tmp2$time_idx - tmp2$time_idx[1]]
    tmp2 = subset(tmp2, date >= start_date)
    
    #	to compute national average: add prop_pop, time and save 	
    tmp <- merge(dt, subset(p_age_loc, loc==regions[m], c(age_index2, pop_prop_age, pop_prop_loc, pop_prop_ageloc)), by=c('age_index2'))
    # add dates: we need to do here because the time index are different by state
    tmp <- merge(tmp, tmp2, by = 'time')
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    #	calculate overall and save as age.cat 0		
    tmp <- merge(dt, subset(p_age_loc, loc==regions[m], c(age_index2, pop_prop_loc)), by=c('age_index2'))
    tmp <- tmp[, list(flow_school_reopen1 = sum(flow_school_reopen1*pop_prop_loc), 
                      flow_school_reopen0 = sum(flow_school_reopen0*pop_prop_loc) ), by= c('iteration', 'time')]
    tmp[, age_index2:=0]
    dt <- rbind(dt,tmp)
    
    # take proportion of total flows
    # overall: needs to compute total flows in a first step to find overall prop flow
    tmp = subset(dt, age_index2 !=0)
    tmp = tmp[, list(flow_school_reopen1_total = sum(flow_school_reopen1),
                     flow_school_reopen0_total = sum(flow_school_reopen0)),
              by = c('iteration','time')]
    tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","time"))
    dt1 = tmp1[age_index2 == 0, list(flow_school_reopen1 = flow_school_reopen1/flow_school_reopen1_total,
                                     flow_school_reopen0 = flow_school_reopen0/flow_school_reopen0_total,
                                     age_index2 = age_index2),
               by = c('iteration','time')]
    # by age
    dt2 = dt[age_index2 != 0, list(flow_school_reopen1 = flow_school_reopen1/sum(flow_school_reopen1),
                                   flow_school_reopen0 = flow_school_reopen0/sum(flow_school_reopen0),
                                   age_index2 = age_index2),
             by = c('iteration','time')]
    dt = rbind(dt1, dt2)
    
    
    # Take ratio and difference 
    dt[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1 + 1e-16) / (flow_school_reopen0 + 1e-16)] # add very small noise for division
    dt[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    
    #	summarise
    dt <- dt[, list(q_school_reopen1= quantile(flow_school_reopen1, prob=ps),
                    q_school_reopen0= quantile(flow_school_reopen0, prob=ps),
                    q_school_reopen1_reopen0_ratio= quantile(flow_school_reopen1_reopen0_ratio, prob=ps),
                    q_school_reopen1_reopen0_diff= quantile(flow_school_reopen1_reopen0_diff, prob=ps),
                    q_label= p_labs), 
             by=c('time','age_index2')]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    dt <- merge(dt, tmp2, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #
  # National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  dt <- subset(ans_overall, date %in% as.Date(dates_common))
  #	calculate overall across age and state and save as age.cat 0		
  tmp <- dt[, list(flow_school_reopen1= sum(flow_school_reopen1*pop_prop_ageloc),
                   flow_school_reopen0= sum(flow_school_reopen0*pop_prop_ageloc)), by= c('iteration', 'date')]
  tmp[, age_index2:=0]
  #	calculate overall across state by age
  dt <- dt[, list(flow_school_reopen1= sum(flow_school_reopen1*pop_prop_age),
                  flow_school_reopen0= sum(flow_school_reopen0*pop_prop_age)), by=c('age_index2','date','iteration')]
  dt <- rbind(dt,tmp)
  # take proportion of total flows
  tmp = subset(dt, age_index2 !=0)
  tmp = tmp[, list(flow_school_reopen1_total = sum(flow_school_reopen1),
                   flow_school_reopen0_total = sum(flow_school_reopen0)),
            by = c('iteration','date')]
  tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","date"))
  dt1 = tmp1[age_index2 == 0, list(flow_school_reopen1 = flow_school_reopen1/flow_school_reopen1_total,
                                   flow_school_reopen0 = flow_school_reopen0/flow_school_reopen0_total,
                                   age_index2 = age_index2),
             by = c('iteration','date')]
  dt2 = dt[age_index2 != 0, list(flow_school_reopen1 = flow_school_reopen1/sum(flow_school_reopen1),
                                 flow_school_reopen0 = flow_school_reopen0/sum(flow_school_reopen0),
                                 age_index2 = age_index2),
           by = c('iteration','date')]
  dt = rbind(dt1, dt2)
  # Take ratio and difference 
  dt[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1 + 1e-16)  / (flow_school_reopen0+ 1e-16)]
  dt[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
  #	summarise
  dt <- dt[, list(q_school_reopen1= quantile(flow_school_reopen1, prob=ps),
                  q_school_reopen0= quantile(flow_school_reopen0, prob=ps),
                  q_school_reopen1_reopen0_ratio= quantile(flow_school_reopen1_reopen0_ratio, prob=ps),
                  q_school_reopen1_reopen0_diff= quantile(flow_school_reopen1_reopen0_diff, prob=ps),
                  q_label= p_labs), 
           by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ans <- select(ans, -time_idx)
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans_school_reopen1 <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1')
  ans_school_reopen1[, variable := "school_reopen1"]
  ans_school_reopen0 <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen0')
  ans_school_reopen0[, variable := "school_reopen0"]
  ans_school_reopen1_reopen0_ratio <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1_reopen0_ratio')
  ans_school_reopen1_reopen0_ratio[, variable := "school_reopen1_reopen0_ratio"]
  ans_school_reopen1_reopen0_diff <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1_reopen0_diff')
  ans_school_reopen1_reopen0_diff[, variable := "school_reopen1_reopen0_diff"]
  
  ans = rbind(ans_school_reopen1, ans_school_reopen0, ans_school_reopen1_reopen0_ratio, ans_school_reopen1_reopen0_diff)
  
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_cumprop_flow_byage_forecast_school_reopen_K12 <- function(in_flow_school_reopen1, in_flow_school_reopen0, start_date, time_idx, age_cat_map, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(in_flow_school_reopen0)[5]==length(regions) )
  stopifnot( dim(in_flow_school_reopen1)[5]==length(regions) )
  
  p_age <- unique(subset(pop_info, select=c(loc,age.cat.new,pop)))
  p_age <- subset(p_age, loc %in% regions)
  p_age <- merge(p_age, age_cat_map, by='age.cat.new')
  p_age <- p_age[, list(pop=sum(pop)), by=c('loc','age.cat2')]	
  p_loc <- p_age[, list(age.cat2=age.cat2, 
                        pop=pop,
                        pop_prop_loc=pop/sum(pop)),
                 by=c('loc')]
  p_age <- p_age[, list(loc=loc, 
                        pop=pop,
                        pop_prop_age=pop/sum(pop)),
                 by=c('age.cat2')]
  p_ageloc <- p_age[, list(age.cat2=age.cat2, 
                           loc = loc,
                           pop=pop,
                           pop_prop_ageloc=pop/sum(pop))]
  p_age_loc = merge(merge(p_age, p_loc, by = c("loc", "age.cat2", "pop")), p_ageloc, by = c("loc", "age.cat2", "pop"))
  setnames(p_age_loc, 'age.cat2','age_index2')
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( in_flow_school_reopen1[,,,,m] ) )
    setnames(dt, 1:5, c('iteration','time','source_age_cat','rec_age_cat','flow_school_reopen1'))
    dt2 <- as.data.table( reshape2::melt( in_flow_school_reopen0[,,,,m] ) )
    setnames(dt2, 1:5, c('iteration','time','source_age_cat','rec_age_cat','flow_school_reopen0'))
    dt = cbind(dt, select(dt2, flow_school_reopen0))
    
    # take total of flows per source age group
    dt = dt[, list(flow_school_reopen1 = sum(flow_school_reopen1),
                   flow_school_reopen0 = sum(flow_school_reopen0)),
            by = c('iteration','time','source_age_cat')]
    setnames(dt, 'source_age_cat','age_index2')
    
    # take cum of the flows over time
    dt = dt[, list(flow_school_reopen1 = cumsum(flow_school_reopen1),
                   flow_school_reopen0 = cumsum(flow_school_reopen0),
                   time = time),
            by = c('iteration','age_index2')]
    
    # aggregate by new age 
    dt2 = dt[age_index2 == 2,]
    setnames(dt2, c("flow_school_reopen1", "flow_school_reopen0"), c("flow_school_reopen1_1019", "flow_school_reopen0_1019"))
    dt = merge(dt, select(dt2, -age_index2), by = c("iteration", "time"), all.x = T)
    dt[age_index2 == 2, flow_school_reopen1 := 9/10 * flow_school_reopen1_1019]
    dt[age_index2 == 2, flow_school_reopen0 := 9/10 * flow_school_reopen0_1019]
    dt[age_index2 == 3, flow_school_reopen1 := flow_school_reopen1 + 1/10 * flow_school_reopen1_1019]
    dt[age_index2 == 3, flow_school_reopen0 := flow_school_reopen0 + 1/10 * flow_school_reopen0_1019]
    dt = select(dt, -flow_school_reopen1_1019, -flow_school_reopen0_1019)
    
    # save time mapping for region m
    tmp1 <- time_idx[,m][ time_idx[,m]>0 ]
    tmp2 <- unique(subset(dt, time<=length(tmp1), select=c(time)))	  
    dt <- subset(dt, time<=length(tmp1))
    tmp2[, time_idx:= tmp1]
    tmp2[, date:= dates[[m]][tmp2$time_idx[1]] + tmp2$time_idx - tmp2$time_idx[1]]
    tmp2 = subset(tmp2, date >= start_date)
    
    #	to compute national average: add prop_pop, time and save 	
    tmp <- merge(dt, subset(p_age_loc, loc==regions[m], c(age_index2, pop_prop_age, pop_prop_loc, pop_prop_ageloc)), by=c('age_index2'))
    # add dates: we need to do here because the time index are different by state
    tmp <- merge(tmp, tmp2, by = 'time')
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    #	calculate overall and save as age.cat 0		
    tmp <- merge(dt, subset(p_age_loc, loc==regions[m], c(age_index2, pop_prop_loc)), by=c('age_index2'))
    tmp <- tmp[, list(flow_school_reopen1 = sum(flow_school_reopen1*pop_prop_loc), 
                      flow_school_reopen0 = sum(flow_school_reopen0*pop_prop_loc) ), by= c('iteration', 'time')]
    tmp[, age_index2:=0]
    dt <- rbind(dt,tmp)
    
    # take proportion of total flows
    # overall: needs to compute total flows in a first step to find overall prop flow
    tmp = subset(dt, age_index2 !=0)
    tmp = tmp[, list(flow_school_reopen1_total = sum(flow_school_reopen1),
                     flow_school_reopen0_total = sum(flow_school_reopen0)),
              by = c('iteration','time')]
    tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","time"))
    dt1 = tmp1[age_index2 == 0, list(flow_school_reopen1 = flow_school_reopen1/flow_school_reopen1_total,
                                     flow_school_reopen0 = flow_school_reopen0/flow_school_reopen0_total,
                                     age_index2 = age_index2),
               by = c('iteration','time')]
    # by age
    dt2 = dt[age_index2 != 0, list(flow_school_reopen1 = flow_school_reopen1/sum(flow_school_reopen1),
                                   flow_school_reopen0 = flow_school_reopen0/sum(flow_school_reopen0),
                                   age_index2 = age_index2),
             by = c('iteration','time')]
    dt = rbind(dt1, dt2)
    
    
    # Take ratio and difference 
    dt[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1 + 1e-16) / (flow_school_reopen0 + 1e-16)] # add very small noise for division
    dt[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
    
    #	summarise
    dt <- dt[, list(q_school_reopen1= quantile(flow_school_reopen1, prob=ps),
                    q_school_reopen0= quantile(flow_school_reopen0, prob=ps),
                    q_school_reopen1_reopen0_ratio= quantile(flow_school_reopen1_reopen0_ratio, prob=ps),
                    q_school_reopen1_reopen0_diff= quantile(flow_school_reopen1_reopen0_diff, prob=ps),
                    q_label= p_labs), 
             by=c('time','age_index2')]
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    dt <- merge(dt, tmp2, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  #
  # National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  dt <- subset(ans_overall, date %in% as.Date(dates_common))
  #	calculate overall across age and state and save as age.cat 0		
  tmp <- dt[, list(flow_school_reopen1= sum(flow_school_reopen1*pop_prop_ageloc),
                   flow_school_reopen0= sum(flow_school_reopen0*pop_prop_ageloc)), by= c('iteration', 'date')]
  tmp[, age_index2:=0]
  #	calculate overall across state by age
  dt <- dt[, list(flow_school_reopen1= sum(flow_school_reopen1*pop_prop_age),
                  flow_school_reopen0= sum(flow_school_reopen0*pop_prop_age)), by=c('age_index2','date','iteration')]
  dt <- rbind(dt,tmp)
  # take proportion of total flows
  tmp = subset(dt, age_index2 !=0)
  tmp = tmp[, list(flow_school_reopen1_total = sum(flow_school_reopen1),
                   flow_school_reopen0_total = sum(flow_school_reopen0)),
            by = c('iteration','date')]
  tmp1 = merge(subset(dt, age_index2 ==0), tmp, by = c("iteration","date"))
  dt1 = tmp1[age_index2 == 0, list(flow_school_reopen1 = flow_school_reopen1/flow_school_reopen1_total,
                                   flow_school_reopen0 = flow_school_reopen0/flow_school_reopen0_total,
                                   age_index2 = age_index2),
             by = c('iteration','date')]
  dt2 = dt[age_index2 != 0, list(flow_school_reopen1 = flow_school_reopen1/sum(flow_school_reopen1),
                                 flow_school_reopen0 = flow_school_reopen0/sum(flow_school_reopen0),
                                 age_index2 = age_index2),
           by = c('iteration','date')]
  dt = rbind(dt1, dt2)
  # Take ratio and difference 
  dt[, flow_school_reopen1_reopen0_ratio := (flow_school_reopen1 + 1e-16)  / (flow_school_reopen0+ 1e-16)]
  dt[, flow_school_reopen1_reopen0_diff := flow_school_reopen1 - flow_school_reopen0]
  #	summarise
  dt <- dt[, list(q_school_reopen1= quantile(flow_school_reopen1, prob=ps),
                  q_school_reopen0= quantile(flow_school_reopen0, prob=ps),
                  q_school_reopen1_reopen0_ratio= quantile(flow_school_reopen1_reopen0_ratio, prob=ps),
                  q_school_reopen1_reopen0_diff= quantile(flow_school_reopen1_reopen0_diff, prob=ps),
                  q_label= p_labs), 
           by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ans <- select(ans, -time_idx)
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans_school_reopen1 <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1')
  ans_school_reopen1[, variable := "school_reopen1"]
  ans_school_reopen0 <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen0')
  ans_school_reopen0[, variable := "school_reopen0"]
  ans_school_reopen1_reopen0_ratio <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1_reopen0_ratio')
  ans_school_reopen1_reopen0_ratio[, variable := "school_reopen1_reopen0_ratio"]
  ans_school_reopen1_reopen0_diff <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q_school_reopen1_reopen0_diff')
  ans_school_reopen1_reopen0_diff[, variable := "school_reopen1_reopen0_diff"]
  
  ans = rbind(ans_school_reopen1, ans_school_reopen0, ans_school_reopen1_reopen0_ratio, ans_school_reopen1_reopen0_diff)
  
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_cum_cases_deaths_by_age_forecast_school_reopen <- function(E_ByAge_school_reopen0, E_ByAge_school_reopen1, pop_info, start_date, dates, age_cat_map, regions)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_ByAge_school_reopen0)[2]==length(regions) )
  stopifnot( dim(E_ByAge_school_reopen1)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  E_ByAge_school_reopen0[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value_school_reopen0'))
    dt2 <- as.data.table( reshape2::melt(  E_ByAge_school_reopen1[,m,,] ) )
    setnames(dt2, 1:4, c('iteration','time','age_cat','value_school_reopen1'))
    dt = cbind(dt, select(dt2, value_school_reopen1))
    
    # add
    tmp <- select(age_cat_map, c('age.cat2', 'age.cat', 'multiplier'))
    setnames(tmp, 'age.cat', 'age_cat')
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat', allow.cartesian=TRUE)
    
    # 	aggregate by age groups c
    dt <- dt[, list(value_school_reopen0=sum(value_school_reopen0*multiplier),
                    value_school_reopen1=sum(value_school_reopen1*multiplier)), by=c('age.cat2','time','iteration')]	
    
    # add age cat 0 for overall
    tmp <- dt[, list(value_school_reopen1 = sum(value_school_reopen1), 
                     value_school_reopen0 = sum(value_school_reopen0) ), by= c('iteration', 'time')]
    tmp[, age.cat2:=0]
    dt <- rbind(dt,tmp)
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp = subset(tmp, date >= start_date)
    dt <- merge(dt, tmp, by='time')
    
    # cumsum
    setkey(dt, iteration, age.cat2, time)
    dt <- dt[, list(time=time,
                    date = date,
                    value_school_reopen0=cumsum(value_school_reopen0),
                    value_school_reopen1=cumsum(value_school_reopen1)), by=c('iteration','age.cat2')]	
    
    # save for national level
    ans_overall[[m]] <- copy(dt)
    
    # take ratio and difference
    dt[, value_school_reopen1_reopen0_ratio := (value_school_reopen1+ 1e-16) / (value_school_reopen0+ 1e-16)]
    dt[, value_school_reopen1_reopen0_diff := value_school_reopen1 - value_school_reopen0]
    
    #	summarise		
    dt <- dt[, list(q_school_reopen1 = quantile(value_school_reopen1, prob=ps),
                    q_school_reopen0 = quantile(value_school_reopen0, prob=ps),
                    q_school_reopen1_reopen0_ratio = quantile(value_school_reopen1_reopen0_ratio, prob=ps),
                    q_school_reopen1_reopen0_diff = quantile(value_school_reopen1_reopen0_diff, prob=ps),
                    q_label=p_labs), 
             by=c('time', 'date', 'age.cat2')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  cat('\nProcessing All States')
  # All states
  tmp <- ans_overall[, list(value_school_reopen0 = sum(value_school_reopen0), value_school_reopen1 = sum(value_school_reopen1)), by = c("iteration", "age.cat2",  "date")]
  # take ratio and difference
  tmp[, value_school_reopen1_reopen0_ratio := (value_school_reopen1+ 1e-16) / (value_school_reopen0+ 1e-16)]
  tmp[, value_school_reopen1_reopen0_diff := value_school_reopen1 - value_school_reopen0]
  #	summarise		
  tmp <- tmp[, list(q_school_reopen1 = quantile(value_school_reopen1, prob=ps),
                    q_school_reopen0 = quantile(value_school_reopen0, prob=ps),
                    q_school_reopen1_reopen0_ratio = quantile(value_school_reopen1_reopen0_ratio, prob=ps),
                    q_school_reopen1_reopen0_diff = quantile(value_school_reopen1_reopen0_diff, prob=ps),
                    q_label=p_labs), 
             by=c('date', 'age.cat2')]	
  tmp[, loc:= "AllStates"]
  tmp[, loc_label:= "All States"]
  
  #	make human readable loc labels
  tmp1 <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp1, by='loc')
  ans <- rbind(select(ans, -time), tmp)
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by=c('age.cat2'))
  setnames(ans,c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans_school_reopen1 <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1')
  ans_school_reopen1[, variable := "school_reopen1"]
  ans_school_reopen0 <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen0')
  ans_school_reopen0[, variable := "school_reopen0"]
  ans_school_reopen1_reopen0_ratio <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1_reopen0_ratio')
  ans_school_reopen1_reopen0_ratio[, variable := "school_reopen1_reopen0_ratio"]
  ans_school_reopen1_reopen0_diff <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1_reopen0_diff')
  ans_school_reopen1_reopen0_diff[, variable := "school_reopen1_reopen0_diff"]
  
  ans = rbind(ans_school_reopen1, ans_school_reopen0, ans_school_reopen1_reopen0_ratio, ans_school_reopen1_reopen0_diff)
  
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
summarise_daily_deaths_by_age_forecast_school_reopen <- function(Edeaths_ByAge_school, pop_info, dates, age_cat_map, regions)
{
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(Edeaths_ByAge_school)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  Edeaths_ByAge_school[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    
    # add
    tmp <- select(age_cat_map, c('age.cat2', 'age.cat', 'multiplier'))
    setnames(tmp, 'age.cat', 'age_cat')
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat', allow.cartesian=TRUE)
    
    # 	aggregate by age groups c
    dt <- dt[, list(value=sum(value*multiplier)), by=c('age.cat2','time','iteration')]	
    
    # add age cat 0 for overall
    tmp <- dt[, list(value = sum(value) ), by= c('iteration', 'time')]
    tmp[, age.cat2:=0]
    dt <- rbind(dt,tmp)
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dt <- merge(dt, tmp, by='time')
    
    # save for national level
    ans_overall[[m]] <- copy(dt)
    
    #	summarise		
    dt <- dt[, list(q = quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c( 'date', 'age.cat2')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  cat('\nProcessing All States')
  # All states
  tmp <- ans_overall[, list(value = sum(value)), by = c("iteration", "age.cat2",  "date")]
  #	summarise		
  tmp <- tmp[, list(q = quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c('date', 'age.cat2')]	
  tmp[, loc:= "AllStates"]
  tmp[, loc_label:= "All States"]
  
  #	make human readable loc labels
  tmp1 <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp1, by='loc')
  ans <- rbind(ans, tmp)
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by=c('age.cat2'))
  setnames(ans,c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q')
  
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
summarise_daily_cases_by_age_forecast_school_reopen <- function(Ecases_ByAge_school, pop_info, dates, age_cat_map, regions)
{
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(Ecases_ByAge_school)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt(  Ecases_ByAge_school[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value'))
    
    # add
    tmp <- select(age_cat_map, c('age.cat2', 'age.cat', 'multiplier'))
    setnames(tmp, 'age.cat', 'age_cat')
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_cat', allow.cartesian=TRUE)
    
    # 	aggregate by age groups c
    dt <- dt[, list(value=sum(value*multiplier)), by=c('age.cat2','time','iteration')]	
    
    # add age cat 0 for overall
    tmp <- dt[, list(value = sum(value) ), by= c('iteration', 'time')]
    tmp[, age.cat2:=0]
    dt <- rbind(dt,tmp)
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    dt <- merge(dt, tmp, by='time')
    
    # save for national level
    ans_overall[[m]] <- copy(dt)
    
    #	summarise		
    dt <- dt[, list(q = quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c( 'date', 'age.cat2')]		
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  cat('\nProcessing All States')
  # All states
  tmp <- ans_overall[, list(value = sum(value)), by = c("iteration", "age.cat2",  "date")]
  #	summarise		
  tmp <- tmp[, list(q = quantile(value, prob=ps),
                    q_label=p_labs), 
             by=c('date', 'age.cat2')]	
  tmp[, loc:= "AllStates"]
  tmp[, loc_label:= "All States"]
  
  #	make human readable loc labels
  tmp1 <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp1, by='loc')
  ans <- rbind(ans, tmp)
  ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
                     data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
               ans, by=c('age.cat2'))
  setnames(ans,c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q')
  
  return(ans)	  
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous <- function(E_effcasesByAge, RtByAge, period_length, pop_info, dates, regions)
  {
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
  stopifnot( dim(RtByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt( E_effcasesByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value_effcases_a'))
    tmp <- as.data.table( reshape2::melt( RtByAge[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_cat','value_Rt_a'))
    dt = cbind(dt, select(tmp, "value_Rt_a"))
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dt)
    
    # 	take the sum of effective cases over age group 
    tmp <- dt[, list(value_effcases=sum(value_effcases_a)), by=c('time','iteration')]	
    dt = merge(dt, tmp, by=c('time','iteration'))
    
    # 	compute Rt
    tmp = dt[value_effcases == 0, list(value_Rt = 0 ), by=c('time','iteration', "date", "loc", "period_idx")]
    dt = dt[value_effcases != 0, list(value_Rt = sum(value_Rt_a*value_effcases_a / value_effcases ) ), by=c('time','iteration', "date", "loc", "period_idx")]
    dt = rbind(tmp, dt)
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt = mean(value_Rt), date = date[1]), by = c('iteration', "loc", "period_idx")] 
    
    #	summarise		
    dt <- dt[, list(q= quantile(value_Rt, prob=ps),
                    q_label=p_labs), 
             by=c("date", "loc")]		
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  # National average
  tmp = unique(select(ans_overall, loc, date))
  tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region
  tmp[, day := weekdays(date)]
  first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
  first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
  if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
  if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
  tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
  tmp[, n_days_period := .N, by = .(period_idx)]
  tmp = subset(tmp, n_days_period == period_length)
  dt <- merge(select(ans_overall, -period_idx), select(tmp, date, period_idx), by='date')
  # take the sum of effective cases over age group and location
  tmp <- dt[, list(value_effcases=sum(value_effcases_a)), by=c('date','iteration')]	
  dt = merge(dt, tmp, by=c('date','iteration'))
  # compute Rt
  tmp = dt[value_effcases == 0, list(value_Rt = 0 ), by=c('date','iteration', "period_idx")]
  dt = dt[value_effcases != 0, list(value_Rt = sum(value_Rt_a*value_effcases_a / value_effcases) ), by=c('date','iteration', "period_idx")]
  dt = rbind(tmp, dt)
  if(period_length > 1) dt = dt[, list(value_Rt = mean(value_Rt), date = date[1]), by = c('iteration', "period_idx")] 
  #	summarise
  dt <- dt[, list(q= quantile(value_Rt, prob=ps),
                  q_label=p_labs), 
           by=c('date')]	
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')	
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- dcast.data.table(ans, loc + loc_label + date ~ q_label, value.var='q')
  ans	
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous_less_than_one <- function(E_effcasesByAge, RtByAge, period_length, threshold, pop_info, dates, regions)
{
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
  stopifnot( dim(RtByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    dt <- as.data.table( reshape2::melt( E_effcasesByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_cat','value_effcases_a'))
    tmp <- as.data.table( reshape2::melt( RtByAge[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_cat','value_Rt_a'))
    dt = cbind(dt, select(tmp, "value_Rt_a"))
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dt)
    
    # 	take the sum of effective cases over age group 
    tmp <- dt[, list(value_effcases=sum(value_effcases_a)), by=c('time','iteration')]	
    dt = merge(dt, tmp, by=c('time','iteration'))
    
    # 	compute Rt
    tmp = dt[value_effcases == 0, list(value_Rt = 0 ), by=c('time','iteration', "date", "loc", "period_idx")]
    dt = dt[value_effcases != 0, list(value_Rt = sum(value_Rt_a*value_effcases_a / value_effcases) ), by=c('time','iteration', "date", "loc", "period_idx")]
    dt = rbind(tmp, dt)
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt = mean(value_Rt), date = date[1]), by = c('iteration', "loc", "period_idx")] 
    
    # summarise
    dt <- dt[, list(value= mean(value_Rt<threshold)), by=c("date", "loc")]	
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  # National average
  tmp = unique(select(ans_overall, loc, date))
  tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region
  tmp[, day := weekdays(date)]
  first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
  first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
  if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
  if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
  tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
  tmp[, n_days_period := .N, by = .(period_idx)]
  tmp = subset(tmp, n_days_period == period_length)
  dt <- merge(select(ans_overall, -period_idx), select(tmp, date, period_idx), by='date')
  # take the sum of effective cases over age group and location
  tmp <- dt[, list(value_effcases=sum(value_effcases_a)), by=c('date','iteration')]	
  dt = merge(dt, tmp, by=c('date','iteration'))
  # compute Rt
  tmp = dt[value_effcases == 0, list(value_Rt = 0 ), by=c('date','iteration', "period_idx")]
  dt = dt[value_effcases != 0, list(value_Rt = sum(value_Rt_a*value_effcases_a / value_effcases) ), by=c('date','iteration', "period_idx")]
  dt = rbind(tmp, dt)
  
  if(period_length > 1) dt = dt[, list(value_Rt = mean(value_Rt), date = date[1]), by = c('iteration', "period_idx")] 
  #	summarise
  dt <- dt[, list(value= mean(value_Rt<threshold)), by=c('date')]	
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')	
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans	
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous_byage_c <- function(E_effcasesByAge, RtByAge, period_length, age_cat_map, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(RtByAge)[2]==length(regions) )
  stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( E_effcasesByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_index','value_effcases_a'))
    tmp <- as.data.table( reshape2::melt( RtByAge[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_index','value_Rt_a'))
    dt = cbind(dt, select(tmp, "value_Rt_a"))
    
    #	minimise merging. so we make a few low dim steps now
    #	we also only take the absolute minimum columns that we need
    #	ie the age.cat.labels are just exhausting memory
    tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
    setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
    setnames(tmp, c('age_cat','age_cat2'), c('age_index','age_index2'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_index')
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dt)
    
    # 	take the sum of effective cases over age group2
    tmp <- dt[, list(value_effcases_c=sum(value_effcases_a)), by=c('time','iteration', 'age_index2')]	
    dt = merge(dt, tmp, by=c('time','iteration', 'age_index2'))
    
    # 	aggregate Rt by age groups c
    tmp = dt[value_effcases_c == 0, list(value_Rt_c = 0 ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = dt[value_effcases_c != 0, list(value_Rt_c = sum(value_Rt_a*value_effcases_a / value_effcases_c) ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = rbind(tmp, dt)
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt_c = mean(value_Rt_c), date = date[1]), by = c('iteration', "loc", "period_idx", 'age_index2')] 
    
    #	summarise
    dt <- dt[, list(q= quantile(value_Rt_c, prob=ps),
                    q_label= p_labs), 
             by=c('age_index2', "loc", "date")]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  ## National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region
  tmp[, day := weekdays(date)]
  first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
  first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
  if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
  if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
  tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
  tmp[, n_days_period := .N, by = .(period_idx)]
  tmp = subset(tmp, n_days_period == period_length)
  dt <- merge(select(ans_overall, -period_idx), select(tmp, date, period_idx), by='date')
  # take the sum of effective cases over age group and location
  tmp <- dt[, list(value_effcases_c=sum(value_effcases_a)), by=c('date','iteration','age_index2')]	
  dt = merge(dt, tmp, by=c('date','iteration', 'age_index2'))
  # compute Rt
  tmp = dt[value_effcases_c == 0, list(value_Rt_c = 0 ), by=c('age_index2', 'date','iteration', "period_idx")]
  dt = dt[value_effcases_c != 0, list(value_Rt_c = sum(value_Rt_a*value_effcases_a / value_effcases_c )), by=c('age_index2', 'date','iteration', "period_idx")]
  dt = rbind(tmp, dt)
  if(period_length > 1) dt = dt[, list(value_Rt_c = mean(value_Rt_c), date = date[1]), by = c('age_index2','iteration', "period_idx")] 
  #	summarise
  dt <- dt[, list(q= quantile(value_Rt_c, prob=ps),
                  q_label=p_labs), 
           by=c('date', 'age_index2')]	
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date ~ q_label, value.var='q')
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous_byage_bystrain_c <- function(E_effcasesByAgeByStrain, RtByAgeByStrain, period_length, age_cat_map, ds, pop_info, dates, regions)
{		
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(RtByAgeByStrain)[2]==length(regions) )
  stopifnot( dim(E_effcasesByAgeByStrain)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  

  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( E_effcasesByAgeByStrain[,m,,,] ) )
    setnames(dt, 1:5, c('iteration','time','age_index','strain_cat', 'value_effcases_a'))
    tmp <- as.data.table( reshape2::melt( RtByAgeByStrain[,m,,,] ) )
    setnames(tmp, 1:5, c('iteration','time','age_index','strain_cat', 'value_Rt_a'))
    dt = cbind(dt, select(tmp, "value_Rt_a"))
    
    #	minimise merging. so we make a few low dim steps now
    #	we also only take the absolute minimum columns that we need
    #	ie the age.cat.labels are just exhausting memory
    tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
    setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
    setnames(tmp, c('age_cat','age_cat2'), c('age_index','age_index2'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_index')
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    # 	take the sum of effective cases over age group2
    tmp <- dt[, list(value_effcases_c=sum(value_effcases_a)), by=c('time','iteration', 'age_index2', 'strain_cat')]	
    dt = merge(dt, tmp, by=c('time','iteration', 'age_index2', 'strain_cat'))
    
    # 	aggregate Rt by age groups c
    tmp = dt[value_effcases_c == 0, list(value_Rt_c = 0 ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx", 'strain_cat')]
    dt = dt[value_effcases_c != 0, list(value_Rt_c = sum(value_Rt_a*value_effcases_a / value_effcases_c) ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx", 'strain_cat')]
    dt = rbind(tmp, dt)
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt_c = mean(value_Rt_c), date = date[1]), by = c('iteration', "loc", "period_idx", 'age_index2', 'strain_cat')] 
    
    #	summarise
    dt <- dt[, list(q= quantile(value_Rt_c, prob=ps),
                    q_label= p_labs), 
             by=c('age_index2', "loc", "date", 'strain_cat')]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  ans <- merge(ans, ds, by = 'strain_cat')
  
  ans <- dcast.data.table(ans, loc + loc_label + age.cat2 + age.cat2.label + date  + strain_cat + strain_cat_label ~ q_label, value.var='q')
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous_byage_less_than_one <- function(E_effcasesByAge, RtByAge, period_length, threshold, age_cat_map, pop_info, dates, regions)
{			
  stopifnot( dim(RtByAge)[2]==length(regions) )
  stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions)) 
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( E_effcasesByAge[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_index','value_effcases_a'))
    tmp <- as.data.table( reshape2::melt( RtByAge[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_index','value_Rt_a'))
    dt = cbind(dt, select(tmp, "value_Rt_a"))
    
    #	minimise merging. so we make a few low dim steps now
    #	we also only take the absolute minimum columns that we need
    #	ie the age.cat.labels are just exhausting memory
    tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
    setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
    setnames(tmp, c('age_cat','age_cat2'), c('age_index','age_index2'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_index')
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dt)
    
    # 	take the sum of effective cases over age group2
    tmp <- dt[, list(value_effcases_c=sum(value_effcases_a)), by=c('time','iteration', 'age_index2')]	
    dt = merge(dt, tmp, by=c('time','iteration', 'age_index2'))
    
    # 	aggregate Rt by age groups c
    tmp = dt[value_effcases_c == 0, list(value_Rt_c = 0 ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = dt[value_effcases_c != 0, list(value_Rt_c = sum(value_Rt_a*value_effcases_a / value_effcases_c) ), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = rbind(tmp, dt)
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt_c = mean(value_Rt_c), date = date[1]), by = c('iteration', "loc", "period_idx", 'age_index2')] 
    
    #	summarise
    dt <- dt[, list(value= mean(value_Rt_c<threshold)), by=c('date','age_index2', 'loc')]
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  ## National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region
  tmp[, day := weekdays(date)]
  first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
  first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
  if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
  if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
  tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
  tmp[, n_days_period := .N, by = .(period_idx)]
  tmp = subset(tmp, n_days_period == period_length)
  dt <- merge(select(ans_overall, -period_idx), select(tmp, date, period_idx), by='date')
  # take the sum of effective cases over age group and location
  tmp <- dt[, list(value_effcases_c=sum(value_effcases_a)), by=c('date','iteration','age_index2')]	
  dt = merge(dt, tmp, by=c('date','iteration', 'age_index2'))
  # compute Rt
  tmp = dt[value_effcases_c == 0, list(value_Rt_c = 0 ), by=c('age_index2', 'date','iteration', "period_idx")]
  dt = dt[value_effcases_c != 0, list(value_Rt_c = sum(value_Rt_a*value_effcases_a / value_effcases_c )), by=c('age_index2', 'date','iteration', "period_idx")]
  dt = rbind(tmp, dt)
  if(period_length > 1) dt = dt[, list(value_Rt_c = mean(value_Rt_c), date = date[1]), by = c('age_index2','iteration', "period_idx")] 
  #	summarise
  dt <- dt[, list(value= mean(value_Rt_c<threshold)), by=c('date','age_index2')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  
  
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  ans  		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_Rt_instantaneous_byage_forecast_school_reopen <- function(E_effcasesByAge_reopen0, E_effcasesByAge_reopen1, RtByAge_school_reopen0, RtByAge_school_reopen1, 
                                                      period_length, age_cat_map, pop_info, dates, regions)
{		
  
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(E_effcasesByAge_reopen0)[2]==length(regions) )
  stopifnot( dim(E_effcasesByAge_reopen1)[2]==length(regions) )
  stopifnot( dim(RtByAge_school_reopen0)[2]==length(regions) )
  stopifnot( dim(RtByAge_school_reopen1)[2]==length(regions) )
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions))  
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions)) 
  {
    
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    #	select data from large table here
    #	no reshape outside the loop 
    dt <- as.data.table( reshape2::melt( RtByAge_school_reopen0[,m,,] ) )
    setnames(dt, 1:4, c('iteration','time','age_index','value_Rt_a_reopen0'))
    tmp <- as.data.table( reshape2::melt( RtByAge_school_reopen1[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_index','value_Rt_a_reopen1'))
    dt = cbind(dt, select(tmp, value_Rt_a_reopen1))
    tmp <- as.data.table( reshape2::melt( E_effcasesByAge_reopen0[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_index','value_effcases_a_reopen0'))
    dt = cbind(dt, select(tmp, value_effcases_a_reopen0))
    tmp <- as.data.table( reshape2::melt( E_effcasesByAge_reopen1[,m,,] ) )
    setnames(tmp, 1:4, c('iteration','time','age_index','value_effcases_a_reopen1'))
    dt = cbind(dt, select(tmp, value_effcases_a_reopen1))
    
    #	minimise merging. so we make a few low dim steps now
    #	we also only take the absolute minimum columns that we need
    #	ie the age.cat.labels are just exhausting memory
    tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
    setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
    setnames(tmp, c('age_cat','age_cat2'), c('age_index','age_index2'))
    
    #	one big merge using integers as key -- this is now fast
    dt <- merge(dt, tmp, by='age_index', allow.cartesian=TRUE)
    
    #	add loc 
    dt[, loc:= regions[m]]
    
    # add time index and date
    tmp <- unique(subset(dt, select=time))
    tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
    tmp[, day := weekdays(date)]
    first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
    first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
    if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
    if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
    tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
    tmp[, n_days_period := .N, by = .(period_idx)]
    tmp = subset(tmp, n_days_period == period_length) # keep complete period
    dt <- merge(dt, select(tmp, date, time, period_idx), by='time')
    
    #	to compute national average
    ans_overall[[m]] = copy(dt)
    
    # 	take the sum of effective cases over age group2
    tmp <- dt[, list(value_effcases_c_reopen0=sum(value_effcases_a_reopen0),
                     value_effcases_c_reopen1=sum(value_effcases_a_reopen1)), by=c('time','iteration', 'age_index2')]	
    dt = merge(dt, tmp, by=c('time','iteration', 'age_index2'))
    
    # 	aggregate Rt by age groups c
    tmp = dt[value_effcases_c_reopen0 == 0, list(value_Rt_c_reopen0 = 0), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    tmp1 = dt[value_effcases_c_reopen0 != 0, list( value_Rt_c_reopen0 = sum(value_Rt_a_reopen0 * value_effcases_a_reopen0 / value_effcases_c_reopen0 ) ), 
              by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    tmp = rbind(tmp, tmp1)
    tmp1 = dt[value_effcases_c_reopen1 == 0, list(value_Rt_c_reopen1 = 0), by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = dt[value_effcases_c_reopen1 != 0, list( value_Rt_c_reopen1 = sum(value_Rt_a_reopen1 * value_effcases_a_reopen1 / value_effcases_c_reopen1 ) ), 
            by=c('age_index2', 'time','iteration', "date", "loc", "period_idx")]
    dt = merge(rbind(dt, tmp1), tmp, by=c('age_index2', 'time','iteration', "date", "loc", "period_idx"))
    
    # aggregate over period_length (1 = daily, 7 = weekly etc.)
    if(period_length > 1) dt = dt[, list(value_Rt_c_reopen0 = mean(value_Rt_c_reopen0), 
                                         value_Rt_c_reopen1 = mean(value_Rt_c_reopen1),
                                         date = date[1]), by = c('iteration', "loc", "period_idx", 'age_index2')] 
    
    # take ratio and difference
    dt[, value_Rt_c_school_reopen1_reopen0_ratio := (value_Rt_c_reopen1+ 1e-16) / (value_Rt_c_reopen0+ 1e-16)]
    dt[, value_Rt_c_school_reopen1_reopen0_diff := value_Rt_c_reopen1 - value_Rt_c_reopen0]
    
    #	summarise
    dt <- dt[, list(q_school_reopen1 = quantile(value_Rt_c_reopen1, prob=ps),
                    q_school_reopen0 = quantile(value_Rt_c_reopen0, prob=ps),
                    q_school_reopen1_reopen0_ratio = quantile(value_Rt_c_school_reopen1_reopen0_ratio, prob=ps),
                    q_school_reopen1_reopen0_diff = quantile(value_Rt_c_school_reopen1_reopen0_diff, prob=ps),
                    q_label=p_labs), 
             by=c('date', 'age_index2', "loc")]		
    
    # build new data object only after summarised
    ans[[m]] <- copy(dt)		
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  ## National average
  cat('\nProcessing United-States')
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  tmp = data.table(date = as.Date(colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]))   # keep date that are common to every region
  tmp[, day := weekdays(date)]
  first_day_forecast_common = as.Date(min(sapply(sapply(dates, as.character), max))) +1
  first_date_period = (first_day_forecast_common - period_length*1:round(365/period_length) )[max(which(first_day_forecast_common - period_length*1:round(365/period_length) >= as.Date("2020-03-09")))]
  if(period_length > 1) tmp = subset(tmp, date >= tmp[day == "Monday", date][1]) # start period indexing from a Monday 
  if(period_length > 7) tmp = subset(tmp, date >= first_date_period) # start period indexing at minimum from the same day and ending period on the same day as the last observation
  tmp[, period_idx := rep(1:ceiling(nrow(tmp)/period_length), each = period_length)[1:nrow(tmp)]] 
  tmp[, n_days_period := .N, by = .(period_idx)]
  tmp = subset(tmp, n_days_period == period_length)
  dt <- merge(select(ans_overall, -period_idx), select(tmp, date, period_idx), by='date')
  # take the sum of effective cases over age group and location
  tmp <- dt[, list(value_effcases_c_reopen0=sum(value_effcases_a_reopen0),
                   value_effcases_c_reopen1=sum(value_effcases_a_reopen1)), by=c('date','iteration','age_index2')]	
  dt = merge(dt, tmp, by=c('date','iteration', 'age_index2'))
  # compute Rt
  tmp = dt[value_effcases_c_reopen0 == 0, list(value_Rt_c_reopen0 = 0), by=c('age_index2', 'iteration', "date", "period_idx")]
  tmp1 = dt[value_effcases_c_reopen0 != 0, list( value_Rt_c_reopen0 = sum(value_Rt_a_reopen0 * value_effcases_a_reopen0 / value_effcases_c_reopen0 ) ), 
            by=c('age_index2', 'iteration', "date", "period_idx")]
  tmp = rbind(tmp, tmp1)
  tmp1 = dt[value_effcases_c_reopen1 == 0, list(value_Rt_c_reopen1 = 0), by=c('age_index2', 'iteration', "date","period_idx")]
  dt = dt[value_effcases_c_reopen1 != 0, list( value_Rt_c_reopen1 = sum(value_Rt_a_reopen1 * value_effcases_a_reopen1 / value_effcases_c_reopen1 ) ), 
          by=c('age_index2', 'iteration', "date",  "period_idx")]
  dt = merge(rbind(dt, tmp1), tmp, by=c('age_index2', 'iteration', "date", "period_idx"))
  
  if(period_length > 1) dt = dt[, list(value_Rt_c_reopen0 = mean(value_Rt_c_reopen0), 
                                       value_Rt_c_reopen1 = mean(value_Rt_c_reopen1), date = date[1]), by = c('age_index2','iteration', "period_idx")] 
  # take ratio and difference
  dt[, value_Rt_c_school_reopen1_reopen0_ratio := (value_Rt_c_reopen1+ 1e-16) / (value_Rt_c_reopen0+ 1e-16)]
  dt[, value_Rt_c_school_reopen1_reopen0_diff := value_Rt_c_reopen1 - value_Rt_c_reopen0]
  dt1 <- dt[, list(q_school_reopen1 = stats::quantile(value_Rt_c_reopen1, prob=ps),
                  q_school_reopen0 = stats::quantile(value_Rt_c_reopen0, prob=ps),
                  q_label=p_labs),  by=c('date', 'age_index2')]	
  dt2 <- dt[, list(q_school_reopen1_reopen0_ratio = stats::quantile(value_Rt_c_school_reopen1_reopen0_ratio, prob=ps),
                  q_school_reopen1_reopen0_diff = stats::quantile(value_Rt_c_school_reopen1_reopen0_diff, prob=ps),
                  q_label=p_labs), 
           by=c('date', 'age_index2')]	
  dt = merge(dt1, dt2, by=c('date', 'age_index2', 'q_label'))
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  dt[,loc:= "US"]
  dt[,loc_label := "United-States"]
  ans <- rbind(ans, dt)
  
  ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), 
               ans, by.y=c('age_index2'), by.x=c('age.cat2'))
  setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
  
  ans_school_reopen0 <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen0')
  ans_school_reopen0[, variable := "school_reopen0"]
  ans_school_reopen1 <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1')
  ans_school_reopen1[, variable := "school_reopen1"]
  ans_school_reopen1_reopen0_ratio <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1_reopen0_ratio')
  ans_school_reopen1_reopen0_ratio[, variable := "school_reopen1_reopen0_ratio"]
  ans_school_reopen1_reopen0_diff <- dcast.data.table(ans, loc + loc_label + age_cat + age_band + date ~ q_label, value.var='q_school_reopen1_reopen0_diff')
  ans_school_reopen1_reopen0_diff[, variable := "school_reopen1_reopen0_diff"]
  
  ans = rbind(ans_school_reopen1, ans_school_reopen0, ans_school_reopen1_reopen0_ratio, ans_school_reopen1_reopen0_diff)
  
  return(ans) 		
}

#' @export
#' @keywords internal
#' @import data.table
summarise_ifr_age_by_state = function(log_ifr_age_base, log_ifr_age_rnde_mid1, log_ifr_age_rnde_mid2, log_ifr_age_rnde_old, regions, dages, pop_info)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot(length(regions) == ncol(log_ifr_age_rnde_mid1))
  stopifnot(nrow(dages) == ncol(log_ifr_age_base))
  
  log_ifr_age_base =  as.data.table( reshape2::melt(log_ifr_age_base) )
  setnames(log_ifr_age_base, c("iterations", "age_cat", "value_base"))
  
  df <- vector('list',length(regions))
  for(m in seq_along(regions)){
    
    cat('\nProcessing state',m,' ',regions[m])	
    
    # load and expand to all age groups
    tmp =  as.data.table( reshape2::melt(log_ifr_age_rnde_mid1[,m]) )
    setnames(tmp,"value_rndeff" )
    tmp[, iterations := 1:nrow(tmp)]
    tmp = tmp[rep(seq_len(nrow(tmp)), each = 6), ]
    tmp[, age_cat := rep(5:10, max(iterations))]
    
    tmp1 = as.data.table( reshape2::melt(log_ifr_age_rnde_mid2[,m] ) )
    setnames(tmp1,"value_rndeff" )
    tmp1[, iterations := 1:nrow(tmp1)]
    tmp1 = tmp1[rep(seq_len(nrow(tmp1)), each = 4), ]
    tmp1[, age_cat := rep(11:14, max(iterations))]
    tmp = rbind(tmp, tmp1)
    
    tmp1 = as.data.table( reshape2::melt(log_ifr_age_rnde_old[,m] ) )
    setnames(tmp1,"value_rndeff" )
    tmp1[, iterations := 1:nrow(tmp1)]
    tmp1 = tmp1[rep(seq_len(nrow(tmp1)), each = 4), ]
    tmp1[, age_cat := rep(15:18, max(iterations))]
    tmp = rbind(tmp, tmp1)
    
    tmp1 = data.table(iterations = rep(1:max(tmp$iterations), each = 4),
                      age_cat = rep(1:4, max(tmp$iterations)),
                      value_rndeff = 0)
    tmp = rbind(tmp, tmp1)
    
    tmp = merge(log_ifr_age_base, tmp, by = c("iterations", "age_cat"))
    tmp[, value := value_base + value_rndeff]
    
    # 	summarise
    tmp <- tmp[, list(qs = quantile(value, prob=ps), qlab=p_labs), by=c('age_cat')]
    tmp <- dcast.data.table(tmp, age_cat~qlab, value.var='qs')
    
    #	add loc 
    tmp[, loc:= regions[m]]	
    
    # build new data object only after summarised
    df[[m]] <- copy(tmp)	  
  }
  
  df <- do.call('rbind',df)
  
  # make human readable labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  df <- merge(df, tmp, by='loc')
  df <- merge(df, dages, by='age_cat')
  
  return(df)
}

#' @export
#' @keywords internal
#' @import data.table
summarise_ifr_age_base = function(log_ifr_age_base, dages, pop_info)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot(nrow(dages) == ncol(log_ifr_age_base))
  
  log_ifr_age_base =  as.data.table( reshape2::melt(log_ifr_age_base) )
  setnames(log_ifr_age_base, c("iterations", "age_cat", "value"))
  
  # 	summarise
  log_ifr_age_base <- log_ifr_age_base[, list(qs = quantile(value, prob=ps), qlab=p_labs), by=c('age_cat')]
  log_ifr_age_base <- dcast.data.table(log_ifr_age_base, age_cat~qlab, value.var='qs')
  
  # make human readable labels
  log_ifr_age_base <- merge(log_ifr_age_base, dages, by='age_cat')
  
  return(log_ifr_age_base)
}

#' @export
#' @keywords internal
#' @import data.table
roundCI <- function(lower,upper,k=2){
  #if (any(c(lower,upper)<0)) stop("not designed for negative values") 
  lowerstring <- format(10^(-k)*floor(lower*10^(k)), nsmall=k) 
  lowerstring = prettyNum(gsub("\\s", "", lowerstring), big.mark= ",")  
  upperstring <- format(10^(-k)*ceiling(upper*10^(k)), nsmall=k)
  upperstring =  prettyNum(gsub("\\s", "", upperstring), big.mark= ",")
  paste("[",paste(lowerstring,upperstring,sep=", "),"]",sep="") 
}

#' @export
#' @keywords internal
#' @import data.table 
round_choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

#' @export
#' @keywords internal
#' @import data.table
extract_samples_e_acases_eff_byage_c <- function(E_effcasesByAge, n, age_cat_map, pop_info, regions){
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(E_effcasesByAge)[2]==length(regions) )
	
	# last date
	ldate <- dim(E_effcasesByAge)[3]
	# just keep last date for speed
	effcases <- E_effcasesByAge[,,ldate,]
	
	# sample n
	iter <- dim(effcases)[1]
	effcases <- effcases[(iter-n+1):iter,,]
	
	ans <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt( effcases[,m,] ) )
		setnames(dt, 1:3, c('iteration','age_cat','value'))
		
		tmp <- subset(age_cat_map, select=c(age.cat2, age.cat))
		setnames(tmp, colnames(tmp), gsub('\\.','_',colnames(tmp)))
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat')
		
		# 	aggregate by age groups c
		dt <- dt[, list(value=sum(value)), by=c('age_cat2','iteration')]	
		setkey(dt, iteration, age_cat2)
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp, by='loc')
	ans <- merge(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))), ans, by.y=c('age_cat2'), by.x=c('age.cat2'))
	
	setnames(ans, c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	ans	
}

#' @export
#' @keywords internal
#' @import data.table
extract_samples_excess_deaths <- function(E_deathsByAge_school_reopen_0, E_deathsByAge_school_reopen_1, n, pop_info, start_date, end_date, dates, age_cat_map, regions)
{
	#pop_info <- plot.pars.basic$pop_info
	#dates <- plot.pars.basic$dates
	#age_cat_map <- copy(age_cat_map_school_chidren)
	#regions <- plot.pars.basic$regions
	
	ps <- c(0.5, 0.025, 0.975)
	p_labs <- c('M','CL','CU')
	
	stopifnot( dim(E_deathsByAge_school_reopen_0)[2]==length(regions) )
	stopifnot( dim(E_deathsByAge_school_reopen_1)[2]==length(regions) )
	
	# sample n from posterior
	iter <- dim(E_deathsByAge_school_reopen_0)[1]
	ans <- vector('list',length(regions))  
	ans_overall <- vector('list',length(regions))  
	#	loop over locations for speed. very hard to take quantiles on such large data
	for(m in seq_along(regions))
	{
		#m <- 1	  
		cat('\nProcessing state',m,' ',regions[m])
		
		dt <- as.data.table( reshape2::melt(  E_deathsByAge_school_reopen_0[(iter-n+1):iter,m,,] ) )
		setnames(dt, 1:4, c('iteration','time','age_cat','value_school_reopen0'))
		dt2 <- as.data.table( reshape2::melt(  E_deathsByAge_school_reopen_1[(iter-n+1):iter,m,,] ) )
		setnames(dt2, 1:4, c('iteration','time','age_cat','value_school_reopen1'))
		dt = cbind(dt, select(dt2, value_school_reopen1))
		
		# add
		tmp <- select(age_cat_map, c('age.cat2', 'age.cat', 'multiplier'))
		setnames(tmp, 'age.cat', 'age_cat')
		
		#	one big merge using integers as key -- this is now fast
		dt <- merge(dt, tmp, by='age_cat', allow.cartesian=TRUE)
		
		# 	aggregate by age groups c
		dt <- dt[, list(value_school_reopen0=sum(value_school_reopen0*multiplier),
										value_school_reopen1=sum(value_school_reopen1*multiplier)), by=c('age.cat2','time','iteration')]	
		
		# add age cat 0 for overall
		tmp <- dt[, list(value_school_reopen1 = sum(value_school_reopen1), 
										 value_school_reopen0 = sum(value_school_reopen0) ), by= c('iteration', 'time')]
		tmp[, age.cat2:=0]
		dt <- rbind(dt,tmp)
		
		# add time index and date
		tmp <- unique(subset(dt, select=time))
		tmp[, date:= dates[[m]][tmp$time[1]] + tmp$time - tmp$time[1]]
		tmp = subset(tmp, date >= start_date)
		dt <- merge(dt, tmp, by='time')
		
		# cumsum
		setkey(dt, iteration, age.cat2, time)
		dt <- dt[, list(time=time,
										date = date,
										value_school_reopen0=cumsum(value_school_reopen0),
										value_school_reopen1=cumsum(value_school_reopen1)), by=c('iteration','age.cat2')]	
		
		# take ratio and difference
		dt[, value_school_reopen1_reopen0_ratio := (value_school_reopen1+ 1e-16) / (value_school_reopen0+ 1e-16)]
		dt[, value_school_reopen1_reopen0_diff := value_school_reopen1 - value_school_reopen0]
		
		#	add loc 
		dt[, loc:= regions[m]]
		
		# just keep last date and overall total
		dt <- subset(dt,date==end_date)
		
		# build new data object only after summarised
		ans[[m]] <- copy(dt)
	}
	ans <- do.call('rbind',ans)
	
	#	make human readable loc labels
	tmp1 <- unique(subset(pop_info, select=c(loc, loc_label)))
	ans <- merge(ans, tmp1, by='loc')
	ans <- merge(rbind(unique(subset(age_cat_map,select=c('age.cat2','age.cat2.label'))),
										 data.table(age.cat2 = 0, age.cat2.label = "Overall")), 
							 ans, by=c('age.cat2'))
	setnames(ans,c('age.cat2','age.cat2.label'), c('age_cat','age_band'))
	
	# just keep last date and overall total
	ans <- subset(ans,date==end_date & age_band=="Overall")
	
	return(ans)	  
}