library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)
library(data.table)

#
#	file names used below
#	these are defined separately so they can be overwritten as needed for PID code
#

GFNAME_jhu_death_data_padded <<- "usa/data/jhu_death_data_padded.rds"
GFNAME_nyt_death_data_padded <<- "usa/data/nyt_death_data_padded.rds"
GFNAME_ihme_hospitalization <<- "usa/data/Hospitalization_all_locs.csv"
GFNAME_states <<- "usa/data/states.csv"
GFNAME_weighted_ifr <<- "usa/data/weighted_ifr_states.RDS"
GFNAME_weighted_ifr_france <<- "usa/data/weighted_ifr_states_france.RDS"
GFNAME_weighted_ifr_netherlands <<- "usa/data/weighted_ifr_states_netherlands.RDS"
GFNAME_global_mobility_report <<- 'usa/data/USA_Google_Mobility.csv'
GFNAME_grouped <<- "usa/data/grouped.csv"
GFNAME_us_population <<- "usa/data/us_population.rds"
GFNAME_us_regions <<- "usa/data/usa-regions.csv"

#
#	functions
#

smooth_fn = function(x, days=3) {
  return(rollmean(x, days, align = "right"))

}


read_death_data <- function(source, smooth = FALSE){
  if (source == "jhu"){
    death_data <- readRDS(GFNAME_jhu_death_data_padded)
  } else if (source ==  "nyt"){
    death_data <- readRDS(GFNAME_nyt_death_data_padded)
  } else if (source == "ihme"){
    data <- read.csv(GFNAME_ihme_hospitalization, stringsAsFactors = FALSE)
    data$date <- ymd(data$date)
    data_subset <- select(data, location_name, date, deaths_mean)
    states <- read.csv(GFNAME_states, stringsAsFactors = FALSE)
    names(states) <- c("location_name", "code")
    data_subset <-left_join(data_subset, states, by = "location_name")
    data_subset <- data_subset[which(!is.na(data_subset$code)),]
    # Choose deaths before current date 
    today <- as.Date("2020-04-27") #Sys.Date() This is the last date of IHME data.
    death_data <- data_subset[which(data_subset$date < today),]
    death_data$daily_deaths <- as.integer(death_data$deaths_mean)
    death_data <- death_data %>%
      group_by(code) %>%
      mutate("cumulative_deaths" = cumsum(daily_deaths))
    death_data$daily_cases <- rep(0, length(death_data$daily_deaths))
  } else if (source == "jhu_adjusted"){
    death_data <- readRDS(GFNAME_jhu_death_data_padded)
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
    death_data <- bind_rows(death_data, nys_df)
    death_data <- death_data[which(death_data$date <= max_date),]
  }
  
  death_data <- death_data[which(death_data$date >= ymd("2020-02-1")),]
  
  if (smooth == TRUE){
    k = 3
    death_data$daily_deaths = c(rep(0, k-1), as.integer(smooth_fn(death_data$daily_deaths, days=k)))
  }
  
  #read regions
  regions <- read.csv('usa/data/usa-regions.csv', stringsAsFactors = FALSE)
  death_data <- merge(death_data, regions, by = 'code')
  return(death_data)
}

read_ifr_data <- function(contact_matrix = "UK"){
  if (contact_matrix == "UK"){
    state_ifr <- readRDS(GFNAME_weighted_ifr)
  } else if (contact_matrix == "France"){
    state_ifr <- readRDS(GFNAME_weighted_ifr_france)
  } else if (contact_matrix == "Netherlands"){
    state_ifr <- readRDS(GFNAME_weighted_ifr_netherlands)
  }
  return(state_ifr)
  
}

read_google_mobility <- function(average = FALSE){
  states <- read.csv(GFNAME_states, stringsAsFactors = FALSE)
  names(states) <- c("sub_region_1", "code")
  google_mobility <- read.csv(GFNAME_global_mobility_report, stringsAsFactors = FALSE)
  google_mobility <- google_mobility[which(google_mobility$country_region_code == "US"),]
  if (average == FALSE){
    #Remove county level data
    google_mobility <- google_mobility[which(google_mobility$sub_region_2 == ""),]
    google_mobility <- left_join(google_mobility, states, by = c("sub_region_1"))
  } else {
    average_us <- google_mobility[which(google_mobility$sub_region_1 == ""),]
    sub_region_1s <- unique(google_mobility$sub_region_1)
    google_mobility <- NULL
    for (i in 1:length(sub_region_1s)){
      tmp <- average_us
      tmp$sub_region_1 <- sub_region_1s[i]
      google_mobility <- rbind(google_mobility, tmp)
    }
    google_mobility <- left_join(google_mobility, states, by = c("sub_region_1"))
  }

  # Format the google mobility data
  google_mobility$date = as.Date(google_mobility$date, format = '%Y-%m-%d')
  google_mobility$X <- NULL

  names(google_mobility) <- c("country_region_code", "country_region", "sub_region_1", "sub_region_2", "metro_area", "iso", "fips",
                              "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                              "workplace", "residential", "code")
  
  google_mobility[, c("retail.recreation", "grocery.pharmacy", "parks", "transitstations", "workplace", "residential")] <- 
    google_mobility[, c("retail.recreation", "grocery.pharmacy", "parks", "transitstations", "workplace", "residential")]/100
  google_mobility[, c("retail.recreation", "grocery.pharmacy", "parks", "transitstations", "workplace")] <- 
    google_mobility[, c("retail.recreation", "grocery.pharmacy", "parks", "transitstations", "workplace")] * -1
  
  google_mobility <- select(google_mobility, "country_region_code", "country_region", "sub_region_1", "sub_region_2",
                              "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                              "workplace", "residential", "code")
  
  return(google_mobility)
}

read_visitdata_mobility <- function(){
  states <- read.csv(GFNAME_states, stringsAsFactors = FALSE)
  names(states) <- c("state", "code")
  data <- read.csv(GFNAME_grouped, stringsAsFactors = FALSE)
  data$date <- ymd(data$date)
  data <- left_join(data, states, by = c("state"))
  data_average <- data %>% group_by(state,date, code) %>% summarize(average=mean(visitIndex,na.rm=T))
  
  data_average <- data.frame("date" = data_average$date, "state" = data_average$state, "code" = data_average$code,
                             "categoryName" = rep("Average", length(data_average$date)),
                             "visitIndex" = data_average$average, 
                             "visitIndexOver65" = rep(NA, length(data_average$date)),
                             "visitIndexUnder65" = rep(NA, length(data_average$date)),
                             "rank" = rep(NA, length(data_average$date)))
  
  data <- bind_rows(data, data_average)
  data <- data %>% 
                select(date, state, code, categoryName, visitIndex) %>%
                spread(key = categoryName, value = visitIndex)
  categories = colnames(data)[4:ncol(data)] #$categoryName) #c("Shops & Services","Grocery")

  
  # data has 100 as baseline - now rescale  between -1 and 1.
  # Choose to multiply all by -1 as they all seem to decrease
  data[,-(1:3)] <- -1*(data[,-(1:3)] - 100)/100
  #data_subset[is.na(data_subset)] <- 0

  names(data) <- names(data) %>% stringr::str_replace_all("\\s","_")
  names(data) <- names(data) %>% stringr::str_replace_all("&","and")
  
  return(data)
}


read_foursquare_mobility <- function()
{
  infile_fsq <- "usa/data/fsq_visit_data_by_age_USstate_200527.csv"
  infile_pop <- "usa/data/us_population.rds"
  
  fsq <- as.data.table(read.csv(infile_fsq, stringsAsFactors = FALSE) )
  setnames(fsq, c('geography','dt'), c('state','date'))
  set(fsq, NULL, 'norm_visits', fsq[, as.numeric(gsub('\\,','',norm_visits))])
  set(fsq, NULL, 'date', fsq[, as.Date(date, format='%d/%m/%Y')])
  tmp <- sort(unique(fsq$age))
  set(fsq, NULL, 'age', fsq[, factor(age, levels=tmp)])
  set(fsq, NULL, 'weekend', fsq[, factor( format(date, "%u") %in% c(6, 7), levels=c(FALSE,TRUE), labels=c('no','yes') )])
  set(fsq, NULL, 'state', fsq[, gsub('New York, NY','NYC',state)])
  #	looking at the raw numbers it is likely that what is reported in NY is excluding NYC
  #	NY has 8.5m pop and NYC has 19.5m pop
  tmp <- dcast.data.table(subset(fsq, grepl('NY',state)), age+date+weekend~state, value.var='norm_visits')	
  tmp[, NY:= NY+NYC]
  tmp[, state:='NY']
  set(tmp, NULL, 'NYC', NULL)
  setnames(tmp, 'NY', 'norm_visits')
  fsq <- rbind( subset(fsq, state!='NY'), tmp )
  
  pop_by_age <- as.data.table(readRDS(infile_pop))
  pop_by_age <- melt(pop_by_age, id.vars=c('Region','code'), variable.name='age',value.name='pop')
  #	NYC data from 2016, based on https://www.baruch.cuny.edu/nycdata/population-geography/pop-demography.htm
  tmp <- data.table(	pop=c(553277,496622,467016,466963,588268,804436,728985,625351,550081,553115,553489,530749,464246,388657,265894,199912,139369,161243),
                     age=c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+'),
                     Region='New York City',
                     code='NYC')
  pop_by_age <- rbind(pop_by_age, tmp)
  set(pop_by_age, pop_by_age[, which(age%in%c('15-19'))], 'pop', pop_by_age[age==c('15-19'), round(2/5*pop,d=0)])
  set(pop_by_age, pop_by_age[, which(age%in%c('15-19'))], 'age','18_24')
  set(pop_by_age, pop_by_age[, which(age%in%c('20-24'))], 'age','18_24')
  set(pop_by_age, pop_by_age[, which(age%in%c('25-29','30-34'))], 'age','25_34')
  set(pop_by_age, pop_by_age[, which(age%in%c('35-39','40-44'))], 'age','35_44')
  set(pop_by_age, pop_by_age[, which(age%in%c('45-49','50-54'))], 'age','45_54')
  set(pop_by_age, pop_by_age[, which(age%in%c('55-59','60-64'))], 'age','55_64')	
  set(pop_by_age, pop_by_age[, which(age%in%c('65-69','70-74','75-79','80-84','85+'))], 'age','65_plus')
  pop_by_age <- pop_by_age[, list(pop=sum(pop)), by=c('Region','code','age')]
  setnames(pop_by_age, c('Region','code'), c('state_name','state'))
  
  
  fsq <- merge(fsq, pop_by_age, by=c('state','age'), all.x=TRUE)
  stopifnot( nrow(subset(fsq, is.na(pop)))==0 )
  
  fsq[, norm_visits_rate:= norm_visits/pop]
  
  fsq <- select(fsq, -norm_visits, -pop)
  fsq <- spread(fsq, key = age, value = norm_visits_rate)
  
  names(fsq)[1] <- "code"
  return(fsq)
}


read_pop_count_us = function(path_to_file = GFNAME_us_population){
  pop_count <- readRDS(path_to_file)
  pop_count <- pop_count[which(!is.na(pop_count$code)),] %>%
    reshape2::melt(id.vars = c("Region", "code")) %>%
    rename(age = variable, pop = value, state = Region)
  pop_count <- pop_count %>%
    group_by(state) %>%
    summarise(popt:= sum(pop)) 

  return(pop_count)
}

read_contact_rate = function(cntct_by,path_to_file_contact, path_to_file_population){
  #	estimates from Melodie Monod using INLA as in van Kassteele AOAS 2017
  load(path_to_file_contact)
  
  #	input variable: length of age bands, i.e. 1 corresponds to [0,1) and 5 corresponds to [0,4)
  if(cntct_by==1){
    contact_tab_matrix = map_contact_tab_to_matrix(countries, contact_tab)
    return(contact_tab_matrix)
    
  } else if(cntct_by==5){ ## aggregate taking into account population size  
    cat('\nAggregating contacts into age bands of length ', cntct_by)
    contact_tab_agg = aggregate_contact_rates(countries, contact_tab, cntct_by, path_to_file_population)
    contact_tab_agg_matrix = map_contact_tab_to_matrix(countries, contact_tab_agg)
    return(contact_tab_agg_matrix)
    
  } else{ 
    stop("cntct_by can take value 1 or 5.")
  }
  
}

read_ifr_data_by_age = function(cntct_by, path_to_file){
  ifr.by.age <- as_tibble(read.csv(path_to_file))
  stopifnot( c('age','ifr_mean')%in%colnames(ifr.by.age) )
  ifr.by.age <- ifr.by.age %>% 
    select(age, ifr_mean)
  #	aggregate data on contacts by 1-year bands to data by desired age bands
  if(cntct_by>1)
  {
    cat('\nAggregating ifr into age bands of length ', cntct_by)	
    tmp <- seq.int(0, 100,by= cntct_by)
    last(tmp) <- last(tmp)+1L
    ifr.by.age <- ifr.by.age %>% 
      mutate(age.agg:= cut(age, breaks=tmp, right=FALSE, labels=seq_len(length(tmp)-1L))) %>%
      group_by(age.agg) %>%
      summarise(ifr_mean:= mean(ifr_mean)) %>%			
      ungroup() %>%
      rename(age= age.agg)
  }
  return(ifr.by.age)
}

read_pop_count_by_age_us = function(path_to_file = GFNAME_us_population){
  pop_by_age <- readRDS(path_to_file)
  pop_by_age <- pop_by_age[which(!is.na(pop_by_age$code)),] %>%
    reshape2::melt(id.vars = c("Region", "code")) %>%
    rename(age = variable, pop = value, state = Region)
  pop_by_age <- pop_by_age %>%
    group_by(state) %>%
    summarise(pop_sum:= sum(pop)) %>%
    inner_join(pop_by_age) %>%
    mutate(pop= pop/pop_sum) %>%
    pivot_wider(id_cols = c('state','age'), names_from='state', values_from = 'pop')
  # remove Total row and age row
  pop_by_age <- pop_by_age[-nrow(pop_by_age),]
  pop_by_age <- as.matrix(pop_by_age[,-1])
  
  return(pop_by_age)
}

read_contact_rate_period = function(cntct_by,path_to_file_contact, path_to_file_population){
  #	estimates from Melodie Monod using INLA as in van Kassteele AOAS 2017
  load(path_to_file_contact)
  
  #	input variable: length of age bands, i.e. 1 corresponds to [0,1) and 5 corresponds to [0,4)
  if(cntct_by==1){
    contact_tab_matrix = map_contact_tab_to_matrix_period(countries, contact_tab)
    return(contact_tab_matrix)
    
  } else if(cntct_by==5){ ## aggregate taking into account population size  
    cat('\nAggregating contacts into age bands of length ', cntct_by)
    contact_tab_agg = aggregate_contact_rates_period(countries, contact_tab, cntct_by, path_to_file_population)
    contact_tab_agg_matrix = map_contact_tab_to_matrix_period(countries, contact_tab_agg)
    return(contact_tab_agg_matrix)
    
  } else{ 
    stop("cntct_by can take value 1 or 5.")
  }
  
}

read_regions <- function(){
  regions <- read.csv(GFNAME_us_regions, stringsAsFactors = FALSE)
  
  return(data)
}

read_colorado_data <- function(){
  data <- read.csv("usa/data/ColoradoCoviddeaths05172020.csv",stringsAsFactors = FALSE)
  data$deathdate <- as.Date(data$deathdate, format = "%m/%d/%Y")
  # Remove cases with missing death data
  data <- data[which(!is.na(data$deathdate)),]
  
  dates <- data.frame("date" = as.Date(min(data$deathdate):max(data$deathdate)))
  
  co_df <- data.frame("code" = rep("CO", length(data$deathdate)),
                      "date" = data$deathdate) %>%
    group_by(code, date) %>%
    summarise("daily_deaths" = n()) 
  co_df <- left_join(dates, co_df, by = "date")
  co_df$code[which(is.na(co_df$code))] <- "CO"
  co_df$daily_deaths[which(is.na(co_df$daily_deaths))] <- 0
  
  num_pad <- (min(co_df$date) - as.Date("2020-02-01"))[[1]]
  
  pad_df <- data.frame("code" = rep("CO", num_pad),
                       "date" = as.Date("2020-02-01") + 0:(num_pad-1),
                       "daily_deaths" = rep(0, num_pad))
  co_df <- bind_rows(pad_df, co_df)
  
  regions <- read.csv('usa/data/usa-regions.csv', stringsAsFactors = FALSE)
  death_data <- left_join(co_df, regions, by = 'code')
  death_data$cumulative_cases = rep(0, length(co_df$date))
  death_data$daily_cases = rep(0, length(co_df$date))
  death_data$cumulative_deaths = cumsum(death_data$daily_deaths)
  
  return(death_data)
}
