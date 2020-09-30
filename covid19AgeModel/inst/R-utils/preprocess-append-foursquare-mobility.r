library(data.table)
library(dplyr)


append_foursquare_mobility_0525 <- function(infile_fsq_before0525 = file_fsq_mobility_before0525, infile_fsq_after0525 = file_fsq_mobility_after0525, 
                                            path_to_file_pop_us = file_us_population)
{
	indir = "~/git/R0t"
	
	file_fsq_mobility_before0525 <- file.path(indir,'usa','data','fsq_visit_data_by_age_USstate_200527.csv')
	file_fsq_mobility_after0525 <- file.path(indir,'usa','data','fsq_visit_data_june_refresh_200622.csv')
	file_fsq_mobility_after0618 <- file.path(indir,'usa','data','fsq_visit_data_june_refresh_200622.csv')
	file_us_population <- file.path(args$indir,"usa","data","us_population_withnyc.rds")
	
  #plotdir <- '~/Box/OR_Work/2020/2020_covid/data_examples'
  #plotdir <- '~/Box\ Sync/2020/R0t/figures'
  
  # FSQ data before 05-26
  fsq <- as.data.table( read.csv(infile_fsq_before0525, stringsAsFactors = FALSE) ) %>%
    mutate(norm_visits = as.numeric(gsub('\\,','',norm_visits)),
           dt = as.Date(dt, format = "%Y-%m-%d"))
  # OPTIONAL: add under_18 category and set the mobility to 18_24
  #tmp = subset(fsq, age == "18_24")
  #fsq <- rbind(fsq, set(tmp, NULL, 'age', "Under_18"))
 
  # FSQ data after 05-26
  fsq_june_refresh <- as.data.table( read.csv(infile_fsq_after0525, stringsAsFactors = FALSE) ) %>%
    mutate(dt = as.Date(dt, format = "%m/%d/%y"))
  # FOR NOW: remover Under_18 age category
  fsq_june_refresh <- subset(fsq_june_refresh, age != "Under_18")
  
  # join the two datasets
  fsq = data.table(bind_rows(fsq, fsq_june_refresh) )
  
  # replace the value on the 05-25 by 05-24: drop at the extreme 
  fsq = rbind(subset(fsq, dt != as.Date("2020-05-25")), mutate(subset(fsq, dt == as.Date("2020-05-24")), dt = as.Date("2020-05-25"))) 
    
  # Reorder data
  fsq <- with(fsq, fsq[order(geography, dt, age), ])
  
  write.csv(fsq, file = file.path(indir, "usa", "data", "fsq_visit_data_FROMJAN_toJUN_200623.csv"), row.names = F)
}

append_foursquare_mobility_0722 <- function(infile_fsq_before0525 = file_fsq_mobility_before0525, infile_fsq_after0525 = file_fsq_mobility_after0525, 
		path_to_file_pop_us = file_us_population)
{
	require(data.table)
	
	indir = "~/git/R0t"		
	file_old <- file.path(indir, "usa", "data", "fsq_visit_data_FROMJAN_toJUN_200623.csv")
	file_update <- file.path(indir,'usa','data','fsq_visit_data_july_refresh_200722.csv')
	
	do <- as.data.table(read.csv(file_old, stringsAsFactors = FALSE))
	dn <- as.data.table(read.csv(file_update, stringsAsFactors = FALSE))
	set(dn, NULL, 'norm_visits', dn[,as.numeric(gsub('\\,','',norm_visits))])
	set(dn, NULL, 'dt', dn[,as.Date(dt, format = "%Y-%m-%d")])
	
	#	ah -- this is a complete new data set	
}