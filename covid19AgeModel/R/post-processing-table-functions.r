#' @export
#' @keywords internal
#' @import data.table
make_flow_source_tables <- function(flow, dates, pop_info, outfile.base)
{
	cat("\n ----------- make flow source tables ----------- \n")
	
	flow[, L:= paste0(sprintf("%.1f", M*100),' [',sprintf("%.1f", CL*100),'-',sprintf("%.1f", CU*100),']')]
	tmp <- flow[, list( ALL_LOC=length(unique(loc))==length(unique(flow$loc)) ), by='date']
	tmp <- subset(tmp, ALL_LOC)
	tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(dates, function(x) any(x==date)))), by='date']
	tmp <- subset(tmp, ALL_NON_FORECAST)
	firstlast.common.date <- c(min(tmp$date),max(tmp$date))
	cat("\nfirst common date is", as.character(firstlast.common.date[1]))
	cat("\nlast common date is", as.character(firstlast.common.date[2]))
	
	for(i in seq_along(firstlast.common.date))
	{
		ans <- vector('list',1+length(unique(flow$rec_age_cat)))
		names(ans) <- c(levels(flow$rec_age_cat_label),'date')
		ans[[1+length(unique(flow$rec_age_cat))]] <- format(firstlast.common.date[i], "%B %d, %Y")
		for(x in levels(flow$rec_age_cat_label))
		{
			
			df <- subset(flow, rec_age_cat_label==x & date==firstlast.common.date[i])
			tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
			df <- merge(tmp, df, by=c('loc','loc_label'), all.x=TRUE)		
			df <- dcast.data.table(df, loc_label~source_age_cat_label, value.var='L')
			df <- subset(df, select=-which(colnames(df)=='NA'))
			for(z in colnames(df))
			{
				set(df, which(is.na(df[[z]])), z, '-')	
			}
			ans[[x]] <- as.data.frame(df)
		}
		
		tmp <- ifelse(i==1, "firstdate", "lastdate")
		cat('\nWriting ',paste0(outfile.base,'-flow-sources-tables-', tmp, '.rds'),' ...')
		saveRDS(ans, file = paste0(outfile.base,'-flow-sources-tables-', tmp, '.rds'), version = 2)
	}	
}

#' @export
#' @keywords internal
#' @import data.table
make_flow_onward_table <- function(flow, dates, pop_info, outfile.base)
{
	cat("\n ----------- make flow onward tables ----------- \n")
	
	flow[, L:= paste0(sprintf("%.1f", M*100),' [',sprintf("%.1f", CL*100),'-',sprintf("%.1f", CU*100),']')]
	tmp <- flow[, list( ALL_LOC=length(unique(loc))==length(unique(flow$loc)) ), by='date']
	tmp <- subset(tmp, ALL_LOC)
	tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(dates, function(x) any(x==date)))), by='date']
	tmp <- subset(tmp, ALL_NON_FORECAST)
	firstlast.common.date <- c(min(tmp$date),max(tmp$date))
	cat("\nfirst common date is", as.character(firstlast.common.date[1]))
	cat("\nlast common date is", as.character(firstlast.common.date[2]))
	
	for(i in seq_along(firstlast.common.date))
	{
		ans <- vector('list',1+length(unique(flow$source_age_cat)))
		names(ans) <- c(levels(flow$rec_age_cat_label),'date')
		ans[[1+length(unique(flow$source_age_cat))]] <- format(firstlast.common.date[i], "%B %d, %Y")
		for(x in levels(flow$source_age_cat_label))
		{
			
			df <- subset(flow, source_age_cat_label==x & date==firstlast.common.date[i])
			tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
			df <- merge(tmp, df, by=c('loc','loc_label'), all.x=TRUE)		
			df <- dcast.data.table(df, loc_label~rec_age_cat_label, value.var='L')
			df <- subset(df, select=-which(colnames(df)=='NA'))
			for(z in colnames(df))
			{
				set(df, which(is.na(df[[z]])), z, '-')	
			}
			ans[[x]] <- as.data.frame(df)
		}
		
		tmp <- ifelse(i==1, "firstdate", "lastdate")
		cat('\nWriting ',paste0(outfile.base,'-flow-onward-tables-', tmp, '.rds'),' ...')
		saveRDS(ans, file = paste0(outfile.base,'-flow-onward-tables-', tmp, '.rds'), version = 2)
	}	  
}

#' @export
#' @keywords internal
#' @import data.table
make_attack_rate_table <- function(attackrate_byage_c,attackrate_overall_c,pop_info,dates,outfile.base){
  cat("\n ----------- make attack rate tables ----------- \n")
  
  attackrate_byage_c[, L:= paste0(sprintf("%.2f", M*100),' [',sprintf("%.2f", CL*100),'-',sprintf("%.2f", CU*100),']')]
  tmp <- attackrate_byage_c[, list( ALL_LOC=length(unique(loc))==length(unique(attackrate_byage_c$loc)) ), by='date']
  tmp <- subset(tmp, ALL_LOC)
  tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(dates, function(x) any(x==date)))), by='date']
  tmp <- subset(tmp, ALL_NON_FORECAST)
  last.common.date <- max(tmp$date)
  cat("\nlast common date is", as.character(last.common.date))
  
  ans <- attackrate_byage_c[date==last.common.date]
  
  # add overall 
  attackrate_overall_c[, L:= paste0(sprintf("%.2f", M*100),' [',sprintf("%.2f", CL*100),'-',sprintf("%.2f", CU*100),']')]
  tmp <- attackrate_overall_c[date==last.common.date]
  tmp[,age_cat:= 'overall']
  tmp[,age_band:= 'overall']
  
  ans <- rbind(ans, tmp)
  
  ans <- dcast.data.table(ans, loc_label  ~ age_band, value.var='L')
  tmp <- unique(subset(pop_info,select='loc_label'))
  ans <- merge(tmp, ans, all=TRUE, by='loc_label')
  ans[is.na(ans)] <- '-'
  setcolorder(ans, c(1,9,2:8))
  
  ans <- list(ans, format(last.common.date,  "%B %d, %Y") )
  
  cat('\nWriting ',paste0(outfile.base,'-attackrate-table.rds'),' ...')
  saveRDS(ans, file = paste0(outfile.base,'-attackrate-table.rds'), version = 2)
  
  return(ans)
}

#' @export
#' @keywords internal
#' @import data.table
make_attackrate_validation_table <- function(attackrate_byage_s,attackrate_overall_c,pop_info,reportdates,regions,study,outfile.base){
	cat("\n ----------- make attack rate validation tables ----------- \n")
	
	# keep states to report on and exclude any age groups not in state reporting
	attackrate_byage_loc <- attackrate_byage_s[loc %in% regions,]
	attackrate_byage_loc <- attackrate_byage_loc[!is.na(attackrate_byage_loc$age_cat),]
	
	attackrate_byage_loc[, L:= paste0(sprintf("%.2f", M*100),' [',sprintf("%.2f", CL*100),'-',sprintf("%.2f", CU*100),']')]
	attackrate_byage_loc <- attackrate_byage_loc[date %in% reportdates]
	
	# add overall 
	attackrate_overall_loc <- attackrate_overall_c[loc %in% regions,]
	attackrate_overall_loc <- attackrate_overall_loc[date %in% reportdates]
	attackrate_overall_loc[, L:= paste0(sprintf("%.2f", M*100),' [',sprintf("%.2f", CL*100),'-',sprintf("%.2f", CU*100),']')]
	attackrate_overall_loc[,age_cat:= 'overall']
	attackrate_overall_loc[,age_band:= 'overall']
	
	ans <- rbind(attackrate_byage_loc, attackrate_overall_loc)
	ans[, M:= paste0(sprintf("%.2f", M*100))]
	ans[, CL:= paste0(sprintf("%.2f", CL*100))]
	ans[, CU:= paste0(sprintf("%.2f", CU*100))]
	setnames(ans,c('M','CL','CU','L'),c('M_est','CL_est','CU_est','L_est'))
	
	tmp <- unique(subset(pop_info,select='loc_label'))
	ans <- merge(tmp, ans, all.y=TRUE, by='loc_label')
	ans[is.na(ans)] <- '-'
	ans <- subset(ans,select=-c(age_cat,time))
	ncol <- ncol(ans)
	setcolorder(ans, c(1,3,4,2,ncol,5,6,7))
	
	# merge in observed data
	ans <- merge(ans,subset(study,select=-c(dates)),by.x=c('loc','age_band'),by.y=c('state','age_band'))
	
	return(ans)
}
