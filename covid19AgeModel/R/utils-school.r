#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
find_childcare_closure_date = function(mobility_data, path_to_data)
{
	
	childcare_closure = as.data.table(read.csv(file.path(path_to_data, "coronavirus-child-care-closures-data.csv")))
	childcare_closure = subset(childcare_closure, !State.Abbreviation %in% c("NYC", "DC") ) # dont include the cities 
	tmp1 = subset(childcare_closure, !is.na(First.Policy.Date) & State.Status.First.Policy == "State ordered closure / except for children of parents working in essential sectors")
	tmp1[, close_date := as.Date(First.Policy.Date, format = "%d/%m/%Y")]
	tmp1[, type := "child-care"]
	tmp1 = merge(unique(select(mobility_data, loc, dip_date, loc_label)), select(tmp1,-State.Status.First.Policy, -First.Policy.Date ), by.x = "loc", by.y = "State.Abbreviation")
	
	# number of states with childcare closure
	df = subset(childcare_closure, State.Status.First.Policy == "State ordered closure / except for children of parents working in essential sectors")
	nrow(df) 
	max(as.Date(df$First.Policy.Date, format = "%d/%m/%Y"))
	
	# number of states with climited hildcare 
	df =  subset(childcare_closure, State.Status.First.Policy == "Remain open with limited group sizes")
	nrow(df) 
	max(na.omit(as.Date(df$First.Policy.Date, format = "%d/%m/%Y")))
	
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2
find_stats_school_closure_contactmatrix_UK_China = function(stan_data, path_to_file_contact_intensities_outbreak_China, path_to_file_contact_intensities_outbreak_UK)
{
	
	#
	# Contact matrices from Zhang et al. Science https://science.sciencemag.org/content/368/6498/1481
	
	contact_intensities_outbreak_China <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_China))
	
	children_label = c("0-4", "5-9", "10-14")
	
	#
	# contacts to children
	tmp = subset(contact_intensities_outbreak_China, age_contact %in% children_label)
	tmp = tmp[, list(count_pre = sum(count_pre), count_post = sum(count_post) ), by = c("age_index", "city")]
	tmp[, multiplier := count_post/count_pre]
	
	# from one child
	tmp[age_index %in% children_label, list(cnt_int_pre = mean(count_pre),
					cnt_int_post = mean(count_post),
					cnt_int_ratio = mean(multiplier))]
	
	# from everyone
	tmp[, list(cnt_int_pre = mean(count_pre),
					cnt_int_post = mean(count_post),
					cnt_int_ratio = mean(multiplier))]
	
	#
	# contacts from child
	tmp = subset(contact_intensities_outbreak_China, age_index %in% children_label)
	tmp = tmp[, list(count_pre = mean(count_pre), count_post = mean(count_post) ), by = c("age_contact", "city")]
	
	# total contacts 
	tmp = tmp[, list(count_pre = sum(count_pre),
					count_post = sum(count_post)), by = "city"]
	tmp[, multiplier := count_post/count_pre]
	
	# average aboth cities
	tmp[, list(cnt_int_pre = mean(count_pre),
					cnt_int_post = mean(count_post),
					cnt_int_ratio = mean(multiplier))]
	
	
	# from everyone
	tmp[, list(cnt_int_pre = mean(count_pre),
					cnt_int_post = mean(count_post),
					cnt_int_ratio = mean(multiplier))]
	
	#
	# maximum contact intensity ratio between the city level and their mean
	tmp = contact_intensities_outbreak_China[,list(count_post_avg = mean(count_post)), by = c("age_index", "age_contact") ]
	tmp = merge(contact_intensities_outbreak_China, tmp, by = c("age_index", "age_contact") )
	tmp[, cnt_int_ratio_city := count_post/count_post_avg]
	tmp[age_index %in% children_label | age_contact %in% children_label, list(max_cnt_int_ratio_city = max(na.omit(cnt_int_ratio_city)),
					min_cnt_int_ratio_city = min(na.omit(cnt_int_ratio_city)))]
	tmp1 = tmp[cnt_int_ratio_city > 0]
	tmp1[age_index %in% children_label | age_contact %in% children_label, list(max_cnt_int_ratio_city = max(na.omit(cnt_int_ratio_city)),
					min_cnt_int_ratio_city = min(na.omit(cnt_int_ratio_city)))]
	
	
	#
	# Contact matrices from Jarvis et al. BMC https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-020-01597-8
	
	contact_intensities_outbreak_uk <- as.data.table(readRDS(path_to_file_contact_intensities_outbreak_UK))
	
	children_label_uk = c("0-4", "5-17")
	
	#
	# contacts to children
	tmp = subset(contact_intensities_outbreak_uk, age_contact %in% children_label_uk)
	tmp = tmp[, list(count_pre = sum(count_pre), count_post = sum(count_post) ), by = c("age_index")]
	tmp[, multiplier := count_post/count_pre]
	
	# from one adult
	tmp[, list(cnt_int_pre = mean(count_pre),
					cnt_int_post = mean(count_post),
					cnt_int_ratio = mean(multiplier))]
	
	#
	# maximum contact intensity ratio between Jarvis and Zhang mean among cities 
	
	# process Zhang: 
	# mean over cities
	tmp = contact_intensities_outbreak_China[,list(count_post_avg = mean(count_post)), by = c("age_index", "age_contact") ]
	tmp = subset(tmp, !age_index %in% children_label & age_contact %in% children_label)
	# summed the Zhang contacts towards 5-9 and 10-14 and called the new age cat 5-17
	tmp1 = tmp[age_contact %in% c("5-9", "10-14"), list(count_post_avg = sum(count_post_avg)), by = "age_index"]
	tmp1[, age_contact := "5-17"]
	tmp = rbind(subset(tmp, !age_contact %in% c("5-9", "10-14")), tmp1)
	# mean contacts from everyone expect 0-15 to 0-4 and from everyone expect 0-15 to 5-15
	tmp = tmp[, list(count_post_avg = mean(count_post_avg)), by = "age_contact"]
	
	# Process Jarvis: 
	# mean contacts from everyone expect 0-17 to 0-4 and from everyone expect 0-17 to 5-17
	tmp1 = subset(contact_intensities_outbreak_uk, age_contact %in% children_label_uk)
	tmp1 = tmp1[,list(count_post= mean(count_post)), by = "age_contact"]
	
	# ratio of Jarvis to Zhang
	tmp2 = merge(tmp1, tmp, by = "age_contact")
	tmp2[, cnt_int_ratio_city := count_post/count_post_avg]
	tmp2[, list(max_cnt_int_ratio_city = max(cnt_int_ratio_city), 
					min_cnt_int_ratio_city = min(cnt_int_ratio_city))]
}