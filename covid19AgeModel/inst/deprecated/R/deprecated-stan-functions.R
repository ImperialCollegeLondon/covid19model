process_make_contact_matrix_by_country_using_popp_logpopdens_logtotalpop_model <- function(pop_info, dpolymod_type, path_to_polymod_data)
{		
  stopifnot(dpolymod_type%in%c('weekend','weekday','anyday'))
  
  #	read polymod data
  dps <- readRDS(path_to_polymod_data)
  dps <- subset(dps, TYPE==dpolymod_type)
  
  #	check correct dimensions
  stopifnot(all(sort(unique(dps$part.age.cat.label))==sort(unique(pop_info$age.cat.label))))
  stopifnot(all(sort(unique(dps$cont.age.cat.label))==sort(unique(pop_info$age.cat.label))))
  
  #	train model
  dtrain <- subset(dps, select=c(m, cont_pop_p, cont_pop_dens, pop_total, part.age.cat.label2, cont.age.cat.label2, LOC ))
  model <- lm(log(m) ~ cont_pop_p + log(cont_pop_dens) + log(pop_total) + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=dtrain)		
  
  #	make prediction data set 
  dpr <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label2,part.age.cat,part.age.cat.label2)))	
  tmp <- subset(pop_info, select=c(loc, age.cat, pop, prop_pop, pop_total, land_area_sqm))
  tmp[, land_area_sqkm:= land_area_sqm*2.589]
  tmp[, cont_pop_dens:= pop/ land_area_sqkm]
  setnames(tmp, c('age.cat','prop_pop','loc'), c('cont.age.cat','cont_pop_p','LOC'))	
  dpr <- merge(dpr, tmp, by=c('cont.age.cat'), allow.cartesian=TRUE)
  set(dpr, NULL, c('cont.age.cat','part.age.cat','land_area_sqkm','land_area_sqm','pop'), NULL)
  
  #	predict
  tmp <- predict(model, dpr, interval = "prediction")
  colnames(tmp) <- c('lm','lm_pr_cl','lm_pr_cu')	
  dpr <- cbind(dpr, tmp)
  set(dpr, NULL, 'm', dpr[,exp(lm)])				#	log median --> median 
  set(dpr, NULL, 'm_pr_cl', dpr[,exp(lm_pr_cl)])	#	log 2.5% --> 2.5%
  set(dpr, NULL, 'm_pr_cu', dpr[,exp(lm_pr_cu)])	#	log 97.5% --> 97.5%
  set(dpr, NULL, c('cont_pop_p','cont_pop_dens','pop_total','lm','lm_pr_cl','lm_pr_cu'), NULL)
  
  #	return
  tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label,cont.age.cat.label2,part.age.cat,part.age.cat.label,part.age.cat.label2)))	
  dpr <- merge(tmp, dpr, by=c('cont.age.cat.label2','part.age.cat.label2'))
  setnames(dpr, 'LOC', 'loc')
  tmp <- unique(subset(pop_info, select=c(loc,loc_label)))
  dpr <- merge(tmp, dpr, by='loc')
  set(dpr, NULL, c('m_pr_cl','m_pr_cu','cont.age.cat.label2','part.age.cat.label2'), NULL)
  
  dpr
}

process_make_contact_matrix_by_country_using_popp_totalpop_model <- function(pop_info, dpolymod_type, path_to_polymod_data)
{		
  stopifnot(dpolymod_type%in%c('weekend','weekday','anyday'))
  
  #	read polymod data
  dps <- readRDS(path_to_polymod_data)
  dps <- subset(dps, TYPE==dpolymod_type)
  
  #	check correct dimensions
  stopifnot(all(sort(unique(dps$part.age.cat.label))==sort(unique(pop_info$age.cat.label))))
  stopifnot(all(sort(unique(dps$cont.age.cat.label))==sort(unique(pop_info$age.cat.label))))
  
  #	train model
  dtrain <- subset(dps, select=c(m, cont_pop_p, pop_total, part.age.cat.label2, cont.age.cat.label2, LOC ))
  model <- lm(log(m) ~ cont_pop_p + pop_total + part.age.cat.label2 : cont.age.cat.label2 - 1L, data=dtrain)		
  
  #	make prediction data set 
  dpr <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label2,part.age.cat,part.age.cat.label2)))	
  tmp <- subset(pop_info, select=c(loc, age.cat, pop, prop_pop, pop_total))
  setnames(tmp, c('age.cat','pop','prop_pop','loc'), c('cont.age.cat','cont_pop','cont_pop_p','LOC'))	
  dpr <- merge(dpr, tmp, by=c('cont.age.cat'), allow.cartesian=TRUE)
  set(dpr, NULL, c('cont.age.cat','part.age.cat','cont_pop'), NULL)
  
  #	predict
  tmp <- predict(model, dpr, interval = "prediction")
  colnames(tmp) <- c('lm','lm_pr_cl','lm_pr_cu')	
  dpr <- cbind(dpr, tmp)
  set(dpr, NULL, 'm', dpr[,exp(lm)])				#	log median --> median 
  set(dpr, NULL, 'm_pr_cl', dpr[,exp(lm_pr_cl)])	#	log 2.5% --> 2.5%
  set(dpr, NULL, 'm_pr_cu', dpr[,exp(lm_pr_cu)])	#	log 97.5% --> 97.5%
  set(dpr, NULL, c('cont_pop_p','pop_total','lm','lm_pr_cl','lm_pr_cu'), NULL)
  
  #	return
  tmp <- unique(subset(dps, select=c(cont.age.cat,cont.age.cat.label,cont.age.cat.label2,part.age.cat,part.age.cat.label,part.age.cat.label2)))	
  dpr <- merge(tmp, dpr, by=c('cont.age.cat.label2','part.age.cat.label2'))
  setnames(dpr, 'LOC', 'loc')
  tmp <- unique(subset(pop_info, select=c(loc,loc_label)))
  dpr <- merge(tmp, dpr, by='loc')
  set(dpr, NULL, c('m_pr_cl','m_pr_cu','cont.age.cat.label2','part.age.cat.label2'), NULL)
  
  dpr
}

process_popbyage = function(pop_by_age){
  pop_by_age <- pop_by_age %>% pivot_wider(id_cols = c('state','age'), names_from='state', values_from = 'pop')
  # remove age col
  tmp <- levels(pop_by_age$age)
  pop_by_age <- as.matrix(pop_by_age[,-1])
  rownames(pop_by_age) <- tmp
  colnames(pop_by_age)[which(colnames(pop_by_age) == "New York City")] = "New_York_City"
  
  return(pop_by_age)
}

stan_data_add_age_band_map_7cat_refLast <- function(processed_data)
{
  stopifnot(processed_data$stan_data$A==18L)
  processed_data$stan_data$A_PARS <- 7L
  processed_data$stan_data$age_band_map <- c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 7L, 7L)
  processed_data
}

stan_data_add_age_band_map_8cat_ref4 <- function(processed_data)
{
  stopifnot(processed_data$stan_data$A==18L)
  processed_data$stan_data$A_PARS <- 8L
  processed_data$stan_data$age_band_map <- c(1L, 1L, 2L, 8L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 7L, 7L) # ref as 8 and remaining increasing
  processed_data$stan_data$A_REF <- 4
  processed_data
}

stan_data_add_age_band_map_3cat_ref2 <- function(processed_data)
{
  stopifnot(processed_data$stan_data$A==18L)
  processed_data$stan_data$A_PARS <- 3L
  processed_data$stan_data$age_band_map <- c(1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L) # ref as 8 and remaining increasing
  processed_data$stan_data$A_REF <- 4
  processed_data
}

pad_mobility <- function(len_mobility, num_pad, min_date, covariates_state, forecast_length, data_state, State){
  if (num_pad <= 0){
    pad_dates_end <- max(covariates_state$date) + 
      days(1:(forecast_length - (min(data_state$date) - min(covariates_state$date)) + 
                (max(data_state$date) - max(covariates_state$date))))
    for_length <- length(pad_dates_end)
    
    # Google mobility
    if (len_mobility == 12){
      len_covariates <- length(covariates_state$grocery.pharmacy)
      padded_covariates <- data.frame("code" = rep(State, length(covariates_state$date) + for_length),
                                      "date" = c(covariates_state$date, pad_dates_end),
                                      "grocery.pharmacy" = c(covariates_state$grocery.pharmacy, 
                                                             rep(median(covariates_state$grocery.pharmacy[(len_covariates-7):len_covariates]),
                                                                 for_length)),
                                      "parks" = c(covariates_state$parks, 
                                                  rep(median(covariates_state$parks[(len_covariates-7):len_covariates]), for_length)), 
                                      "residential" = c(covariates_state$residential, 
                                                        rep(median(covariates_state$residential[(len_covariates-7):len_covariates]), for_length)),
                                      "retail.recreation" = c(covariates_state$retail.recreation, 
                                                              rep(median(covariates_state$retail.recreation[(len_covariates-7):len_covariates]), 
                                                                  for_length)),
                                      "transitstations" = c(covariates_state$transitstations, 
                                                            rep(median(covariates_state$transitstations[(len_covariates-7):len_covariates]), 
                                                                for_length)),
                                      "workplace" = c(covariates_state$workplace, 
                                                      rep(median(covariates_state$workplace[(len_covariates-7):len_covariates]), for_length)))
    } else if(len_mobility == 10) { # foursquare mobility
      len_covariates <- length(covariates_state$shops.services)
      padded_covariates <- data.frame("code" = rep(State, length(covariates_state$date) + for_length),
                                      "date" = c(covariates_state$date, pad_dates_end),
                                      "shops.services" = c(covariates_state$shops.services, 
                                                           rep(median(covariates_state$shops.services[(len_covariates-7):len_covariates]),
                                                               for_length)),
                                      "grocery" = c(covariates_state$grocery, 
                                                    rep(median(covariates_state$grocery[(len_covariates-7):len_covariates]), for_length)), 
                                      "outdoors.recreation" = c(covariates_state$outdoors.recreation, 
                                                                rep(median(covariates_state$outdoors.recreation[(len_covariates-7):len_covariates]), for_length)),
                                      "gasstations" = c(covariates_state$gasstations, 
                                                        rep(median(covariates_state$gasstations[(len_covariates-7):len_covariates]), 
                                                            for_length)),
                                      "professional" = c(covariates_state$professional, 
                                                         rep(median(covariates_state$professional[(len_covariates-7):len_covariates]), for_length)),
                                      "fast.food.restaurants" = c(covariates_state$fast.food.restaurants, 
                                                                  rep(median(covariates_state$fast.food.restaurants[(len_covariates-7):len_covariates]), for_length)),
                                      "medical" = c(covariates_state$medical, 
                                                    rep(median(covariates_state$medical[(len_covariates-7):len_covariates]), for_length)))
      
    }
  } else {
    pad_dates_front <- min_date + days(1:num_pad-1)
    pad_dates_end <- max(covariates_state$date) + 
      days(1:(forecast_length + (max(data_state$date) - max(covariates_state$date))))
    for_length <- length(pad_dates_end)
    if(len_mobility == 12){
      len_covariates <- length(covariates_state$grocery.pharmacy)
      padded_covariates <- data.frame("code" = rep(State, num_pad + length(covariates_state$date) + for_length),
                                      "date" = c(pad_dates_front, covariates_state$date, pad_dates_end),
                                      "grocery.pharmacy" = c(as.integer(rep(0, num_pad)), covariates_state$grocery.pharmacy, 
                                                             rep(median(covariates_state$grocery.pharmacy[(len_covariates-7):len_covariates]), 
                                                                 for_length)),
                                      "parks" = c(as.integer(rep(0, num_pad)), covariates_state$parks, 
                                                  rep(median(covariates_state$parks[(len_covariates-7):len_covariates]), for_length)), 
                                      "residential" = c(as.integer(rep(0, num_pad)), covariates_state$residential, 
                                                        rep(median(covariates_state$residential[(len_covariates-7):len_covariates]), 
                                                            for_length)),
                                      "retail.recreation" = c(as.integer(rep(0, num_pad)), covariates_state$retail.recreation,
                                                              rep(median(covariates_state$retail.recreation[(len_covariates-7):len_covariates]), 
                                                                  for_length)),
                                      "transitstations" = c(as.integer(rep(0, num_pad)), covariates_state$transitstations, 
                                                            rep(median(covariates_state$transitstations[(len_covariates-7):len_covariates]), 
                                                                for_length)),
                                      "workplace" = c(as.integer(rep(0, num_pad)), covariates_state$workplace, 
                                                      rep(median(covariates_state$workplace[(len_covariates-7):len_covariates]), 
                                                          for_length)))
      
    } else if (len_mobility == 10){
      len_covariates <- length(covariates_state$shops.services)
      padded_covariates <- data.frame("code" = rep(State, num_pad + length(covariates_state$date) + for_length),
                                      "date" = c(pad_dates_front, covariates_state$date, pad_dates_end),
                                      "shops.services" = c(as.integer(rep(0, num_pad)), covariates_state$shops.services, 
                                                           rep(median(covariates_state$shops.services[(len_covariates-7):len_covariates]), 
                                                               for_length)),
                                      "grocery" = c(as.integer(rep(0, num_pad)), covariates_state$grocery, 
                                                    rep(median(covariates_state$grocery[(len_covariates-7):len_covariates]), for_length)), 
                                      "outdoors.recreation" = c(as.integer(rep(0, num_pad)), covariates_state$outdoors.recreation, 
                                                                rep(median(covariates_state$outdoors.recreation[(len_covariates-7):len_covariates]), 
                                                                    for_length)),
                                      "gasstations" = c(as.integer(rep(0, num_pad)), covariates_state$gasstations,
                                                        rep(median(covariates_state$gasstations[(len_covariates-7):len_covariates]), 
                                                            for_length)),
                                      "professional" = c(as.integer(rep(0, num_pad)), covariates_state$professional, 
                                                         rep(median(covariates_state$professional[(len_covariates-7):len_covariates]), 
                                                             for_length)),
                                      "fast.food.restaurants" = c(as.integer(rep(0, num_pad)), covariates_state$fast.food.restaurants, 
                                                                  rep(median(covariates_state$fast.food.restaurants[(len_covariates-7):len_covariates]), 
                                                                      for_length)),
                                      "medical" = c(as.integer(rep(0, num_pad)), covariates_state$medical, 
                                                    rep(median(covariates_state$medical[(len_covariates-7):len_covariates]),for_length)))
      
    }
  }
  
  return(padded_covariates)
}

create_features <- function(len_mobility, padded_covariates, covariates_forecast){
  if (len_mobility == 12){
    return (data.frame('school' = covariates_forecast$SchoolClose, 'emergency' = covariates_forecast$EmergDec, 
                       'stayHome' = covariates_forecast$StayAtHome, 'quarantine' = covariates_forecast$Quarantine,
                       'gatherRestrict' = covariates_forecast$GathRestrictAny,
                       'residential' = padded_covariates$residential, 
                       'transit' = padded_covariates$transitstations, 
                       'grocery' = padded_covariates$grocery.pharmacy,
                       'parks' = padded_covariates$parks,
                       'retail' =padded_covariates$retail.recreation,
                       'workplace' = padded_covariates$workplace,
                       'averageMobility' = (padded_covariates$grocery.pharmacy + padded_covariates$parks +
                                              padded_covariates$retail.recreation + padded_covariates$workplace)/4)
    )
    
  } else if (len_mobility ==10){
    return (data.frame('school' = covariates_forecast$SchoolClose, 'emergency' = covariates_forecast$EmergDec, 
                       'stayHome' = covariates_forecast$StayAtHome, 'quarantine' = covariates_forecast$Quarantine,
                       'gatherRestrict' = covariates_forecast$GathRestrictAny,
                       'fastfood' = padded_covariates$fast.food.restaurants, 
                       'professional' = padded_covariates$professional, 
                       'grocery' = padded_covariates$grocery,
                       'shops' = padded_covariates$shops.services,
                       'outdoors' = padded_covariates$outdoors.recreation,
                       'gasstations' = padded_covariates$gasstations,
                       'medical' = padded_covariates$medical,
                       'averageMobility' = (padded_covariates$shops.services + padded_covariates$grocery +
                                              padded_covariates$professional + padded_covariates$outdoors.recreation +
                                              padded_covariates$medical + padded_covariates$fast.food.restaurants)/6)
    )
    
  }
}

