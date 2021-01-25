cat(" \n -------------------------------- \n \n Running save-posterior-samples-new-strain.r \n \n -------------------------------- \n")

library(rstan)
library(data.table)
library(dplyr)
library(abind)
suppressMessages(library(covid19AgeModel, quietly = TRUE))
pkg.dir <- system.file(package = "covid19AgeModel" )

`%notin%` <- Negate(`%in%`)

args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015i4_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015i4_cmdstanv-test_new_strain'
args_dir[['job_tag']] <- 'test_new_strain'
args_dir[['numb_chains']] <- 8
args_dir[['rel_transmissibility_new_strain']] <- 170
args_dir[['prop_cases_new_strain_first_day']] <- 01
args_dir[['school.closure.2']] <- '0'
args_dir[['rebound.mobility']] <- '0'

# save args for report before loading those from running session 
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-numb_chains')	
  stopifnot(args_line[[9]]=='-rel_transmissibility_new_strain')	
  stopifnot(args_line[[11]]=='-prop_cases_new_strain_first_day') 
  stopifnot(args_line[[13]]=='-school.closure.2') 
  stopifnot(args_line[[15]]=='-rebound.mobility') 
  
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['numb_chains']] <- args_line[[8]]
  args_dir[['rel_transmissibility_new_strain']] <- args_line[[10]]
  args_dir[['prop_cases_new_strain_first_day']] <- args_line[[12]]
  args_dir[['school.closure.2']] <- args_line[[14]]
  args_dir[['rebound.mobility']] <- args_line[[16]]
} 

school.closure.2 = args_dir$school.closure.2 == '1'
with_rebound_mobility = args_dir$rebound.mobility == '1'
suffix = paste0('_new_strain', '_school_closure_2_', school.closure.2,'_rebound_mobility_',with_rebound_mobility)


## start script
cat(" \n --------------------------------  with arguments -------------------------------- \n")
str(args_line)

cat(" \n -------------------------------- check that HMC chains have run -------------------------------- \n")

do <- data.table(F=list.files(args_dir$out_dir, pattern='_stanout.RData', recursive=TRUE, full.name=TRUE))
do[, STANMF:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\1',basename(F))]
do[, JOB_TAG:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\2',basename(F))]
do[, JOB_ID:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\3',basename(F))]
do <- subset(do, grepl(args_dir$stanModelFile,STANMF) & grepl(args_dir$job_tag, JOB_TAG))
cat(paste("\n", nrow(do),"/",args_dir$numb_chains, "chains are finished \n"))

#if(nrow(do) != args_dir$numb_chains) stop()

outfile.base <- unique( do[, file.path(dirname(dirname(F)), paste0(STANMF,'-',JOB_TAG))] )
outfile.base2 <- unique( do[, file.path(dirname(dirname(F)))] )

args_dir[['JOBID']] <- do[1, gsub('^([^-]+)-([^-]+)-([^-]+)_([^-]+)$','\\3',basename(F))]

stopifnot(length(outfile.base)==1 )

cat(" \n -------------------------------- load jobs outputs -------------------------------- \n")

#	load all input variables for this analysis run
z <- load( gsub('pbs_stanout.RData','pbs_stanin.RData',do[1,F]) )
stan_data <- readRDS( gsub('pbs_stanout.RData',paste0('pbs_stan_data',suffix,'.RDS'),do[1,F]) ) 
cntct_by <- as.integer(args$cntct_by)
cntct_bands <- stan_data$A
seedAge <- args$seedAge

# account for the changes of variable's name
with_avg_mobility_data <- 0
if(!is.null(args$pooled) ) with_avg_mobility_data <- args$pooled
if(!is.null( args$with_avg_mobility_data)) with_avg_mobility_data <- args$with_avg_mobility_data
change_covariates_coef_after_upswing <- 0
if(!is.null( args$change_covariates_coef_after_upswing)) change_covariates_coef_after_upswing <- args$change_covariates_coef_after_upswing
with_contact_intensities_zhang <- 0
if(!is.null( args$with_contact_intensities_zhang)) with_contact_intensities_zhang <- args$with_contact_intensities_zhang
if(is.null(args$cntct_by)) args$cntct_by = args$contact_by ## there was a change of variable 
if(!is.null(args$states)) states = args$states

stopifnot(c('dates','deaths_by_state') %in% z)
regions = states
deaths_by_region = deaths_by_state

if("google_mobility" %in% z) mobility_data = google_mobility

str(args)

#	reading job output, merge separate stanfits into one consolidated stanfit object
rf <- vector('list', nrow(do))
median_lp_ <- vector('numeric', nrow(do))
for(i in seq_len(nrow(do)))
{
  cat('Loading output in ',do[i,F],'\n')
  z <- load(do[i,F])
  stopifnot('fit' %in% z)
  median_lp_[i] = median(rstan:::extract(fit)$lp__)
  rf[[i]] <- fit
}
fit <- rstan:::sflist2stanfit(rf)
re <- rstan::extract(fit)

# exclude chain that did not converge using lp__
outliers = NULL
lowerq = quantile(re$lp__)[2]
upperq = quantile(re$lp__)[4]
iqr = upperq - lowerq #
mild.threshold.upper = (iqr * 1.5) + upperq
mild.threshold.lower = lowerq - (iqr * 1.5)
# Any data point outside (> mild.threshold.upper or < mild.threshold.lower) these values is a mild outlier
num.outlier = sum(median_lp_ < mild.threshold.lower | median_lp_ > mild.threshold.upper)

cat("/n There is (are) ", num.outlier, "chain that did not converge /n")
# remove outliers if any
if(num.outlier != 0){
  outliers = which(median_lp_ < mild.threshold.lower | median_lp_ > mild.threshold.upper)
  cat("chain(s)", outliers, "did not converge and is (are) removed /n")
  rf <- rf[-outliers]
  fit <- rstan:::sflist2stanfit(rf)
  re <- rstan::extract(fit)
}


cat(" \n -------------------------------- load: generated quantities -------------------------------- \n")
do <- data.table(F=list.files(args_dir$out_dir, pattern=paste0('_stangqs',suffix,'.RDS'), recursive=TRUE, full.name=TRUE))
do[, STANMF:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\1',basename(F))]
do[, JOB_TAG:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\2',basename(F))]
do[, JOB_ID:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\3',basename(F))]
do <- subset(do, grepl(args_dir$stanModelFile,STANMF) & grepl(args_dir$job_tag, JOB_TAG))

# remove outliers if any
if(num.outlier != 0){
  for(outlier in outliers){
    outlier.rows = grepl(paste0("\\[", outlier,"\\]"), do$F)
    do = do[!outlier.rows,]
  }
}

if(nrow(do)!=0)
{
  rf.gqs <- list()
  re.gqs = list()
  
  chains = as.numeric(unique(gsub(".*\\[(.+)\\].*", "\\1", do[,JOB_ID])))
  locations = as.numeric(unique(gsub(".*_location(.+)_stangqs.*", "\\1", do[,JOB_ID])))
  
  stan_data <- gqs_add_stan_data_for_flows(stan_data,dates)
  
  for(i in chains)
  {
    
    rf.gqs[[i]] <- list()
    
    for(Location in locations){
      index = which(  grepl(paste0("_location", Location, "_"), do[,JOB_ID]) & grepl(paste0("\\[", i, "\\]"), do[,JOB_ID])  )
      cat('Loading output in ', do[index,F],'\n')
      rf.gqs[[i]][[Location]] <- readRDS(do[index,F])
      
      
      re.gqs[[i]] = list()
      for (var in names(rf.gqs[[i]][[Location]])){
        re.gqs[[i]][[var]] =  array(unlist(lapply(rf.gqs[[i]], "[[", var)), dim = c(dim(rf.gqs[[i]][[1]][[var]]), length(locations)))
      }
    }
  }
  
  vars = names(re.gqs[[chains[1]]])[names(re.gqs[[chains[1]]]) %notin% names(re)]
  
  for (var in vars){
    listvar = list( do.call("abind",list(lapply(re.gqs, "[[", var), along = 1)) )
    names(listvar) = var
    re <- c(re, listvar)
  }
  
  # permute the dimension to match previous dim
  if("E_casesByAge" %in% vars) re[["E_casesByAge"]] <- aperm(re[["E_casesByAge"]], c(1, 4, 2, 3))
  if("E_deathsByAge" %in% vars) re[["E_deathsByAge"]] <- aperm(re[["E_deathsByAge"]], c(1, 4, 2, 3))
  if("E_antibodyByAge" %in% vars) re[["E_antibodyByAge"]] <- aperm(re[["E_antibodyByAge"]], c(1, 4, 2, 3))
  if("RtByAge" %in% vars) re[["RtByAge"]] <- aperm(re[["RtByAge"]], c(1, 4, 2, 3))
  if("lambdaByAge" %in% vars) re[["lambdaByAge"]] <- aperm(re[["lambdaByAge"]], c(1, 4, 2, 3))
  if("E_effcasesByAge" %in% vars) re[["E_effcasesByAge"]] <- aperm(re[["E_effcasesByAge"]], c(1, 4, 2, 3))
  
  if("E_casesByStrain" %in% vars) re[["E_casesByStrain"]] <- aperm(re[["E_casesByStrain"]], c(1, 4, 2, 3))
  if("E_deathsByStrain" %in% vars) re[["E_deathsByStrain"]] <- aperm(re[["E_deathsByStrain"]], c(1, 4, 2, 3))
  if("E_antibodyByStrain" %in% vars) re[["E_antibodyByStrain"]] <- aperm(re[["E_antibodyByStrain"]], c(1, 4, 2, 3))
  if("RtByStrain" %in% vars) re[["RtByStrain"]] <- aperm(re[["RtByStrain"]], c(1, 4, 2, 3))
  if("lambdaByStrain" %in% vars) re[["lambdaByStrain"]] <- aperm(re[["lambdaByStrain"]], c(1, 4, 2, 3))
  if("E_effcasesByStrain" %in% vars) re[["E_effcasesByStrain"]] <- aperm(re[["E_effcasesByStrain"]], c(1, 4, 2, 3))
  
  if("E_casesByAgeByStrain" %in% vars) re[["E_casesByAgeByStrain"]] <- aperm(re[["E_casesByAgeByStrain"]], c(1, 5, 3, 4, 2))
  if("E_deathsByAgeByStrain" %in% vars) re[["E_deathsByAgeByStrain"]] <- aperm(re[["E_deathsByAgeByStrain"]], c(1, 5, 3, 4, 2))
  if("E_antibodyByAgeByStrain" %in% vars) re[["E_antibodyByAgeByStrain"]] <- aperm(re[["E_antibodyByAgeByStrain"]], c(1, 5, 3, 4, 2))
  if("RtByAgeByStrain" %in% vars) re[["RtByAgeByStrain"]] <- aperm(re[["RtByAgeByStrain"]], c(1, 5, 3, 4, 2))
  if("lambdaByAgeByStrain" %in% vars) re[["lambdaByAgeByStrain"]] <- aperm(re[["lambdaByAgeByStrain"]], c(1, 5, 3, 4, 2))
  if("E_effcasesByAgeByStrain" %in% vars) re[["E_effcasesByAgeByStrain"]] <- aperm(re[["E_effcasesByAgeByStrain"]], c(1, 5, 3, 4, 2))
  
  gc()
}

# subsample n
if(length(unique(lapply(re, function(x) dim(x)[1])))!=1)
{
  stop('number of iterations in Stan and gqs don\'t match')
}
iter <- nrow(re[[1]])
n <- min(5000, iter)
for(i in names(re))
{
  if(!is.na(dim(re[[i]])[6])){
    re[[i]] = re[[i]][(iter-n+1):iter,,,,,]
  } else if(!is.na(dim(re[[i]])[5])){
    re[[i]] = re[[i]][(iter-n+1):iter,,,,]
  } else if(!is.na(dim(re[[i]])[4])){
    re[[i]] = re[[i]][(iter-n+1):iter,,,]
  } else if(!is.na(dim(re[[i]])[3])){
    re[[i]] = re[[i]][(iter-n+1):iter,,]
  } else if(!is.na(dim(re[[i]])[2])){
    re[[i]] =  re[[i]][(iter-n+1):iter,]
  } else{
    re[[i]] = re[[i]][(iter-n+1):iter]
  }
}

#	region ID and region names
dc <- data.table(region=seq_along(regions), region_name=regions)
n.region = length(regions)

#	dates for each region
da <- as.data.table( reshape2::melt( dates ) )
setnames(da, 1:2, c('dates','region_name'))
da <- da[, list(time=seq_along(dates), dates=dates),by='region_name']

# full names of regions
region_names <- pop_info %>% select(loc,loc_label) %>% unique() %>% arrange(loc) %>% inner_join(dc,by=c('loc'='region_name'))

#	age categories
dages <- data.table(age_cat= 1:cntct_bands)
dages[, age_band:= paste0('[',(age_cat-1)*args$cntct_by,'-',age_cat*args$cntct_by,')') ]
if(cntct_bands == 18) dages$age_band[cntct_bands] = "[85,100)"
dages[, age_band:= factor(dages$age_band, levels=dages$age_band)]

#	observed deaths over time
dd <- as.data.table( reshape2::melt( deaths_by_region ) )
setnames(dd, 1:2, c('obs_deaths','region_name'))
dd <- dd[, list(time=seq_along(obs_deaths), obs_deaths=obs_deaths),by='region_name']

# map for strain
ds = data.table(strain_cat = 1:2, strain_cat_label = c('initial strain', 'new strain'))

cat(" \n -------------------------------- processing job outputs -------------------------------- \n")


stan_data <- gqs_add_stan_data_for_flows(stan_data,dates)


#
#	processing basic quantities
cat(" \n -------------------------------- processing basic quantities: start -------------------------------- \n")
basic <- list(regions=regions,
              region_names=region_names,
              dates=dates,
              dd=dd,
              dc=dc,
              da=da,
              ds = ds,
              pop_by_age=pop_by_age,
              pop_info=pop_info,
              serial_interval=serial_interval,
              cntct_bands=cntct_bands,
              death_data=death_data,
              stan_data=stan_data,
              mobility_data=mobility_data,
              dages=dages,
              reported_cases=reported_cases,
              reported_deaths=deaths_by_state,
              deathByAge=deathByAge,
              deathByAge_data=deathByAge_data,
              change_covariates_coef_after_upswing=change_covariates_coef_after_upswing,
              with_contact_intensities_zhang = with_contact_intensities_zhang, 
              outfile.base=outfile.base,
              outfile.base2=outfile.base2,
              script_dir=pkg.dir,
              with_avg_mobility_data=with_avg_mobility_data,
              JOBID = args_dir$JOBID)
cat("\n save file:", paste0(outfile.base,'-stanout-basic',suffix,'.RDS'))
saveRDS(basic, file = paste0(outfile.base,'-stanout-basic', suffix,'.RDS'))
basic <- NULL
gc()
cat(" \n -------------------------------- processing basic quantities: end -------------------------------- \n")


#
#	expected deaths for both strains (unstratified by age)
if("E_deathsByStrain" %in% names(re))
{
  cat(" \n -------------------------------- processing E_deathsByStrain: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_deathsByStrain-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_deathsByStrain,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_deathsByStrain <- NULL
  gc()
  cat(" \n -------------------------------- processing E_deathsByStrain: done -------------------------------- \n")	
}

#
#	the sum of expected deaths over both strains (unstratified by age)
if("E_deaths" %in% names(re))
{
  cat(" \n -------------------------------- processing E_deaths: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_deaths-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_deaths,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_deaths <- NULL
  gc()
  cat(" \n -------------------------------- processing E_deaths: done -------------------------------- \n")	
}


#
#	expected cases for both strains (unstratified by age)
if("E_casesByStrain" %in% names(re))
{
  cat(" \n -------------------------------- processing E_casesByStrain: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_casesByStrain-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_casesByStrain,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_casesByStrain <- NULL
  gc()
  cat(" \n -------------------------------- processing E_casesByStrain: done -------------------------------- \n")	
}

#
#	sum of expected cases over both strains (unstratified by age)
if("E_cases" %in% names(re))
{
  cat(" \n -------------------------------- processing E_cases: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_cases-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_cases,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_cases <- NULL
  gc()
  cat(" \n -------------------------------- processing E_cases: done -------------------------------- \n")	
}

#
#	effective cases by age by strain
if("E_effcasesByAgeByStrain" %in% names(re))
{
  cat(" \n -------------------------------- processing E_effcasesByAgeByStrain: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_effcasesByAgeByStrain-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_effcasesByAgeByStrain,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_effcasesByAgeByStrain <- NULL
  gc()
  cat(" \n -------------------------------- processing E_effcasesByAgeByStrain: done -------------------------------- \n")	
}

#
#	processing Rt by age for both strains over time
if("RtByAgeByStrain" %in% names(re))
{
  cat(" \n -------------------------------- processing RtByAgeByStrain: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-RtByAgeByStrain-gqs',suffix,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$RtByAgeByStrain,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$RtByAgeByStrain <- NULL
  gc()
  cat(" \n -------------------------------- processing RtByAgeByStrain: done -------------------------------- \n")	
}

cat(" \n -------------------------------- \n \n End save-posterior-samples-new-strain.r \n \n -------------------------------- \n")