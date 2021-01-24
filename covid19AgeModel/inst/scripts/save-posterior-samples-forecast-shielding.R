
library(rstan)
library(data.table)
library(dplyr)
library(abind)
suppressMessages(library(covid19AgeModel, quietly = TRUE))
pkg.dir <- system.file(package = "covid19AgeModel" )

`%notin%` <- Negate(`%in%`)

#	for dev purposes: 
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015e8_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015e8_cmdstanv-4states_AZCTFKNYC_Sep20_Levin'
args_dir[['job_tag']] <- '4states_AZCTFKNYC_Sep20_Levin'
args_dir[['numb_chains']] <- 8
args_dir[['school.reopen']] = 1
args_dir[['with_forecast']] = 1
args_dir[['multiplier_cntct_school_opening']] <- 1
args_dir[['school_level']]= "K5"
args_dir[['shield']]=1



# save args for report before loading those from running session 
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-numb_chains')	
  stopifnot(args_line[[9]]=='-school.reopen')	
  stopifnot(args_line[[11]]=='-multiplier_cntct_school_opening') 
  stopifnot(args_line[[13]]=='-school_level')	
  stopifnot(args_line[[15]]=='-shield')	
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['numb_chains']] <- args_line[[8]]
  args_dir[['school.reopen']] <- as.numeric(args_line[[10]])
  args_dir[['multiplier_cntct_school_opening']] <- as.numeric(args_line[[12]])
  args_dir[['school_level']] <- as.character( args_line[[14]] )
  args_dir[['shield']] <- as.character( args_line[[16]] )
} 

# if(args_dir$school.reopen){
#   multiplier =  (args_dir$multiplier_cntct_school_opening)*100
#   suffix_sensitivity = paste0('_sensitivity_school_reopen_1', '_multiplier_', multiplier, '_level_', args_dir$school_level)
# } else{
#   suffix_sensitivity = '_sensitivity_school_reopen_0'
# }
multiplier =  (args_dir$multiplier_cntct_school_opening)*100
suffix_sensitivity = paste0('_sensitivity_shielding_',as.integer(args_dir$shield),'_school_reopen_', 
                            as.integer(args_dir$school.reopen),'_multiplier_', multiplier,
                            '_level_', args_dir$school_level)
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
stan_data <- readRDS( gsub('pbs_stanout.RData',paste0('pbs_stan_data',suffix_sensitivity,'.RDS'),do[1,F]) ) 
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

# 
# cat(" \n -------------------------------- save: fit -------------------------------- \n")
# cat("\n save file:", paste0(outfile.base,'-stanout-fit.RDS'))
# saveRDS(fit, file = paste0(outfile.base,'-stanout-fit.RDS') )
# fit <- NULL
# gc()


cat(" \n -------------------------------- load: generated quantities -------------------------------- \n")
do <- data.table(F=list.files(args_dir$out_dir, pattern=paste0('_stangqs',suffix_sensitivity,'.RDS'), recursive=TRUE, full.name=TRUE))
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
      
      if (dim(rf.gqs[[i]][[Location]][['reduced_flows']])[2]!=stan_data$NMAX_WEEKS_REDUCED_FLOWS){
        tmp <- dim(rf.gqs[[i]][[Location]][['reduced_flows']])
        tmp[2] <- stan_data$NMAX_WEEKS_REDUCED_FLOWS - dim(rf.gqs[[i]][[Location]][['reduced_flows']])[2]
        rf.gqs[[i]][[Location]][['reduced_flows']] <- abind(rf.gqs[[i]][[Location]][['reduced_flows']],
                                                            array(NA,dim = tmp),along=2)
      }
      
      
      if (dim(rf.gqs[[i]][[Location]][['full_flows']])[2]!=stan_data$NMAX_WEEKS_FULL_FLOWS){
        tmp <- dim(rf.gqs[[i]][[Location]][['full_flows']])
        tmp[2] <- stan_data$NMAX_WEEKS_FULL_FLOWS - dim(rf.gqs[[i]][[Location]][['full_flows']])[2]
        rf.gqs[[i]][[Location]][['full_flows']] <- abind(rf.gqs[[i]][[Location]][['full_flows']],array(NA,dim = tmp),along=2)
      }
      
      
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
  if(!is.na(dim(re[[i]])[5])){
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
cat("\n save file:", paste0(outfile.base,'-stanout-basic',suffix_sensitivity,'.RDS'))
saveRDS(basic, file = paste0(outfile.base,'-stanout-basic', suffix_sensitivity,'.RDS'))
basic <- NULL
gc()
cat(" \n -------------------------------- processing basic quantities: end -------------------------------- \n")


#
#	processing E_deathsByAge
if("E_deathsByAge" %in% names(re))
{
  cat(" \n -------------------------------- processing E_deathsByAge: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_deathsByAge-gqs',suffix_sensitivity,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_deathsByAge,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_deathsByAge <- NULL
  gc()
  cat(" \n -------------------------------- processing E_deathsByAge: done -------------------------------- \n")	
}

#
#	processing E_casesByAge
if("E_casesByAge" %in% names(re))
{
  cat(" \n -------------------------------- processing E_casesByAge: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_casesByAge-gqs',suffix_sensitivity,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_casesByAge,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_casesByAge <- NULL
  gc()
  cat(" \n -------------------------------- processing E_casesByAge: done -------------------------------- \n")	
}

#
#	processing eff cases by Age
if("E_effcasesByAge" %in% names(re))
{
  cat(" \n -------------------------------- processing eff cases by Age: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-E_effcasesByAge-gqs',suffix_sensitivity,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$E_effcasesByAge,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$E_effcasesByAge <- NULL
  gc()
  cat(" \n -------------------------------- processing eff cases by Age: done -------------------------------- \n")	
}

#
#	processing eff cases by Age
if("RtByAge" %in% names(re))
{
  cat(" \n -------------------------------- processing RtByAge: start -------------------------------- \n")
  file = paste0(outfile.base,'-stanout-RtByAge-gqs',suffix_sensitivity,'.RDS')
  while(!file.exists(file)){
    tryCatch( saveRDS(re$RtByAge,file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
  }
  re$RtByAge <- NULL
  gc()
  cat(" \n -------------------------------- processing RtByAge: done -------------------------------- \n")	
}

#
#	processing flows
cat(" \n -------------------------------- processing flows: start -------------------------------- \n")
flows_gqs <- list()
if("reduced_flows" %in% names(re)) flows_gqs$reduced_flows <- re$reduced_flows
if("full_flows" %in% names(re)) flows_gqs$full_flows <- re$full_flows
file = paste0(outfile.base,'-stanout-flows-gqs', suffix_sensitivity,'.RDS')
cat("\n save file:", file)
while(!file.exists(file)){
  tryCatch( saveRDS(flows_gqs, file), error=function(e){cat("ERROR :",conditionMessage(e), ", let's try again \n")})
}
flows_gqs <- NULL
gc()

cat(" \n -------------------------------- processing flows: end -------------------------------- \n")


cat(" \n -------------------------------- \n \n End save-posterior-samples.r \n \n -------------------------------- \n")