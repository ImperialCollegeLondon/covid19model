library(rstan)
library(data.table)
library(abind)

# save args for report before loading those from running session 
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-numb_chains')	
  args <- list()
  args[['stanModelFile']] <- args_line[[2]]
  args[['out_dir']] <- args_line[[4]]
  args[['job_tag']] <- args_line[[6]]
  args[['numb_chains']] <- args_line[[8]]
} 

args_dir = args

## start script
cat(" \n -------------------------------- \n \n Running \n \n -------------------------------- \n")
str(args)

cat(" \n -------------------------------- \n \n check that HMC chains have run \n \n -------------------------------- \n")

do <- data.table(F=list.files(args_dir$out_dir, pattern='_stanout.RData', recursive=TRUE, full.name=TRUE))
do[, STANMF:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\1',basename(F))]
do[, JOB_TAG:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\2',basename(F))]
do[, JOB_ID:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\3',basename(F))]
do <- subset(do, grepl(args_dir$stanModelFile,STANMF) & grepl(args_dir$job_tag, JOB_TAG))
cat(paste("\n", nrow(do),"/",args_dir$numb_chains, "chains are finished \n"))

if(nrow(do) != args_dir$numb_chains) stop()

outfile.base <- unique( do[, file.path(dirname(dirname(F)), paste0(STANMF,'-',JOB_TAG))] )

cat(" \n -------------------------------- \n \n read job outputs \n \n -------------------------------- \n")
z <- load( gsub('pbs_stanout.RData','pbs_stanin.RData',do[1,F]) )

if(!is.null(args$states)) states = args$states
regions = states
# create not in function
`%notin%` <- Negate(`%in%`)
#	reading job output, merge separate stanfits into one consolidated stanfit object
rf <- vector('list', nrow(do))
for(i in seq_len(nrow(do)))
{
  cat('Loading output in ',do[i,F],'\n')
  z <- load(do[i,F])
  stopifnot('fit' %in% z)
  rf[[i]] <- fit
}
fit <- rstan:::sflist2stanfit(rf)
re <- rstan::extract(fit)
gc()

do <- data.table(F=list.files(args_dir$out_dir, pattern='_stangqs.RDS', recursive=TRUE, full.name=TRUE))
do[, STANMF:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\1',basename(F))]
do[, JOB_TAG:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\2',basename(F))]
do[, JOB_ID:= gsub('^([^-]+)-([^-]+)-([^-]+)$','\\3',basename(F))]
do <- subset(do, grepl(args_dir$stanModelFile,STANMF) & grepl(args_dir$job_tag, JOB_TAG))

if(nrow(do)!=0){
  rf.gqs <- list()
  re.gqs = list()
  
  chains = as.numeric(unique(gsub(".*\\[(.+)\\].*", "\\1", do[,JOB_ID])))
  locations = as.numeric(unique(gsub(".*_location(.+)_.*", "\\1", do[,JOB_ID])))
  
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
  if("RtByAge" %in% vars) re[["RtByAge"]] <- aperm(re[["RtByAge"]], c(1, 4, 2, 3))
  
  gc()
}

cat(" \n -------------------------------- \n \n save job outputs \n \n -------------------------------- \n")

if(!is.null(args$states)) states = args$states

# save the outputs in a RData object
estimated_cases_raw <- aperm(apply(re$E_casesByAge, 1:3, sum), c(1, 3, 2))
estimated_deaths_raw <- re$E_deaths
reported_cases <- reported_cases
reported_deaths <- deaths_by_state
out = re
out$Rt_adj <- out$Rt
out$mu = out$R0
JOBID <- paste0(args$stanModelFile, "-",args$job_tag)

save(fit, stan_data, dates, reported_cases, reported_deaths, states,
   estimated_cases_raw, estimated_deaths_raw, out,JOBID,
   deathByAge_data, file=paste0(outfile.base,'-stanfit.Rdata'))

cat(" \n -------------------------------- \n \n end \n \n -------------------------------- \n")
