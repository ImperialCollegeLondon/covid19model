# post-processing-test-flows.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-test-flows.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(abind, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes: olli
args_dir <- list()
args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703c_cmdstanv'
args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703c_cmdstanv-19states_stdctn_2'
args_dir[['job_tag']] <- '19states_stdctn_2'

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
} 


## start script
cat(" \n -------------------------------- with debug arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic<- readRDS(file)

file <- paste0(outfile.base,'-stanout-E_casesByAge-gqs.RDS')
cat("\n read RDS:", file)
E_casesByAge <- readRDS(file)

file <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
cat("\n read RDS:", file)
plot.pars.gqs <- readRDS(file)

stan_data <- gqs_add_stan_data_for_flows(plot.pars.basic$stan_data,plot.pars.basic$dates)

reduced_age_bands_map_id <- lapply(unique(stan_data$reduced_age_bands_map),function(x){which(stan_data$reduced_age_bands_map==x)})
for (Location in 1:dim( E_casesByAge)[2]){
  cat('debug location ',Location,'\n')
  E_casesByAge_subset = lapply(stan_data$reduced_flows_Monday_idx[1:stan_data$N_WEEKS_REDUCED_FLOWS[Location],Location], 
         function(x){
           tmp <- E_casesByAge[,Location,x:(x+stan_data$n_days-1L),]
           if (length(dim(tmp))==3){
             tmp <- aperm(tmp,c(1,3,2))
             tmp <- rowSums(tmp, dims = 2)
           }
           ans <- matrix(nrow = dim(tmp)[1],ncol = 0)
           for (i in 1:length(reduced_age_bands_map_id)) {
             ans <- cbind(ans,apply(tmp[,reduced_age_bands_map_id[[i]]],1,sum))
           }
           return(ans)
           })
  E_casesByAge_subset = array(unlist(E_casesByAge_subset),dim = c(dim(E_casesByAge_subset[[1]]),length(E_casesByAge_subset)))
  E_casesByAge_subset <- aperm(E_casesByAge_subset,c(1,3,2))
  reduced_flows_subset <- plot.pars.gqs$reduced_flows[,1:stan_data$N_WEEKS_REDUCED_FLOWS[Location],,,Location] 
  reduced_flows_subset <- aperm(reduced_flows_subset,c(1,2,4,3))
  reduced_flows_subset <- rowSums(reduced_flows_subset, dims = 3) 
  cat('process reduced flows : ',range(E_casesByAge_subset / reduced_flows_subset, na.rm=TRUE),'\n')
  E_casesByAge_subset2 = lapply(stan_data$full_flows_Monday_idx[1:stan_data$N_WEEKS_FULL_FLOWS[Location],Location], 
                               function(x){
                                 tmp <- E_casesByAge[,Location,x:(x+stan_data$n_days-1L),]
                                 if (length(dim(tmp))==3){
                                   tmp <- aperm(tmp,c(1,3,2))
                                   tmp <- rowSums(tmp, dims = 2)
                                 }
                                 return(tmp)
                               })
  E_casesByAge_subset2 = array(unlist(E_casesByAge_subset2),dim = c(dim(E_casesByAge_subset2[[1]]),length(E_casesByAge_subset2)))
  E_casesByAge_subset2 <- aperm(E_casesByAge_subset2,c(1,3,2))
  full_flows_subset <- plot.pars.gqs$full_flows[,,,,Location] 
  full_flows_subset <- aperm(full_flows_subset,c(1,2,4,3))
  full_flows_subset <- rowSums(full_flows_subset, dims = 3) 
  cat('process full flows : ',range(E_casesByAge_subset2 / full_flows_subset, na.rm=TRUE),'\n')
}

cat(" \n -------------------------------- \n \n Completed post-processing-test-flows.R \n \n -------------------------------- \n")