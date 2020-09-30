library(rstan)

args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-csv_file')
  stopifnot(args_line[[3]]=='-rda_file')	
  args <- list()
  args[['csv_file']] <- args_line[[2]]
  args[['rda_file']] <- args_line[[4]]
} 


#	convert to stanfit object and save
fit <- rstan::read_stan_csv(args$csv_file)
save(fit, file=args$rda_file)




