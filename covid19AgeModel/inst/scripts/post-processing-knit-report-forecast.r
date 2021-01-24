# post-processing-knit-report.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-knit-report.R \n \n -------------------------------- \n")

suppressMessages(library(rmarkdown, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))
pkg.dir <- system.file(package = "covid19AgeModel" )

#	for dev purposes: melodie
if(0)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200821b9_cmdstanv'
  args_dir[['out_dir']] <- '/Users/melodiemonod/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_200821b9_cmdstanv-test_new_script2'
  args_dir[['job_tag']] <- 'test_new_script2'
  args_dir[['rmd_path_to_file']] <- '/Users/melodiemonod/git/R0t/covid19AgeModel/inst/scripts/post-processing-make-report-forecast.Rmd'
  args_dir[['report_dir']] <- '/Users/melodiemonod/Downloads'
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-rmd_file')
  stopifnot(args_line[[3]]=='-stanModelFile')	
  stopifnot(args_line[[5]]=='-out_dir')
  stopifnot(args_line[[7]]=='-job_tag')
  stopifnot(args_line[[9]]=='-report_dir')
  args_dir <- list()
  args_dir[['rmd_file']] <- args_line[[2]]
  args_dir[['stanModelFile']] <- args_line[[4]]
  args_dir[['out_dir']] <- args_line[[6]]
  args_dir[['job_tag']] <- args_line[[8]]
  args_dir[['report_dir']] <- args_line[[10]]
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
args_dir[['report_path_to_file']] <- file.path(args_dir[['report_dir']], paste0("report_forecast_", args_dir[['stanModelFile']], "-", args_dir[['job_tag']], ".html") )
args_dir[['rmd_path_to_file']] <- file.path(pkg.dir, args_dir[['rmd_file']])

str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

##	make report
cat(paste("\n ----------- create report ----------- \n"))


rmarkdown::render( args_dir[['rmd_path_to_file']], 
                   output_file= args_dir[['report_path_to_file']], 
                   params = list(
                     stanModelFile = args_dir$stanModelFile,
                     job_dir= args_dir$out_dir,
                     job_tag= args_dir$job_tag
                   ))

cat(paste("\n -------------------------------- \n \n post-processing-knit-report.R \n \n -------------------------------- \n"))
