# post-processing-knit-report.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n post-processing-knit-report.R \n \n -------------------------------- \n")

suppressMessages(library(rmarkdown, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))
pkg.dir <- system.file(package = "covid19AgeModel" )

#	for dev purposes: olli
if(1)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200703f_cmdstanv'
	args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200703f_cmdstanv-19states_stdctn_2'
	args_dir[['job_tag']] <- '19states_stdctn_2'
	args_dir[['rmd_file']] <- 'usa/code/postprocessing/post-processing-make-report.Rmd'
	args_dir[['report_dir']] <- '~/Box Sync/covid/reports'
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
args_dir[['report_path_to_file']] <- file.path(args_dir[['report_dir']], paste0("report_", args_dir[['stanModelFile']], "-", args_dir[['job_tag']], ".html") )
args_dir[['rmd_path_to_file']] <- file.path(pkg.dir, args_dir[['rmd_file']])

str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

##	make report
cat(paste("\n ----------- create report ----------- \n"))


rmarkdown::render( args_dir[['rmd_path_to_file']], 
		output_file= args_dir[['report_path_to_file']], 
		params = list(
				stanModelFile = args_dir$stanModelFile,
				job_dir= args_dir$out_dir,
				job_tag= args_dir$job_tag,
				states = plot.pars.basic$regions
		))

cat(paste("\n -------------------------------- \n \n post-processing-knit-report.R \n \n -------------------------------- \n"))
