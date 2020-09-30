# post-processing-etas.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-etas.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(magick, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes: olli
if(0)
{
	args_dir <- list()
	args_dir[['out_dir']] <- '~/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_200728c2_cmdstanv-30states_updatedeath2707'
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_200728c2_cmdstanv'
	args_dir[['job_tag']] <- '30states_updatedeath2707'
	args_dir[['overwrite']] <- 1	
	args_dir[['with_forecast']] <- 0
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-stanModelFile')	
	stopifnot(args_line[[3]]=='-out_dir')
	stopifnot(args_line[[5]]=='-job_tag')
	stopifnot(args_line[[7]]=='-overwrite')
	stopifnot(args_line[[9]]=='-with_forecast')

	args_dir <- list()
	args_dir[['stanModelFile']] <- args_line[[2]]
	args_dir[['out_dir']] <- args_line[[4]]
	args_dir[['job_tag']] <- args_line[[6]]
	args_dir[['overwrite']] <- as.integer(args_line[[8]])
	args_dir[['with_forecast']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
		args_dir$stanModelFile , "-", args_dir$job_tag)


# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)
file <- paste0(outfile.base,'-stanout-transmission_pars.RDS')
cat("\n read RDS:", file)
plot.pars.trmspars <- readRDS(file)
file <- paste0(outfile.base,'-stanout-impact_intv-gqs.RDS')
cat("\n read RDS:", file)
plot.pars.intv <- readRDS(file)


age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)
apply_eta_to_index_and_cnt <- as.logical( plot.pars.basic$stan_data$with_eta2 )
bics_age_cat_map <- make_bics_age_cat_map(plot.pars.basic$pop_info)

file <- paste0(outfile.base,'-a_eta.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
	cat(" \n -------------------------------- \n summarise etas: start \n -------------------------------- \n")
	tryCatch({
		detas <- make_etabyage_summaries(plot.pars.basic$pop_info,
			plot.pars.intv,
			plot.pars.basic$regions,
			age_cat_map,
			plot.pars.basic$dates)
		cat("\nWrite ",file," ... ")	
		saveRDS(detas, file=file)										
	})
	cat(" \n -------------------------------- \n summarise etas: end \n -------------------------------- \n")
}
if(file.exists(file))
{
	detas <- readRDS(file)
}


cat(" \n -------------------------------- \n plot etas: start \n -------------------------------- \n")
tryCatch({
	plot_etabyage(detas, outfile.base)			
})
cat(" \n -------------------------------- \n plot etas: end \n -------------------------------- \n")


file <- paste0(outfile.base,'-contact_patterns_over_time.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
	cat(" \n -------------------------------- \n summarise contact matrices: start \n -------------------------------- \n")
	tryCatch({
		cnts <- make_contact_patterns_over_time_summaries(plot.pars.basic$pop_info,
			plot.pars.basic$stan_data,
			plot.pars.intv,
			plot.pars.basic$regions,
			plot.pars.basic$dates,
			apply_eta_to_index_and_cnt, 
			plot.pars.basic$with_contact_intensities_zhang,
			plot.pars.basic$mobility_data)
		cat("\nWrite ",file," ... ")
		saveRDS(cnts, file=file)		
	})
	cat(" \n -------------------------------- \n summarise contact matrices: end \n -------------------------------- \n")
}
if(file.exists(file))
{
	cnts <- readRDS(file)
}

file <- paste0(outfile.base,'-marginal_contact_intensities_over_time_by_age.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
  cat(" \n -------------------------------- \n summarise marginal contact intensities by age: start \n -------------------------------- \n")
  tryCatch({		
    mcnts <- make_contact_intensities_over_time_summaries(plot.pars.basic$pop_info,
		plot.pars.basic$stan_data,
		plot.pars.intv,
		age_cat_map,
		plot.pars.basic$regions,
		plot.pars.basic$dates,
		apply_eta_to_index_and_cnt,
		plot.pars.basic$with_contact_intensities_zhang)
    
    ocnts <- subset(mcnts, age_cat==0, select=-c(age_cat, age_band))
    mcnts <- subset(mcnts, age_cat!=0)
    cat("\nWrite ",file," ... ")
    saveRDS(mcnts, file=file)
    file2 <- gsub('_by_age','',file)
    cat("\nWrite ",file," ... ")
    saveRDS(ocnts, file=file2)
  })
  cat(" \n -------------------------------- \n summarise marginal contact intensities by age: end \n -------------------------------- \n")
}
if(file.exists(file))
{
  mcnts <- readRDS(file)
  file <- gsub('_by_age','',file)
  ocnts <- readRDS(file)
}

#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ", as.character(date.max)	  )
  mcnts <- subset(mcnts, date<=date.max)
  cnts <- subset(cnts, dates<=date.max)
  ocnts <- subset(ocnts, date<=date.max)
}


cat(" \n -------------------------------- \n plot contact matrices: start \n -------------------------------- \n")
tryCatch({
	fsq.infile.dir <- file.path(args_dir$out_dir, paste0(args_dir$stanModelFile, "-", args_dir$job_tag,"-", plot.pars.basic$JOBID))
	plot_contact_patterns_over_time(cnts, fsq.infile.dir, outfile.base)
})
cat(" \n -------------------------------- \n plot contact matrices: end \n -------------------------------- \n")



cat(" \n -------------------------------- \n plot marginal contact intensities by age: start \n -------------------------------- \n")
plot_marginal_contacts_by_age(mcnts, outfile.base=outfile.base)
cat(" \n -------------------------------- \n plot marginal contact intensities by age: end \n -------------------------------- \n")




cat(" \n -------------------------------- \n plot marginal contact intensities: start \n -------------------------------- \n")
plot_marginal_contacts(ocnts, outfile.base=outfile.base)
cat(" \n -------------------------------- \n plot marginal contact intensities: end \n -------------------------------- \n")



file <- paste0(outfile.base,'-marginal_contact_intensities_across_states_weighted.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
  cat(" \n -------------------------------- \n summarise marginal contact intensities across states: start \n -------------------------------- \n")
  tryCatch({		
    weighted_contacts <- make_contact_intensities_across_states_summaries(plot.pars.basic$regions, 
                                                                          plot.pars.basic$mobility_data,
                                                                          plot.pars.basic$dates,
                                                                          plot.pars.basic$pop_info,
                                                                          plot.pars.basic$stan_data,
                                                                          plot.pars.intv,
                                                                          bics_age_cat_map,
                                                                          apply_eta_to_index_and_cnt,
                                                                          plot.pars.basic$with_contact_intensities_zhang,
                                                                          four_weeks = FALSE)
    
    all_ages <- weighted_contacts[which(is.na(weighted_contacts$bics_age_cat_label)),]
    all_ages$bics_age_cat_label <- NULL
    all_ages$L <- NULL
    all_ages$U <- NULL
    all_ages$min <- NULL
    all_ages$max <- NULL
    all_ages$prop <- NULL

    all_ages$M <- round(all_ages$M, 2)
    all_ages$CL <- round.choose(all_ages$CL, 0.01, 0)
    all_ages$CU <- round.choose(all_ages$CU, 0.01, 0)

    cat("\nWrite ",file," ... ")
    saveRDS(all_ages, file=file, version = 2)
    
    age_specific <- weighted_contacts[which(!is.na(weighted_contacts$bics_age_cat_label)),]
    file2 <- paste0(outfile.base,'-marginal_contact_intensities_across_states_weighted_age.rds')
    saveRDS(age_specific, file=file2, version = 2)
    
    p <- make_age_contact_boxplot(age_specific, outfile.base)
    
    
  })
  cat(" \n -------------------------------- \n summarise marginal contact intensities across states: end \n -------------------------------- \n")
}

cat(" \n -------------------------------- \n \n Completed post-processing-etas.R \n \n -------------------------------- \n")
