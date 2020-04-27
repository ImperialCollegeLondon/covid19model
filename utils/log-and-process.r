library(ggplot2)

source("utils/ifr-tools.r")

log_simulation_inputs <- function(
  run_name, region_to_country_map,  ifr.by.country,
  infection_to_onset, onset_to_death, model_version="v2"){
  # model_version defaults to 2 as from version 3 it gets passed.
  # stores the countries
  
  # Stores the IFRS by region
  ifr.by.region = rep(0.0, length(region_to_country_map))
  all_countries = rep("", length(region_to_country_map))
  for(i in 1:length(region_to_country_map))
  {
    Region = names(region_to_country_map)[i]
    Country = region_to_country_map[[Region]]
    all_countries[i] = Country
    ifr.by.region[i] =ifr.by.country$ifr[ifr.by.country$country == Country]
  }
  input_table = data.frame(
    "region"=names(region_to_country_map),
    "country"=all_countries,
    "IFR"=ifr.by.region,
     row.names = NULL, check.rows = FALSE, check.names = TRUE,
     stringsAsFactors = default.stringsAsFactors())

  write.csv(input_table, paste0(
    "results/", run_name, "-inputs-active_regions_ifr.csv"
    ))
  # The input parameters
  parameter_table = data.frame(
    "infection_to_onset"=infection_to_onset,
    "onset_to_death"=onset_to_death
  )
  write.csv(parameter_table, paste0(
    "results/", run_name, "-inputs-distribution-parameters.csv"
    ))
  fileConn<-file(paste0(
    "results/", run_name, "-model-version.dat"
    ))
  
  writeLines(c(model_version), fileConn)
  close(fileConn)
}

reprocess_simulation <- function (run_name) {
  load(paste0("results/", run_name,'-stanfit.Rdata'))

  # Check that fit data includes certain inputs and if not 
  # Generate them based on some defaults
  if (!exists("region_to_country_map", inherits = FALSE)){
    message("region_to_country_map did not exist creating it")
    for(country in countries){
      region_to_country_map[[country]] <- country
    }
  }

  if (!exists("VERSION", inherits = FALSE)){
    message("VERSION did not exist defaulting to 2")
    VERSION="v2"
  }

  if (!exists("infection_to_onset", inherits = FALSE) 
    || !exists("onset_to_death", inherits = FALSE)){
    message("infection_to_onset and onset_to_death did not exist creating them")
    infection_to_onset <- c("mean"=5.1, "deviation"=0.86)
    onset_to_death <- c("mean"=18.8, "deviation"=0.45)
  }
  ifr.by.country = return_ifr()

  # Extract info from fit
  extracted_fit = rstan::extract(fit)
  if (is.null(extracted_fit)){
    message("ERROR processing file, a fit could not be extracted, skipping")
  } else {
    log_simulation_inputs(run_name, region_to_country_map, 
      ifr.by.country,infection_to_onset, onset_to_death, VERSION)
    postprocess_simulation(run_name, extracted_fit, countries, dates)
  }

}

postprocess_simulation <- function (
  run_name, extracted_fit, countries, dates
){
  message("----------------------------------------------------")
  message(" Plot intervals")
  message("----------------------------------------------------")
  plot_intervals(run_name, extracted_fit, countries, dates)

  message("----------------------------------------------------")
  message(" Plot covariate-size-effects.r")
  message("----------------------------------------------------")
  covariate_size_effects_error <- system(paste0("Rscript covariate-size-effects.r ", run_name,'-stanfit.Rdata'), intern=FALSE)

  message("----------------------------------------------------")
  message(" Plot plot-3-panel.r")
  message("----------------------------------------------------")
  plot_3_panel_error <- system(paste0("Rscript plot-3-panel.r ", run_name,'-stanfit.Rdata'), intern=FALSE)

  message("----------------------------------------------------")
  message(" Plot plot-forecast.r")
  message("----------------------------------------------------")
  plot_forecast_error <- system(paste0("Rscript plot-forecast.r ",run_name,'-stanfit.Rdata'), intern=FALSE)

  message("----------------------------------------------------")
  message(" Plot make-table.r")
  message("----------------------------------------------------")
  make_table_error <- system(paste0("Rscript make-table.r ",run_name,'-stanfit.Rdata'), intern=FALSE)
  errstr = ""
  if(covariate_size_effects_error != 0){
    errstr = paste(errstr, 
      sprintf("Error while plotting covariate size effects! Code: %d\n", covariate_size_effects_error)
    )
  }
  if(plot_3_panel_error != 0){
    errstr = paste(errstr, 
      sprintf("Generation of 3-panel plots failed! Code: %d\n", plot_3_panel_error))
  }
  if(plot_forecast_error != 0) {
    errstr = paste(errstr, 
      sprintf("Generation of forecast plot failed! Code: %d\n", plot_forecast_error))
  }
  if(make_table_error != 0){
    errstr = paste(errstr, 
      sprintf("Generation of alpha covar table failed! Code: %d\n", make_table_error))
  }
  if(errstr != ""){
    message("----------------------------------------------------")
    message(" Error report")
    message("----------------------------------------------------")
    stop(errstr)
  }
}

plot_intervals <- function(run_name, extracted_fit, countries, dates){

  mu = (as.matrix(extracted_fit$mu))
  colnames(mu) = countries
  g = (bayesplot::mcmc_intervals(mu,prob = .9))
  ggsave(sprintf("results/%s-mu.png",run_name),g,width=4,height=6)
  tmp = lapply(1:length(dates), function(i) (extracted_fit$Rt_adj[,length(dates[[i]]),i]))
  Rt_adj = do.call(cbind,tmp)
  colnames(Rt_adj) = countries
  g = (bayesplot::mcmc_intervals(Rt_adj,prob = .9))
  ggsave(sprintf("results/%s-final-rt.png",run_name),g,width=4,height=6)
  write.csv(Rt_adj, sprintf("results/%s-final-Rt.csv",run_name))
  write.csv(mu, sprintf("results/%s-final-mu.csv",run_name))
}

process_stanfit_file <- function (file_name) {
    if(!file.exists(file_name)){
        message(sprintf("WARNING: Skipping, file not found %s \n", file_name))
    } else {
        # if the file is not in results/ throw an error
        is_in_results <- grepl(file_name, "results/")
        if (is.null(is_in_results)){
            message(sprintf("WARNING:  Cannot process %s no in folder './results/'\n", file_name))
        } else {
            trimmed_file_name = gsub(
                "^.*results/(.*)-stanfit.Rdata$",
                "\\1",
                file_name
            )
            message(sprintf("Processing: %s", file_name))
            reprocess_simulation(trimmed_file_name)
        }
    }
    
}