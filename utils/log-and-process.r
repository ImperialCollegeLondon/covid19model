library(ggplot2)

source("utils/ifr-tools.r")

log_simulation_inputs <- function(
  run_name, region_to_country_map,  ifr.by.country,
  infection_to_onset, onset_to_death){

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

  if (!exists("infection_to_onset", inherits = FALSE)){
    message("infection_to_onset and onset_to_death did not exist creating them")
    infection_to_onset <- c("mean"=5.1, "deviation"=0.86)
    onset_to_death <- c("mean"=18.8, "deviation"=0.45)
  }

  ifr.by.country = return_ifr()

  # Extract info from fit
  extracted_fit = rstan::extract(fit)
  log_simulation_inputs(run_name, region_to_country_map, 
    ifr.by.country,infection_to_onset, onset_to_death)
  postprocess_simulation(run_name, extracted_fit, countries, dates)

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
  system(paste0("Rscript covariate-size-effects.r ", run_name,'-stanfit.Rdata'))
  message("----------------------------------------------------")
  message(" Plot plot-3-panel.r")
  message("----------------------------------------------------")
  system(paste0("Rscript plot-3-panel.r ", run_name,'-stanfit.Rdata'))
  message("----------------------------------------------------")
  message(" Plot plot-forecast.r")
  message("----------------------------------------------------")
  system(paste0("Rscript plot-forecast.r ",run_name,'-stanfit.Rdata'))
  message("----------------------------------------------------")
  message(" Plot make-table.r")
  message("----------------------------------------------------")
  system(paste0("Rscript make-table.r ",run_name,'-stanfit.Rdata'))
}

plot_intervals <- function(run_name, extracted_fit, countries, dates){

  mu = (as.matrix(extracted_fit$mu))
  colnames(mu) = countries
  g = (bayesplot::mcmc_intervals(mu,prob = .9))
  ggsave(smessagef("results/%s-mu.png",run_name),g,width=4,height=6)
  tmp = lapply(1:length(dates), function(i) (extracted_fit$Rt_adj[,length(dates[[i]]),i]))
  Rt_adj = do.call(cbind,tmp)
  colnames(Rt_adj) = countries
  g = (bayesplot::mcmc_intervals(Rt_adj,prob = .9))
  ggsave(smessagef("results/%s-final-rt.png",run_name),g,width=4,height=6)

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