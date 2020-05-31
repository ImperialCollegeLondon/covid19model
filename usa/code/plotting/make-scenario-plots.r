library(dplyr)
library(lubridate)
library(denstrip)
library(geofacet)

source("usa/code/plotting/format-data-plotting.r")

colours_set <- c("pink", "gold", "skyblue")

make_scenario_comparison_plots_mobility_deaths <- function(JOBID, StanModel, len_forecast, last_date_data,
                                                           mobility_increases = 20, ext = ".png"){
  print(sprintf("Plotting scenario comparison plots for deaths for mobility increase: %s", paste(mobility_increases, collapse=" ")))
  lab <- paste(mobility_increases, collapse="_")
  codes <- read.csv("usa/data/states.csv", stringsAsFactors = FALSE)
  names(codes) <- c("full_state", "state")
  
  mob_data <- NULL
   
  for (mobility_increase in mobility_increases){
    print(paste0("Loading:",  paste0('usa/results/sim-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, '-stanfit.Rdata')))
    load(paste0('usa/results/sim-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, '-stanfit.Rdata'))
    out <- rstan::extract(fit)
    
    for (i in 1:length(states)){
      data_state_plot <- format_data(i = i, dates = dates, states = states, 
                                     estimated_cases_raw = estimated_cases_raw, 
                                     estimated_deaths_raw = estimated_deaths_raw, 
                                     reported_cases = reported_cases,
                                     reported_deaths = reported_deaths,
                                     out = out, forecast = 0)
      # Cuts data on last_data_date
      data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
      subset_data <- select(data_state_plot, state, date, reported_deaths, estimated_deaths, 
                            deaths_min, deaths_max)
      if (mobility_increase == 0){
        subset_data$key <- rep(paste0("Constant mobility"), length(subset_data$state))
      } else {
        subset_data$key <- rep(paste0("Increased mobility ", mobility_increase, "%"), length(subset_data$state))
      }
      mob_data <- rbind(mob_data, subset_data)
    }    
  }
  
  mob_data <- left_join(mob_data, codes, by = "state")
  saveRDS(mob_data, paste0('usa/results/deaths-scenario-out-',JOBID,'.RDS'))
  one_key <- unique(mob_data$key)
  data_half <- mob_data[which(mob_data$key == one_key[1]),]
  mob_data$key <- factor(mob_data$key, levels = sort(one_key, decreasing = TRUE))
 
  for (i in 1:length(states)){
    mob_data_subset <- mob_data[which(mob_data$state == states[i]),]
    data_half_subset <- data_half[which(data_half$state == states[i]),]
    p <- ggplot(mob_data_subset) +
      geom_bar(data = data_half_subset, aes(x = date, y = reported_deaths), stat='identity') +
      geom_ribbon(aes(x = date, ymin = deaths_min, ymax = deaths_max, group = key, fill = key), alpha = 0.5) +
      geom_line(aes(x = date, y = estimated_deaths, group = key, col = key), lwd = 1.5) +
      scale_fill_manual(name = "Scenarios", values = colours_set) + 
      scale_colour_manual(name = "Scenarios", values = colours_set) +
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), limits = c(as.Date("2020-02-24"), last_date_data)) + 
      scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) +
      xlab("") + ylab("Daily number of deaths") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "right",
            strip.background = element_rect(colour="black", fill="white"))
    
    ggsave(paste0("usa/figures/", states[[i]], "_scenarios_", len_forecast, '_', lab, '_', JOBID, "_deaths", ext), 
           p, height = 5, width = 7)
  }
}


make_scenario_comparison_plots_mobility_cases <- function(JOBID, StanModel, len_forecast, last_date_data,
                                                    mobility_increases = c(0, 20), ext = ".png", individual = FALSE){
  print(sprintf("Plotting scenario comparison plots for cases for mobility increase: %s", paste(mobility_increases, collapse=" ")))
  codes <- read.csv("usa/data/states.csv", stringsAsFactors = FALSE)
  names(codes) <- c("full_state", "state")
  
  lab <- paste(mobility_increases, collapse="_")
  
  mob_data <- NULL
  
  for (mobility_increase in mobility_increases){
    load(paste0('usa/results/sim-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, '-stanfit.Rdata'))
    out <- rstan::extract(fit)
    
    for (i in 1:length(states)){
      data_state_plot <- format_data(i = i, dates = dates, states = states, 
                                     estimated_cases_raw = estimated_cases_raw, 
                                     estimated_deaths_raw = estimated_deaths_raw, 
                                     reported_cases = reported_cases,
                                     reported_deaths = reported_deaths,
                                     out = out, forecast = 0)
      # Cuts data on last_data_date
      data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
      subset_data <- select(data_state_plot, state, date, reported_cases, predicted_cases, 
                            cases_min, cases_max)
      if (mobility_increase == 0){
        subset_data$key <- rep(paste0("Constant mobility"), length(subset_data$state))
      } else {
        subset_data$key <- rep(paste0("Increased mobility ", mobility_increase, "%"), length(subset_data$state))
      }
      mob_data <- rbind(mob_data, subset_data)
    }    
  }
  
  mob_data <- left_join(mob_data, codes, by = "state")
  saveRDS(mob_data, paste0('usa/results/cases-scenario-out-',JOBID,'.RDS'))
  one_key <- unique(mob_data$key)
  data_half <- mob_data[which(mob_data$key == one_key[1]),]
  
  mob_data$key <- factor(mob_data$key, levels = sort(one_key, decreasing = TRUE))
  
  for (i in 1:length(states)){
    mob_data_subset <- mob_data[which(mob_data$state == states[i]),]
    data_half_subset <- data_half[which(data_half$state == states[i]),]
    p <- ggplot(mob_data_subset) +
      geom_bar(data = data_half_subset, aes(x = date, y = reported_cases), stat='identity') +
      geom_ribbon(aes(x = date, ymin = cases_min, ymax = cases_max, group = key, fill = key), alpha = 0.5) +
      geom_line(aes(x = date, y = predicted_cases, group = key, col = key), lwd = 1.5) + 
      scale_fill_manual(name = "Scenarios", values = colours_set) + 
      scale_colour_manual(name = "Scenarios", values = colours_set) +
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), limits = c(as.Date("2020-02-24"), last_date_data)) + 
      scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) +
      xlab("") + ylab("Daily number of new infections") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "right",
            strip.background = element_rect(colour="black", fill="white")) #+ 
    #ggtitle(states[i])
    
    ggsave(paste0("usa/figures/", states[[i]], "_scenarios_", len_forecast, '_', 
                  lab, '_', JOBID, "_cases", ext), p, height = 5, width = 7) 
 
  }
}


make_scenario_comparison_plots_mobility_rt <- function(JOBID, StanModel, len_forecast, last_date_data,
                                                       mobility_increase, ext = ".png", individual = FALSE){
  codes <- read.csv("usa/data/states.csv", stringsAsFactors = FALSE)
  names(codes) <- c("full_state", "state")
  
  print(sprintf("Making rt scenario comparision plots for mobilities: %s", paste(mobility_increase, collapse=" ")))
  
  mob_data <- NULL
  
  # Read in covariates
  covariates <- readRDS("usa/data/covariates.RDS")
  covariates$Quarantine = NULL
  covariates$GathRecomAny = NULL
  covariates_ended <- readRDS("usa/data/covariates_ended.RDS")
  covariates_ended$Quarantine = NULL
  covariates_ended$GathRecomAny = NULL
  
  covariates_all <- NULL
  
  for (mob in mobility_increase){
    load(paste0('usa/results/sim-', StanModel, '-', len_forecast, '-', mob, '-', 
                JOBID, '-stanfit.Rdata'))
    out <- rstan::extract(fit)
    
    for (i in 1:length(states)){
      data_state_plot <- format_data(i = i, dates = dates, states = states, 
                                     estimated_cases_raw = estimated_cases_raw, 
                                     estimated_deaths_raw = estimated_deaths_raw, 
                                     reported_cases = reported_cases,
                                     reported_deaths = reported_deaths,
                                     out = out, forecast = 0)
      # Cuts data on last_data_date
      data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
      subset_data <- select(data_state_plot, state, date, rt, rt_max, rt_min, 
                            rt_max2, rt_min2)
      if (mob == 0){
        subset_data$key <- rep(paste0("Constant mobility"), length(subset_data$state))
      } else {
        subset_data$key <- rep(paste0("Increased mobility ", mob, "%"), length(subset_data$state))
      }
      mob_data <- rbind(mob_data, subset_data)
    
      if (mob == mobility_increase[length(mobility_increase)]){
        covariates_long <- gather(covariates[which(covariates$StatePostal == states[i]), 
                                             2:ncol(covariates)], 
                                  key = "key", value = "value")
        covariates_long$x <- rep(NA, length(covariates_long$key))
        covariates_long$time <- rep("start", length(covariates_long$key))
        
        
        covariates_ended_long <- gather(covariates_ended[which(covariates_ended$StatePostal == states[i]), 
                                                         2:ncol(covariates_ended)], 
                                        key = "key", value = "value")
        covariates_ended_long$x <- rep(NA, length(covariates_ended_long$key))
        covariates_ended_long$time <- rep("ease", length(covariates_ended_long$key))
        
        covariates_long <- rbind(covariates_long, covariates_ended_long)
        covariates_long$time <- factor(covariates_long$time, levels = c("start", "ease"))
        
        un_dates <- unique(covariates_long$value)
        for (k in 1:length(un_dates)){
          idxs <- which(covariates_long$value == un_dates[k])
          max_val <- ceiling(max(data_state_plot$rt_max)) + 0.3
          for (k in idxs){
            covariates_long$x[k] <- max_val
            max_val <- max_val - 0.3
          }
        }
        
        covariates_long$state <- rep(states[[i]], length(covariates_long$key))
        
        covariates_all <- bind_rows(covariates_all,
                                    covariates_long)
      }
    }
  }
  
  mob_data <- left_join(mob_data, codes, by = "state")
  saveRDS(mob_data, paste0('usa/results/rt-scenario-out-',JOBID,'.RDS'))
  covariates_all <- left_join(covariates_all, codes, by = "state")
  
  one_key <- unique(mob_data$key)
  mob_data$key <- factor(mob_data$key, levels = sort(one_key, decreasing = TRUE))
  
  plot_labels <- c("Emergency decree", 
                   "Restrict public events",
                   "Business closure",
                   "Restaurant closure",
                   "School closure",
                   "Stay at home mandate")
  
  
  for (i in 1:length(states)){
    subset_data <- mob_data[which(mob_data$state == states[i]),]
    subset_covariates_long <- covariates_all[which(covariates_all$state == states[i]),]
    
    p <- ggplot(subset_data) +
      geom_ribbon(aes(x = date, ymin = rt_min, ymax = rt_max, group = key, fill = key), alpha = 0.5) +
      geom_line(aes(x = date, y = rt, group = key, col = key), lwd = 1.5) + 
      geom_hline(yintercept = 1, color = 'black', size = 0.7) + 
      scale_colour_manual(name = "Scenarios", values = colours_set) +
      scale_fill_manual(name = "Scenarios", values = colours_set) + 
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"),  lim = c(as.Date("2020-02-24"), last_date_data)) + 
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      xlab("") +
      ylab(expression(R[t])) +
      guides(shape = guide_legend(order = 2), col = guide_legend(order = 1), fill = guide_legend(order = 0)) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.grid.minor = element_blank(),
            legend.position = "right")
      
      ggsave(paste0("usa/figures/",  states[i], "_scenarios_", len_forecast, '_', JOBID, "_rt", ext),
             p, height = 5, width = 7)
  }
}
