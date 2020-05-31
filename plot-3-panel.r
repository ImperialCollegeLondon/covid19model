library(tidyr)
library(dplyr)
library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)
library(matrixStats)
library(scales)
library(gridExtra)
library(ggpubr)
library(bayesplot)
library(cowplot)
library(svglite)
library(ggplot2)

source("utils/geom-stepribbon.r")
#---------------------------------------------------------------------------
make_three_pannel_plot <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args)==1){
    filename2 = args[1]
    percent_pop = FALSE
  } else {
    filename2 = args[1]
    percent_pop = args[2]
  }
  
  load(paste0("results/", filename2))
  print(sprintf("loading: %s",paste0("results/",filename2)))
  covariates = read.csv('data/interventions.csv', stringsAsFactors = FALSE)
  names_covariates = c('Schools + Universities','Self-isolating if ill', 'Public events', 
                       'Lockdown', 'Social distancing encouraged')
  covariates <- covariates %>%
    filter((Type %in% names_covariates))
  covariates <- covariates[,c(1,2,4)]
  covariates <- spread(covariates, Type, Date.effective)
  names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
  covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
  covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
  covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
  covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
  covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
  covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")
  
  all_data <- data.frame()
  all_data_out <- data.frame()
  intervention_data <- data.frame()
  for(i in 1:length(countries)){
    print(i)
    N <- length(dates[[i]])
    country <- countries[[i]]
    
    predicted_cases <- colMeans(prediction[,1:N,i])
    predicted_cases_li <- colQuantiles(prediction[,1:N,i], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,i], probs=.975)
    predicted_cases_li2 <- colQuantiles(prediction[,1:N,i], probs=.25)
    predicted_cases_ui2 <- colQuantiles(prediction[,1:N,i], probs=.75)
    
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,i], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,i], probs=.975)
    estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.25)
    estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.75)
    
    rt <- colMeans(out$Rt_adj[,1:N,i])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
    rt_li2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.25)
    rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.75)
    
    
    # delete these 2 lines
    covariates_country <- covariates[which(covariates$Country == country), 2:6] 
    covariates_country_long <- gather(covariates_country, key = "key", 
                                      value = "value")
    covariates_country_long$x <- rep(NULL, length(covariates_country_long$key))
    un_dates <- unique(covariates_country_long$value)
    
    for (k in 1:length(un_dates)){
      idxs <- which(covariates_country_long$value == un_dates[k])
      max_val <- round(max(rt_ui)) + 0.3
      for (j in idxs){
        covariates_country_long$x[j] <- max_val
        max_val <- max_val - 0.3
      }
    }
    
    
    covariates_country_long$value <- as_date(covariates_country_long$value) 
    covariates_country_long$country <- rep(country, 
                                           length(covariates_country_long$value))
    
    data_country <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "country" = rep(country, length(dates[[i]])),
                               "reported_cases" = reported_cases[[i]], 
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "predicted_min2" = predicted_cases_li2,
                               "predicted_max2" = predicted_cases_ui2,
                               "deaths" = deaths_by_country[[i]],
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "death_min2" = estimated_deaths_li2,
                               "death_max2"= estimated_deaths_ui2,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui,
                               "rt_min2" = rt_li2,
                               "rt_max2" = rt_ui2)
    
    colnames_csv <- c("time","country", "reported_cases", "predicted_cases","predicted_min", "predicted_max",
                                      "deaths", "estimated_deaths",
                                      "death_min", "death_max","rt", "rt_min","rt_max")
    data_country_out_temp <- data_country[,colnames_csv]
    colnames(data_country_out_temp) <- c("time","country", "reported_cases", 
                                    "predicted_infections_mean","predicted_infections_lower_CI_95", "predicted_infections_higher_CI_95",
                                    "reported_deaths",
                                    "estimated_deaths_mean", "estimated_deaths_lower_CI_95", "estimated_deaths_higher_CI_95",
                                    "mean_time_varying_reproduction_number_R(t)", "time_varying_reproduction_number_R(t)_lower_CI_95",
                                    "time_varying_reproduction_number_R(t)_higher_CI_95")
    
    all_data <- rbind(all_data, data_country)
    all_data_out <- rbind(all_data_out, data_country_out_temp)
    intervention_data <- rbind(intervention_data, covariates_country_long)
    
    make_plots(data_country = data_country, 
               covariates_country_long = covariates_country_long,
               filename2 = filename2,
               country = country,
               percent_pop = percent_pop)
    
  }
  write.csv(all_data, paste0("results/", "base-plot.csv"))
  write.csv(intervention_data, paste0("results/", "base-intervention.csv"))
  write.csv(all_data_out, paste0("web/data/", "results.csv"))
}

#---------------------------------------------------------------------------
make_plots <- function(data_country, covariates_country_long, 
                       filename2, country, percent_pop){
  
  if (country == 'United_Kingdom')
    country = 'United Kingdom'
  data_cases_95 <- data.frame(data_country$time, data_country$predicted_min, 
                              data_country$predicted_max)
  names(data_cases_95) <- c("time", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))
  data_cases_50 <- data.frame(data_country$time, data_country$predicted_min2, 
                              data_country$predicted_max2)
  names(data_cases_50) <- c("time", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$time))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_ribbon(data = data_cases, 
                aes(x = time, ymin = cases_min, ymax = cases_max, fill = key)) +
    xlab("") +
    ylab("Daily number of infections\n") +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + ggtitle(country) +
    guides(fill=guide_legend(ncol=1))
  
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    ylab("Daily number of deaths\n") + 
    xlab("") +
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  
  plot_labels <- c("Complete lockdown", 
                   "Public events banned",
                   "School closure",
                   "Self isolation",
                   "Social distancing \n encouraged")
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$time, 
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
  data_rt_50 <- data.frame(data_country$time, data_country$rt_min2, 
                           data_country$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  p3 <- ggplot(data_country) +
    geom_stepribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                        group = key,
                                        fill = key)) +
    geom_hline(yintercept = 1, color = 'black', size = 0.1) + 
    geom_segment(data = covariates_country_long,
                 aes(x = value, y = 0, xend = value, yend = max(x)), 
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_country_long, aes(x = value, 
                                                   y = x, 
                                                   group = key, 
                                                   shape = key, 
                                                   col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
    scale_shape_manual(name = "Interventions", labels = plot_labels,
                       values = c(21, 22, 23, 24, 25, 12)) + 
    scale_colour_discrete(name = "Interventions", labels = plot_labels) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"), 
                 limits = c(data_country$time[1], 
                            data_country$time[length(data_country$time)])) + 
    scale_y_continuous(expand = expansion(mult=c(0,0.1))) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  if (country == 'United Kingdom')
    country = 'United_Kingdom'
  # Special plot settings for mobile
  p3_mobile <- p3  +
    theme(legend.position="below")
  
  # Plots for Web, Desktop version
  dir.create("web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/desktop/", country, "_infections", ".svg"), 
            p1, base_height = 4, base_asp = 1.618)
  save_plot(filename = paste0("web/figures/desktop/", country, "_deaths", ".svg"), 
            p2, base_height = 4, base_asp = 1.618)
  save_plot(filename = paste0("web/figures/desktop/", country, "_rt", ".svg"), 
            p3, base_height = 4, base_asp = 1.618 * 2)
  
  # Plots for Web, Mobile version
  dir.create("web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/mobile/", country, "_infections", ".svg"), 
            p1, base_height = 4, base_asp = 1.1)
  save_plot(filename = paste0("web/figures/mobile/", country, "_deaths", ".svg"), 
            p2, base_height = 4, base_asp = 1.1)
  save_plot(filename = paste0("web/figures/mobile/", country, "_rt", ".svg"), 
            p3_mobile, base_height = 4, base_asp = 1.1)
  
  # Special plot settings for mobile
  p3_mobile <- p3  +
    theme(legend.position="below")
  
  # Plots for Web, Desktop version
  dir.create("web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/desktop/", country, "_infections", ".svg"), 
            p1, base_height = 4, base_asp = 1.618)
  save_plot(filename = paste0("web/figures/desktop/", country, "_deaths", ".svg"), 
            p2, base_height = 4, base_asp = 1.618)
  save_plot(filename = paste0("web/figures/desktop/", country, "_rt", ".svg"), 
            p3, base_height = 4, base_asp = 1.618 * 2)
  
  # Plots for Web, Mobile version
  dir.create("web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/mobile/", country, "_infections", ".svg"), 
            p1, base_height = 4, base_asp = 1.1)
  save_plot(filename = paste0("web/figures/mobile/", country, "_deaths", ".svg"), 
            p2, base_height = 4, base_asp = 1.1)
  save_plot(filename = paste0("web/figures/mobile/", country, "_rt", ".svg"), 
            p3_mobile, base_height = 4, base_asp = 1.1)
  
  p <- plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(1, 1, 2))
  save_plot(filename = paste0("figures/", country, "_three_pannel_", filename2, ".png"), 
            p, base_width = 14)
}


make_three_pannel_plot()
