library(ggplot2)
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

source("utils/geom-stepribbon.r")
#---------------------------------------------------------------------------
make_forecast_plot <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]
  
  load(paste0("results/", filename))
  all_data <- data.frame()
  all_data_forecast <- data.frame()
  for(i in 1:length(countries)){
    N <- length(dates[[i]])
    N2 <- N + 7
    country <- countries[[i]]
    
    predicted_cases <- colMeans(prediction[,1:N,i])
    predicted_cases_li <- colQuantiles(prediction[,1:N,i], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,i], probs=.975)
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,i], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,i], probs=.975)
    
    estimated_deaths_forecast <- colMeans(estimated.deaths[,1:N2,i])[N:N2]
    estimated_deaths_li_forecast <- colQuantiles(estimated.deaths[,1:N2,i], probs=.025)[N:N2]
    estimated_deaths_ui_forecast <- colQuantiles(estimated.deaths[,1:N2,i], probs=.975)[N:N2]
    
    rt <- colMeans(out$Rt_adj[,1:N,i])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
    
    data_country <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "country" = rep(country, length(dates[[i]])),
                               #"country_population" = rep(country_population, length(dates[[i]])),
                               "reported_cases" = reported_cases[[i]], 
                               "reported_cases_c" = cumsum(reported_cases[[i]]), 
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "deaths" = deaths_by_country[[i]],
                               "deaths_c" = cumsum(deaths_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui)
    
    times <- as_date(as.character(dates[[i]]))
    times_forecast <- times[length(times)] + 0:7
    data_country_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, 8),
                                        "estimated_deaths_forecast" = estimated_deaths_forecast,
                                        "death_min_forecast" = estimated_deaths_li_forecast,
                                        "death_max_forecast"= estimated_deaths_ui_forecast)
    all_data <- rbind(all_data, data_country)
    all_data_forecast <- rbind(all_data_forecast, data_country_forecast)
    make_single_plot(data_country = data_country, 
                     data_country_forecast = data_country_forecast,
                     filename = filename,
                     country = country)
  }
  write.csv(all_data, paste0("results/", "base-forecast-plot.csv"))
  write.csv(all_data_forecast, paste0("results/", "forecast-plot.csv"))
}

make_single_plot <- function(data_country, data_country_forecast, filename, country){
  
  data_deaths <- data_country %>%
    select(time, deaths, estimated_deaths) %>%
    gather("key" = key, "value" = value, -time)
  
  data_deaths_forecast <- data_country_forecast %>%
    select(time, estimated_deaths_forecast) %>%
    gather("key" = key, "value" = value, -time)
  
  # Force less than 1 case to zero
  data_deaths$value[data_deaths$value < 1] <- NA
  data_deaths_forecast$value[data_deaths_forecast$value < 1] <- NA
  data_deaths_all <- rbind(data_deaths, data_deaths_forecast)
  
  p <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_line(data = data_country, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_country_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_country, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_country_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab("Date") +
    ylab("Daily number of deaths\n") + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    scale_y_continuous(trans='log10', labels=comma) + 
    coord_cartesian(ylim = c(1, 100000), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  print(p)
  
  ggsave(file= paste0("figures/", country, "_forecast_", filename, ".png"), 
         p, width = 10)
  
  # Produce plots for Website
  dir.create("web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/desktop/", country, "_forecast", ".svg"), 
            p, base_height = 4, base_asp = 1.618 * 2 * 8/12)
  dir.create("web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("web/figures/mobile/", country, "_forecast", ".svg"), 
            p, base_height = 4, base_asp = 1.1)
}
#-----------------------------------------------------------------------------------------------
make_forecast_plot()

