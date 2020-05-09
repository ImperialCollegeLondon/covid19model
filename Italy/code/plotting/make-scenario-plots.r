library(dplyr)
library(lubridate)
source("Subnational_Analysis/code/plotting/format_data_plotting.R")

make_scenario_comparison_plots_mobility <- function(JOBID, StanModel, len_forecast, last_date_data, 
                                                    baseline, mobility_increase = 20){
  print(paste0("Making scenario comparision plots for ", mobility_increase , "%"))
  load(paste0('Subnational_Analysis/results/sim-constant-mob-', StanModel, '-', len_forecast, '-0-', JOBID, '-stanfit.Rdata'))
  out <- rstan::extract(fit)
  
  mob_data <- NULL
  for (i in 1:length(countries)){
    data_state_plot <- format_data(i = i, dates = dates, countries = countries, 
                                   estimated_cases_raw = estimated_cases_raw, 
                                   estimated_deaths_raw = estimated_deaths_raw, 
                                   reported_cases = reported_cases,
                                   reported_deaths = reported_deaths,
                                   out = out, forecast = 0, SIM = TRUE)
    # Cuts data on last_data_date
    data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
    
    subset_data <- select(data_state_plot, country, date, reported_deaths, estimated_deaths, 
                          deaths_min, deaths_max)
    subset_data$key <- rep("Constant mobility", length(subset_data$country))
    mob_data <- rbind(mob_data, subset_data)
  }    
  
  if (baseline == TRUE){
    load(paste0('Subnational_Analysis/results/sim-increase-mob-baseline-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, '-stanfit.Rdata'))
  } else {
    load(paste0('Subnational_Analysis/results/sim-increase-mob-current-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, '-stanfit.Rdata'))
  }
  out <- rstan::extract(fit)
  
  for (i in 1:length(countries)){
    data_state_plot <- format_data(i = i, dates = dates, countries = countries, 
                                   estimated_cases_raw = estimated_cases_raw, 
                                   estimated_deaths_raw = estimated_deaths_raw, 
                                   reported_cases = reported_cases,
                                   reported_deaths = reported_deaths,
                                   out = out, forecast = 0, SIM = TRUE)
    # Cuts data on last_data_date
    data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
    subset_data <- select(data_state_plot, country, date, reported_deaths, estimated_deaths, 
                          deaths_min, deaths_max)
    subset_data$key <- rep("Increased mobility", length(subset_data$country))
    mob_data <- rbind(mob_data, subset_data)
  }    
  
  data_half <- mob_data[which(mob_data$key == "Increased mobility"),]
  mob_data$key <- factor(mob_data$key)
  data_half$key <- factor(data_half$key)
  
  nametrans <- read.csv("Subnational_Analysis/Italy/province_name_translation.csv")
  
  mob_data <- mob_data %>% filter(!(country %in% c("Lombardia","Marche","Veneto","Toscana","Piemonte","Emilia-Romagna","Liguria"))) %>%
                            droplevels()
  
  mob_data$country<-recode(mob_data$country, Lombardia="Lombardy",Piemonte="Piedmont",Toscana="Tuscany",P.A._Bolzano="Bolzano",
                           P.A._Trento="Trento",Puglia="Apulia",Sardegna="Sardinia",Sicilia="Sicily",Valle_dAosta="Aosta")
  
  data_half <- data_half %>% filter(!(country %in% c("Lombardia","Marche","Veneto","Toscana","Piemonte","Emilia-Romagna","Liguria"))) %>%
                              droplevels()

  data_half$country<-recode(data_half$country, Lombardia="Lombardy",Piemonte="Piedmont",Toscana="Tuscany",P.A._Bolzano="Bolzano",
                            P.A._Trento="Trento",Puglia="Apulia",Sardegna="Sardinia",Sicilia="Sicily",Valle_dAosta="Aosta")
    
  last_date_data<-mob_data$date[nrow(mob_data)]
  
  p <- ggplot(mob_data) +
    geom_bar(data = data_half, aes(x = date, y = reported_deaths), stat='identity') +
    geom_ribbon(aes(x = date, ymin = deaths_min, ymax = deaths_max, group = key, fill = key), alpha = 0.5) +
    #geom_line(aes(date,deaths_max),color="black",size=0.2)+
    #geom_line(aes(date,deaths_min),color="black",size=0.2)+
    #geom_line(aes(date,estimated_deaths),color="black",size=0.3)+
    #geom_ribbon(aes(x = date, ymin = deaths_min, ymax = deaths_max, fill = "ICL"), alpha = 0.5) +
    scale_fill_manual(name = "", values = c("pink", "skyblue")) + 
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), limits = c(as.Date("2020-03-02"), last_date_data)) + 
    facet_wrap(~country, scales = "free") + 
    xlab("") + ylab("Daily number of deaths") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14), axis.title = element_text( size = 14 ),axis.text = element_text( size = 14 ),
          legend.position = "right",strip.text = element_text(size = 14),legend.text=element_text(size=14))
  
  ggsave(paste0("Subnational_Analysis/figures/scenarios_increase_baseline-", len_forecast, '-', mobility_increase, '-', JOBID, "top_7.pdf"), p, height = 15, width = 20)
  
  if (baseline == TRUE){
    ggsave(paste0("Subnational_Analysis/figures/scenarios_increase_baseline-", len_forecast, '-', mobility_increase, '-', JOBID, ".pdf"), p, height = 15, width = 20)
  } else {
    ggsave(paste0("Subnational_Analysis/figures/scenarios_increase_current-", len_forecast, '-', mobility_increase, '-', JOBID, ".pdf"), p, height = 15, width = 20)
  }
}
