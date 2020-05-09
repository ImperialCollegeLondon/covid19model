library(dplyr)
library(lubridate)
library(grid)
library(gtable)
source("Italy/code/plotting/format-data-plotting.R")

make_scenario_comparison_plots_mobility <- function(JOBID, StanModel, len_forecast, last_date_data, 
                                                    baseline=FALSE, mobility_increase = 20,top=7){
  print(paste0("Making scenario comparision plots for ", mobility_increase , "%"))
  load(paste0('Italy/results/sim-constant-mob-', StanModel, '-', len_forecast, '-0-', JOBID, '-stanfit.Rdata'))

  countries <- states
  
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
    load(paste0('Italy/results/sim-increase-mob-baseline-', StanModel, '-', len_forecast, '-', mobility_increase, '-', JOBID, 
                '-stanfit.Rdata'))
    out <- rstan::extract(fit)
  } else {
    load(paste0('Italy/results/sim-increase-mob-current-', StanModel, '-', len_forecast, '-', mobility_increase, '-', 
                JOBID, '-stanfit.Rdata'))
    out <- rstan::extract(fit)
  }
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
  
  #nametrans <- read.csv("Subnational_Analysis/Italy/province_name_translation.csv")
  
  # To do top 7:
  if(top==7){
   mob_data <- mob_data %>% filter(country %in% c("Lombardy","Marche","Veneto","Tuscany","Piedmont","Emilia-Romagna","Liguria")) %>%
                              droplevels()

    data_half <- data_half %>% filter(country %in% c("Lombardy","Marche","Veneto","Tuscany","Piedmont","Emilia-Romagna","Liguria")) %>%
        droplevels()
  }
  if(top==8){
  # # To do all others"
   mob_data <- mob_data %>% filter((country %in% c("Abruzzo","Basilicata","Calabria","Campania","Friuli-Venezia_Giulia","Lazio","Molise"))) %>%
                             droplevels()
  data_half <- data_half %>% filter((country %in% c("Abruzzo","Basilicata","Calabria","Campania","Friuli-Venezia_Giulia","Lazio","Molise"))) %>%
      droplevels()
  }
  if(top==9){
    # # To do all others"
    mob_data <- mob_data %>% filter((country %in% c("Bolzano","Trento","Apulia","Sardinia","Sicily","Umbria","Aosta"))) %>%
      droplevels()
    data_half <- data_half %>% filter((country %in%  c("Bolzano","Trento","Apulia","Sardinia","Sicily","Umbria","Aosta"))) %>%
      droplevels()
  }

  last_date_data<-mob_data$date[nrow(mob_data)]
  
  #mob_data$label <- mob_data$key %>% str_replace_all(" ", "_") %>% recode( Constant_Mobility= "Mobility held constant", Increased_Mobility = "Increased mobility: ",mobility_increase,"% return to pre-lockdown level")
  
  levels(mob_data$key)=c("Mobility held constant",paste0("Increased mobility: ",mobility_increase,"% return to pre-lockdown level"))
  
  p <- ggplot(mob_data) +
    geom_bar(data = mob_data, aes(x = date, y = reported_deaths), stat='identity') +
    geom_ribbon(aes(x = date, ymin = deaths_min, ymax = deaths_max, group = key, fill = key), alpha = 0.5) +
    #geom_line(aes(date,deaths_max),color="black",size=0.2)+
    #geom_line(aes(date,deaths_min),color="black",size=0.2)+
    #geom_line(aes(date,estimated_deaths),group = key,size=0.5)+
    geom_line(aes(date,estimated_deaths, group = key, color = key),size = 1)  +scale_colour_manual(values= c("skyblue","red"))+
    #geom_ribbon(aes(x = date, ymin = deaths_min, ymax = deaths_max, fill = "ICL"), alpha = 0.5) +
    scale_fill_manual(name = "", labels = c("Mobility held constant", paste0("Increased mobility: ",mobility_increase,"% return to pre-lockdown level")), values = c("skyblue","red")) + 
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), limits = c(as.Date("2020-03-02"), last_date_data)) + 
    #facet_wrap(~country, scales = "free",nrow=7) + 
    facet_grid(country ~key, scales = "free_y")+
    xlab("") + ylab("Daily number of deaths") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 26), axis.title = element_text( size = 26 ),axis.text = element_text( size = 26),
          legend.position = "none",strip.text = element_text(size = 26),legend.text=element_text(size=26))
  ggsave(paste0("Italy/figures/scenarios_increase_baseline-", len_forecast, '-', mobility_increase, '-', JOBID, "top_",top,".png"), p, height = 30, width = 20)

}
