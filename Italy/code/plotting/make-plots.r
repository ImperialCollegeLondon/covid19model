library(lubridate)
library(ggplot2)
source("Italy/code/utils/read-data-subnational.r")
source("Italy/code/plotting/format-data-plotting.r")
source("Italy/code/plotting/make-three-panel-plots.r")
source("Italy/code/plotting/make-rt-plot.r")

make_plots_all <- function(filename, SIM=FALSE, label = "", last_date_data, ext = ".png"){
  print("In subnational code")
  load(filename)
  countries <- states
  
  out <- rstan::extract(fit)
  
  rt_data_long <- NULL
  rt_data_wide <- NULL
  
  interventions <- read_interventions()
  
  covariates <- interventions
  covariates$Country  <- as.factor(covariates$Country)
  for (i in 1:length(countries)){
    print(countries[i])

    data_country_plot <- format_data(i = i, dates = dates, countries = countries, 
                                     estimated_cases_raw = estimated_cases_raw, 
                                     estimated_deaths_raw = estimated_deaths_raw, 
                                     reported_cases = reported_cases,
                                     reported_deaths = reported_deaths,
                                     out = out, SIM = SIM)
    # Cuts data on last_data_date
    data_country_plot <- data_country_plot[which(data_country_plot$date <= last_date_data),]
    # Read in covariates

    covariates_long <- gather(covariates[which(covariates$Country == countries[i]), 
                                         2:ncol(covariates)], 
                              key = "key", value = "value")
    covariates_long$x <- rep(NA, length(covariates_long$key))
    un_dates <- unique(covariates_long$value)
    
    for (k in 1:length(un_dates)){
      idxs <- which(covariates_long$value == un_dates[k])
      max_val <- ceiling(max(data_country_plot$rt_max) +0.3)
      for (k in idxs){
        covariates_long$x[k] <- max_val
        max_val <- max_val - 0.3
      }
    }
    
    print(sprintf("Last line of data: %s", data_country_plot$date[length(data_country_plot$rt)]))
    
   
    len <- length(data_country_plot$rt)
    rt_data_state_long <- data.frame("state" = c(as.character(countries[i]), as.character(countries[i])),
                                     "x" = c("start", "end"),
                                     "rt" = c(data_country_plot$rt[1], 
                                              mean(data_country_plot$rt[(len-6):len])),
                                     "rt_min" = c(data_country_plot$rt_min[1],
                                                  mean(data_country_plot$rt_min[(len-6):len])),
                                     "rt_max" = c(data_country_plot$rt_max[1],
                                                  mean(data_country_plot$rt_max[(len-6):len])))
    rt_data_long <- rbind(rt_data_long, rt_data_state_long)
    
    # Make the three panel plot
    make_three_panel_plots(data_country_plot, jobid = JOBID, country = countries[i], 
                          covariates_long = covariates_long, label = label)
  }
  
  print("Making rt plot")
  rt_data_long$x <- factor(rt_data_long$x, levels = c("start", "end"))
  
  region_to_macro=rbind(
                data.frame(country= c("Aosta","Liguria","Lombardy","Piedmont"), macro="NorthWest"),
                data.frame(country=c("Emilia-Romagna","Friuli-Venezia_Giulia","Trento","Bolzano","Veneto"),macro="NorthEast"),
                data.frame(country=c("Lazio","Marche","Tuscany","Umbria"),macro="Centre"),
                data.frame(country=c("Abruzzo","Apulia","Basilicata","Calabria","Campania","Molise"),macro="South"),
                data.frame(country=c("Sardinia","Sicily"),macro="Islands")
  )
  names(region_to_macro) <- c("state", "macro")
  rt_data_long = rt_data_long %>%inner_join(region_to_macro,)
  

  make_rt_point_plot(rt_data_long, JOBID = JOBID, label = label)
  

}