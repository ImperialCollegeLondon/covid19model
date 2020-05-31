library(ggplot2)
library(geofacet)
library(broom)
library(maptools)
library(rgeos)
library(rgdal)
library(colorspace)
library(sf)
library(dplyr)
library(tidyr)
library(matrixStats)

source("usa/code/utils/read-data-usa.r")

plot_infectiousness_regions <- function(JOBID, StanModel, ext = ".pdf", last_date_data, individual = TRUE){
  data <- readRDS(paste0("usa/results/infectiousness_all_", JOBID, ".RDS"))
  data_set_raw <- do.call(rbind, data)
  
  df <- read_death_data("jhu")
  df <- select(df, code, region_census_sub_revised, date)
  
  data_set <- left_join(data_set_raw, df, by=c("state" = "code", "date")) %>%
    group_by(date, region_census_sub_revised) %>%
    summarise(infectioussness = sum(infectioussness),
              infectioussness_li = sum(infectioussness_li), 
              infectioussness_ui = sum(infectioussness_ui),
              infectioussness_li2 = sum(infectioussness_li2),
              infectioussness_ui2 = sum(infectioussness_ui2))
  
  data_95 <- data.frame(data_set$date, data_set$region_census_sub_revised, data_set$infectioussness_li, 
                        data_set$infectioussness_ui)
  names(data_95) <- c("date", "region", "i_min", "i_max")
  data_95$key <- rep("nintyfive", length(data_95$date))
  data_50 <- data.frame(data_set$date, data_set$region_census_sub_revised, data_set$infectioussness_li2, 
                        data_set$infectioussness_ui2)
  names(data_50) <- c("date", "region", "i_min", "i_max")
  data_50$key <- rep("fifty", length(data_50$date))
  data <- rbind(data_95, data_50)
  #levels(data$key) <- c("ninetyfive", "fifty")
  
  load(paste0('usa/results/', StanModel, '-', JOBID, '-stanfit.Rdata'))
  out <- rstan::extract(fit)
  infections <- out$prediction
  
  inf_data <- NULL
  for (i in 1:length(states)){
    N <- length(dates[[i]])
    
    inf_states <- infections[,,i]
    means <- colMeans(inf_states[,1:N])
    li <- colQuantiles(inf_states[,1:N], probs = 0.025)
    ui <- colQuantiles(inf_states[,1:N], probs = 0.975)
    li2 <- colQuantiles(inf_states[,1:N], probs = 0.25)
    ui2 <- colQuantiles(inf_states[,1:N], probs = 0.75)
    
    inf_data_tmp <- data.frame("state" = rep(states[i], length(dates[[i]])),
                           "date" = dates[[i]],
                           "means" = means,
                           "li" = li,
                           "ui" = ui,
                           "li2" = li2,
                           "ui2" = ui2)
    inf_data <- rbind(inf_data, inf_data_tmp)
  }

  data_set_inf <- left_join(inf_data, df, by=c("state" = "code", "date")) %>%
    group_by(date, region_census_sub_revised) %>%
    summarise(infections = sum(means),
              infections_li = sum(li),
              infections_ui = sum(ui),
              infections_li2 = sum(li2),
              infections_ui2 = sum(ui2))


  data_95_inf <- data.frame(data_set_inf$date, data_set_inf$region_census_sub_revised, data_set_inf$infections_li,
                            data_set_inf$infections_ui)
  names(data_95_inf) <- c("date", "region", "i_min", "i_max")
  data_95_inf$key <- rep("nintyfive_inf", length(data_95_inf$date))
  data_50_inf <- data.frame(data_set_inf$date, data_set_inf$region_census_sub_revised, data_set_inf$infections_li2,
                            data_set_inf$infections_ui2)
  names(data_50_inf) <- c("date", "region", "i_min", "i_max")
  data_50_inf$key <- rep("fifty_inf", length(data_50_inf$date))
  data_inf <- rbind(data_95_inf, data_50_inf)
  #levels(data_inf$key) <- c("ninetyfive_inf", "fifty_inf")

  data_all <- rbind(data, data_inf)
  data_all$key <- factor(data_all$key, levels = c("nintyfive_inf",  "fifty_inf", "nintyfive", "fifty"))
  
  data_all$region_fac = factor(data_all$region, levels = c('Pacific', 'Great Plains', 'Northeast Corridor', 'Mountain','Great Lakes','Southern Appalachia', ' ', 'TOLA','South Atlantic'), ordered = T)
  
  p <- ggplot(data_all) +
    #geom_line(aes(x = date, y = infectioussness, group = state)) + 
    geom_ribbon(aes(x = date, ymin = i_min, ymax = i_max, group = key, fill = key)) +
    ylab("\nNumber of people") + xlab("") + 
    facet_wrap(~region_fac,  scales = "free", drop = F) + 
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                 limits = c(as.Date("2020-02-5"), last_date_data)) + 
    scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) +
    scale_fill_manual(name = "", labels = c("# new infections [95% CI]", "# new infections [50% CI]", "# current infections [95% CI]",
                                            "# current infections [50% CI]"),
                      values = c(alpha("deepskyblue4", 0.45), 
                                 alpha("deepskyblue4", 0.55),
                                 alpha("darkorchid", 0.45), 
                                 alpha("darkorchid", 0.55))) + 
                        
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "bottom", 
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          strip.background = element_rect(colour="black", fill="white"))
  
  g = ggplotGrob(p)
  empty = (g$layout$name %in% c("panel-3-1", "strip-t-1-3", 'axis-t-1-3', 'axis-b-1-3'))
  
  ## remove empty panels
  g$grobs[which(empty)] = NULL
  ## remove them from the layout
  g$layout = g$layout[!empty,]

  g = arrangeGrob(g)
  
  ggsave(paste0("usa/figures/", JOBID, "_infectiousness_regions", ext), g, width = 10, height = 10)
  print(paste0("saving usa/figures/", JOBID, "_infectiousness_regions", ext))
  
  regions <- read.csv("usa/data/usa-regions.csv", stringsAsFactors = FALSE)
  
  
  data_usa <- data_set %>%
    group_by(date) %>%
    summarise("total_infected_p" = sum(infectioussness)/sum(regions$pop_count)*100,
              "li_p" = sum(infectioussness_li)/sum(regions$pop_count)*100,
              "ui_p" = sum(infectioussness_ui)/sum(regions$pop_count)*100, 
              "total_infected" = sum(infectioussness),
              "li" = sum(infectioussness_li),
              "ui" = sum(infectioussness_ui))

  # format(round(data_today$infectioussness, digits = -3), big.mark=",", trim=TRUE)
  
  if (individual == TRUE){
    data_set_states <- inf_data
    data_95 <- data.frame(data_set_states$date, data_set_states$state, 
                          data_set_states$li, data_set_states$ui)
    names(data_95) <- c("date", "state", "i_min", "i_max")
    data_95$key <- rep("nintyfive_inf", length(data_95$date))
    data_50 <- data.frame(data_set_states$date, data_set_states$state, 
                          data_set_states$li2, data_set_states$ui2)
    names(data_50) <- c("date", "state", "i_min", "i_max")
    data_50$key <- rep("fifty_inf", length(data_50$date))
    data <- rbind(data_95, data_50)

    data_95_inf <- data.frame(data_set_raw$date, data_set_raw$state, data_set_raw$infectioussness_li, 
                          data_set_raw$infectioussness_ui)
    names(data_95_inf) <- c("date", "state", "i_min", "i_max")
    data_95_inf$key <- rep("nintyfive", length(data_95_inf$date))
    data_50_inf <- data.frame(data_set_raw$date, data_set_raw$state, data_set_raw$infectioussness_li2, 
                          data_set_raw$infectioussness_ui2)
    names(data_50_inf) <- c("date", "state", "i_min", "i_max")
    data_50_inf$key <- rep("fifty", length(data_50_inf$date))
    data_inf <- rbind(data_95_inf, data_50_inf)

    data_all <- rbind(data, data_inf)
    data_all$key <- factor(data_all$key, levels = c("nintyfive_inf",  "fifty_inf", "nintyfive", "fifty"))
    
    for (i in 1:length(states)){
      data_subset = data_all[which(data_all$state == states[i]),]
      p <- ggplot(data_subset) +
        #geom_line(aes(x = date, y = infectioussness, group = state)) + 
        geom_ribbon(aes(x = date, ymin = i_min, ymax = i_max, group = key, fill = key)) +
        ylab("\nNumber of people") + xlab("") + 
        scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                     limits = c(as.Date("2020-02-5"), last_date_data)) + 
        scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) +
        scale_fill_manual(name = "", labels = c("# new infections [95% CI]", "# new infections [50% CI]", "# current infections [95% CI]",
                                                "# current infections [50% CI]"),
                          values = c(alpha("deepskyblue4", 0.45), 
                                     alpha("deepskyblue4", 0.55),
                                     alpha("darkorchid", 0.45), 
                                     alpha("darkorchid", 0.55))) + 
        
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              #legend.position = "None", 
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              strip.background = element_rect(colour="black", fill="white"))
      ggsave(paste0("usa/figures/",states[[i]], "_", JOBID, "_infectiousness", ext), p, height = 10, width = 14)
      
    }
  }
}

