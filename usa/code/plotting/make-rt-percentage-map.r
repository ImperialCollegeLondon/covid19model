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

make_rt_map <- function(JOBID,  StanModel, last_date_data,
                        ext = ".png"){
  
  load(paste0('usa/results/', StanModel, '-', JOBID, '-stanfit.Rdata'))
  out <- rstan::extract(fit)
  
  percentages <- vector(length = length(states))
  percentage_group <- vector(length = length(states))
  for (i in 1:length(states)){
    dates_states <- dates[[i]]
    idx <- which(dates_states == last_date_data)
    rts <- rowMeans(out$Rt_adj[,(idx-6):idx,i])
    
    percentages[i] <- mean(rts < 1)
    
    if (percentages[i] < 0.20){
      percentage_group[i] <- 1
    } else if (percentages[i] < 0.40){
      percentage_group[i] <- 2
    } else if (percentages[i] < 0.60){
      percentage_group[i] <- 3
    } else if (percentages[i] < 0.80){
      percentage_group[i] <- 4
    } else {
      percentage_group[i] <- 5
    }
  }   
  
  data <- data.frame("code" = states,
                     "less" = percentages,
                     "more" = 1-percentages)
  data_long <- gather(data, "key" = key, "value" = value, -code)
  
  # Load in shape file 
  shp0 <-
    readOGR("usa/data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
            encoding = "UTF-8",
            use_iconv = TRUE,
            stringsAsFactors = FALSE
    )
  shp0 <- spTransform(shp0, CRS=CRS("+init=epsg:26978"))
  
  alaska <- shp0[shp0$STATEFP=="02",]
  alaska <- elide(alaska, rotate=-39)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 4)
  alaska <- elide(alaska, shift=c(-2400000, -1000000))
  proj4string(alaska) <- proj4string(shp0)
  
  hawaii <- shp0[shp0$STATEFP=="15",]
  hawaii <- elide(hawaii, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5200000, -1400000))
  proj4string(hawaii) <- proj4string(shp0)
  
  shp0 <- shp0[!shp0$STATEFP %in% c("02", "15"),] # Remove old AK and HI
  shp0 <- rbind(shp0, alaska, hawaii) # Add in shifted AK and HI
  
  shp0_broom <- tidy(shp0, region = "NAME")
  df <- data.frame(shp0_broom)
  
  names(df) <- c("long",  "lat",   "order", "hole",  "piece", "group", "state_name")
  
  nam <- read.csv("usa/data/states.csv")
  names(nam) <- c("state_name", "code")
  df <- left_join(df, nam, by = "state_name")
  
  rt_percentage <- data.frame("code" = states,
                              "percentage" = percentages,
                              "percentage_group" = factor(percentage_group))
  df <- left_join(df, rt_percentage, by = "code")
  
  df = df[!df$state_name %in% c("Puerto Rico"),]
  p1 <- ggplot(data = df) + 
    geom_polygon(data = df, 
                 aes(x = long, y = lat, group = group, fill = percentage_group),  
                 colour = "black") +
    #scale_fill_continuous_diverging(name = expression("Probability R"[t]<1),
    #                                palette = "Red-Green", mid = 0.5, na.value = "grey",
    #                                labels = scales::percent_format(accuracy = 1),limits=c(0,1)) +
    scale_discrete_manual("fill", name = expression("Probability R"[t]<1), 
                          values = c("#7b3294", "#c2a5cf", "#f7f7f7", "#a6dba0", "#008837"),
                          labels = c("x < 20%", expression(paste("20%"<=" x < 40%")), 
                                     expression(paste("40%"<=" x < 60%")), expression(paste("60%"<=" x < 80%")),
                                     expression(paste("x">="80%")))) + 
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.line = element_blank())
  print(p1)
  ggsave(paste0("usa/figures/", JOBID, "_rt_map_chloropleth", ext), p1, width = 12)
  
  data <- data[which(data$code != "DC"),]
  more_fifty_percent <- length(which(data$less > 0.5))
  more_ninty_five_percent <- length(which(data$less > 0.95))
  saveRDS(c(sprintf("%s", more_fifty_percent), sprintf("%s", more_ninty_five_percent)), 
          paste0("usa/results/", JOBID, "-chloropleth-percentage.RDS"), version = 2)
}


