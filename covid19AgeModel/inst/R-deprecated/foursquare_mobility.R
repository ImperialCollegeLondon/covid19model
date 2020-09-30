library(lubridate)
library(tidyverse)
source("usa/code/utils/read-data-usa-2.r")

foursquare_data <- read_visitdata_mobility()
ihme_data <- read_death_data("ihme")

nam <- names(foursquare_data)
# Format long
foursquare_data_long <- foursquare_data %>%
  rowwise() %>%
  mutate("average" = mean(shops.services, grocery, outdoors.recreation, professional, medical)) %>%
  select(-shops.services, -grocery, -outdoors.recreation, -professional, -medical) %>%
  gather("key" = key, "value" = value, -date, -sub_region_1, -code)

# Unique states
unique_states <- unique(foursquare_data$code)

# Read in covaraite dates 
covariates <- readRDS("usa/data/covariates.RDS")
covariates <- select(covariates, StatePostal, EmergDec, StayAtHome)

# Sort out covariates long
covariates_all <- NULL
dates_df <- NULL
for (state in unique_states){
  subset_state <- foursquare_data_long[which(foursquare_data_long$code == state),]
  # Subsets the covariates on state
  covariates_long <- gather(covariates[which(covariates$StatePostal == state), 
                                       2:ncol(covariates)], 
                            key = "key", value = "value")
  covariates_long$code <- rep(state, length(covariates_long$key)) 
  
  covariates_long$x <- rep(NA, length(covariates_long$key))
  covariates_long$sub_region_1 <- rep(subset_state$sub_region_1[1], length(covariates_long$key))
  un_dates <- unique(covariates_long$value)
  
  for (k in 1:length(un_dates)){
    idxs <- which(covariates_long$value == un_dates[k])
    max_val <- 1
    min_val <- floor(min(subset_state$value))
    for (k in idxs){
      covariates_long$x[k] <- max_val
      max_val <- max_val - 0.3
    }
  }
  
  death_state <- ihme_data[which(ihme_data$code == state),]
  dates_df <- rbind(dates_df, 
                    data.frame("sub_region_1" = rep(subset_state$sub_region_1[1], 1),
                               "number" = c("one"),  #, "onehundred", "twohundred"),
                               "dates"= c(death_state$date[which(death_state$cumulative_deaths >=1)[1]])))#,
                                          #death_state$date[which(death_state$cumulative_deaths >=100)[1]],
                                          #death_state$date[which(death_state$cumulative_deaths >=200)[1]])))
                         
  
  covariates_all <- rbind(covariates_all, covariates_long)
}


g <- ggplot(foursquare_data_long) +
  geom_line(aes(x = date, y = -value, group = key, col = key)) + 
  geom_segment(data = covariates_all,
               aes(x = value, y = min_val, xend = value, yend = max(x, na.rm=TRUE)), 
               linetype = "dashed", colour = "grey", alpha = 0.75) +
  geom_vline(data = dates_df, aes(xintercept = dates, group = number)) + 
  geom_point(data = covariates_all, aes(x = value, 
                                        y = x, 
                                        group = key, 
                                        shape = key), size = 2) +
  labs(colour="Mobility", shape="Interventions") +
  facet_wrap(~sub_region_1, ncol = 5) + 
  xlab("") + ylab("Change in mobility")  +  #ylim(-100, 175) +
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
               limits = c(as.Date("2020-02-25"), as.Date("2020-04-26"))) + 
  scale_shape_discrete(name = "Intervention", 
                       labels = c("Emergency decree", "Stay at home order")) +
  scale_colour_discrete(name = "Mobility", 
                       labels = c("Average", "Fast food restaurant", "Gas stations")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(col = guide_legend(title.position="top", title.hjust = 0.5),
         shape = guide_legend(title.position="top", title.hjust = 0.5))
print(g)
ggsave("usa/figures/foursquare_mobility.png", g, height = 15, width = 12)
ggsave("usa/figures/foursquare_mobility.svg", g, height = 15, width = 12)


  
 
