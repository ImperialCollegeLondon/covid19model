library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Read in data
nyt <- data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# Removes some teritories
not_states <- c("Northern Mariana Islands", "Guam", "Virgin Islands", "Puerto Rico")
nyt <- nyt[which(!(nyt$state %in% not_states)),]

# Read in state code look up table - assuming 50 states + DC
codes <- read.csv("usa/data/states.csv", stringsAsFactors = FALSE)
names(codes) <- c("state", "code")
nyt <- left_join(nyt, codes, by = "state")

# Sort dates
nyt$date <- ymd(nyt$date)

# Work out cumulative cases by state
data <- nyt %>%
  group_by(code, date) %>%
  summarise(
    cumulative_cases = sum(cases),
    cumulative_deaths = sum(deaths)
  )

data_daily <- NULL
unique_codes <- unique(data$code)

less10deaths <- c()
less50deaths <- c()
# Working out daily deaths and cases
for (i in 1:length(unique_codes)){
  subset <- data[which(data$code == unique_codes[i]),]
  # Sort dates into ordere
  subset <- subset[order(subset$date),]
  # Fill in daily deaths
  daily_cases <- vector(length = length(subset$date))
  daily_deaths <- vector(length = length(subset$date))
  for (j in 1:length(subset$date)){
    if (j == 1){
      daily_cases[j] <- subset$cumulative_cases[j]
      daily_deaths[j] <- subset$cumulative_deaths[j]
    } else {
      daily_cases[j] <- subset$cumulative_cases[j] - subset$cumulative_cases[j-1]
      daily_deaths[j] <- subset$cumulative_deaths[j] - subset$cumulative_deaths[j-1]
      
      if (daily_cases[j] < 0){
        daily_cases[j] = 0
        print(sprintf("%s has a decrease in cumulative deaths on %s", unique_codes[i], subset$date[j]))
      }
      if (daily_deaths[j] < 0){
        daily_deaths[j] = 0
        print(sprintf("%s has a decrease in cumulative deaths on %s", unique_codes[i], subset$date[j]))
      }
    }
  }
  subset$daily_cases <- daily_cases
  subset$daily_deaths <- daily_deaths
  data_daily <- rbind(data_daily, subset)
  
  if (max(subset$cumulative_deaths) < 10){
    less10deaths <- c(less10deaths, unique_codes[i])
  }
  if (max(subset$cumulative_deaths) < 30){
    less50deaths <- c(less50deaths, unique_codes[i])
  }
}

print(sprintf("Last date in data is: %s", max(data_daily$date)))
print(sprintf("First date in data is: %s", min(data_daily$date)))
print("Countries with less than 10 cumulative deaths")
print(less10deaths)
print("Countries with less than 50 cumulative deaths")
print(less50deaths)


#-----------------------------------------------------------------------------
# Plotting code
p <- ggplot(data_daily) +
  geom_col(aes(x = date, y = cumulative_cases, group = code, fill = code)) +
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) + 
  facet_wrap(~code) +
  theme_bw() + 
  ylab("Cumulative cases") +
  xlab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "None") 
ggsave("figures/state_cases.png", p, height = 10, width = 15)

p1 <- ggplot(data_daily) +
  geom_col(aes(x = date, y = daily_cases, group = code, fill = code)) +
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) + 
  facet_wrap(~code) +
  theme_bw() + 
  ylab("Daily number of cases") +
  xlab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "None") 
ggsave("usa/figures/state_cases_daily.png", p1, height = 10, width = 15)

p2 <- ggplot(data_daily) +
  geom_col(aes(x = date, y = cumulative_deaths, group = code, fill = code)) +
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) + 
  facet_wrap(~code) +
  ylab("Cumulative deaths") +
  xlab("") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "None")
ggsave("usa/figures/state_deaths.png", p2, height = 10, width = 15)
print(p2)

p3 <- ggplot(data_daily) +
  geom_col(aes(x = date, y = daily_deaths, group = code, fill = code)) +
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) + 
  facet_wrap(~code) +
  ylab("Daily number of deaths") +
  xlab("") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "None")
ggsave("usa/figures/state_deaths_daily.png", p3, height = 10, width = 15)
print(p3)


#-----------------------------------------------------------------------------
# Padding with zeros

date_min <- ymd("2020-01-01")

all_data <- NULL

for (i in 1:length(unique_codes)){
  # Subset per state
  state_subset <- data_daily[which(data_daily$code == unique_codes[i]),]
  # Order by date
  state_subset <- state_subset[order(state_subset$date),]
  # Pad previous dates with zeros
  pad <- state_subset$date[1] - date_min
  pad_dates <- date_min + days(1:pad[[1]]-1)
  padded_data <- data.frame("code" = rep(unique_codes[i], pad),
                        "date" = pad_dates,
                        "cumulative_cases" = as.integer(rep(0, pad)),
                        "cumulative_deaths" = as.integer(rep(0, pad)), 
                        "daily_cases" = as.integer(rep(0, pad)),
                        "daily_deaths" = as.integer(rep(0, pad)))
  
  state_data <- bind_rows(padded_data, state_subset)
  
  all_data <- bind_rows(all_data, state_data)
}

# Save RDS
saveRDS(all_data, file = "usa/data/nyt_death_data_padded.rds")

