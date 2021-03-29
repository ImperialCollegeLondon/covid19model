# No need to run this file; it is just provided for sake of giving an idea to people
# what was done in paper in terms of pre-processing
library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(magrittr)
library(ggpubr)
library(tidyr)
library(readxl)
library(tools)
library(ggplot2)
library(flextable)
library(stringr)
library(here)
library(zoo)

# read rt values
rt_all <- 
  readRDS(here('tiers/data/aggregates_infections_rt.rds')) %>%
  filter(type == 'R') %>%
  mutate(Rt_se = ((log(CIup) - log(CIlow))/3.29),
         value = log(value)) %>%
  select(area, value, period_start, Rt_se) %>%
  na.omit() %>%
  rename(Rt = value, ltla = area, date = period_start)

# read npis
npis <- 
  read_csv(here('tiers/data/npis_23Mar_02Dec.csv')) %>%
  select(-c(X1,index, source_record)) %>%
  mutate(date = as.Date(date, format = '%d/%m/%Y')) %>%
  complete(date = seq.Date(as.Date('2020-03-23'), max(rt_all$date), by="day"), nesting(ltla, region, utla)) %>%
  arrange(ltla, date) %>%
  mutate_all(na.locf)
  
# read static combined
static <- 
  read_csv(here('tiers/data/all_statc_combined.csv')) %>%
  rename(ltla = LTLA19NM)
data <- 
  left_join(npis, rt_all) %>%
  left_join(static)
# saveRDS(data,here('rt_covars_combined.rds'))
