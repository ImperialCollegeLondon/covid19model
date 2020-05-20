library(tidyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(magrittr)

read_obs_data <- function(){
  # Read the deaths and cases data
  d <- read.csv("Italy/data/dpc-covid19-ita-regioni.csv")

  # Original column names
  # [1] "data"                       "stato"                      "codice_regione"             "denominazione_regione"      "lat"                       
  # [6] "long"                       "ricoverati_con_sintomi"     "terapia_intensiva"          "totale_ospedalizzati"       "isolamento_domiciliare"    
  # [11] "totale_positivi"            "variazione_totale_positivi" "nuovi_positivi"             "dimessi_guariti"            "deceduti"                  
  # [16] "totale_casi"                "tamponi"                    "note_it"                    "note_en"  
  
  d <- d %>% select(data,denominazione_regione,totale_casi, deceduti)
  colnames(d)<-c("DateRep","country","Cases","Deaths")
  
  d$DateRep<-as.Date(d$DateRep)
  d$country<-str_replace_all(d$country, " ", "_")
  d$country<-as.factor(d$country)
  
  #d$Cases<-as.numeric(d$Cases)
  #d$Deaths<-as.numeric(d$Deaths)
  #df<-d %>% group_by(DateRep) %>%  summarise(Cases=sum(Cases),Deaths = sum(Deaths))
  #write.csv(df,"Italy_18-04.csv")
  
  # Getting daily data from the cummulative data set
  d=  d %>% group_by(country) %>% 
            arrange(DateRep) %>% 
            mutate(Deaths = Deaths - lag(Deaths,default=0)) %>% 
            mutate(Cases = Cases - lag(Cases,default=0)) %>% 
            ungroup()
  
  # padding before
  regions<-levels(d$country)
  days<-length(seq(as.Date('2019/12/31'),as.Date(d$DateRep[1]-1),"days"))
  
  zeroes<-data.frame(DateRep=rep(seq(as.Date('2019/12/31'),as.Date(d$DateRep[1]-1),"days"),length(regions)),
                        country=regions[rep(seq_len(length(regions)), each = days)],
                        Cases=0,Deaths=0)
   
  d=bind_rows(zeroes,d)
  d$Cases[d$Cases<0] <- 0
  d$Deaths[d$Deaths<0] <- 0
  
  # Changing region names
  nametrans <- read.csv("Italy/data/province_name_translation.csv")
  colnames(nametrans)[which(colnames(nametrans)=="denominazione_regione")]<-"country"
  nametrans$country<-as.factor(nametrans$country)
  nametrans$country<-str_replace_all(nametrans$country, " ", "_")
  d_Italy<-data.frame(country="Italy",google_county="Italy",county="Italy")
  nametrans <- bind_rows(nametrans,d_Italy)
  d <- inner_join(d,nametrans,by.x="country",by.y="country") # fix names of regions
  d <- d[,-which(colnames(d) %in% c("country","google_county"))]
  colnames(d)[which(colnames(d)=="county")] <- "country"
  d$country<-str_replace_all(d$country, " ", "_")
  
  d <- d %>% select("country","DateRep","Cases","Deaths")

  return(d)
}

read_ifr_data <- function(regions){
  ifr.Italy.regional <- read.csv("Italy/data/weighted_ifrs_italy.csv")
  colnames(ifr.Italy.regional)[which(colnames(ifr.Italy.regional)=="state")]<-"country"
  colnames(ifr.Italy.regional)[which(colnames(ifr.Italy.regional)=="IFR")]<-"ifr"
  ifr.Italy.regional$country <- str_replace_all(ifr.Italy.regional$country, " ", "_")
  ifr.Italy.regional$country[which( ifr.Italy.regional$country=="Friuli_Venezia-Giulia")]<-"Friuli_Venezia_Giulia"
  ifr.Italy.regional$country[which( ifr.Italy.regional$country=="Provincia_Autonoma_Bolzano")]<-"P.A._Bolzano"
  ifr.Italy.regional$country[which( ifr.Italy.regional$country=="Provincia_Autonoma_Trento")]<-"P.A._Trento"
  ifr.Italy.regional$country[which( ifr.Italy.regional$country=="Valle_D'Aosta")]<-"Valle_d'Aosta"
  colnames(ifr.Italy.regional)[which(colnames(ifr.Italy.regional)=="total_pop")] <- "popt" 
  
  # ## get IFR and population
  ifr.by.country = read.csv("data/popt_ifr.csv")
  ifr.by.country$country = as.character(ifr.by.country[,2])
  
  # Changing region names
  nametrans <- read.csv("Italy/data/province_name_translation.csv")
  colnames(nametrans)[which(colnames(nametrans)=="denominazione_regione")]<-"country"
  nametrans$country<-as.factor(nametrans$country)
  nametrans$country<-str_replace_all(nametrans$country, " ", "_")
  ifr.Italy.regional <- inner_join(ifr.Italy.regional,nametrans,by.x="country",by.y="country") # fix names of regions
  ifr.Italy.regional <- ifr.Italy.regional[,-which(colnames(ifr.Italy.regional) %in% c("country","google_county"))]
  colnames(ifr.Italy.regional)[which(colnames(ifr.Italy.regional)=="county")] <- "country"
  ifr.Italy.regional$country<-str_replace_all(ifr.Italy.regional$country, " ", "_")
  
  ifr.Italy.regional <- ifr.Italy.regional %>% select("country","X","ifr","popt")
  ifr.national<-ifr.by.country[which(ifr.by.country$country=="Italy"),]$ifr
  ifr.Italy.regional <- bind_rows(ifr.Italy.regional,data.frame(country="Italy",ifr=ifr.national,popt=60359546)) 
  ifr.Italy.regional$X[which(ifr.Italy.regional$country=="Italy")]<-23
  
  return(ifr.Italy.regional)
}

read_google_mobility <- function(Country){
  google_mobility <- read.csv('data/Global_Mobility_Report.csv', stringsAsFactors = FALSE)
  google_mobility$date = as.Date(google_mobility$date, format = '%Y-%m-%d')
  google_mobility[, c(6,7,8,9,10,11)] <- google_mobility[, c(6,7,8,9,10,11)]/100
  google_mobility[, c(6,7,8,9,10)] <- google_mobility[, c(6,7,8,9,10)] * -1
  google_mobility<-google_mobility[,c(2,3,5,6,7,8,9,10,11)]
  colnames(google_mobility)[which(colnames(google_mobility)=="country_region")]<-"state"
  colnames(google_mobility)[which(colnames(google_mobility)=="sub_region_1")]<-"country"
  colnames(google_mobility)[which(colnames(google_mobility)=="grocery_and_pharmacy_percent_change_from_baseline")]<-"grocery.pharmacy"
  colnames(google_mobility)[which(colnames(google_mobility)=="parks_percent_change_from_baseline")]<-"parks"
  colnames(google_mobility)[which(colnames(google_mobility)=="transit_stations_percent_change_from_baseline")]<-"transitstations"
  colnames(google_mobility)[which(colnames(google_mobility)=="workplaces_percent_change_from_baseline")]<-"workplace"
  colnames(google_mobility)[which(colnames(google_mobility)=="residential_percent_change_from_baseline")]<-"residential"
  colnames(google_mobility)[which(colnames(google_mobility)=="retail_and_recreation_percent_change_from_baseline")]<-"retail.recreation"
  google_mobility$country[which(google_mobility$country =="")]<-"Italy"
  
  mobility <- google_mobility
  nametrans <- read.csv("Italy/data/province_name_translation.csv")
  Italy<-data.frame(denominazione_regione="Italy",google_county="Italy",county="Italy")
  nametrans<-bind_rows(nametrans,Italy)
  mobility$country<-as.factor(mobility$country)
  nametrans$google_county<-as.factor(nametrans$google_county)
  #mobility <- mobility %>% filter(country !="")
  colnames(nametrans)[which(colnames(nametrans)=="google_county")]<-"country"
  mobility <- inner_join(mobility,nametrans,by.x="country",by.y="country") # fix names of regions
  mobility$country<-str_replace_all(mobility$denominazione_regione, " ", "_")
  mobility <- mobility %>% select(country,date,grocery.pharmacy,parks,residential,retail.recreation,transitstations,workplace)
  
  # Changing region names
  nametrans <- read.csv("Italy/data/province_name_translation.csv")
  colnames(nametrans)[which(colnames(nametrans)=="denominazione_regione")]<-"country"
  nametrans$country<-as.factor(nametrans$country)
  nametrans$country<-str_replace_all(nametrans$country, " ", "_")
  mobility <- mobility %>% filter(country !="Italy")
  mobility <- inner_join(mobility,nametrans,by.x="country",by.y="country") # fix names of regions
  mobility <- mobility[,-which(colnames(mobility) %in% c("country","google_county"))]
  colnames(mobility)[which(colnames(mobility)=="county")] <- "country"
  mobility$country<-str_replace_all(mobility$country, " ", "_")
  mobility <- mobility %>% select("country","date","grocery.pharmacy","parks","residential","retail.recreation","transitstations","workplace") 
  return(mobility)
}

read_interventions <- function(){
  covariates<-read.csv("Italy/data/Italy_events.csv")
  covariates=covariates[c(1:105),c(1:5)]
  
  covariates=covariates %>% select(Regions,Intervention,Effective_date) %>% 
    pivot_wider(names_from=Intervention,values_from=Effective_date)
  colnames(covariates)=c("country","School_closures","Case_based_measures","Social_distancing","Public_events","Lockdown")
  
  covariates$School_closures=dmy(covariates$School_closures)
  covariates$Case_based_measures=dmy(covariates$Case_based_measures)
  covariates$Social_distancing=dmy(covariates$Social_distancing)
  covariates$Lockdown=dmy(covariates$Lockdown)
  covariates$Public_events=dmy(covariates$Public_events)
  colnames(covariates)=c( "Country","schools_universities","self_isolating_if_ill","social_distancing_encouraged","public_events","lockdown" )
  covariates=as.data.frame(covariates)
  #covariates$country <- factor(covariates$country)
  covariates$Country <- factor(covariates$Country)
  write.csv(covariates,"Italy/data/Italy_interventions.csv")
  
  # add all of Italy
  covariates_countries <- read_csv("data/interventions.csv")
  covariates_countries <- covariates_countries[which(covariates_countries$Country=="Italy"), c('Country', 'Type', 'Date effective')]
  
  covariates_countries <- spread(covariates_countries, Type, 'Date effective')
  colnames(covariates_countries)=c("Country","schools_universities","public_events","lockdown","social_distancing_encouraged","self_isolating_if_ill")
  
  covariates_countries$schools_universities<-as.Date(covariates_countries$schools_universities,format="%Y-%m-%d")
  covariates_countries$public_events<-as.Date(covariates_countries$public_events,format="%Y-%m-%d")
  covariates_countries$lockdown<-as.Date(covariates_countries$lockdown,format="%Y-%m-%d")
  covariates_countries$social_distancing_encouraged<-as.Date(covariates_countries$social_distancing_encouraged,format="%Y-%m-%d")
  covariates_countries$self_isolating_if_ill<-as.Date(covariates_countries$self_isolating_if_ill,format="%Y-%m-%d")
  
  covariates<-bind_rows(covariates_countries,covariates)
  
  # Changing region names
  nametrans <- read.csv("Italy/data/province_name_translation.csv")
  colnames(nametrans)[which(colnames(nametrans)=="denominazione_regione")]<-"country"
  nametrans$country<-as.factor(nametrans$country)
  nametrans$country<-str_replace_all(nametrans$country, " ", "_")
  colnames(nametrans)[which(colnames(nametrans)=="country")]<-"Country"
  covariates <- inner_join(covariates,nametrans,by.x="Country",by.y="Country") # fix names of regions
  covariates <- covariates[,-which(colnames(covariates) %in% c("Country","google_county"))]
  colnames(covariates)[which(colnames(covariates)=="county")] <- "Country"
  covariates$Country<-str_replace_all(covariates$Country, " ", "_")
  
  covariates <- covariates %>% select("Country","schools_universities","public_events","lockdown","social_distancing_encouraged","self_isolating_if_ill")
  return(covariates)
}
