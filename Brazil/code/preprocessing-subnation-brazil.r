library(tidyverse)
library(lubridate)

path <- paste0("Brazil/","data")
df_brazil <- read.csv(paste0(path,"/brazil-deaths.csv"), stringsAsFactors = FALSE)
df_brazil$DateRep <- as.POSIXct(as.character(df_brazil$data))
colnames(df_brazil) <- c("fullregion","region","date","Cases","cumulative_cases","Deaths","cumulative_deaths","DateRep")

df_brazil <- df_brazil[c("DateRep","region","Cases","Deaths")]

regions <- unique(df_brazil$region)
days<-length(seq(as.Date('2020/01/01'),as.Date(df_brazil$DateRep[1]-1),"days"))
zeroes<-data.frame(DateRep=rep(seq(as.Date('2020/01/01'),as.Date(df_brazil$DateRep[1]-1),"days"),length(regions)),
                   region=regions[rep(seq_len(length(regions)), each = days)],
                   Cases=0,Deaths=0)
zeroes$DateRep <- as.POSIXct(zeroes$DateRep)
df_brazil=bind_rows(zeroes,df_brazil)


df_pop<-read.csv(paste0(path,"/brazil-population.csv"),sep=";")
df_pop<-df_pop[c("region","population")]
df_pop <- df_pop[1:(nrow(df_pop)-1),]
df=merge(x = df_brazil, y = df_pop, by = "region", all = TRUE)
df=df[order(as.Date(df$DateRep, format="%Y-%m-%d")),]
write_csv(df,"Brazil/data/df-Brazil.csv")

#### Adding google mobility data
mobility.aux = read.csv("data/Global_Mobility_Report.csv"))
mobility.aux2 = subset(mobility.aux,mobility.aux$country_region=="Brazil")
mobility.aux2 = mobility.aux2[which(mobility.aux2$sub_region_1!=""),]

region=c("DF","AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PR","PB","PA","PE","PI","RN","RS","RJ","RO","RR","SC","SE","SP","TO")
county=c("Federal District",'State of Acre','State of Alagoas','State of Amapá','State of Amazonas','State of Bahia','State of Ceará','State of Espírito Santo','State of Goiás','State of Maranhão','State of Mato Grosso','State of Mato Grosso do Sul','State of Minas Gerais','State of Paraná','State of Paraíba','State of Pará','State of Pernambuco','State of Piauí','State of Rio Grande do Norte','State of Rio Grande do Sul','State of Rio de Janeiro','State of Rondônia','State of Roraima','State of Santa Catarina','State of Sergipe','State of São Paulo','State of Tocantins')
df_region_codes = data.frame(region,county) # Please check these are right -- HHH checked
mobility<-merge(x = mobility.aux2, y = df_region_codes, by.x = c("sub_region_1"),by.y=c("county"), all = TRUE)

## identify columns
colnames(mobility)[grep("retail",colnames(mobility))] = "retail_recreation"
colnames(mobility)[grep("pharmacy",colnames(mobility))] = "grocery_pharmacy"
colnames(mobility)[grep("parks",colnames(mobility))] = "parks"
colnames(mobility)[grep("transit",colnames(mobility))] = "transitstations"
colnames(mobility)[grep("residential",colnames(mobility))] = "residential"
colnames(mobility)[grep("workplace",colnames(mobility))] = "workplace"

mobility<-mobility[c('region','date','grocery_pharmacy','parks','residential','retail_recreation','transitstations','workplace')]
mobility[,c('grocery_pharmacy','parks','residential','retail_recreation','transitstations','workplace')] = mobility[,c('grocery_pharmacy','parks','residential','retail_recreation','transitstations','workplace')]/100
mobility[is.na(mobility)]=0
mobility = mobility[order(as.Date(mobility$date, format="%Y-%m-%d")),]
mobility = mobility[order(mobility$region),]

write_csv(mobility,paste0(path,"/mobility-processed-brazil.csv"))

#Function to fill missings in mobility
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

process_data <- function(countries,N2,d){
  weight_fatality<-read.csv(paste0("Brazil/data/IFRS-all.csv"))[c("X","State","IFR_Peru_poorer")]
  cfr.by.country<-weight_fatality
  colnames(cfr.by.country)<-c(" ","region","weighted_fatality")
  serial.interval = read.csv("data/serial_interval.csv")
  pad_serial.interval <- data.frame(
    "X"=(length(serial.interval$fit)+1):200,
    "fit"=rep(1e-17, 200)
  )
  serial.interval = rbind(serial.interval, pad_serial.interval)
  ONSET_to_DEATH=18.8
  # using covariates as dates we want - currently not used
  interventions <- read.csv2(paste0("Brazil/data/brazil-interventions.csv"), sep=";")
  interventions[,2] <- dmy(as.character(interventions[,2]))
  interventions[,3] <- dmy(as.character(interventions[,3]))
  interventions[,4] <- dmy(as.character(interventions[,4]))
  interventions[,5] <- dmy(as.character(interventions[,5]))
  colnames(interventions) = c("region","Emergency","Retail and Service","Transport","School Closing")
  
  dates = list()
  reported_cases = list()
  stan_data = list(M=length(countries),N=NULL,covariate1=NULL,covariate2=NULL,covariate3=NULL,covariate4=NULL,deaths=NULL,f=NULL,
                   N0=6,cases=NULL,SI=serial.interval$fit[1:N2],
                   EpidemicStart = NULL, pop = NULL) # N0 = 6 to make it consistent with Rayleigh
  
  deaths_by_country = list()
  
  # various distributions required for modeling
  mean1 = 5.1; cv1 = 0.86; # infection to onset
  mean2 = ONSET_to_DEATH; cv2 = 0.45 # onset to death
  x1 = rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
  x2 = rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
  
  ecdf.saved = ecdf(x1+x2)
  aux.epidemicStart = NULL
  
  
  
  for(Country in countries) {
    #Country = countries[1]
    #df_pop
    #cfr.by.country
    
    IFR=cfr.by.country$weighted_fatality[cfr.by.country$region == Country]
    
    d1_pop = df_pop[df_pop$region==Country,]
    
    d1=d[d$region==Country,]
    d1$DateRep = format(d1$DateRep, "%Y-%m-%d")
    d1 = d1[order(as.Date(d1$DateRep)),] # ensure date ordering
    d1$DateRep = as.Date(d1$DateRep)
    
    mobility1=mobility[mobility$region==Country,]
    mobility1 = mobility1[order(as.Date(mobility1$date)),]  # ensure date ordering
    mobility1$date = as.Date(mobility1$date)
    
    # merge d1 and mobility - repeating the ones without data
    aux = left_join(d1,mobility1,by=c("DateRep" = "date"))
    # input missing fisrt column
    aux$region.y = f1(as.character(aux$region.y))
    # input missing mobility
    idx = which(colnames(aux) %in% c("grocery_pharmacy","parks","residential","retail_recreation","transitstations","workplace"))
    aux[,idx] = apply(aux[,idx], 2, function(x) f1(x))
    
    mobility1 = aux[,c("region.x","DateRep","grocery_pharmacy","parks","residential","retail_recreation","transitstations","workplace")]
    colnames(mobility1)[1:2] = c("county","date")
    mobility1 = mobility1[order(as.Date(mobility1$date)),]
    
    ## adding interventions to d1
    aux.int = interventions[interventions$region==Country,]
    d1$Emergency = rep(0,nrow(d1))
    d1$Retail = rep(0,nrow(d1))
    d1$Transport = rep(0,nrow(d1))
    d1$Schools = rep(0,nrow(d1))
    
    ## check if the intervention happened or not
    ifelse(!is.na(aux.int$Emergency),d1$Emergency[which(as.Date(d1$DateRep)==as.Date(aux.int$Emergency)):nrow(d1)] <- 1,
           d1$Emergency<-0)
    
    ifelse(!is.na(aux.int$`Retail and Service`),d1$Retail[which(as.Date(d1$DateRep)==as.Date(aux.int$`Retail and Service`)):nrow(d1)] <- 1,
           d1$Retail<-0)
    
    ifelse(!is.na(aux.int$Transport),d1$Transport[which(as.Date(d1$DateRep)==as.Date(aux.int$Transport)):nrow(d1)] <- 1,
           d1$Transport<-0)
    
    ifelse(!is.na(aux.int$`School Closing`),d1$Schools[which(as.Date(d1$DateRep)==as.Date(aux.int$`School Closing`)):nrow(d1)] <- 1,
           d1$Schools <- 0)
    
    index = which(d1$Cases>0)[1]
    index1 = which(cumsum(d1$Deaths)>=10)[1] # also 5
    index2 = index1-30
    
    print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))
    d1=d1[index2:nrow(d1),]
    aux.epidemicStart = c(aux.epidemicStart,d1$DateRep[index1+1-index2])
    stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2)
    stan_data$pop = c(stan_data$pop, d1_pop$population)
    mobility1 = mobility1[index2:nrow(mobility1),]
    
    dates[[Country]] = d1$DateRep
    # hazard estimation
    N = length(d1$Cases)
    N0=N
    print(sprintf("%s has %d days of data",Country,N))
    forecast = N2 - N
    # IFR is the overall probability of dying given infection
    convolution = function(u) (IFR * ecdf.saved(u))
    
    f = rep(0,N2) # f is the probability of dying on day i given infection
    f[1] = (convolution(1.5) - convolution(0))
    for(i in 2:N2) {
      f[i] = (convolution(i+.5) - convolution(i-.5))
    }
    
    reported_cases[[Country]] = as.vector(as.numeric(d1$Cases))
    deaths=c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
    cases=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
    deaths_by_country[[Country]] = as.vector(as.numeric(d1$Deaths))
    
    library(forecast)
    #covariate for mobility now being passed
    covariates2 <- as.data.frame(mobility1[, c("grocery_pharmacy","parks","residential","retail_recreation","transitstations","workplace")])
    models = apply(covariates2, 2, function(x) auto.arima(x, seasonal = T))
    mat.forecast = lapply(models, function(x) forecast(x,length((N+1):(N+forecast)))$mean)
    covariates2[(N+1):(N+forecast),] <- cbind(mat.forecast$grocery_pharmacy,mat.forecast$parks,mat.forecast$residential,mat.forecast$retail_recreation,
                                                                 mat.forecast$transitstations,mat.forecast$workplace)
    average <- (covariates2[,1] + covariates2[,4]+ covariates2[,6])/3
    stan_data$covariate1 = cbind(stan_data$covariate1,covariates2[,3]) #Residential
    stan_data$covariate2 = cbind(stan_data$covariate2,covariates2[,5]) #Transitstations
    stan_data$covariate3 = cbind(stan_data$covariate3,average)         #Mean of  Grocery, Retail, workplace
    stan_data$covariate4 = cbind(stan_data$covariate4,covariates2[,2]) #Parks
    
    stan_data$N = c(stan_data$N,N)
    stan_data$f = cbind(stan_data$f,f)
    stan_data$deaths = cbind(stan_data$deaths,deaths)
    stan_data$cases = cbind(stan_data$cases,cases)
    
    stan_data$N2=N2
    stan_data$x=1:N2
    if(length(stan_data$N) == 1) {
      stan_data$N = as.array(stan_data$N)
    }
    
    
  }
  stan_data$X = list(stan_data$covariate1,stan_data$covariate2,stan_data$covariate3,stan_data$covariate4)
  stan_data$P = length(stan_data$X)
  stan_data$X = array(unlist(stan_data$X, use.names = FALSE), dim = c( stan_data$N2 , stan_data$M , stan_data$P ) )
  stan_data$X  = aperm(stan_data$X, c(2,1,3))
  
  return(list("stan_data" = stan_data, "dates" = dates, "reported_cases"=reported_cases, "deaths_by_country" = deaths_by_country))
  
}
