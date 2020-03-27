library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)

countries <- c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  "United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria", 
  "Sweden",
  "Switzerland"
)

args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0) {
  args = 'base'
} 
StanModel = args[1]

print(sprintf("Running %s",StanModel))

## Reading all data
d=readRDS('data/COVID-19-up-to-date.rds') # get new data with script inside data folder

## get CFR
cfr.by.country = read.csv("data/weighted_fatality.csv")
cfr.by.country$country = as.character(cfr.by.country[,2])
cfr.by.country$country[cfr.by.country$country == "United Kingdom"] = "United_Kingdom"

serial.interval = read.csv("data/serial_interval.csv")
covariates = read.csv('data/interventions.csv', stringsAsFactors = FALSE)
covariates <- covariates[1:11, c(1,2,3,4,5,6, 7, 8)]

## making sure if intervention after lockdown then date is same as of the lockdown date
covariates$schools_universities[covariates$schools_universities > covariates$lockdown] <- covariates$lockdown[covariates$schools_universities > covariates$lockdown]
covariates$travel_restrictions[covariates$travel_restrictions > covariates$lockdown] <- covariates$lockdown[covariates$travel_restrictions > covariates$lockdown] 
covariates$public_events[covariates$public_events > covariates$lockdown] <- covariates$lockdown[covariates$public_events > covariates$lockdown]
covariates$sport[covariates$sport > covariates$lockdown] <- covariates$lockdown[covariates$sport > covariates$lockdown]
covariates$social_distancing_encouraged[covariates$social_distancing_encouraged > covariates$lockdown] <- covariates$lockdown[covariates$social_distancing_encouraged > covariates$lockdown]
covariates$self_isolating_if_ill[covariates$self_isolating_if_ill > covariates$lockdown] <- covariates$lockdown[covariates$self_isolating_if_ill > covariates$lockdown]

p <- ncol(covariates) - 1
forecast = 0

DEBUG = FALSE
if(DEBUG == FALSE) {
  N2 = 75 
}  else  {
  ### For faster runs:
  countries = c("Austria","Belgium") #,Spain")
  N2 = 60
}

dates = list()
reported_cases = list()
stan_data = list(M=length(countries),N=NULL,p=p,x1=poly(1:N2,2)[,1],x2=poly(1:N2,2)[,2],
                 y=NULL,covariate1=NULL,covariate2=NULL,covariate3=NULL,covariate4=NULL,covariate5=NULL,covariate6=NULL,covariate7=NULL,deaths=NULL,f=NULL,
                 N0=6,cases=NULL,LENGTHSCALE=7,SI=serial.interval$fit[1:N2]) # N0 = 6 to make it consistent with Rayleigh
deaths_by_country = list()

for(Country in countries) {
  CFR=cfr.by.country$weighted_fatality[cfr.by.country$country == Country]
  
  covariates1 <- covariates[covariates$Country == Country, 2:8]
  
  d1=d[d$Countries.and.territories==Country,]
  d1$date = as.Date(d1$DateRep,format='%Y-%m-%d')
  d1$t = decimal_date(d1$date) 
  d1=d1[order(d1$t),]
  index = which(d1$Cases>0)[1]
  index2 = which(cumsum(d1$Deaths)>=5)[1]-30
  print(sprintf("First non-zero cases is on day %d, and 30 days before 5 days is day %d",index,index2))
  d1=d1[index2:nrow(d1),]
  
  for (ii in 1:ncol(covariates1)) {
    covariate = names(covariates1)[ii]
    d1[covariate] <- (as.Date(d1$DateRep) >= as.Date(covariates1[1,covariate]))*1  # should this be > or >=?
  }
  
  dates[[Country]] = d1$date
  # hazard estimation
  N = length(d1$Cases)
  print(sprintf("%s has %d days of data",Country,N))
  forecast = N2 - N
  if(forecast < 0) {
    print(sprintf("%s: %d", Country, N))
    print("ERROR!!!! increasing N2")
    N2 = N
    forecast = N2 - N
  }
  
  h = rep(0,forecast+N) # discrete hazard rate from time t = 1, ..., 100
  if(DEBUG) { # OLD -- but faster for testing this part of the code
    mean = 18.8
    cv = 0.45
    
    for(i in 1:length(h))
      h[i] = (CFR*pgammaAlt(i,mean = mean,cv=cv) - CFR*pgammaAlt(i-1,mean = mean,cv=cv)) / (1-CFR*pgammaAlt(i-1,mean = mean,cv=cv))
  } else { # NEW
    mean1 = 5.1; cv1 = 0.86; # infection to onset
    mean2 = 18.8; cv2 = 0.45 # onset to death
    ## assume that CFR is probability of dying given infection
    x1 = rgammaAlt(5e6,mean1,cv1) # infection-to-onset ----> do all people who are infected get to onset?
    x2 = rgammaAlt(5e6,mean2,cv2) # onset-to-death
    f = ecdf(x1+x2)
    convolution = function(u) (CFR * f(u))
    
    h[1] = (convolution(1.5) - convolution(0)) 
    for(i in 2:length(h)) {
      h[i] = (convolution(i+.5) - convolution(i-.5)) / (1-convolution(i-.5))
    }
  }
  s = rep(0,N2)
  s[1] = 1 
  for(i in 2:N2) {
    s[i] = s[i-1]*(1-h[i-1])
  }
  f = s * h
  
  
  
  
  y=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
  reported_cases[[Country]] = as.vector(as.numeric(d1$Cases))
  deaths=c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
  cases=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
  deaths_by_country[[Country]] = as.vector(as.numeric(d1$Deaths))
  covariates2 <- as.data.frame(d1[, colnames(covariates1)])
  # x=1:(N+forecast)
  covariates2[N:(N+forecast),] <- covariates2[N,]
  
  ## append data
  stan_data$N = c(stan_data$N,N)
  stan_data$y = c(stan_data$y,y[1]) # just the index case!
  # stan_data$x = cbind(stan_data$x,x)
  stan_data$covariate1 = cbind(stan_data$covariate1,covariates2[,1])
  stan_data$covariate2 = cbind(stan_data$covariate2,covariates2[,2])
  stan_data$covariate3 = cbind(stan_data$covariate3,covariates2[,3])
  stan_data$covariate4 = cbind(stan_data$covariate4,covariates2[,4])
  stan_data$covariate5 = cbind(stan_data$covariate5,covariates2[,5])
  stan_data$covariate6 = cbind(stan_data$covariate6,covariates2[,6])
  stan_data$covariate7 = cbind(stan_data$covariate7,covariates2[,7]) 
  stan_data$f = cbind(stan_data$f,f)
  stan_data$deaths = cbind(stan_data$deaths,deaths)
  stan_data$cases = cbind(stan_data$cases,cases)
  
  stan_data$N2=N2
  stan_data$x=1:N2
  if(length(stan_data$N) == 1) {
    stan_data$N = as.array(stan_data$N)
  }
}

stan_data$covariate2 = 0 * stan_data$covariate2 # remove travel bans
stan_data$covariate4 = 0 * stan_data$covariate5 # remove sport

#stan_data$covariate1 = stan_data$covariate1 # school closure
stan_data$covariate2 = stan_data$covariate7 # self-isolating if ill
#stan_data$covariate3 = stan_data$covariate3 # public events
# create the `any intervention` covariate
stan_data$covariate4 = 1*as.data.frame((stan_data$covariate1+
                                          stan_data$covariate3+
                                          stan_data$covariate5+
                                          stan_data$covariate6+
                                          stan_data$covariate7) >= 1)
stan_data$covariate5 = stan_data$covariate5 # lockdown
stan_data$covariate6 = stan_data$covariate6 # social distancing encouraged
stan_data$covariate7 = 0 # models should only take 6 covariates

if(DEBUG) {
  for(i in 1:length(countries)) {
    write.csv(
      data.frame(date=dates[[i]],
                 `school closure`=stan_data$covariate1[1:stan_data$N[i],i],
                 `self isolating if ill`=stan_data$covariate2[1:stan_data$N[i],i],
                 `public events`=stan_data$covariate3[1:stan_data$N[i],i],
                 `government makes any intervention`=stan_data$covariate4[1:stan_data$N[i],i],
                 `lockdown`=stan_data$covariate5[1:stan_data$N[i],i],
                 `social distancing encouraged`=stan_data$covariate6[1:stan_data$N[i],i]),
      file=sprintf("results/%s-check-dates.csv",countries[i]),row.names=F)
  }
}

stan_data$y = t(stan_data$y)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan'))

if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40,warmup=20,chains=2)
} else { 
  fit = sampling(m,data=stan_data,iter=4000,warmup=2000,chains=8,thin=4,control = list(adapt_delta = 0.90, max_treedepth = 10))
}  

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

save.image(paste0('results/',StanModel,'-',JOBID,'.Rdata'))

save(fit,prediction,dates,reported_cases,deaths_by_country,countries,estimated.deaths,estimtaed.deaths.cf,out,covariates,file=paste0('results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

#x = 1:(length(y)+forecast)
library(bayesplot)
alpha = (as.matrix(out$alpha))
colnames(alpha) = plot_labels
g = (mcmc_intervals(alpha, prob = .9)) 
ggsave(sprintf("figures/%s-%s-covars-alpha-log.png",StanModel,JOBID),g,width=4,height=6)
g = (mcmc_intervals(as.matrix(alpha),prob = .9,transformations = function(x) exp(-x))) 
ggsave(sprintf("figures/%s-%s-covars-alpha.png",StanModel,JOBID),g,width=4,height=6)
mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
ggsave(sprintf("figures/%s-%s-covars-mu.png",StanModel,JOBID),g,width=4,height=6)

system(paste0("Rscript plotting_3_pannel_fn.R ",StanModel,'-',JOBID,'-stanfit.Rdata'))
system(paste0("Rscript forecast_fn.R ",StanModel,'-',JOBID,'.Rdata'))

