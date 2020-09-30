library(data.table)
library(rstan)
library(dplyr)
library(bayesplot)
library(covid19AgeModel)

args = list(
  seed = 42, 
  chain = 1,
  outdir= "~/Box\ Sync/2020/R0t/results/ifr_by_age_prior",
  indir = "~/git/R0t",
  cmdstan = 1,
  #stanModelFile = 'base_age_prior_ifr_200820a7_cmdstanv',
  stanModelFile = 'base_age_prior_ifr_200820b5_cmdstanv',
  job_tag= 'new_data_spain_England'
)

## command line parsing if any
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-seed', !is.na(as.integer(args_line[[2]])))	
  stopifnot(args_line[[3]]=='-chain', !is.na(as.integer(args_line[[4]])))
  stopifnot(args_line[[5]]=='-indir')
  stopifnot(args_line[[7]]=='-outdir')
  stopifnot(args_line[[9]]=='-cmdstan')
  stopifnot(args_line[[11]]=='-stanModelFile')
  stopifnot(args_line[[13]]=='-jobtag')

  args <- list()
  args[['seed']] <- as.integer(args_line[[2]])
  args[['chain']] <- as.integer(args_line[[4]])
  args[['indir']] <- args_line[[6]]
  args[['outdir']] <- args_line[[8]]
  args[['cmdstan']] <- as.integer(args_line[[10]])
  args[['stanModelFile']] <- args_line[[12]]
  args[['job_tag']] <- args_line[[14]]
} 

pkg.dir <- system.file(package = "covid19AgeModel" )

args$file_stanModel <- file.path(pkg.dir, 'ifr-by-age',paste0(args$stanModelFile,'.stan'))
args$file_track_trace = file.path(pkg.dir, "data-ifr-by-age", "track_trace_countries.csv")
args$file_pop.data_EU = file.path(pkg.dir, "data-ifr-by-age", "pop.data_EU.csv")
args$file_PropDeathsByAge_Sweden_week24 = file.path(pkg.dir, "data-ifr-by-age", "PropDeathsByAge_Sweden_week24.csv")
args$file_CasesByAge_Sweden_week24 = file.path(pkg.dir, "data-ifr-by-age", "CasesByAge_Sweden_week24.csv")
args$file_SeroByAge_Sweden_week9to21 = file.path(pkg.dir, "data-ifr-by-age","SeroByAge_Sweden.csv")
tmp <- Sys.getenv("PBS_JOBID")
args$job_id <- ifelse(tmp!='', tmp, as.character(abs(round(rnorm(1) * 1e6))) )
args$job_dir <- file.path(args$outdir,paste0(args$stanModelFile,'-',args$job_tag,'-',args$job_id)) 

## start script
cat(sprintf("Running\n"))

str(args)
set.seed(args$seed)

## make job dir
dir.create( args$job_dir )


## save input args
saveRDS( args, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_args.RDS')))

#
# import supplementary tables Levin. et al https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v1.full.pdf
track_trace <- read.csv(args$file_track_trace, header = T)

#
# import pop count
demography = read.csv(args$file_pop.data_EU)

#
# import data for Sweden 
propdeath_SE = read.csv( args$file_PropDeathsByAge_Sweden_week24 )
cases_SE = read.csv( args$file_CasesByAge_Sweden_week24 )
sero_SE = read.csv(args$file_SeroByAge_Sweden_week9to21)


#### Track and trace countries ####
tmp_tt = as.data.table(select(track_trace, c(Study, Age.Group, Median.Age, Population, Deaths, Infection.Rate, Lower.Bound, Upper.Bound)))
setnames(tmp_tt, c("Infection.Rate", "Lower.Bound", "Upper.Bound", "Population", "Median.Age"), c("sero", "seroL", "seroU", "pop", "Median_Age"))
tmp_tt = subset(tmp_tt, !is.na(seroL))
tmp_tt[, from_age := ifelse(Age.Group == "80+", 80, as.numeric(gsub("(.+)-.+", "\\1", Age.Group))) ]
tmp_tt[, to_age := ifelse(Age.Group == "80+", 90, as.numeric(gsub(".*-(.+)", "\\1", Age.Group)))]
tmp_tt[, sero := as.numeric(sero)/100]
tmp_tt[, seroL := as.numeric(seroL)/100]
tmp_tt[, seroU := as.numeric(seroU)/100]
tmp_tt[, pop := as.numeric(gsub(",","",pop))]
tmp_tt[, Deaths := as.numeric(Deaths)]
tmp_tt[, Median_Age := as.numeric(Median_Age)]
tmp_tt = select(tmp_tt, c(Study, Median_Age, pop, Deaths, sero, seroL, seroU, from_age, to_age))
tmp_tt[Study=='Iceland' & Median_Age==75, "seroL"] <- 0.002

#### Sero-prevalence countries ####
difr = data.table(Study = c(rep("Belgium", 6), rep("Geneva", 5), rep("Spain", 9), rep("England", 4)),
                  Median_Age = c(12, 35, 55, 70, 80, 89.5, 7, 14.5, 34.5, 57, 74.5, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 31, 54.5, 69.5, 82.5),
                  from_age = c(0, 25, 45, 65, 75, 85, 5, 10, 20, 50, 65, 0, 10, 20, 30, 40, 50, 60, 70, 80, 18, 45, 65, 75),
                  to_age = c(24,44, 64, 74, 84, 94, 9, 19, 49, 64, 84, 9, 19, 29, 39, 49, 59, 69, 79, 90, 44, 64, 74, 90))

# 
# Add death data
# Belgium ref https://www.medrxiv.org/content/10.1101/2020.06.20.20136234v1.full.pdf
difr[Study == "Belgium" & Median_Age == 12, Deaths := round(1e-06*1579570)]
difr[Study == "Belgium" & Median_Age == 35, Deaths := round(12e-06*1481713+8e-06*1474971) ]
difr[Study == "Belgium" & Median_Age == 55, Deaths := round(181e-06*1544678+84e-06*1535850)]
difr[Study == "Belgium" & Median_Age == 70, Deaths := round(1208e-06*548448+666e-06*598561)]
difr[Study == "Belgium" & Median_Age == 80, Deaths := round(3986e-06*296421+2440e-06*394264)]
difr[Study == "Belgium" & Median_Age == 89.5, Deaths := round(17464e-06*107542+14646e-06*219117)]
# Geneva ref https://www.thelancet.com/action/showPdf?pii=S1473-3099%2820%2930584-3
difr[Study == "Geneva" & Median_Age == 7, Deaths := 0]
difr[Study == "Geneva" & Median_Age == 14.5, Deaths := 0]
difr[Study == "Geneva" & Median_Age == 34.5, Deaths := 2]
difr[Study == "Geneva" & Median_Age == 57, Deaths := 16]
difr[Study == "Geneva" & Median_Age == 74.5, Deaths := 268]
# Spain ref https://www.medrxiv.org/content/10.1101/2020.08.06.20169722v1
difr[Study == "Spain" & Median_Age == 4.5, Deaths := 3+2]
difr[Study == "Spain" & Median_Age == 14.5, Deaths := 3+3]
difr[Study == "Spain" & Median_Age == 24.5, Deaths := 18+17]
difr[Study == "Spain" & Median_Age == 34.5, Deaths := 48+29]
difr[Study == "Spain" & Median_Age == 44.5, Deaths := 192+103]
difr[Study == "Spain" & Median_Age == 54.5, Deaths := 705+318]
difr[Study == "Spain" & Median_Age == 64.5, Deaths := 1904+749]
difr[Study == "Spain" & Median_Age == 74.5, Deaths := 4145+1986]
difr[Study == "Spain" & Median_Age == 84.5, Deaths := 5299+3704]
# England ref https://fingertips.phe.org.uk/static-reports/mortality-surveillance/excess-mortality-in-england-week-ending-17-Jul-2020.html#age-group-males
difr[Study == "England" & Median_Age == 31, Deaths := 323+201]
difr[Study == "England" & Median_Age == 54.5, Deaths := 3064+1593]
difr[Study == "England" & Median_Age == 69.5, Deaths := 4609+2496]
difr[Study == "England" & Median_Age == 82.5, Deaths := 9330+9461+6426+11124]
# Sweden ref - WEEK 24 https://www.folkhalsomyndigheten.se/globalassets/statistik-uppfoljning/smittsamma-sjukdomar/veckorapporter-covid-19/2020/covid-19-veckorapport-vecka-24_final.pdf
death_SE = as.data.table(merge(propdeath_SE, cases_SE, by = c("AGE_LOWER", "AGE_UPPER", "GENDER")))
death_SE[, from_age := c(rep(0, 4), rep(20, 10), rep(65, 6))]
death_SE[, to_age := c(rep(19, 4), rep(64, 10), rep(95, 6))] 
death_SE[, Deaths := round(PROP_DEATHS_EXTRACTED * CASES_EXTRACTED / 100) ]
death_SE = death_SE[, list(Deaths = sum(Deaths),
                           Median_Age = median(c(from_age, to_age))), by = c("from_age", "to_age")]
death_SE[, Study := "Sweden"]

# 
# Add seroprevalence data
# Belgium ref https://www.medrxiv.org/content/10.1101/2020.06.20.20136234v1.full.pdf
difr[Study == "Belgium" & Median_Age == 12, sero := 0.06]
difr[Study == "Belgium" & Median_Age == 12, seroL := 0.042]
difr[Study == "Belgium" & Median_Age == 12, seroU := 0.086]
difr[Study == "Belgium" & Median_Age == 35, sero := 0.059]
difr[Study == "Belgium" & Median_Age == 35, seroL := 0.042]
difr[Study == "Belgium" & Median_Age == 35, seroU := 0.083]
difr[Study == "Belgium" & Median_Age == 55, sero := 0.062]
difr[Study == "Belgium" & Median_Age == 55, seroL := 0.047]
difr[Study == "Belgium" & Median_Age == 55, seroU := 0.083]
difr[Study == "Belgium" & Median_Age == 70, sero := 0.041]
difr[Study == "Belgium" & Median_Age == 70, seroL := 0.023]
difr[Study == "Belgium" & Median_Age == 70, seroU := 0.072]
difr[Study == "Belgium" & Median_Age == 80, sero := 0.070]
difr[Study == "Belgium" & Median_Age == 80, seroL := 0.042]
difr[Study == "Belgium" & Median_Age == 80, seroU := 0.117]
difr[Study == "Belgium" & Median_Age == 89.5, sero := 0.132]
difr[Study == "Belgium" & Median_Age == 89.5, seroL := 0.089]
difr[Study == "Belgium" & Median_Age == 89.5, seroU := 0.196]
# Geneva ref https://www.thelancet.com/action/showPdf?pii=S1473-3099%2820%2930584-3
difr[Study == "Geneva" & Median_Age == 7, sero := 1200/26466]
difr[Study == "Geneva" & Median_Age == 7, seroL := 400/26466]
difr[Study == "Geneva" & Median_Age == 7, seroU := 2400/26466]
difr[Study == "Geneva" & Median_Age == 14.5, sero := 6100/53180]
difr[Study == "Geneva" & Median_Age == 14.5, seroL := 3900/53180]
difr[Study == "Geneva" & Median_Age == 14.5, seroU := 8800/53180]
difr[Study == "Geneva" & Median_Age == 34.5, sero := 28800/219440]
difr[Study == "Geneva" & Median_Age == 34.5, seroL := 21400/219440]
difr[Study == "Geneva" & Median_Age == 34.5, seroU := 37300/219440]
difr[Study == "Geneva" & Median_Age == 57, sero := 10300/98528]
difr[Study == "Geneva" & Median_Age == 57, seroL := 7200/98528]
difr[Study == "Geneva" & Median_Age == 57, seroU := 13900/98528]
difr[Study == "Geneva" & Median_Age == 74.5, sero := 5700/83574]
difr[Study == "Geneva" & Median_Age == 74.5, seroL := 3200/83574]
difr[Study == "Geneva" & Median_Age == 74.5, seroU := 8800/83574]
# Spain ref https://www.medrxiv.org/content/10.1101/2020.08.06.20169722v1
difr[Study == "Spain" & Median_Age == 4.5, sero := (71.7+88)/(2205.5+2078.3)]
difr[Study == "Spain" & Median_Age == 4.5, seroL := (42.5+55.1)/(2205.5+2078.3)]
difr[Study == "Spain" & Median_Age == 4.5, seroU := (119.7+139)/(2205.5+2078.3)]
difr[Study == "Spain" & Median_Age == 14.5, sero := (93.5+105.1)/(2557.9+2396.7)]
difr[Study == "Spain" & Median_Age == 14.5, seroL := (71.2+81.7)/(2557.9+2396.7)]
difr[Study == "Spain" & Median_Age == 14.5, seroU := (122.5+134.7)/(2557.9+2396.7)]
difr[Study == "Spain" & Median_Age == 24.5, sero := (142.9+137.4)/(2479.1+2404.1)]
difr[Study == "Spain" & Median_Age == 24.5, seroL := (116.2+111.2)/(2479.1+2404.1)]
difr[Study == "Spain" & Median_Age == 24.5, seroU := (170.9+169.3)/(2479.1+2404.1)]
difr[Study == "Spain" & Median_Age == 34.5, sero := (139.7+156.7)/(2978.1+3012.4)]
difr[Study == "Spain" & Median_Age == 34.5, seroL := (114+132)/(2978.1+3012.4)]
difr[Study == "Spain" & Median_Age == 34.5, seroU := (170.9+185.8)/(2978.1+3012.4)]
difr[Study == "Spain" & Median_Age == 44.5, sero := (209+206.8)/(3916.7+3877.8)]
difr[Study == "Spain" & Median_Age == 44.5, seroL := (180+177.9)/(3916.7+3877.8)]
difr[Study == "Spain" & Median_Age == 44.5, seroU := (242.4+240)/(3916.7+3877.8)]
difr[Study == "Spain" & Median_Age == 54.5, sero := (184+184.4)/(3493.8+3563.5)]
difr[Study == "Spain" & Median_Age == 54.5, seroL := (157.8+158.8)/(3493.8+3563.5)]
difr[Study == "Spain" & Median_Age == 54.5, seroU := (214.3+213.8)/(3493.8+3563.5)]
difr[Study == "Spain" & Median_Age == 64.5, sero := (127.2+140.4)/(2598.2+2803.4)]
difr[Study == "Spain" & Median_Age == 64.5, seroL := (105.3+117.2)/(2598.2+2803.4)]
difr[Study == "Spain" & Median_Age == 64.5, seroU := (153.3+167.9)/(2598.2+2803.4)]
difr[Study == "Spain" & Median_Age == 74.5, sero := (83.7+98.9)/(1783.7+2138.1)]
difr[Study == "Spain" & Median_Age == 74.5, seroL := (65.5+79)/(1783.7+2138.1)]
difr[Study == "Spain" & Median_Age == 74.5, seroU := (106.7+123.4)/(1783.7+2138.1)]
difr[Study == "Spain" & Median_Age == 84.5, sero := (45.6+80.2)/(993.3+1605.8)]
difr[Study == "Spain" & Median_Age == 84.5, seroL := (31.8+58.7)/(993.3+1605.8)]
difr[Study == "Spain" & Median_Age == 84.5, seroU := (64.9+108.9)/(993.3+1605.8)]
# England ref https://www.medrxiv.org/content/10.1101/2020.08.12.20173690v2 balanced by population prop 2011 census https://www.ons.gov.uk/census
difr[Study == "England" & Median_Age == 31, sero := (7.9*6.78 + 7.8*(6.89+6.62) + 6.1*(6.69+7.33)) / ((6.78+6.89+6.62+6.69+7.33)*100)]
difr[Study == "England" & Median_Age == 31, seroL := (7.3*6.78 + 7.4*(6.89+6.62) + 5.7*(6.69+7.33)) / ((6.78+6.89+6.62+6.69+7.33)*100)]
difr[Study == "England" & Median_Age == 31, seroU := (8.5*6.78 + 8.3*(6.89+6.62) + 6.6*(6.69+7.33)) / ((6.78+6.89+6.62+6.69+7.33)*100)]
difr[Study == "England" & Median_Age == 54.5, sero := (6.4*(7.32+6.41) + 5.9*(5.65+5.98) ) / ((7.32+6.41+5.65+5.98)*100)]
difr[Study == "England" & Median_Age == 54.5, seroL := (6*(7.32+6.41) + 5.5*(5.65+5.98) ) / ((7.32+6.41+5.65+5.98)*100)]
difr[Study == "England" & Median_Age == 54.5, seroU := (6.9*(7.32+6.41) + 6.4*(5.65+5.98) ) / ((7.32+6.41+5.65+5.98)*100)]
difr[Study == "England" & Median_Age == 69.5, sero := 3.2/100]
difr[Study == "England" & Median_Age == 69.5, seroL := 2.8/100]
difr[Study == "England" & Median_Age == 69.5, seroU := 3.6/100]
difr[Study == "England" & Median_Age == 82.5, sero := 3.3/100]
difr[Study == "England" & Median_Age == 82.5, seroL := 2.9/100]
difr[Study == "England" & Median_Age == 82.5, seroU := 3.8/100]
# Sweden - Week 21 ref https://www.folkhalsomyndigheten.se/contentassets/9c5893f84bd049e691562b9eeb0ca280/pavisning-antikroppar-genomgangen-covid-19-blodprov-oppenvarden-delrapport-1.pdf
tmp = as.data.table(select(subset(sero_SE, WEEK == 21), -WEEK))
tmp[, VALUE := VALUE/100]
tmp = as.data.table(reshape2::dcast(tmp, formula = AGE_LOWER + AGE_UPPER~VARIABLE, value.var = "VALUE"))
tmp = merge(death_SE, tmp, by.x = c("from_age", "to_age"), by.y = c("AGE_LOWER", "AGE_UPPER"))
difr = rbind(difr, tmp)

# 
# Add population count
difr[Study == "Belgium" & Median_Age == 12, pop := 3228894]
difr[Study == "Belgium" & Median_Age == 35, pop := 2956684]
difr[Study == "Belgium" & Median_Age == 55, pop := 3080528]
difr[Study == "Belgium" & Median_Age == 70, pop := 1147009]
difr[Study == "Belgium" & Median_Age == 80, pop := 690685]
difr[Study == "Belgium" & Median_Age == 89.5, pop := 326659]
difr[Study == "Geneva" & Median_Age == 7, pop := 26466]
difr[Study == "Geneva" & Median_Age == 14.5, pop := 53180]
difr[Study == "Geneva" & Median_Age == 34.5, pop := 219440]
difr[Study == "Geneva" & Median_Age == 57, pop := 98528]
difr[Study == "Geneva" & Median_Age == 74.5, pop := 83574]
difr[Study == "Sweden" & Median_Age == 9.5, pop := sum(subset(demography, country == "Sweden" & age %in% 0:19)$pop)]
difr[Study == "Sweden" & Median_Age == 42, pop := sum(subset(demography, country == "Sweden" & age %in% 20:64)$pop)]
difr[Study == "Sweden" & Median_Age == 80, pop := sum(subset(demography, country == "Sweden" & age %in% 65:95)$pop)]
difr[Study == "Spain" & Median_Age == 4.5,  pop := (2205.5+2078.3)*1000]
difr[Study == "Spain" & Median_Age == 14.5, pop := (2557.9+2396.7)*1000]
difr[Study == "Spain" & Median_Age == 24.5, pop := (2479.1+2404.1)*1000]
difr[Study == "Spain" & Median_Age == 34.5, pop := (2978.1+3012.4)*1000]
difr[Study == "Spain" & Median_Age == 44.5, pop := (3916.7+3877.8)*1000]
difr[Study == "Spain" & Median_Age == 54.5, pop := (3493.8+3563.5)*1000]
difr[Study == "Spain" & Median_Age == 64.5, pop := (2598.2+2803.4)*1000]
difr[Study == "Spain" & Median_Age == 74.5, pop := (1783.7+2138.1)*1000]
difr[Study == "Spain" & Median_Age == 84.5, pop := (993.3+1605.8)*1000]
difr[Study == "England" & Median_Age == 31, pop := 3595321+3650881+3509221+3549116+3885934]
difr[Study == "England" & Median_Age == 54.5, pop := 3879815+3400095+2996992+3172277]
difr[Study == "England" & Median_Age == 69.5, pop := 2508154+2044129]
difr[Study == "England" & Median_Age == 82.5, pop := 1669345+1258773+776311]

#### group track and trace countries and sero-prevalence countries #### 
difr = rbind(tmp_tt, difr)

#### add casesU, casesL, study ID
difr[, casesU := as.integer(difr$seroU*difr$pop)]
difr[, casesL := as.integer(difr$seroL*difr$pop)]
difr[, casesM := as.integer(difr$sero*difr$pop)]
stopifnot(nrow(subset(difr, casesL<Deaths))==0)
difr[, studyID := as.integer(as.factor(difr$Study))]


#
# prediction, add corresponding age_idcs for the observed age bands
age_predict <- c(0, seq(2.5, 97.5, 5))
age_predict <- seq(0, 100, 1)
difr[, lower_age_idx := findInterval(difr$from_age, age_predict)]
difr[, upper_age_idx := pmin(length(age_predict),1+findInterval(difr$to_age, age_predict))]

#
# add number aux cases variables
difr[, casesN := casesU-casesL+1L]

#
# make some initial guess on log_IFR
difr[, log_IFR := log(Deaths/(sero*pop))]
difr[, logit_IFR := log( (Deaths/(sero*pop))/(1-Deaths/(sero*pop))  )]
difr[, IFR := Deaths/(sero*pop)]

#
# explore prior for GP
if(0)
{
	require(invgamma)
	qinvgamma(c(.01,.99), 8, 30)
	x<- 2*c(-1,1)*10; exp(x)/(1+exp(x))	
}

#
# explore if logit could give reasonable fit --> yes
if(0)
{
	glm.m <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='log'), data=difr)
	age.grid <- seq(0,100,1)
	glm.ifr.pr <- predict(glm.m, newdata=data.frame(Median_Age=age.grid), se.fit=TRUE)
	glm.ifr.pr <- data.table(age=age.grid, log_ifr=glm.ifr.pr$fit, log_ifr_sd=glm.ifr.pr$se.fit)
	ggplot(glm.ifr.pr) + 
			geom_ribbon(aes(x=age, ymin=log_ifr-2*log_ifr_sd, ymax=log_ifr+2*log_ifr_sd )) + 
			geom_line(aes(x=age, y=log_ifr)) +
			geom_point(data=difr, aes(x=Median_Age, y=log_IFR), colour='blue') +
			scale_x_continuous(breaks=seq(0,100,10)) +
			theme_bw()
	
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	age.grid <- seq(0,100,1)
	glm.ifr.pr2 <- predict(glm.m2, newdata=data.frame(Median_Age=age.grid), se.fit=TRUE)
	glm.ifr.pr2 <- data.table(age=age.grid, logit_ifr=glm.ifr.pr2$fit, logit_ifr_sd=glm.ifr.pr2$se.fit)
	ggplot(glm.ifr.pr2) + 
			geom_ribbon(aes(x=age, ymin=logit_ifr-2*logit_ifr_sd, ymax=logit_ifr+2*logit_ifr_sd )) + 
			geom_line(aes(x=age, y=logit_ifr)) +
			geom_point(data=difr, aes(x=Median_Age, y=logit_IFR), colour='blue') +
			scale_x_continuous(breaks=seq(0,100,10)) +
			theme_bw()
}

#
# Prepare stan data
stan_data <- list(
	N = nrow(difr),
	N_predict = length(age_predict),
	M = length(unique(difr$Study)),
  	deaths = difr$Deaths,
  	casesU = difr$casesU,
  	casesL = difr$casesL,
	casesM = difr$casesM,
  	casesN = difr$casesN,
	sero_p_lower = difr$seroL,
	sero_p_upper = difr$seroU,
	pop_count = difr$pop,
  	study = difr$studyID,
  	age = age_predict,
  	age_study = difr$Median_Age,
  	lower_age_idx = difr$lower_age_idx,
  	upper_age_idx = difr$upper_age_idx,
  	max_casesN = max( difr$casesN )
	)

#
# add aux variables
stan_data$log_choose_fct <- matrix(nrow = stan_data$max_casesN,  ncol = stan_data$N)
stan_data$casesAux <- matrix(nrow = stan_data$max_casesN,  ncol = stan_data$N)
for(i in 1:nrow(difr))
{
	stan_data$log_choose_fct[1:difr$casesN[i],i] <- lchoose( stan_data$casesL[i]:stan_data$casesU[i], stan_data$deaths[i] ) 
	stan_data$casesAux[1:difr$casesN[i],i] <- stan_data$casesL[i]:stan_data$casesU[i]
  
  	if(difr$casesN[i] != stan_data$max_casesN)
	{
		stan_data$log_choose_fct[(difr$casesN[i]+1):stan_data$max_casesN,i] <- -1
	}		
  	if(difr$casesN[i] != stan_data$max_casesN)
	{
		stan_data$casesAux[(difr$casesN[i]+1):stan_data$max_casesN,i] <- -1	
	}
}


#
# subsample aux variables
set.seed(42)
#maxCasesAux <- 1e3
maxCasesAux <- 2
for(i in 1:nrow(difr))
{
	if(stan_data$casesN[i]>maxCasesAux)
	{
		if(maxCasesAux==2)
		{
			tmp <- c(1, stan_data$casesN[i])
		}
		if(maxCasesAux>2)
		{
			tmp <- sort(sample(stan_data$casesN[i], maxCasesAux, replace = FALSE))	
		}		
		stan_data$casesAux[1:maxCasesAux,i] <- stan_data$casesAux[tmp,i]
		stan_data$log_choose_fct[1:maxCasesAux,i] <- stan_data$log_choose_fct[tmp,i]
		stan_data$casesN[i] <- maxCasesAux
	}
}
stan_data$casesAux <- stan_data$casesAux[1:maxCasesAux,]
mode(stan_data$casesAux) <- "integer"
stan_data$log_choose_fct <- stan_data$log_choose_fct[1:maxCasesAux,]
stan_data$max_casesN <- maxCasesAux

#
# define initial values
if(grepl('0820a1|0820a4',args[['stanModelFile']]))
{
	tmp <- unname(coef(lm(log_IFR~Median_Age, data=subset(difr, is.finite(log_IFR)))))
	stan_inits <- list()
	stan_inits$alpha= tmp[1]
	stan_inits$beta= tmp[2]
	stan_inits$study_rnde= rep(0, stan_data$M)	
}
if(grepl('0820a5|0820a6',args[['stanModelFile']]))
{	
	stan_inits <- list()
	stan_inits$beta= log(0.01)
	stan_inits$study_rnde= rep(0, stan_data$M)	
}
if(grepl('0820a7|0820a8',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha= tmp[1]
	stan_inits$beta= tmp[2]
	stan_inits$study_rnde= rep(0, stan_data$M)
}
if(grepl('0820a9|0820a10|0820a11|0820a12',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha0= 0
	stan_inits$alpha1= 0	
	stan_inits$beta0= tmp[1]
	stan_inits$beta1= tmp[2]
	stan_inits$study_rnde= rep(0, stan_data$M)
	stan_inits$study_rnde2= rep(0, stan_data$M)
}
if(grepl('0820a13',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha0= 0
	stan_inits$alpha1= 0	
	stan_inits$beta0= tmp[1]
	stan_inits$beta1= tmp[2]
	stan_inits$study_rnde= rep(0, stan_data$M)
}
if(grepl('0820a14',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$overd= 2e3
	stan_inits$beta0= tmp[1]
	stan_inits$beta1= tmp[2]
	stan_inits$study_rnde= rep(0, stan_data$M)
}
if(grepl('0820a15',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha <- tmp[1]
	stan_inits$beta <- tmp[2]
	stan_inits$study_rnde <- rep(0, stan_data$M)
	#stan_inits$sero_p <- (stan_data$sero_p_lower + stan_data$sero_p_upper)/2
	stan_inits$sero_p <- difr$casesM/difr$pop
}
if(grepl('0820b1',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha <- tmp[1]
	stan_inits$beta <-  tmp[2]
	stan_inits$study_rnde <- rep(0, stan_data$M)
	stan_inits$age_rnde <- rep(0, 9)
}
if(grepl('0820b2',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha <- tmp[1]
	stan_inits$beta <-  tmp[2]
	stan_inits$study_rnde <- rep(0, stan_data$M)
	stan_inits$gp_len_sc <- 25
	stan_inits$gp_msd <- 1
}
if(grepl('0820b3|0820b4|0820b5',args[['stanModelFile']]))
{		
	glm.m2 <- glm( IFR ~ Median_Age, weights=casesM, family=binomial(link='logit'), data=difr)
	tmp <- unname(coef(glm.m2))
	stan_inits <- list()
	stan_inits$alpha <- tmp[1]
	stan_inits$beta <-  tmp[2]
	stan_inits$obs_rnde <- rep(0, stan_data$N)
	stan_inits$gp_len_sc <- 25
	stan_inits$gp_msd <- 1
}

#
# save image before running Stan
tmp <- names(.GlobalEnv)
tmp <- tmp[!grepl('^.__|^\\.|^model$',tmp)]
save(list=tmp, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_stanin.RData')) )

#
# run Stan
cat('\nRunning Stan... \n')
if(!args$cmdstan)
{  
  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  model <- stan_model(args$file_stanModel)
  fit <- sampling(model, 
		  data=stan_data,
		  iter=10e3, warmup=5e3,
		  #iter=20, warmup=10,
		  chains=3,
		  seed=42,
		  verbose=TRUE, 
		  cores = 1L,
		  init=rep(list(stan_inits), 3),
		  control = list(adapt_delta = 0.99, max_treedepth = 15, stepsize=0.02))
  #z <- extract(fit)
  ## save image after running Stan
  save(fit, file=file.path(args$job_dir, paste0(basename(args$job_dir), '_stanout.RData')) )
}
if(args$cmdstan)
{
  ## write data file
  stan_rdump( names(stan_data), file=file.path(args$job_dir, paste0(basename(args$job_dir), '_cmdstanin.R')), envir=list2env(stan_data))
  ## write init file
  stan_rdump( names(stan_inits), file=file.path(args$job_dir, paste0(basename(args$job_dir), '_cmdstaninit.R')), envir=list2env(stan_inits))
}


