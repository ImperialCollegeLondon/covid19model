library(data.table)
library(ggplot2)
library(viridis)
library(rstan)
library(bayesplot)

#	for dev purposes
if(0)
{
  args_dir <- list()
  args_dir[['out_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/death-figures/'
  args_dir[['in_dir']] <- '/rdsgpfs/general/project/ratmann_covid19/live/age_renewal_usa/death-figures/'
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-out_dir')
  stopifnot(args_line[[3]]=='-in_dir')
  args_dir <- list()
  args_dir[['out_dir']] <- args_line[[2]]
  args_dir[['in_dir']] <- args_line[[4]]
} 

outfile.base = args_dir[['out_dir']]
infile.base = args_dir[['in_dir']]

cat(" \n -------------------------------- \n \n plot deaths by age over time\n \n -------------------------------- \n")

# read data
deathbyage <- read.csv('~/R0t/usa/data/DeathsByAge_US_200629.csv')
deathbyage <- data.table(deathbyage)
deathbyage[,date:=as.Date(date)]

# map age
tmp <- copy(deathbyage)
tmp[,age_from:=unlist(lapply(strsplit(as.character(age),'-'),function(x)x[1]))]
tmp[,age_to:=unlist(lapply(strsplit(as.character(age),'-'),function(x)x[2]))]
tmp[,age_from:=as.numeric(gsub("\\+","",age_from))]
tmp[,age_to:=as.numeric(age_to)]
start <- c(0,20,35,65,80,100)
end <- c(20,35,65,80,100,110)
deathbyage2 <- data.table()
for (i in 1:nrow(tmp)){
  if(is.na(tmp$age_to[i])){
    tmp1 <- which(tmp$age_from[i] >= start)[sum(tmp$age_from[i] >= start)]
    tmp2 <- 6
  }else{
    tmp1 <- which(tmp$age_from[i] >= start)[sum(tmp$age_from[i] >= start)]
    tmp2 <- which(tmp$age_to[i] <= end)[sum(tmp$age_to[i] <= end)]
  }
  tmp3 <- seq(tmp1,tmp2,1)
  tmp4 <- subset(tmp[i,c('date', 'code', 'age', 'cum.deaths',
                         'daily.deaths', 'age_from', 'age_to')])
  tmp4 <- do.call("rbind", replicate(length(tmp3), tmp4, simplify = FALSE))
  tmp4$age_cat <- tmp3
  deathbyage2 <- rbind(deathbyage2,tmp4)
}

tmp <- data.table(age_cat=1:6,
                  age_cat_label=paste0(start,'-',end))
tmp[age_cat==5|age_cat==6,age_cat_label:='80+']
deathbyage2 <- merge(deathbyage2, tmp, by='age_cat')
deathbyage2[,age_cat:=NULL]

# death by age after age mapping
deathbyage2 <- deathbyage2[!is.na(daily.deaths)]
deathbyage2 <- deathbyage2[,list(daily.deaths=sum(daily.deaths)),by=c('date','code','age_cat_label')]


ggplot(deathbyage2,aes(x=date,y=daily.deaths))+
  geom_bar( position = "dodge", stat="identity",width = 1)+
  theme_bw()+
  facet_grid(code+age_cat_label~.,scales = 'free_y')+
  labs(x='dates',y='daily deaths')

ggsave(paste0(outfile.base, 'death_over_time.pdf'),width = 6,height = 5*length(unique(deathbyage2$code)),limitsize = FALSE)

# proportion of deaths by age after age mapping
range(deathbyage2$date)
selected.dates <- seq(from=as.Date("2020-03-23"),to= as.Date("2020-06-29"),by=7)
dweek <- data.table(date=selected.dates,
                    week=seq_len(length(selected.dates)))

deathbyage3 <- copy(deathbyage2)
tmp <-  deathbyage3[,list(sum=sum(daily.deaths)),by=c('date','code')]  
deathbyage3 <- merge(deathbyage3,tmp,by=c('date','code'))
deathbyage3[,daily.deaths.prop:=daily.deaths/sum]

ggplot(deathbyage3,aes(x=date,y=daily.deaths.prop))+
  geom_area(aes(fill = age_cat_label),position = 'stack')+
  theme_bw()+
  facet_wrap(code~.,ncol = 1)+
  scale_x_date()+
  scale_y_continuous(expand = c(0,0),limit=c(0,1),labels=scales::percent)+
  labs(x='dates',y='daily deaths')+
  theme(legend.position = 'bottom')+
  guides(row = guide_legend(nrow = 1))+
  scale_fill_viridis_d()

ggsave(paste0(outfile.base,'prop_death_over_time.pdf'),width = 6,height = 1.5*length(unique(deathbyage2$code)),limitsize = FALSE)

cat(" \n -------------------------------- \n \n process death by age \n \n -------------------------------- \n")
# deaths by age
deathbyage5 <- deathbyage2[,list(deaths=sum(daily.deaths)),
                           by=c('code','age_cat_label')]
tmp <- deathbyage5[,list(sum=sum(deaths)),by='code']
deathbyage5 <- merge(deathbyage5,tmp,by='code')
deathbyage5[,deaths.prop:=deaths/sum]
deathbyage5 <- deathbyage5[sum>=100]

# construct regression
dt <- subset(deathbyage5,select = c('code', 'age_cat_label', 'deaths'))
unique(dt$code)

cat(" \n -------------------------------- \n \n process covariates \n \n -------------------------------- \n")

# population size
pop_info <- readRDS(file=paste0(infile.base,'pop_info.rds'))
dage <- unique(subset(pop_info,select = c('age.cat', 'age.cat.label')))
setnames(dage, c('age.cat', 'age.cat.label'),c('age_cat', 'age_cat_label'))
dage[age_cat %in% 1:4, age_cat_label2:='0-20']
dage[age_cat %in% 5:8, age_cat_label2:='20-35']
dage[age_cat %in% 9:13, age_cat_label2:='35-65']
dage[age_cat %in% 14:16, age_cat_label2:='65-80']
dage[age_cat %in% 17:18, age_cat_label2:='80+']
pop_size <- subset(pop_info,select = c('age.cat','pop','loc'))
setnames(pop_size, c('age.cat'),c('age_cat'))
pop_size <- merge(pop_size, subset(dage,select = c('age_cat','age_cat_label2')),by='age_cat')
setnames(pop_size,c('loc','age_cat_label2'),c('code','age_cat_label'))
pop_size<- pop_size[,list(pop=sum(pop)),by=c('code','age_cat_label')]
tmp <- pop_size[,list(sum=sum(pop)),by='code']
pop_size <- merge(pop_size, tmp, by = 'code')
pop_size[,pop.prop:=pop/sum]
dt <- merge(dt, subset(pop_size,select = c('code','age_cat_label','pop')),by=c('code','age_cat_label'),all.x = TRUE)

# contacts
dcontact <- readRDS(paste0(infile.base,'dcontact.rds'))
setnames(dcontact,colnames(dcontact),gsub('[.]','_',colnames(dcontact)))
tmp <- subset(dage,select = c('age_cat','age_cat_label2'))
setnames(tmp,colnames(tmp), paste0('part_',colnames(tmp)))
dcontact <- merge(dcontact, tmp, by='part_age_cat')
setnames(tmp,colnames(tmp), gsub('part_','cont_',colnames(tmp)))
dcontact <- merge(dcontact, tmp, by='cont_age_cat')
contact <- dcontact[,list(m=sum(m)),by=c('loc', 'type', 'part_age_cat_label2', 'cont_age_cat_label2')]
setnames(contact,c('loc','part_age_cat_label2', 'cont_age_cat_label2'),
         c('code','part_age_cat_label', 'cont_age_cat_label'))
contact <- reshape2::dcast(contact, code+part_age_cat_label+cont_age_cat_label~type, value.var = 'm')
contact[,overall:= 2/7 * weekend + 5/7 * weekday]
tmp <-  subset(pop_size,select = c('code','age_cat_label','pop'))
colnames(tmp)[2:3] <- paste0('part_',colnames(tmp)[2:3])
contact <- merge(contact,tmp,by=c('code','part_age_cat_label'),all.x = TRUE)
colnames(tmp)[2:3] <- gsub('part_','cont_',colnames(tmp)[2:3])
contact <- merge(contact,tmp,by=c('code','cont_age_cat_label'),all.x = TRUE)
contact[,c:=overall/cont_pop]
contact[,m:=c*part_pop]
contact <- contact[,list(m=sum(m)),by=c('cont_age_cat_label','code')]
setnames(contact, 'cont_age_cat_label','age_cat_label')
dt <- merge(dt, contact, by=c('age_cat_label','code'),all.x = TRUE)

# weekly deaths
ddate <- data.table(date = seq(as.Date("2020-03-23"),as.Date("2020-06-29"),1),
                    week= c(rep(1:14,each=7),15))
deathbyage6 <- merge(deathbyage2,ddate,by='date')
deathsbyweek <- deathbyage6[,list(daily.deaths=sum(daily.deaths)),
                            by=c('week','code')]

# max deaths per week/ population size for each state
deathsbyweek.max <- deathsbyweek[,list(max.daily.deaths=max(daily.deaths)),by='code']
pop_size_state <- unique(subset(pop_size,select = c('code','sum')))
deathsbyweek.max <- merge(deathsbyweek.max, pop_size_state, by ='code')
deathsbyweek.max[,prop_max_weekly_deaths:=max.daily.deaths/sum]
tmp <- subset(deathsbyweek.max,select = c('code','prop_max_weekly_deaths'))
dt <- merge(dt, tmp, by='code')

# the first date when each state reach 10 cumulative deaths
death_data <- readRDS(paste0(infile.base,'death_data_all.rds'))
death_data <- setDT(death_data)[, cdeaths := cumsum(Deaths), code][]
first_date <- matrix(nrow=1,ncol = length(unique(death_data$code)))
for (i in 1:length(unique(death_data$code))){
  first_date[1,i] <-   death_data[code==unique(death_data$code)[i] & cdeaths >=10, min(date)]
}
first_date <- first_date - min(first_date) 
tmp <- data.table(code=unique(death_data$code),
                  fdate=as.vector(first_date))
dt <- merge(dt, tmp, by='code',all.x = TRUE)

# population size by ethnic per state
ethnic <- data.table(read.csv(paste0(infile.base,'us_ethnic_composition/ACSDT1Y2018.B03002_data_with_overlays_2020-05-28T151356.csv',skip=1)))
ethnic <- subset(ethnic,select=colnames(ethnic)[!grepl('Error',colnames(ethnic))])
col.kept <- colnames(ethnic)[lengths(strsplit(colnames(ethnic),'..',fixed = TRUE))==4 | lengths(strsplit(colnames(ethnic),'..',fixed = TRUE))==1]
ethnic <- subset(ethnic,select = col.kept)
ethnic <- melt(ethnic,id.vars = c("id","Geographic.Area.Name"))
colnames(ethnic) <- c('id','state','variable','population')
ethnic[,variable:=as.character(variable)]
ethnic[,type:=unlist(lapply(strsplit(ethnic$variable,'..',fixed = TRUE),function(x)x[[3]]))]
ethnic[,subtype:=unlist(lapply(strsplit(ethnic$variable,'..',fixed = TRUE),function(x)x[[4]]))]
set(ethnic,NULL,c("id","variable"),NULL)

dstate <- unique(subset(death_data,select=c('state', 'code')))
ethnic <- merge(ethnic, dstate, by='state')
tmp <- ethnic[,list(population=sum(population)), by=c('code','type')]
tmp <- reshape2::dcast(tmp,code~type,value.var='population')
dt <- merge(dt, tmp, by='code',all.x = TRUE)
tmp <- ethnic[,list(population=sum(population)), by=c('code','subtype')]
tmp <- reshape2::dcast(tmp,code~subtype,value.var='population')
dt <- merge(dt, tmp, by='code',all.x = TRUE)

# process covariates
dt[,AGE_0_20:=as.integer(age_cat_label=='0-20')]
dt[,AGE_20_35:=as.integer(age_cat_label=='20-35')]
dt[,AGE_35_65:=as.integer(age_cat_label=='35-65')]
dt[,AGE_65_80:=as.integer(age_cat_label=='65-80')]

# remove na
tmp <- copy(dt)
lapply(tmp,function(x)sum(is.na(x)))
tmp[is.na(Hispanic.or.Latino)]
tmp <- tmp[!is.na(Hispanic.or.Latino)]
set(tmp,NULL,c('code', 'age_cat_label'),NULL)
colnames(tmp) <- gsub('[.]','_',colnames(tmp))
colnames(tmp) <- toupper(colnames(tmp))

cat(" \n -------------------------------- \n \n run regression \n \n -------------------------------- \n")
# run stan
stan_code <- "
data{
   int<lower=1> N;
   int<lower=0> DEATHS[N]; 
   real POP[N]; 
   real M[N]; 
   real PROP_MAX_WEEKLY_DEATHS[N]; 
   real FDATE[N]; 
   real HISPANIC_OR_LATINO[N]; 
   real AMERICAN_INDIAN_AND_ALASKA_NATIVE_ALONE[N]; 
   real ASIAN_ALONE[N]; 
   real BLACK_OR_AFRICAN_AMERICAN_ALONE[N]; 
   real NATIVE_HAWAIIAN_AND_OTHER_PACIFIC_ISLANDER_ALONE[N]; 
   real TWO_OR_MORE_RACES[N]; 
   real WHITE_ALONE[N]; 
   int AGE_0_20[N]; 
   int AGE_20_35[N]; 
   int AGE_35_65[N]; 
   int AGE_65_80[N]; 
}
parameters{
   real a;
   real phi;
   real pop;
   real m;
   real prop_max_weekly_deaths;
   real fdate;
   real hispanic_or_latino;
   real american_indian_and_alaska_native_alone;
   real asian_alone;
   real black_or_african_american_alone;
   real native_hawaiian_and_other_pacific_islander_alone;
   real two_or_more_races;
   real white_alone;
   real age_0_20;
   real age_20_35;
   real age_35_65;
   real age_65_80;
}
transformed parameters{
  vector[N] lbd;
  for ( i in 1:N ) {
      lbd[i] = exp( a +  pop * POP[i] +  m * M[i] +  prop_max_weekly_deaths * PROP_MAX_WEEKLY_DEATHS[i] +  
      fdate * FDATE[i] +  hispanic_or_latino * HISPANIC_OR_LATINO[i]  + 
      american_indian_and_alaska_native_alone * AMERICAN_INDIAN_AND_ALASKA_NATIVE_ALONE[i] +  asian_alone * ASIAN_ALONE[i] +  
      black_or_african_american_alone * BLACK_OR_AFRICAN_AMERICAN_ALONE[i] +  native_hawaiian_and_other_pacific_islander_alone * NATIVE_HAWAIIAN_AND_OTHER_PACIFIC_ISLANDER_ALONE[i] +  
      two_or_more_races * TWO_OR_MORE_RACES[i] +  white_alone * WHITE_ALONE[i] +  
      age_0_20 * AGE_0_20[i] +  age_20_35 * AGE_20_35[i] +  age_35_65 * AGE_35_65[i] +  age_65_80 * AGE_65_80[i]);
  }
}
model{
  DEATHS ~ neg_binomial_2( lbd , phi);
  phi ~ cauchy(0, 3);
  pop~ normal( 0 , 10 ); 
  m~ normal( 0 , 10 ); 
  prop_max_weekly_deaths~ normal( 0 , 10 ); 
  fdate~ normal( 0 , 10 ); 
  hispanic_or_latino~ normal( 0 , 10 ); 
  american_indian_and_alaska_native_alone~ normal( 0 , 10 ); 
  asian_alone~ normal( 0 , 10 ); 
  black_or_african_american_alone~ normal( 0 , 10 ); 
  native_hawaiian_and_other_pacific_islander_alone~ normal( 0 , 10 ); 
  two_or_more_races~ normal( 0 , 10 ); 
  white_alone~ normal( 0 , 10 ); 
  age_0_20~ normal( 0 , 10 ); 
  age_20_35~ normal( 0 , 10 ); 
  age_35_65~ normal( 0 , 10 ); 
  age_65_80~ normal( 0 , 10 ); 
  a ~ normal( 0 , 100 );
}
generated quantities{
  int DEATHS_predict[N];
  real log_lik[N];
  for(i in 1:N){
    DEATHS_predict[i] = neg_binomial_2_rng(lbd[i],phi);
    log_lik[i] = neg_binomial_2_lpmf(DEATHS[i] | lbd[i], phi);
  }
}
"

# standardise 
tmp <- as.list(tmp)
tmp$N <- lengths(tmp)[1]
for (i in names(tmp)) {
  if(i!='DEATHS' & !grepl('AGE_',i) & i!='N'){
    tmp[[i]] <- (tmp[[i]]-mean(tmp[[i]]))/sd(tmp[[i]])
  }
}

fit    <- stan(model_code = stan_code,
                data = tmp,
                iter = 2000,
                warmup = 500,
                cores = 1,
                chains = 1,
                init = list(list(phi=1, a=0,  pop= 0,  m= 0,  prop_max_weekly_deaths= 0,  fdate= 0,  hispanic_or_latino= 0,  american_indian_and_alaska_native_alone= 0,  asian_alone= 0,  black_or_african_american_alone= 0,  native_hawaiian_and_other_pacific_islander_alone= 0,   two_or_more_races= 0,  white_alone= 0,  age_0_20= 0,  age_20_35= 0,  age_35_65= 0,  age_65_80= 0)))

saveRDS(fit,file=paste0(outfile.base,'regression.rds'))

# assess
params <- rstan::extract(fit,pars=names(fit)[!grepl('lbd|lp_',names(fit))])
params <- lapply(params, function(x)quantile(x,probs=c(0.025,0.5,0.975)))
dt <- data.table(do.call(rbind, params))
dt$variable <-  names(params)
dt.params <- dt[variable %in% names(fit)[!grepl('log_lik|DEATHS_predict',names(fit))]]
dt.deaths <- dt[variable %in% names(fit)[grepl('DEATHS_predict',names(fit))]]
dt.deaths$deaths <- tmp$DEATHS
dt.deaths[,sum(deaths >= `2.5%` & deaths <= `97.5%`)]/nrow(dt.deaths)
# 0.98125
range(summary(fit)$summary[, "n_eff"])
# 418.1533 2213.7257
range(summary(fit)$summary[, "Rhat"])
# 0.9993332 1.0102129

# plot
color_scheme_set("blue")
mcmc_intervals(fit, pars = names(fit)[!grepl('lbd|lp_|log_lik|DEATHS_predict',names(fit))])
ggsave(paste0(outfile.base,'regression.pdf'),width = 8,height = 10)







