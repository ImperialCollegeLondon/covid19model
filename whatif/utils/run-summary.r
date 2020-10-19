library(epidemia)
library(here)
library(data.table)
library(matrixStats)
library(tidyverse)
source("whatif/utils/plot-with-eta.r")
set.seed(1234)
cumul <- function(object) {
  dfs <- split(
    as.data.frame(t(object$draws)),
    object$group
  )
  dfs <- lapply(
    dfs,
    function(x) t(apply(x, 2, cumsum))
  )
  object$draws <- do.call(cbind, dfs)
  return(object)
}

summary = function(x) {
  d = colQuantiles(x,probs=c(.025,.5,.975))
  colnames(d) = c("deaths_li","deaths","deaths_ui")
  d
}

get_cf_deaths <- function(fit_obj, eta=NULL){
  if(is.null(eta)){
    deaths = posterior_predict(fit_obj, type="deaths", seed=12345)
  }
  else{
    deaths = posterior_predict_(fit_obj, eta =eta, type="deaths", seed=12345)
  }
  return(cumul(deaths))
}

DEATHS_PER_MILLION=TRUE

date = as.Date("2020-07-01")
data = readRDS(here("whatif/results/whatif-experiments.rds"))

df = data.table(data$args$data)[date == date, list(deaths=sum(deaths),scenario="observed"),by=country]
df$deaths_li = df$deaths_ui = df$deaths

deaths1 = get_cf_deaths (data$fit, data$eta1)
deaths2 = get_cf_deaths (data$fit, data$eta2)
deaths3 = get_cf_deaths (data$fit3)
deaths4 = get_cf_deaths (data$fit4)


df <- as.data.frame(df)

df = rbind(df,data.frame(country=deaths1$group[deaths1$time == date],
                             summary(deaths1$draws[,deaths1$time == date]),scenario="Counterfactual 1"))
df = rbind(df,data.frame(country=deaths2$group[deaths2$time == date],
                         summary(deaths2$draws[,deaths2$time == date]),scenario="Counterfactual 2"))
df = rbind(df,data.frame(country=deaths3$group[deaths3$time == date],
                         summary(deaths3$draws[,deaths3$time == date]),scenario="Counterfactual 3"))
df = rbind(df,data.frame(country=deaths4$group[deaths4$time == date],
                         summary(deaths4$draws[,deaths4$time == date]),scenario="Counterfactual 4"))

df$scenario = factor(df$scenario,levels=c("observed","Counterfactual 1","Counterfactual 2","Counterfactual 3", "Counterfactual 4"))

popt = read.csv(here("data/popt_ifr.csv"))
df$country = as.character(df$country)
df$country[df$country == "United_Kingdom"] = "United Kingdom"
df=merge(df,popt,by="country")
if(DEATHS_PER_MILLION){
  df$deaths = (df$deaths / (df$popt))*1e6
  df$deaths_ui = (df$deaths_ui / (df$popt))*1e6
  df$deaths_li = (df$deaths_li / (df$popt))*1e6
}

library(ggpubr)
# df$country[9:12] = "England & Wales"
g = ggplot(df,aes(x=country,y=deaths,group=scenario,fill=scenario))
g = g + geom_bar(stat="identity",position=position_dodge())
#g = g + facet_wrap(~country,scales = "free")
g = g + theme_pubr()
g = g + scale_x_discrete("",expand=c(0,0))
g = g + scale_y_continuous("total deaths per million\n",expand=c(0,0))  + scale_fill_brewer(palette="Paired")
g 
ggsave(here("whatif/figures/scenarios.pdf"),g,width=8,height=6)
library(stargazer)

df.out = df[order(df$scenario),]
fmt = vapply(1:nrow(df.out), function(i) sprintf("%.0f [%.0f-%.0f]",df.out[i,"deaths"],df.out[i,"deaths_li"],df.out[i,"deaths_ui"]), "")
fmt[1:3] = paste(round(unlist(df.out[1:3,"deaths"])))
df.out = df.out[,c("country","scenario")]
df.out$deaths = fmt

df.out1 <- 
  df.out %>%
  mutate(scenario = as.character(scenario)) %>% 
  filter(scenario %in% c('Counterfactual 1', 'Counterfactual 2', 'observed')) %>%
  mutate(scenario = case_when(country=='Denmark' & scenario == 'observed' ~ 'Denmark',
                              country=='Sweden' & scenario == 'observed' ~ 'Sweden',
                              country=='United Kingdom' & scenario == 'observed' ~ 'United Kingdom',
                              country!='Sweden' & scenario == 'Counterfactual 1' ~ 'Sweden',
                              country=='Sweden' & scenario == 'Counterfactual 1' ~ 'United Kingdom',
                              country!='Denmark' & scenario == 'Counterfactual 2' ~ 'Denmark',
                              country=='Denmark' & scenario == 'Counterfactual 2' ~ 'United Kingdom',
                              TRUE ~ scenario))
df.out2 <- 
  df.out %>%
  mutate(scenario = as.character(scenario)) %>% 
  filter(scenario %in% c('Counterfactual 3', 'Counterfactual 4', 'observed')) %>%
  mutate(scenario = case_when(country=='Denmark' & scenario == 'observed' ~ 'Denmark',
                              country=='Sweden' & scenario == 'observed' ~ 'Sweden',
                              country=='United Kingdom' & scenario == 'observed' ~ 'United Kingdom',
                              country!='Sweden' & scenario == 'Counterfactual 3' ~ 'Sweden',
                              country=='Sweden' & scenario == 'Counterfactual 3' ~ 'United Kingdom',
                              country!='Denmark' & scenario == 'Counterfactual 4' ~ 'Denmark',
                              country=='Denmark' & scenario == 'Counterfactual 4' ~ 'United Kingdom',
                              TRUE ~ scenario))

df.out1 <- spread(df.out1,country,deaths)
df.out2 <- spread(df.out2,country,deaths)
names(df.out1) <- c("", "Denmark", "Sweden", "United Kingdom")
names(df.out2) <- c("", "Denmark", "Sweden", "United Kingdom")

stargazer(df.out1,rownames=F,summary=F)
stargazer(df.out2,rownames=F,summary=F)

df.out1
df.out2

