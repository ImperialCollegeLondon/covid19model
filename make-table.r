library(matrixStats)
args <- commandArgs(trailingOnly = TRUE)
filename <- args[1]
forecast = 7
load(paste0("results/",filename))
print(sprintf("loading: %s",paste0("results/",filename)))
out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths

df_pop= read.csv("data/popt_ifr.csv", stringsAsFactors = FALSE)
df_pop$country[df_pop$country == "United Kingdom"] = "United_Kingdom"

len_dates <- 0
for (date in dates){
  if (length(date) > len_dates){
      len_dates <- length(date)
      dates_all <- date
  }
}

date_till_percentage = date_iso <- as.character(Sys.Date())

cases <- vector("list", length = length(countries))
total_cases <- vector("list", length = length(countries))
total_cases_ui <- vector("list", length = length(countries))
total_cases_li <- vector("list", length = length(countries))
deaths <- vector("list", length = length(countries))
total_deaths <- vector("list", length = length(countries))
rt <- vector("list", length = length(countries))
fraction_infected <- vector("list", length = length(countries))
fraction_infected_li <- vector("list", length = length(countries))
fraction_infected_ui <- vector("list", length = length(countries))
fraction_obs_infected <- vector("list", length = length(countries))
fraction_total_obs_infected <- vector("list", length = length(countries))
y <- vector("list", length = length(countries))

for(i in 1:length(countries)) {
  Country = countries[i]
  x = dates[[i]]
  N = length(x)
  x  = c(x,x[length(x)]+1:forecast)
  padding <- len_dates - length(dates[[i]])
  y[[i]] = c(rep(0, padding),reported_cases[[i]], rep(NA, forecast))
  
  cases[[i]] = c(rep(0, padding), round(colMeans(prediction[,1:length(x),i])))
  
  total_cases[[i]] = c( round(cumsum(colMeans(prediction[,1:length(x),i]))))
  # chk = c(round((colMeans(rowCumsums(prediction[,1:length(x),i])))))
  
  total_cases_li[[i]] = c(
    round((colQuantiles(rowCumsums(prediction[,1:length(x),i]),probs=.025))))
  total_cases_ui[[i]] = c(
    round((colQuantiles(rowCumsums(prediction[,1:length(x),i]),probs=.975))))
  
  deaths[[i]] =  c(rep(0, padding), round(colMeans(estimated.deaths[,1:length(x),i])))
  total_deaths[[i]] =  c(rep(0, padding), round(cumsum(colMeans(estimated.deaths[,1:length(x),i]))))
  rt[[i]] = c(rep(NA, padding), colMeans(out$Rt[,1:length(x),i]))
  
  fraction_infected[[i]] = c(rep(0, padding), total_cases[[i]]/ df_pop[df_pop$country==Country,]$popt)
  fraction_infected_li[[i]] = c(rep(0, padding), 
                                total_cases_li[[i]]/ df_pop[df_pop$country==Country,]$popt)
  fraction_infected_ui[[i]] = c(rep(0, padding), 
                                total_cases_ui[[i]]/ df_pop[df_pop$country==Country,]$popt)
  fraction_obs_infected[[i]] = c(rep(0, padding), y[[i]] / cases[[i]])
  fraction_total_obs_infected[[i]] = c(rep(0, padding), cumsum(y[[i]]) / cases[[i]])
  
  total_cases[[i]]  = c(rep(0, padding),total_cases[[i]])
}

dates_all  = c(dates_all,dates_all[length(dates_all)]+1:forecast)
cases <- do.call(rbind, cases)
cases_df <- as.data.frame(cases)
names(cases_df) <- dates_all
cases_df$countries <- countries
# write.csv(cases_df, "figures/cases.csv")

total_cases <- do.call(rbind, total_cases)
total_cases_df <- as.data.frame(total_cases)
names(total_cases_df) <- dates_all
total_cases_df$countries <- countries
# write.csv(total_cases_df, "figures/total_cases.csv")

deaths <- do.call(rbind, deaths)
deaths_df <- as.data.frame(deaths)
names(deaths_df) <- dates_all
deaths_df$countries <- countries
# write.csv(deaths_df, "figures/deaths.csv")

total_deaths <- do.call(rbind, total_deaths)
total_deaths_df <- as.data.frame(total_deaths)
names(total_deaths_df) <- dates_all
total_deaths_df$countries <- countries
# write.csv(total_deaths_df, "figures/total_deaths.csv")

rt <- do.call(rbind, rt)
rt_df <- as.data.frame(rt)
names(rt_df) <- dates_all
rt_df$countries <- countries
# write.csv(rt_df, "figures/rt.csv")

fraction_infected <- do.call(rbind, fraction_infected)
fraction_infected_df <- as.data.frame(fraction_infected)
names(fraction_infected_df) <- dates_all
fraction_infected_df$countries <- countries
# write.csv(fraction_infected_df, "figures/fraction_infected.csv")

fraction_infected_li <- do.call(rbind, fraction_infected_li)
fraction_infected_li_df <- as.data.frame(fraction_infected_li)
names(fraction_infected_li_df) <- dates_all
fraction_infected_li_df$countries <- countries
# write.csv(fraction_infected_li_df, "figures/fraction_infected_li.csv")

fraction_infected_ui <- do.call(rbind, fraction_infected_ui)
fraction_infected_ui_df <- as.data.frame(fraction_infected_ui)
names(fraction_infected_ui_df) <- dates_all
fraction_infected_ui_df$countries <- countries
# write.csv(fraction_infected_ui_df, "figures/fraction_infected_ui.csv")

total_infected = data.frame(countries=countries,mean=fraction_infected[,dates_all == date_till_percentage],
                            li=fraction_infected_li[,dates_all == date_till_percentage],ui=fraction_infected_ui[,dates_all == date_till_percentage])
total_infected$value = sprintf("%.02f%% [%.02f%%-%.02f%%]",
                              total_infected$mean*100,total_infected$li*100,total_infected$ui*100)
total_infected[order(total_infected$countries),c("countries","value")]

total_infected <- total_infected[,c("countries","value")]
write.csv(total_infected,paste0("results/total_infected_",date_till_percentage,".csv"),row.names=F)

# Store copy for web output
dir.create("web/data/", showWarnings = FALSE, recursive = TRUE)
write.csv(total_infected,paste0("web/data/total_infected.csv"),row.names=F)

fraction_obs_infected <- do.call(rbind, fraction_obs_infected)
fraction_obs_infected_df <- as.data.frame(fraction_obs_infected)
names(fraction_obs_infected_df) <- dates_all
fraction_obs_infected_df$countries <- countries
# write.csv(fraction_obs_infected_df, "figures/fraction_obs_infected.csv")


fraction_total_obs_infected <- do.call(rbind, fraction_total_obs_infected)
fraction_total_obs_infected_df <- as.data.frame(fraction_total_obs_infected)
names(fraction_total_obs_infected_df) <- dates_all
fraction_total_obs_infected_df$countries <- countries
# write.csv(fraction_total_obs_infected_df, "figures/fraction_total_obs_infected.csv")

