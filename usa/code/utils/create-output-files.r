create_output_files <- function(JOBID, max_date) {
  three_panel <- readRDS(paste0("usa/results/", "three-panel-data-",JOBID,".RDS"))
  infectious <- readRDS(paste0("usa/results/", "infectious-individuals-out-",JOBID,".RDS"))
  model_output <- merge(three_panel, infectious, by = c('state','date'))
  write.csv(model_output, paste0("usa/results/", "model-output-",max_date,".csv"), row.names = FALSE)
  
  death_scenario <- readRDS(paste0('usa/results/deaths-scenario-out-',JOBID,'.RDS'))
  mobzero <- death_scenario[death_scenario$key=='Constant mobility',]
  mob20 <- death_scenario[death_scenario$key=='Increased mobility 20%',]
  mob40 <- death_scenario[death_scenario$key=='Increased mobility 40%',]
  mobzero <- mobzero[,c("state", "date","estimated_deaths","deaths_min","deaths_max")]
  mob20 <- mob20[,c("state", "date","estimated_deaths","deaths_min","deaths_max")]
  mob40 <- mob40[,c("state", "date","estimated_deaths","deaths_min","deaths_max")]
  colnames(mobzero) <- c("state", "date","constant_mobility_estimated_deaths_mean","constant_mobility_estimated_deaths_lower_CI_95","constant_mobility_estimated_deaths_higher_CI_95")
  colnames(mob20) <- c("state", "date","mobility_increase_20_estimated_deaths_mean","mobility_increase_20_estimated_deaths_lower_CI_95","mobility_increase_20_estimated_deaths_higher_CI_95")
  colnames(mob40) <- c("state", "date","mobility_increase_40__estimated_deaths_mean","mobility_increase_40__estimated_deaths_lower_CI_95","mobility_increase_40_estimated_deaths_higher_CI_95")
  deaths_scenarios <- merge(mobzero, mob20,by = c('state', 'date'))
  deaths_scenarios <- merge(deaths_scenarios, mob40,by = c('state', 'date'))
  deaths_scenarios <- deaths_scenarios[deaths_scenarios$date > max_date,]
  write.csv(deaths_scenarios, file = paste0('usa/results/deaths-scenarios-',max_date,'.csv'), row.names = FALSE)
  
  cases_scenario <- readRDS(paste0('usa/results/cases-scenario-out-',JOBID,'.RDS'))
  mobzero <- cases_scenario[cases_scenario$key=='Constant mobility',]
  mob20 <- cases_scenario[cases_scenario$key=='Increased mobility 20%',]
  mob40 <- cases_scenario[cases_scenario$key=='Increased mobility 40%',]
  
  mobzero <- mobzero[,c("state", "date","predicted_cases","cases_min","cases_max")]
  mob20 <- mob20[,c("state", "date","predicted_cases","cases_min","cases_max")]
  mob40 <- mob40[,c("state", "date","predicted_cases","cases_min","cases_max")]
  
  colnames(mobzero) <- c("state", "date","constant_mobility_estimated_infections_mean","constant_mobility_estimated_infections_lower_CI_95","constant_mobility_estimated_infections_higher_CI_95")
  colnames(mob20) <- c("state", "date","mobility_increase_20_estimated_infections_mean","mobility_increase_20_estimated_infections_lower_CI_95","mobility_increase_20_estimated_infections_higher_CI_95")
  colnames(mob40) <- c("state", "date","mobility_increase_40__estimated_infections_mean","mobility_increase_40__estimated_infections_lower_CI_95","mobility_increase_40_estimated_infections_higher_CI_95")
  cases_scenarios <- merge(mobzero, mob20,by = c('state', 'date'))
  cases_scenarios <- merge(cases_scenarios, mob40,by = c('state', 'date'))
  cases_scenarios <- cases_scenarios[cases_scenarios$date > max_date,]
  write.csv(cases_scenarios, file = paste0('usa/results/infections-scenarios-',max_date,'.csv'), row.names = FALSE)
  
  rt_scenario <- readRDS(paste0('usa/results/rt-scenario-out-',JOBID,'.RDS'))
  mobzero <- rt_scenario[rt_scenario$key=='Constant mobility',]
  mob20 <- rt_scenario[rt_scenario$key=='Increased mobility 20%',]
  mob40 <- rt_scenario[rt_scenario$key=='Increased mobility 40%',]
  
  mobzero <- mobzero[,c("state", "date","rt","rt_min","rt_max")]
  mob20 <- mob20[,c("state", "date","rt","rt_min","rt_max")]
  mob40 <- mob40[,c("state", "date","rt","rt_min","rt_max")]
  
  colnames(mobzero) <- c("state", "date","constant_mobility_mean_time_varying_reproduction_number_R(t)","constant_mobility_time_varying_reproduction_number_R(t)_lower_CI_95","time_varying_reproduction_number_R(t)_Higher_CI_95")
  colnames(mob20) <- c("state", "date","mobility_increase_20_mean_time_varying_reproduction_number_R(t)","mobility_increase_20_time_varying_reproduction_number_R(t)_lower_CI_95","mobility_increase_20_time_varying_reproduction_number_R(t)_Higher_CI_95")
  colnames(mob40) <- c("state", "date","mobility_increase_20_mean_time_varying_reproduction_number_R(t)","mobility_increase_40_time_varying_reproduction_number_R(t)_lower_CI_95","mobility_increase_40_time_varying_reproduction_number_R(t)_Higher_CI_95")
  rt_scenarios <- merge(mobzero, mob20,by = c('state', 'date'))
  rt_scenarios <- merge(rt_scenarios, mob40,by = c('state', 'date'))
  rt_scenarios <- rt_scenarios[rt_scenarios$date > max_date,]
  write.csv(rt_scenarios, file = paste0('usa/results/time-varying-reproduction-number-scenarios-',max_date,'.csv'), row.names = FALSE)
  
}