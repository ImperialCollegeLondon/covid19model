calculate_infectiousness = function(states,out, JOBID) {
  infectiousness_all <- vector("list", length = length(states))
  all_data_out <- data.frame()
  for (i in 1:length(states)){
    state <- states[[i]]
    N <- length(dates[[i]])
    print(state)
    inf <- colMeans(out$infectiousness[,1:N,i])
    inf_li <- colQuantiles(out$infectiousness[,1:N,i],prob=.025)
    inf_ui <- colQuantiles(out$infectiousness[,1:N,i],prob=.975)
    inf_li2 <- colQuantiles(out$infectiousness[,1:N,i],prob=.25)
    inf_ui2 <- colQuantiles(out$infectiousness[,1:N,i],prob=.75)
    state_data  <- data.frame("date" = dates[[i]],
                              "state" = rep(state, length(dates[[i]])),
                              "infectioussness" = inf,
                              "infectioussness_li" = inf_li,
                              "infectioussness_ui" = inf_ui,
                              "infectioussness_li2" = inf_li2,
                              "infectioussness_ui2" = inf_ui2)
    infectiousness_all[[i]] <- state_data
    state_data_csv <- state_data[,c("date", "state", "infectioussness", "infectioussness_li", "infectioussness_ui")]
    colnames(state_data_csv) <- c("date", "state", "mean_infectious_individuals", "infectious_individuals_lower_CI_95", "infectious_individuals_higher_CI_95")
    all_data_out <- rbind(all_data_out, state_data_csv)
  }
  saveRDS(all_data_out, paste0("usa/results/", "infectious-individuals-out-",JOBID,".RDS")) 
  saveRDS(infectiousness_all, paste0('usa/results/infectiousness_all_', JOBID, '.RDS'))
}