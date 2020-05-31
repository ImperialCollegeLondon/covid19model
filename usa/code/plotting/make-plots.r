source("usa/code/plotting/format-data-plotting.r")
source("usa/code/plotting/make-three-panel-plots.r")
source("usa/code/plotting/make-rt-plot.r")

library(ggpubr)

make_plots_all <- function(filename, SIM=FALSE, label = "", last_date_data,
                           ext = ".png", group = NULL){
  print("Making three panel plots")
  load(filename)
  out <- rstan::extract(fit)
  
  rt_data_long <- NULL
  rt_data_wide <- NULL
  if(!SIM){
    
    all_data_out <- data.frame()
  }
  for (i in 1:length(states)){
    
    print(states[i])
    data_state_plot <- format_data(i = i, dates = dates, states = states, 
                                   estimated_cases_raw = estimated_cases_raw, 
                                   estimated_deaths_raw = estimated_deaths_raw, 
                                   reported_cases = reported_cases,
                                   reported_deaths = reported_deaths,
                                   out = out)
    colnames_csv <- c("date","state", "reported_cases", "predicted_cases","cases_min", "cases_max",
                      "reported_deaths","estimated_deaths",
                      "deaths_min", "deaths_max","rt", "rt_min","rt_max")
    if (!SIM){
      data_state_out_temp <- data_state_plot[,colnames_csv]
      colnames(data_state_out_temp) <- c("date","state", "reported_cases", 
                                         "predicted_infections_mean","predicted_infections_lower_CI_95", "predicted_infections_higher_CI_95_cumulative",
                                         "reported_deaths", "estimated_deaths_mean", "estimated_deaths_lower_CI_95", "estimated_deaths_higher_CI_95",
                                         "mean_time_varying_reproduction_number_R(t)", "time_varying_reproduction_number_R(t)_lower_CI_95",
                                         "time_varying_reproduction_number_R(t)_higher_CI_95")
      all_data_out <- rbind(all_data_out, data_state_out_temp)
    }
    # Cuts data on last_data_date
    data_state_plot <- data_state_plot[which(data_state_plot$date <= last_date_data),]
    
    # Read in covariates
    covariates <- readRDS("usa/data/covariates.RDS")
    covariates$Quarantine = NULL
    covariates$GathRecomAny = NULL
    covariates_long <- gather(covariates[which(covariates$StatePostal == states[i]), 
                                         2:ncol(covariates)], 
                              key = "key", value = "value")
    covariates_long$x <- rep(NA, length(covariates_long$key))
    covariates_long$time <- rep("start", length(covariates_long$key))
    
    covariates_ended <- readRDS("usa/data/covariates_ended.RDS")
    covariates_ended$Quarantine = NULL
    covariates_ended$GathRecomAny = NULL
    covariates_ended_long <- gather(covariates_ended[which(covariates_ended$StatePostal == states[i]), 
                                                     2:ncol(covariates_ended)], 
                                    key = "key", value = "value")
    covariates_ended_long$x <- rep(NA, length(covariates_ended_long$key))
    covariates_ended_long$time <- rep("ease", length(covariates_ended_long$key))
    
    covariates_long <- rbind(covariates_long, covariates_ended_long)
    covariates_long$time <- factor(covariates_long$time, levels = c("start", "ease"))
    
    un_dates <- unique(covariates_long$value)
    for (k in 1:length(un_dates)){
      idxs <- which(covariates_long$value == un_dates[k])
      max_val <- ceiling(max(data_state_plot$rt_max)) + 0.3
      for (k in idxs){
        covariates_long$x[k] <- max_val
        max_val <- max_val - 0.3
      }
    }
    
    date_emerg_dec <- covariates_long$value[which(covariates_long$key == "EmergDec" & covariates_long$time == "start")]
    idx_emerg_dec <- which(dates[[i]] == date_emerg_dec)
    if (length(idx_emerg_dec) == 0 || idx_emerg_dec < 3){
      print("Don't simulate before emerg dec")
      rt <- NA
      rt_li <- NA
      rt_ui <- NA
    } else {
      mean_emerg_chain <- rowMeans(out$Rt_adj[,(idx_emerg_dec-3):(idx_emerg_dec+3),i])
      rt <- mean(mean_emerg_chain)
      rt_li <- quantile(mean_emerg_chain, prob=.025)
      rt_ui <- quantile(mean_emerg_chain, prob=.975)
    }
    
    idx <- which(dates[[i]] == last_date_data)
    mean_chain <- rowMeans(out$Rt_adj[,(idx-6):idx,i])
    rt_current <- mean(mean_chain)
    rt_li_current <- quantile(mean_chain, prob=.025)
    rt_ui_current <- quantile(mean_chain,prob=.975)
    
    
    # Collate rt information
    len <- length(data_state_plot$rt)
    rt_data_state_long <- data.frame("state" = c(states[i], states[i]),
                                     "x" = c("start", "end"),
                                     "rt" = c(rt, rt_current),
                                     "rt_min" = c(rt_li, rt_li_current),
                                     "rt_max" = c(rt_ui, rt_ui_current))
    rt_data_long <- rbind(rt_data_long, rt_data_state_long)
    
    
    # Make the three panel plot
    plots <- make_three_panel_plots(data_state_plot, jobid = JOBID, state = states[i], 
                                    covariates_long = covariates_long, label = label,
                                    ext = ext)
  }
  if(!SIM){
    saveRDS(all_data_out, paste0("usa/results/", "three-panel-data-",JOBID,".RDS"))
  }
  # Get state groupings
  groupings <- read.csv("usa/data/usa-regions.csv", stringsAsFactors = FALSE)
  groupings <- select(groupings, code, region_census_sub_revised,  state_name)
  names(groupings) <- c("state", "groupings", "state_name")
  rt_data_long <- left_join(rt_data_long, groupings, by = "state")
  
  print("Making rt plot")
  rt_data_long$x <- factor(rt_data_long$x, levels = c("start", "end"))
  make_rt_point_plot(rt_data_long, JOBID = JOBID, label = label, ext = ext)
  
}
