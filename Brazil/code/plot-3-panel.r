make_data_plot <- function(filename){
  load(paste0('Brazil/results/',filename,'-stanfit.Rdata'))
  interventions <- read.csv2(paste0("Brazil/data/brazil-interventions.csv"), sep=";")
  interventions[,2] <- dmy(as.character(interventions[,2]))
  interventions[,3] <- dmy(as.character(interventions[,3]))
  interventions[,4] <- dmy(as.character(interventions[,4]))
  interventions[,5] <- dmy(as.character(interventions[,5]))
  colnames(interventions) = c("region","Emergency","Retail and Service","Transport","School Closing")
  table_paper = NULL
  for(i in 1:length(countries)){
    print(i)
    N <- length(dates[[i]])
    country <- countries[[i]]
    
    predicted_cases <- colMeans(prediction[,1:N,i])
    predicted_cases_li <- colQuantiles(prediction[,1:N,i], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,i], probs=.975)
    predicted_cases_li2 <- colQuantiles(prediction[,1:N,i], probs=.25)
    predicted_cases_ui2 <- colQuantiles(prediction[,1:N,i], probs=.75)
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,i], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,i], probs=.975)
    estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.25)
    estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.75)
    
    out <- rstan::extract(fit)
    rt <- colMeans(out$Rt_adj[,1:N,i])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
    rt_li2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.25)
    rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.75)
    
    data_country <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "country" = rep(country, length(dates[[i]])),
                               "reported_cases" = reported_cases[[i]],
                               "reported_cases_c" = cumsum(reported_cases[[i]]),
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "predicted_min2" = predicted_cases_li2,
                               "predicted_max2" = predicted_cases_ui2,
                               "deaths" = deaths_by_country[[i]],
                               "deaths_c" = cumsum(deaths_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "death_min2" = estimated_deaths_li2,
                               "death_max2"= estimated_deaths_ui2,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui,
                               "rt_min2" = rt_li2,
                               "rt_max2" = rt_ui2)
    
    aux = data_country[,c("reported_cases","predicted_cases","predicted_min2","predicted_max2","deaths","death_min2","death_max2")]
    aux2 = data.frame(as.character(country),
                      tail(apply(aux, 2, cumsum),1),
                      (df_pop[which(df_pop$region==country),2])
    )
    table_paper = rbind.data.frame(table_paper,aux2)
    data_cases_95 <- data.frame(data_country$time, data_country$predicted_min,
                                data_country$predicted_max)
    names(data_cases_95) <- c("time", "cases_min", "cases_max")
    data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))
    data_cases_50 <- data.frame(data_country$time, data_country$predicted_min2,
                                data_country$predicted_max2)
    names(data_cases_50) <- c("time", "cases_min", "cases_max")
    data_cases_50$key <- rep("fifty", length(data_cases_50$time))
    data_cases <- rbind(data_cases_95, data_cases_50)
    levels(data_cases$key) <- c("ninetyfive", "fifty")
    make_plots(data_country, data_cases, country,filename, interventions)
  }
  colnames(table_paper) = c("State","reported_cases","predicted_cases","predicted_min2","predicted_max2",
                            "deaths","death_min2","death_max2","pop")
  make_table(table_paper)
}
make_plots <-  function(data_country, data_cases, country, filename, interventions){
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(data = data_cases,
                aes(x = time, ymin = cases_min, ymax = cases_max, fill = key)) +
    xlab("") +
    ylab("Daily number of infections\n") +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") + ggtitle(df_region_codes[which(df_region_codes[,1]==country),2]) +
    guides(fill=guide_legend(ncol=1))
  
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min,
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2,
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")+ coord_fixed(ratio = 10)
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    ylab("Daily number of deaths\n") +
    xlab("") +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$time,
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
  data_rt_50 <- data.frame(data_country$time, data_country$rt_min2,
                           data_country$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  # interventions
  # # delete these 2 lines
  covariates_country <- interventions[which(interventions$region == country),-1]
  covariates_country_long <- gather(covariates_country, key = "key",
                                    value = "value")
  covariates_country_long$x <- rep(NULL, length(covariates_country_long$key))
  un_dates <- unique(covariates_country_long$value)
  
  for (k in 1:length(un_dates)){
    idxs <- which(covariates_country_long$value == un_dates[k])
    max_val <- round(max(data_country$rt_max)) + 0.3
    for (j in idxs){
      covariates_country_long$x[j] <- max_val
      max_val <- max_val - 0.3
    }
  }
  
  covariates_country_long$value <- as_date(covariates_country_long$value)
  covariates_country_long$country <- rep(country,
                                         length(covariates_country_long$value))
  
  plot_labels <- c("Emergency","Retail and Service","School Closing","Transport")
  
  p3 <- ggplot(data_country) +
    geom_ribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max,
                                    group = key,
                                    fill = key)) +
    geom_hline(yintercept = 1, color = 'black', size = 0.1) +
    geom_segment(data = covariates_country_long,
                 aes(x = value, y = 0, xend = value, yend = max(x)),
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_country_long, aes(x = value,
                                                   y = x,
                                                   group = key,
                                                   shape = key,
                                                   col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) +
    scale_shape_manual(name = "Interventions", labels = plot_labels,
                       values = c(21, 22, 23, 24, 25, 12)) +
    scale_colour_discrete(name = "Interventions", labels = plot_labels) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"),
                 limits = c(data_country$time[1],
                            data_country$time[length(data_country$time)])) +
    scale_y_continuous(expand = expansion(mult=c(0,0.1))) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  
  
  ptmp <- plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(0.75, 0.75, 1))
  #print(ptmp)
  #save_plot(filename = paste0("Brazil/figures/", country, "_three_pannel_", filename2, ".png"), p, base_width = 14)
  #ggsave(ptmp, file=paste0("Brazil/figures/", country, "_three_pannel_", JOBID,'-',filename2, ".png"), width = 14)
  ggsave(ptmp,
         file=paste0("Brazil/figures/",country,"-three-pannel-", filename, ".png"), width = 14, height = 5)
  
}

make_table <- function(table_paper){
  weight_fatality<-read.csv(paste0("Brazil/data/IFRS-all.csv"))[c("X","State","IFR_Peru_poorer")]
  cfr.by.country<-weight_fatality
  colnames(cfr.by.country)<-c(" ","region","weighted_fatality")
  ifrs=cfr.by.country[c("region","weighted_fatality")]
  colnames(ifrs) = c("State","IFR")
  table=merge(table_paper,ifrs,x.by="State")
  table_f = table[c("State","IFR","pop","deaths","predicted_cases","predicted_min2","predicted_max2")]
  table_f$death_per_capita = table_f$deaths/as.double(table$pop)
  table_f$attackrate = 100*table_f$predicted_cases/table_f$pop
  table_f$attackrate_min2 = 100*table_f$predicted_min2/table_f$pop
  table_f$attackrate_max2 = 100*table_f$predicted_max2/table_f$pop
  table_f$predicted_cases = signif(table_f$predicted_cases, 3)
  table_f$predicted_min2 = signif(table_f$predicted_min2, 3)
  table_f$predicted_max2 = signif(table_f$predicted_max2, 3)
  table_f$IFR = signif(100*table_f$IFR, 2)
  table_f$death_per_capita = round(signif(1000000*table_f$death_per_capita, 3),3)
  table_f$attackrate = signif(table_f$attackrate, 3)
  table_f$attackrate_min2 = signif(table_f$attackrate_min2, 3)
  table_f$attackrate_max2 = signif(table_f$attackrate_max2, 3)
  table_f = table_f[order(table_f$deaths,decreasing = T),]
  table_paper_final=table_f[c("State","IFR","pop","deaths","death_per_capita",
                              "predicted_cases","predicted_min2","predicted_max2",
                              "attackrate","attackrate_min2","attackrate_max2")]
  colnames(table_paper_final)=c("State","IFR","Pop.","Deaths","Deaths ppm",
                                "cases","case5%","case95%",
                                "AR","AR5%","AR95%")
  print(table_paper_final)
  write.csv(table_paper_final, file=paste0("Brazil/figures/TABLE_FINAL_counterfactual_",filename, ".txt"),row.names=FALSE)
}
