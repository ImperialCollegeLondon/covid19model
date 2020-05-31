
make_three_panel_plots <- function(data_state, covariates_long,
                                   jobid, state, label = "", ext = ".png"){
  nam <- read.csv("usa/data/states.csv")
  full_name <- (nam$State[which(nam$Abbreviation == state)])
  
  data_cases_95 <- data.frame(data_state$date, data_state$cases_min, 
                              data_state$cases_max)
  names(data_cases_95) <- c("date", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$date))
  data_cases_50 <- data.frame(data_state$date, data_state$cases_min2, 
                              data_state$cases_max2)
  names(data_cases_50) <- c("date", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$date))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  p1 <- ggplot(data_state) +
    geom_bar(data = data_state, aes(x = date, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_ribbon(data = data_cases, 
                aes(x = date, ymin = cases_min, ymax = cases_max, fill = key)) +
    xlab("") +
    ylab("Daily number of infections") +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                 limits = c(as.Date("2020-02-29"), max(data_state$date))) + 
    scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None", 
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)) + 
          #panel.grid.minor = element_blank()) + 
    guides(fill=guide_legend(ncol=1)) + 
    ggtitle("") 
  
  data_deaths_95 <- data.frame(data_state$date, data_state$deaths_min, 
                               data_state$deaths_max)
  names(data_deaths_95) <- c("date", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$date))
  data_deaths_50 <- data.frame(data_state$date, data_state$deaths_min2, 
                               data_state$deaths_max2)
  names(data_deaths_50) <- c("date", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$date))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  if (state == "AK"){
    p2 <-   ggplot(data_state, aes(x = date)) +
      geom_bar(data = data_state, aes(y = reported_deaths, fill = "reported"),
               fill = "coral4", stat='identity', alpha=0.5) +
      geom_ribbon(
        data = data_deaths,
        aes(ymin = death_min, ymax = death_max, fill = key)) +
      xlab("") +
      ylab("Daily number of deaths") +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) + 
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                   limits = c(as.Date("2020-02-5"), max(data_state$date))) +
      scale_fill_manual(name = "", labels = c("50%", "95%"),
                        values = c(alpha("deepskyblue4", 0.55), 
                                   alpha("deepskyblue4", 0.45))) + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "None", 
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12)) + 
      #panel.grid.minor = element_blank()) + 
      guides(fill=guide_legend(ncol=1)) + 
      ggtitle(full_name[[1]]) 
  } else {
    p2 <-   ggplot(data_state, aes(x = date)) +
      geom_bar(data = data_state, aes(y = reported_deaths, fill = "reported"),
               fill = "coral4", stat='identity', alpha=0.5) +
      geom_ribbon(
        data = data_deaths,
        aes(ymin = death_min, ymax = death_max, fill = key)) +
      xlab("") +
      ylab("Daily number of deaths") +
      scale_y_continuous(labels = comma, expand=expansion(mult=c(0,0.1))) + 
      scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                   limits = c(as.Date("2020-02-5"), max(data_state$date))) +
      scale_fill_manual(name = "", labels = c("50%", "95%"),
                        values = c(alpha("deepskyblue4", 0.55), 
                                   alpha("deepskyblue4", 0.45))) + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "None", 
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12)) + 
      #panel.grid.minor = element_blank()) + 
      guides(fill=guide_legend(ncol=1)) + 
      ggtitle(full_name[[1]]) 
  }
  
  
  
  plot_labels <- c("Emergency decree", 
                   "Restrict public events",
                   "Business closure",
                   "Restaurant closure",
                   "School closure",
                   "Stay at home mandate")
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_state$date, 
                           data_state$rt_min, data_state$rt_max)
  names(data_rt_95) <- c("date", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$date))
  data_rt_50 <- data.frame(data_state$date, data_state$rt_min2, 
                           data_state$rt_max2)
  names(data_rt_50) <- c("date", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$date))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  dat_emerg_dec <- covariates_long$value[which(covariates_long$key == "EmergDec" & covariates_long$time == "start")]
  data_state_cut <- data_state[which(data_state$date >= dat_emerg_dec),]
  data_rt_cut <- data_rt[which(data_rt$date >= dat_emerg_dec),]
  
  p3 <- ggplot(data_state_cut) +
    geom_ribbon(data = data_rt_cut, aes(x = date, ymin = rt_min, ymax = rt_max, 
                                        group = key,
                                        fill = key)) +
    geom_hline(yintercept = 1, color = 'black', size = 0.7) + 
    geom_segment(data = covariates_long,
                 aes(x = value, y = 0, xend = value, yend = max(x, na.rm=TRUE)), 
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_long, aes(x = value, 
                                           y = x, 
                                           group = key, 
                                           shape = key, 
                                           col = time), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "Credible intervals", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
    scale_shape_discrete(name = "Interventions", labels = plot_labels) + 
    scale_colour_discrete(name = "Timing", labels = c("Started", "Eased")) + 
    guides(shape = guide_legend(order = 2), col = guide_legend(order = 1), fill = guide_legend(order = 0)) +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                 limits = c(as.Date("2020-02-29"), max(data_state$date))) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    theme(legend.position="right") + 
    ggtitle("") 

    p <- plot_grid(p2, p1, p3, ncol = 3, rel_widths = c(1.2, 1.2, 2.5))
    save_plot(filename = paste0("usa/figures/", state, "_three_panel_", jobid, "_", label, ext), 
              p, base_width = 19, base_height = 4.5)


  return (p)
}
