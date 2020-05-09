make_three_panel_plots <- function(data_country, covariates_long, 
                                   jobid, country, label=label){
  
  data_cases_95 <- data.frame(data_country$date, data_country$cases_min, 
                              data_country$cases_max)
  names(data_cases_95) <- c("date", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$date))
  data_cases_50 <- data.frame(data_country$date, data_country$cases_min2, 
                              data_country$cases_max2)
  names(data_cases_50) <- c("date", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$date))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = date, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_ribbon(data = data_cases, 
                aes(x = date, ymin = cases_min, ymax = cases_max, fill = key)) +
    xlab("") +
    ylab("Daily number of infections") +
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) + 
    scale_y_continuous(labels = comma, expand=c(0,0.1)) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1)) + 
    ggtitle(country) 
  
  data_deaths_95 <- data.frame(data_country$date, data_country$deaths_min, 
                               data_country$deaths_max)
  names(data_deaths_95) <- c("date", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$date))
  data_deaths_50 <- data.frame(data_country$date, data_country$deaths_min2, 
                               data_country$deaths_max2)
  names(data_deaths_50) <- c("date", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$date))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  p2 <-   ggplot(data_country, aes(x = date)) +
    geom_bar(data = data_country, aes(y = reported_deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    xlab("") +
    ylab("Daily number of deaths") +
    scale_y_continuous(labels = comma, expand=c(0,0.1)) + 
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b")) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$date, 
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("date", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$date))
  data_rt_50 <- data.frame(data_country$date, data_country$rt_min2, 
                           data_country$rt_max2)
  names(data_rt_50) <- c("date", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$date))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  plot_labels <- c("Lockdown", "Public events banned", "School and universities closed", "Self isolate if ill", "Social distancing encouraged")
  
  p3 <- ggplot(data_country) +
    geom_ribbon(data = data_rt, aes(x = date, ymin = rt_min, ymax = rt_max, 
                                        group = key,
                                        fill = key)) +
    geom_hline(yintercept = 1, color = 'black', size = 1) + 
    geom_segment(data = covariates_long,
                 aes(x = value, y = 0, xend = value, yend = max(x, na.rm=TRUE)), 
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_long, aes(x = value, 
                                           y = x, 
                                           group = key, 
                                           shape = key, 
                                           col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
    scale_shape_discrete(name = "Interventions", labels = plot_labels) + 
    scale_colour_discrete(name = "Interventions", labels = plot_labels) + 
    scale_x_date(date_breaks = "2 weeks", labels = date_format("%e %b"), 
                 limits = c(data_country$date[1], 
                            data_country$date[length(data_country$date)])) + 
    scale_y_continuous(expand=c(0,0.1)) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  
  p <- plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(1, 1, 2))
  save_plot(filename = paste0("Italy//figures/", country, "-three-panel-", label, "-", jobid, ".png"), 
            p, base_width = 14)

  return (p)
}