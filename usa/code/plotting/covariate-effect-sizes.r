library(ggplot2)
library(scales)
library(bayesplot)
library(matrixStats)
library(cowplot)
#library(svglite)
library(boot)

source("usa/code/utils/read-data-usa.r")

round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

make_covariate_effect_size_plot = function(JOBID,  StanModel, ext = ".png", filename=NULL, country="usa", exclude_vars = NULL) {
  if(is.null(filename)) {
    load(paste0(country, '/results/', StanModel, "-", JOBID, '-stanfit.Rdata'))
  } else {
    load(filename)
  }
  
  out = rstan::extract(fit)
  
  my.plot = function(alpha,label,plot_labels=colnames(alpha), ylab = "Mobility\n", 
                     xlabs = c("0%\n(no effect on transmissibility)","25%","50%","75%","100%\n(ends transmissibility)"),
                     xbreaks = seq(0,1,.25),
                     line = 1) {
    data = mcmc_intervals_data(alpha,prob_outer=.95,transformation=function(x) 1-2*inv.logit(-x),point_est="mean")
    
    levels(data$parameter) = gsub("t(", "", levels(data$parameter), fixed=TRUE)
    levels(data$parameter) = sub("\\)$", "", levels(data$parameter), fixed=FALSE)
    data$parameter = (as.character(data$parameter))
    
    no_point_est <- all(data$point_est == "none")
    x_lim <- range(c(data$ll, data$hh))
    x_range <- diff(x_lim)
    x_lim[1] <- x_lim[1] - 0.05 * x_range
    x_lim[2] <- x_lim[2] + 0.05 * x_range
    layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
      vline_0(color = "gray90", size = 0.5)
    } else {
      geom_blank(
        mapping = NULL, data = NULL,
        show.legend = FALSE, inherit.aes = FALSE)
    }
    args_outer <- list(mapping = aes_(x = ~ll, xend = ~hh, y = ~parameter, 
                                      yend = ~parameter)) #, color = bayesplot::get_color("mid"))
    args_inner <- list(mapping = aes_(x = ~l, xend = ~h, y = ~parameter, 
                                      yend = ~parameter), size = 2, show.legend = FALSE)
    args_point <- list(mapping = aes_(x = ~m, y = ~parameter), 
                       data = data, size = 4, shape = 21)
    
    args_point$color <- "blue" #get_color("dark_highlight")
    
    point_func <- geom_point
    layer_outer <- do.call(geom_segment, args_outer)
    layer_inner <- do.call(geom_segment, args_inner)
    layer_point <- do.call(point_func, args_point)
    
    data$parameter = factor(as.character(data$parameter),levels=plot_labels[order(plot_labels,decreasing=T)])
    # data = data[order(-data$m),]
    p = ggplot(data) +theme_bw() +  geom_point(aes(x=m,y=parameter),position = position_dodge(-.5), size = 6) + 
      geom_linerange(aes(xmin=ll,xmax=hh,y=parameter),
                     position = position_dodge(-.5), size=1) + 
      scale_x_continuous(breaks=xbreaks,labels = xlabs,
                         expand=c(0.005,0.005),expression(paste("\nRelative % reduction in  ",R[t])))  +
      #scale_colour_manual(name = "", #labels = c("50%", "95%"),
      #                    values = c(("coral4"), ("seagreen"))) + 
      
      geom_vline(xintercept=c(0,line),colour="darkgray") +
      scale_y_discrete(ylab) +
      theme(plot.margin = margin(0, 2, 0, .5, "cm"),
            text = element_text(size=16))
    print(paste0("saving usa/figures/covars_effect_sizes_", label, "_", JOBID, ext))
    ggsave(paste0("usa/figures/covars_effect_sizes_", label, "_", JOBID, ext), p, height = 6, width = 8)
    
    if (label == "country"){
      reductions <- data.frame("parameter" = data$parameter,
                               "text" = sprintf("%0.1f\\%% [%0.1f\\%% - %0.1f\\%%]", 
                                                data$m*100, 
                                                round.choose(data$ll*100, 0.1, 0), 
                                                round.choose(data$hh*100, 0.1, 1)))
      saveRDS(reductions, paste0("usa/results/", JOBID, "-covariate-reductions.RDS"), version = 2)
    }

  }
  
  
  
  if(length(exclude_vars) != 0) {
    alpha = data.frame(as.matrix(out$alpha[,-exclude_vars]))
    colnames(alpha) = labels(terms(formula))[-exclude_vars]
  }
  else {
    alpha = data.frame(as.matrix(out$alpha))
    colnames(alpha) = labels(terms(formula))
  }

  colnames(alpha) <- c("Average mobility", "Transit", "Residential")
  my.plot(alpha,"country")
  
  if(exists("formula_partial_regional")) {
     #alpha = data.frame(out$alpha_region[,,2])
    alpha = data.frame(out$alpha_region)
     df <- read_death_data("jhu")
     # region_codes = read.csv("usa/data/usa-regions.csv")
     # region_codes = unique(region_codes[,c("region_census_sub_revised","region_code")])
     # region_codes = region_codes[order(region_codes$region_code),]
     # write.csv(region_codes,"usa/data/usa-region-codes.csv",row.names=F)
     region_names = read.csv("usa/data/usa-region-codes.csv",stringsAsFactors = F)$region_census_sub_revised
     # region_names = unique(df$region_census_sub_revised)
     if(ncol(alpha) > 8) {
      colnames(alpha) = c(region_names,region_names)
      my.plot(alpha[,1:8], plot_labels = colnames(alpha[,1:8]), #c(unique(df$region_census_sub_revised)), 
              label = "regional-intercept", ylab = "Intercept\n", 
              xlabs = c("-50%", "-25%", "0%","25%","50%"),
              xbreaks = seq(-0.5,0.5,.25),
              line = 0)
      my.plot(alpha[,9:16], plot_labels = colnames(alpha[,9:16]), #c(unique(df$region_census_sub_revised)), 
              label = "regional-mobility", ylab = "Average mobility\n", 
              xlabs = c("-50%", "-25%", "0%","25%","50%"),
              xbreaks = seq(-0.5,0.5,.25),
              line = 0)

     } else{
       colnames(alpha) = region_names
       my.plot(alpha, plot_labels = colnames(alpha), #c(unique(df$region_census_sub_revised)), 
               label = "regional", ylab = "Average mobility\n", 
               xlabs = c("-50%", "-25%", "0%","25%","50%"),
               xbreaks = seq(-0.5,0.5,.25),
               line = 0)
       
     }
     
  }
  if(exists("formula_partial_state")) {
    alpha = data.frame(out$alpha_state)
    colnames(alpha) = states
    transit_states <- c('NY', 'DC', 'MA', 'CA', 'WA', 'IL', 'MD', 'NJ')
    transit_states <- transit_states[order(transit_states)]
    alpha <- select(alpha, transit_states)
    colnames(alpha) <- c("California", "District of Columbia", "Illinos", "Massachusetts", "Maryland", "New Jersey", "New York" ,"Washington")
    my.plot(alpha, plot_labels = c("California", "District of Columbia", "Illinos", "Massachusetts", "Maryland", "New Jersey", "New York" ,"Washington"), label = "state", ylab = "Transit\n", 
            xlabs = c("-50%", "-25%", "0%","25%","50%"),
            xbreaks = seq(-0.5,0.5,.25),
            line = 0)
  }
}

#make_covariate_effect_size_plot(1063344,"regional-state",filename="~/stanfits/regional-state-1063344-stanfit.Rdata")
#make_covariate_effect_size_plot(294774,"regional-state",filename="~/stanfits/regional-state-294774-stanfit.Rdata")

