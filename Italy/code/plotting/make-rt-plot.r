# Make arrow RT plot
library(ggplot2)
library(ggrepel)

make_rt_point_plot <- function(rt_data_long, JOBID, label= ""){
  # Only choose end
  rt_data_end <- rt_data_long[which(rt_data_long$x == "end"),]
  rt_data_end <- rt_data_end[order(-rt_data_end$rt),]
  rt_data_end$state <- factor(rt_data_end$state, levels = rt_data_end$state)
  
  rt_data_long$state[which(rt_data_long$state=="Friuli-Venezia_Giulia")]<-"Friuli-Venezia Giulia"
  rt_data_end$state[which(rt_data_end$state=="Friuli-Venezia_Giulia")]<-"Friuli-Venezia Giulia"
    
  p1 <- ggplot(rt_data_end) + 
    geom_point(aes(x = state, y = rt, col=macro),  stat="identity") + 
    geom_errorbar(aes(x = state, ymin = rt_min, ymax = rt_max, col=macro), width=0) + 
    geom_hline(aes(yintercept=1)) + 
    xlab("Region") + ylab(expression(R[t])) + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_colour_discrete(name = "") + 
    coord_flip() + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),legend.position = "bottom")+guides(colour = guide_legend(nrow = 2))
    #ggtitle("Final Rt") 
  p1
  ggsave(paste0("Italy/figures/rt_point_final", "_", label, "_", JOBID, ".pdf"), 
         p1, width = 5, height=10)

  # Only choose start
  rt_data_start <- rt_data_long[which(rt_data_long$x == "start"),]
  rt_data_start <- rt_data_start[order(-rt_data_start$rt),]
  rt_data_start$state <- factor(rt_data_start$state, levels = rt_data_start$state)
  
  rt_data_start$state[which(rt_data_start$state=="Friuli-Venezia_Giulia")]<-"Friuli-Venezia Giulia"
  
  p2 <- ggplot(rt_data_start) + 
    geom_point(aes(x = state, y = rt, col = macro),  stat="identity") + 
    geom_errorbar(aes(x = state, ymin = rt_min, ymax = rt_max, col=macro), width = 0) + 
    geom_hline(aes(yintercept=1)) + 
    xlab("Region") + ylab(expression(R[t])) + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_colour_discrete(name = "") +
    coord_flip() + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),legend.position = "bottom") +guides(colour = guide_legend(nrow = 2))
    #ggtitle("Inital Rt")
  ggsave(paste0("Italy/figures/rt_point_start_", label, "_", JOBID, ".png"), p2, width = 5, height=10)
  
}
