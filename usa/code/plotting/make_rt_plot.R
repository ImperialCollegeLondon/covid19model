# Make arrow RT plot
library(ggplot2)
library(ggstance)
library(ggrepel)

make_rt_point_plot <- function(rt_data_long, label, JOBID, ext = ".png"){
  # Only choose end
  rt_data_end <- rt_data_long[which(rt_data_long$x == "end"),]
  rt_data_end <- rt_data_end[order(-rt_data_end$rt),]
  rt_data_end$state <- factor(rt_data_end$state, levels = rt_data_end$state)
  rt_data_end$state_name <- factor(rt_data_end$state_name, levels = rt_data_end$state_name)
  rt_data_end$grouping <- factor(rt_data_end$groupings)
  p1 <- ggplot(rt_data_end) + 
    geom_point(aes(x = state_name, y = rt, col = groupings),  stat="identity") + 
    geom_errorbar(aes(x = state_name, ymin = rt_min, ymax = rt_max, col = grouping), width=0) + 
    geom_hline(aes(yintercept=1)) + 
    xlab("State") + ylab(expression(R[t])) + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_colour_discrete(name = "") + 
    coord_flip() + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank()) +
    ggtitle("Final Rt") 
  ggsave(paste0("usa/figures/rt_point_final_", label, "_", JOBID, ext), p1, width = 7, height=7)
  
 
  print(sprintf("%.2f%% of states have mean less than 1", (length(which(rt_data_end$rt < 1))/length(rt_data_end$state_name))*100))
  print(sprintf("%.2f%% of states have CI less than 1", (length(which(rt_data_end$rt_max < 1))/length(rt_data_end$state_name))*100))
  saveRDS(c(sprintf("%d", length(which(rt_data_end$rt < 1))), 
            sprintf("%d", length(which(rt_data_end$rt_max < 1)))),
          file = paste0("usa/results/", JOBID, "-rt.RDS"), version = 2)

  # Only choose start
  rt_data_start <- rt_data_long[which(rt_data_long$x == "start"),]
  rt_data_start <- rt_data_start[order(-rt_data_start$rt),]
  rt_data_start$state <- factor(rt_data_start$state, levels = rt_data_start$state)
  rt_data_start$state_name <- factor(rt_data_start$state_name, levels = rt_data_start$state_name)
  rt_data_start$grouping <- factor(rt_data_start$groupings)
  
  p2 <- ggplot(rt_data_start) + 
    geom_point(aes(x = state_name, y = rt, col = groupings),  stat="identity") + 
    geom_errorbar(aes(x = state_name, ymin = rt_min, ymax = rt_max, col = grouping), width = 0) + 
    geom_hline(aes(yintercept=1)) + 
    xlab("State") + ylab(expression(R[t])) + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_colour_discrete(name = "") + 
    coord_flip() + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank()) +
    ggtitle("Inital Rt")
  ggsave(paste0("usa/figures/rt_point_start_", label, "_", JOBID, ext), p2, width = 7, height=7)
  
  rt_levels_state <- rt_data_end$state
  rt_levels_state_name <- rt_data_end$state_name
  rt_data_long$state <- factor(rt_data_long$state, levels = rt_levels_state)
  #rt_data_long$x <- factor(rt_data_long$x, levels = c("end", "start"))
  rt_data_long$state_name <- factor(rt_data_long$state_name, levels = rt_levels_state_name)
  p3 <- ggplot(rt_data_long) + 
    geom_point(aes(y = state_name, x = rt, shape = x, col = groupings),  stat="identity", 
               position=position_dodge(width = -0.5)) + 
    geom_pointrangeh(aes(x = rt, xmin=rt_min, xmax=rt_max, y=state_name, group = x, col = groupings),
                   position = position_dodge(width= -0.5), size = 0.05) + 
    ylab("") + xlab(expression(R[t])) + 
  #  scale_x_log10(expand = c(0, 0),breaks=0:6) + 
    scale_colour_discrete(name = "") + 
    scale_shape_discrete(name = "", labels = c("Emergency Decree", "Current")) + 
    geom_vline(xintercept=0:6,colour="gray",alpha=.5) +
    geom_vline(aes(xintercept=1)) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank())
  ggsave(paste0("usa/figures/rt_point_", label, "_", JOBID, ext), p3, width = 8, height=7)
}
