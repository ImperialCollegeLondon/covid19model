# Make arrow RT plot
library(ggplot2)
library(ggstance)
library(ggrepel)

make_rt_point_plot <- function(rt_data_long, label, JOBID, ext = ".png"){
  rt_levels_state <- unique(rt_data_long$state)
  rt_data_long$state <- factor(rt_data_long$state, levels = rt_levels_state)
  rt_levels_state_name <- unique(rt_data_long$state_name)
  rt_data_long$state_name <- factor(rt_data_long$state_name, levels = rt_levels_state_name)
  
  p1 <- ggplot(rt_data_long) + 
    geom_point(aes(y = state_name, x = rt, shape = x, col = groupings),  stat="identity", 
               position=position_dodge(width = -0.5)) + 
    geom_pointrangeh(aes(x = rt, xmin=rt_min, xmax=rt_max, y=state_name, group = x, col = groupings),
                   position = position_dodge(width= -0.5), size = 0.05) + 
    ylab("") + xlab(expression(R[t])) + 
    scale_x_log10(expand = c(0, 0),breaks=0:6) + 
    scale_colour_discrete(name = "") + 
    scale_shape_discrete(name = "", labels = c("Initial", "Current")) + 
    geom_vline(xintercept=0:6,colour="gray",alpha=.5) +
    geom_vline(aes(xintercept=1)) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank())
  ggsave(paste0("usa/figures/rt_point_", label, "_", JOBID, ext), p1, width = 8, height=7)
}
