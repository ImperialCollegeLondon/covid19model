# df_contacts <- read.csv("data/US_nr_contacts_state_age_100m_2020_06_22.csv")
df_flows <- read.csv("data/US_state_to_state_2020_06_22.csv")
df_flows$day <- as.Date(df_flows$day)
df_flows <- df_flows[order(df_flows$day),]
state_names <- unique(df_flows$STATE_NAME)
max_flow <- max(df_flows$cnt_travelers)

pb <- txtProgressBar(min = 0, max = length(state_names), style = 3)
i <- 0
for (state in state_names) {
  df_flows_state <- df_flows[df_flows$STATE_NAME == state,]
  next_states <- unique(df_flows_state$next_STATE_NAME)
  max_flow_state <- max(df_flows_state$cnt_travelers)
  
  for (next_state in next_states) {
    df_tmp <- df_flows_state[df_flows_state$next_STATE_NAME==next_state,]
    png(paste("usa/figures/state_flows/", state, "_", next_state, ".png"), width=500, height=700)
    par(pty="s")
    plot(df_tmp$day, 
         df_tmp$cnt_travelers,
         ty="l", xaxt="n", xlab="", ylab="Number of Travellers", main=paste(state, " to ", next_state),
         ylim=c(0, max_flow_state))
    axis.Date(1, at = seq(df_tmp$day[1], df_tmp$day[dim(df_tmp)[1]], by = "week"),
              format = "%d-%m-%Y", las = 2)
    dev.off()
  }
  i <- i + 1
  setTxtProgressBar(pb, i)
}


latest_day <- df_flows$day[dim(df_flows)[1]]
df_flow_latest <- df_flows[df_flows$day ==latest_day,] 
for (state in state_names) { 
  df_flows_state <- df_flow_latest[df_flow_latest$STATE_NAME == state,]
  if (dim(df_flows_state)[1] == 0) {
    next
  }
  max_flow_state <- max(df_flows_state$cnt_travelers)
  png(paste("usa/figures/state_flows/latest_", state, ".png"), width=800, height=1000)
  par(pty="s")
  plot(factor(df_flows_state$next_STATE_NAME), df_flows_state$cnt_travelers, 
       ylim=c(0, max_flow_state), pch=16, ylab="Number of Travellers", main=state, xaxt="n")
  axis(1, at=seq_along(df_flows_state$cnt_travelers),labels=df_flows_state$next_STATE_NAME, las=2)
  dev.off()
}

