library(matrixStats)
library(lubridate,warn.conflicts = FALSE)

make_country_csvs <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]

  load(paste0("results/", filename))

  forecast_days = 7

  for(i in 1:length(countries)){
    N <- length(dates[[i]])
    N2 <- N + forecast_days
    country <- countries[[i]]

    raw_pred <- prediction[,1:N2,i]

    predicted_cases <- colMeans(raw_pred)
    predicted_cases_li = colQuantiles(raw_pred, probs=.025)
    predicted_cases_ui = colQuantiles(raw_pred, probs=.975)

    raw_deaths <- estimated.deaths[,1:N2,i]

    estimated_deaths <- colMeans(raw_deaths)
    estimated_deaths_li <- colQuantiles(raw_deaths, probs=.025)
    estimated_deaths_ui <- colQuantiles(raw_deaths, probs=.975)

    ddates <- as_date(as.character(dates[[i]]))
    ddates <- c(ddates, ddates[length(ddates)] + 1:forecast_days)

    raw_rt <- out$Rt_adj[,1:N2,i]

    rt <- colMeans(raw_rt)
    rt_li <- colQuantiles(raw_rt, probs=.025)
    rt_ui <- colQuantiles(raw_rt, probs=.975)

    raw_reported = reported_cases[[i]][1:N2]
    raw_deaths = deaths_by_country[[i]][1:N2]

    df <- data.frame(
      "time" = ddates,
      "country" = rep(country, N2),
      "reported_cases" = cumsum(raw_reported),
      "reported_deaths" = cumsum(raw_deaths),
      "predicted_cases" = cumsum(predicted_cases),
      "predicted_cases_min" = cumsum(predicted_cases_li),
      "predicted_cases_max" = cumsum(predicted_cases_ui),
      "estimated_deaths" = format(cumsum(estimated_deaths), scientific=FALSE),
      "estimated_deaths_min" = format(cumsum(estimated_deaths_li), scientific=FALSE),
      "estimated_deaths_max" = format(cumsum(estimated_deaths_ui), scientific=FALSE),
      "rt" = rt,
      "rt_min" = rt_li,
      "rt_max" = rt_ui
    )

    write.csv(
      df,
      paste0('results/country-', country, '.csv'),
      row.names=FALSE,
      na=""
    )
  }

}

make_country_csvs()