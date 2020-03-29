library(lubridate)
# library(readxl)

# date_offset <- 0
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
# date_iso <- as.character(Sys.Date() - date_offset)
# url <- sprintf(url_string, date_iso)

url_page <- "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
tryCatch({
  code <- download.file(url, "data/COVID-19-up-to-date.csv")
  if (code != 0) {
    stop("Error downloading file")
  }
},
error = function(e) {
  stop(sprintf("Error downloading file '%s': %s, please check %s",
               url, e$message, url_page))
})


d <- read.csv("data/COVID-19-up-to-date.csv", stringsAsFactors = FALSE)
d$t <- lubridate::decimal_date(as.Date(d$dateRep, format = "%d/%m/%Y"))
d <- d[order(d$'countriesAndTerritories', d$t, decreasing = FALSE), ]
names(d)[names(d) == "countriesAndTerritories"] <- "Countries.and.territories"
names(d)[names(d) == "deaths"] <- "Deaths"
names(d)[names(d) == "cases"] <- "Cases"
names(d)[names(d) == "dateRep"] <- "DateRep"
saveRDS(d, "data/COVID-19-up-to-date.rds")
