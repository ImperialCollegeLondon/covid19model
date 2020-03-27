library(lubridate)
library(readxl)

date_offset <- 0
url_string <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-%s.xlsx"
date_iso <- as.character(Sys.Date() - date_offset)
url <- sprintf(url_string, date_iso)

url_page <- "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
tryCatch({
  code <- download.file(url, "data/COVID-19-up-to-date.xlsx")
  if (code != 0) {
    stop("Error downloading file")
  }
},
error = function(e) {
  stop(sprintf("Error downloading file '%s': %s, please check %s",
               url, e$message, url_page))
})


d <- readxl::read_excel("data/COVID-19-up-to-date.xlsx", progress = FALSE)
d$t <- lubridate::decimal_date(d$DateRep)
d <- d[order(d$'Countries and territories', d$t, decreasing = TRUE), ]
names(d)[names(d) == "Countries and territories"] <- "Countries.and.territories"
saveRDS(d, "data/COVID-19-up-to-date.rds")
