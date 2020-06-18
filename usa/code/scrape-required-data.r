library(lubridate)
library(utils)
library(httr)

## update NYT data
system(paste0("Rscript usa/code/data-scrape/process-death-data-nyt.r"),intern=FALSE)
## update JHU data
system(paste0("Rscript usa/code/data-scrape/process-death-data-jhu.r usa/data/states_mapping_file.tsv"),intern=FALSE)
## update interventions data
url <- "https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv"
url_page <- "https://github.com/COVID19StatePolicy/SocialDistancing"
tryCatch({
  r <- RETRY("GET", url, 
             write_disk("usa/data/USstatesCov19distancingpolicy.csv", overwrite=TRUE))
  
  if (http_error(r)) {
    stop("Error downloading file")
  }
},
error = function(e) {
  stop(sprintf("Error downloading file '%s': %s, please check %s",
               url, e$message, url_page))
})
## process interventions_data
system(paste0("Rscript usa/code/data-scrape/process-interventions.r"),intern=FALSE)
## Update google_mobility
url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
url_page <- "https://www.google.com/covid19/mobility/"
tryCatch({
  r <- RETRY("GET", url, 
             write_disk("usa/data/Global_Mobility_Report.csv", overwrite=TRUE))
  
  if (http_error(r)) {
    stop("Error downloading file")
  }
},
error = function(e) {
  stop(sprintf("Error downloading file '%s': %s, please check %s",
               url, e$message, url_page))
})
## Update visitdata 
system('usa/code/utils/mobility-reg/get-visitdata.sh',intern=FALSE)

## Do prediction from foursquare to google mobility
## please uncomment this only if you want to run this
## we don't gurantee this regression will work if foursqaure changes it data format
# system(paste0("Rscript usa/code/utils/mobility-reg/mobility-regression.r"),intern=FALSE)
