# print("Fetching data from ECDC")
# fetch_ecdc_error <- system("Rscript data/fetch-ecdc.r",intern=FALSE)
# if(fetch_ecdc_error != 0){
#   stop(sprintf("Error while fetching data from the ECDC! Code: %d", fetch_ecdc_error))
# }

run_base_error <- system("Rscript base.r base",intern=FALSE)
if(run_base_error != 0){
  stop(sprintf("Error while running base.r! Code: %d", run_base_error))
}
