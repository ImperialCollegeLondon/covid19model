library(jsonlite)
d=readRDS('data/COVID-19-up-to-date.rds')
countries <- c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  "United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria", 
  "Sweden",
  "Switzerland",
  "Greece",
  "Portugal",
  "Netherlands"
)

web_fix_fonts <- function(path){
  x <- readLines(path)
  y <- gsub("Aerial", "Arial, Helvetica, sans-serif",x)
  y <- gsub("Arimo", "Arial, Helvetica, sans-serif",y)
  y <- gsub("DejaVu Sans", "Arial, Helvetica, sans-serif", y)
  cat(y, file=path, sep="\n")
}

verify_web_output <- function(){
  plot_names <- c("deaths", "forecast", "infections", "rt")
  plot_versions <- c("mobile", "desktop")
  
  args <- commandArgs(trailingOnly = TRUE)
  filename2 <- args[1]
  load(paste0("results/", filename2))
  
  date_results <- list()
  
  print("Verifying plots and fixing fonts")
  for(country in countries) {
    for (plot_version in plot_versions) {
      for (plot_name in plot_names) {
        path = sprintf("web/figures/%s/%s_%s.svg", plot_version, country, plot_name)
        
        if (! file.exists(path)) {
          stop(sprintf("Missing web output during verification: %s", path))
        }
        
        # Fix wrong fonts
        web_fix_fonts(path)
      }
    }
    
    d1=d[d$Countries.and.territories==country,]
    d1$date = as.Date(d1$DateRep,format='%d/%m/%Y')
    latest_date = max(d1$date)
    date_results[[country]] = latest_date
    
  }

  # Fix fonts for covariate change estimates
  web_fix_fonts("web/figures/desktop/covars-alpha-reduction.svg")
  web_fix_fonts("web/figures/mobile/covars-alpha-reduction.svg")
  
  print("Writing latest updates")
  dir.create("web/data/", showWarnings = FALSE, recursive = TRUE)
  write_json(date_results, "web/data/latest-updates.json", auto_unbox=TRUE)
}

print("Verifying web output")
verify_web_output()
print("Web verification successful")
