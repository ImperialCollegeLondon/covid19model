## Download Oxford US states' responses to COVID-19

pkg.dir <- system.file(package = "covid19AgeModel" )

url = "https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv"

data = read.csv(url(url))

write.csv(data, file = file.path(pkg.dir, "data", paste0("OxCGRT_US_subnational_", format(Sys.Date(), "%y%m%d"),".csv")), row.names = F)

