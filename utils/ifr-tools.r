
return_ifr <- function (){
    ifr.by.country = read.csv("data/popt_ifr.csv")
    ifr.by.country$country = as.character(ifr.by.country[,2])
    ifr.by.country$country[ifr.by.country$country == "United Kingdom"] = "United_Kingdom"

    return(ifr.by.country)
}