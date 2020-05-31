#AUTHOR: Ben Jeffrey and OJ Watson
#Copied from https://github.com/ncov-ic/ncov-outputs/blob/usa_incoming_nyt_jh/src/usa_incoming_nyt_jhu/script.R

library(tidyverse)
library(janitor)
library(RCurl)
library(cdlTools)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)
mapping_file <- args[[1]]

state_mapping_data <- read.csv(mapping_file, 
                            sep = "\t", stringsAsFactors=FALSE) #file to map locations to states 

# create the URLs
jhu_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
first <- as.Date("2020-01-22")
ends <- format(lubridate::as_date(first : Sys.Date()), "%m-%d-%Y.csv")
jhu_urls <- paste0(jhu_url, ends)

# check that they exist
jhu_found <- RCurl::url.exists(jhu_urls)

message("Downloading raw data from JHU")
# download them
jhu_dfs <- lapply(jhu_urls[jhu_found], data.table::fread)

jhu_dfs <- lapply(seq_len(length(jhu_dfs)), function(x) {
  jhu_dfs[[x]]$date <- as.Date(gsub(".csv", "", ends[x], fixed=TRUE), "%m-%d-%Y")
  return(jhu_dfs[[x]])
} )

# fix their names for binding
cleaned_dfs <- lapply(jhu_dfs, function(x) {
  
  # make these consistent
  names(x)[which(names(x) %in% c("Province/State", "Province_State"))] <- "Province_State"
  names(x)[which(names(x) %in% c("Country/Region", "Country_Region"))] <- "Country_Region"
  names(x)[which(names(x) %in% c("Last Update", "Last_Update"))] <- "Last_Update"
  
  # Add in combined key
  x$Combined_Key <- gsub("^, |^, , ","", paste(x$Admin2, x$Province_State, x$Country_Region, sep = ", "))
  return(x)
})

# group and bind
jhu_df <- as.data.frame(data.table::rbindlist(cleaned_dfs, fill = TRUE))
nms <- names(jhu_df)
start_nms <- c("Admin2","Province_State","Country_Region")
jhu_df <- jhu_df[,c(start_nms, setdiff(nms, start_nms))]

# data cleaning (x easier to type)
x <- jhu_df

message("Formatting Locations")
# select required columns
cols <- c("Province_State",
        "Country_Region",
        "Confirmed",
        "Deaths",
        "date")
x <- x[,cols]
x <- x[x$Country_Region == "US",]

# get names of states 
states <- cdlTools::stateNames
states$STATENAME[states$STATENAME == "Deleware"] <- "Delaware" #incorrect spelling

states <- states[!(states$STATENAME %in% c("Puerto Rico", "Guam", "American Samoa")),]
if (nrow(states) != 51){
    stop("Incorrect number of states to filter on")
}

#map province_state column to state codes and remove unknown, case insensitive
States_Col_list <- list()
unknown <- c()
for (Loc in unique(x$Province_State)){
    got_code <- FALSE

    loc <- tolower(Loc)
    #if loc is county followed by state code, extracts the state code
    if (length(str_split(loc, ", ")[[1]]) == 2){
        
        if (toupper(str_split(Loc, ", ")[[1]][[2]]) %in% states$STATE){
            code <- str_split(loc, ", ")[[1]][[2]]
            States_Col_list[Loc] <- toupper(code)

            got_code <- TRUE
        } 

    }
    #if loc in the states data from cdltools pull the code 
    if (loc %in% tolower(states$STATENAME) & !(got_code)){

        code <- states[tolower(states$STATENAME) == loc,]$STATE
        States_Col_list[Loc] <- code

    #otherwise searches mapping file for name of provinc
    }else if (loc %in% tolower(state_mapping_data$Location) & !(got_code)){

        s <- state_mapping_data[tolower(state_mapping_data$Location) == loc,]$State
        if (s == "Skip"){
           
            x <- x[!(x$Province_State == Loc),] #remove from dataframe if mapping file says skip
        }else{

            #if not skip takes name of state from mapping file and maps it to code 
            code <- states[tolower(states$STATENAME) == tolower(s),]$STATE
            States_Col_list[Loc] <- code
        }

    #if anything is unknown will throw an error telling user to update mapping file accordingly
    }else if(!got_code){
        unknown <- c(unknown, Loc)
    }
}

#if unknown locations are found will tell user and then break
if (length(unknown) > 0){
    message(sprintf("Unknown location detected: %s", unknown))
    stop("Please update mapping file")
}

#create new column of state codes and populate using States_Col_list
x$State_Code <- rep(NA, nrow(x))
for (i in unique(x$Province_State)){
    code <- States_Col_list[[i]]
    x[x$Province_State == i,]$State_Code <- code
}

x <- x[,c("Confirmed", "Deaths", "date", "State_Code")]

#remove NA
x[is.na(x)] <- 0

#sum deaths and confirmed cases for each date and state 
x <- group_by(x, State_Code, date) %>% 
        summarise_at(c("Confirmed", "Deaths"), sum) %>%
            as.data.frame()


#add rows with zeros for deaths and confirmed cases if there is no data for this state and day
all_dates <- seq(min(x$date),
                max(x$date),
                by = "days")
#must be a list or will be coerced to numeric
all_dates <- as.list(all_dates) 
for (d in all_dates){
    for (s in unique(x$State_Code)){

        if (nrow(x[x$date == d & x$State_Code == s,]) == 0){
            row <- list(s,d,0,0)
            x <- rbind(x, row)
        }
    }
}

x <- x[with(x, order(State_Code, date)), ] #sort by state and date 

#after adding rows for dates with no case or death data need to check that the cumulative case and death columns are still accurate
all <- list()
for (s in unique(x$State_Code)){
    split <- x[x$State_Code == s,] #get data for each state
    
    for (n in seq(1, nrow(split))){ #iterate over row numbers
        c <- split[n,]$Confirmed
        d <- split[n,]$Deaths

        if (n != 1){
            if (d < split[n-1,]$Deaths){ #if less than previous row assign this value to this row)
                split[n,]$Deaths <- split[n-1,]$Deaths
            } 
            if (c < split[n-1,]$Confirmed){
                split[n,]$Confirmed <- split[n-1,]$Confirmed
            }
        }
    }
    #also add daily cases and deaths
    split$daily_cases <- diff(c(0,split$Confirmed))
    split$daily_deaths <- diff(c(0,split$Deaths))

    all[[length(all) + 1]] <- split
}
x <- do.call("rbind", all) #rebuild dataframe

names(x) <- c("code", "date", "cumulative_cases", 
            "cumulative_deaths", "daily_cases", 
            "daily_deaths") #change for consistency with nyt 
        


date_min <- ymd("2020-01-01")

all_data <- NULL

unique_codes <- unique(x$code)
for (i in 1:length(unique_codes)){
  print(i)
  # Subset per state
  state_subset <- x[which(x$code == unique_codes[i]),]
  # Order by date
  state_subset <- state_subset[order(state_subset$date),]
  # Pad previous dates with zeros
  pad <- state_subset$date[1] - date_min
  pad_dates <- date_min + days(1:pad[[1]]-1)
  padded_data <- data.frame("code" = rep(unique_codes[i], pad),
                            "date" = pad_dates,
                            "cumulative_cases" = as.integer(rep(0, pad)),
                            "cumulative_deaths" = as.integer(rep(0, pad)), 
                            "daily_cases" = as.integer(rep(0, pad)),
                            "daily_deaths" = as.integer(rep(0, pad)))
  
  state_data <- bind_rows(padded_data, state_subset)
  
  all_data <- bind_rows(all_data, state_data)
}

saveRDS(all_data, "usa/data/jhu_death_data_padded.rds")

