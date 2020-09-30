library(tidyr)
library(data.table)

aggregate_contact_rates_period = function(countries, path_to_file_contact, age_bands, path_to_file_population){
  ## PARTS OF THIS CODE IS TAKEN FROM "Contact-patterns" repository BELONGING TO kassteele
  ## https://github.com/kassteele/Contact-patterns
  
  pop.data = read.csv(path_to_file_population) 
  
  # create map to country abbrevation to read inputs
  country.abb = data.table(country = c("Belgium", "Germany", "Finland", "United_Kingdom", "Italy", "Luxembourg", "Netherlands", "Poland"),
                           abb = c("BE", "DE", "FI", "GB", "IT", "LU", "NL", "PL"))
  # Get ages and number of age classes
  age <- 0:99
  n.age <- length(age)
  
  # Define age cateogries
  age.cat <- sapply(strsplit(gsub("\\[|\\]|\\(|\\+", "", age_bands$age), "-"), as.numeric)
  if (is.na(age.cat[[length(age.cat)]][2])) age.cat[[length(age.cat)]][2] <- 99
  age.cat <- matrix(unlist(age.cat), ncol = 2, byrow = TRUE)
  age.cat <- cut(age, breaks = c(age.cat[,1],100), include.highest = FALSE, right = FALSE)
  
  contact.tab.agg = list()
  
  for(Country in countries){
    pop.data.c = subset(pop.data, country == Country & age %in% 0:99)
    
    # Reshape pop.data in wide format.
    pop.data.wide <- cbind(
      data.frame(age = age),
      matrix(pop.data.c$pop, nrow = n.age, ncol = 1, dimnames = list(NULL, c("pop"))))
    pop.data.wide <- cbind(pop.data.wide, age.cat = age.cat)
    
    contact.tab.agg[[Country]] = list()
    
    for(period in c("weekday", "weekend")){
      
      load(path_to_file_contact(country.abb[which(country.abb$country == Country), ]$abb, period))
      
      contact.tab.c.p = polymod.tab
      
      # Create a dataframe contact.data with n.age^2 rows.
      contact.data <- cbind(
        expand.grid(part.age = age, cont.age = age),
        matrix(contact.tab.c.p$m, nrow = n.age^2, ncol = 1, dimnames = list(NULL, c("m"))))
      
      # Add age categories to contact.data and pop.data
      contact.data <- cbind(contact.data, expand.grid(part.age.cat = age.cat, cont.age.cat = age.cat))
      
      # Aggregate population numbers
      pop.data.agg <- aggregate(cbind(pop) ~ age.cat, FUN = sum, data = pop.data.wide)
      
      # Aggegrate contact intensities over ages
      record.id.part <- match(x = contact.data$part.age, table = pop.data.wide$age)
      record.id.part.agg <- match(x = contact.data$part.age.cat, table = pop.data.agg$age.cat)
      contact.data.agg <- within(contact.data, {
        m   <- pop.data.wide[record.id.part, "pop" ]*m  /pop.data.agg[record.id.part.agg, "pop" ]
      })
      contact.data.agg <- aggregate(cbind(m) ~ part.age.cat + cont.age.cat, FUN = sum, data = contact.data.agg)
      
      # Reorder columns
      contact.data.agg <- contact.data.agg[, c("part.age.cat", "cont.age.cat", "m")] 
      
      # rename the age
      contact.data.agg = dplyr::rename(contact.data.agg, part.age = part.age.cat, cont.age = cont.age.cat)
      
      contact.tab.agg[[Country]][[period]] = contact.data.agg
    }
  }
  
 return(contact.tab.agg)
}

map_contact_tab_to_matrix_period = function(countries, contact_tab){
  contact_tab_matrix = list()
  
  for(Country in countries){
    
    contact_tab_matrix[[Country]] = list()
  
      for(period in c("weekend", "weekday")){
      
      contact_tab_matrix.c.p <- pivot_wider(contact_tab[[Country]][[period]], id_cols= part.age, names_from = cont.age, values_from = m)
      stopifnot( all( colnames(contact_tab_matrix.c.p)[-1] == contact_tab_matrix.c.p$part.age ) )
      contact_tab_matrix[[Country]][[period]] <- unname(as.matrix(contact_tab_matrix.c.p[,-1]))
    }
  }
  return(contact_tab_matrix)
}

aggregate_contact_rates = function(countries, contact_tab, age_bands, path_to_file_population){
  
  pop.data = read.csv(path_to_file_population) 
  
  # Get ages and number of age classes
  age <- 0:99
  n.age <- length(age)
  
  # Define age cateogries
  #age.cat <- cut(age, breaks = seq(0, 100, cntct_by), include.highest = FALSE, right = FALSE)
  age.cat <- sapply(strsplit(gsub("\\[|\\]|\\(|\\+", "", age_bands$age), "-"), as.numeric)
  if (is.na(age.cat[[length(age.cat)]][2])) age.cat[[length(age.cat)]][2] <- 99
  age.cat <- matrix(unlist(age.cat), ncol = 2, byrow = TRUE)
  age.cat <- cut(age, breaks = c(age.cat[,1],100), include.highest = FALSE, right = FALSE)
  
  contact.tab.agg = list()
  
  for(Country in countries){
    pop.data.c = subset(pop.data, country == Country & age %in% 0:99)
    
    # Reshape pop.data in wide format.
    pop.data.wide <- cbind(
      data.frame(age = age),
      matrix(pop.data.c$pop, nrow = n.age, ncol = 1, dimnames = list(NULL, c("pop"))))
    pop.data.wide <- cbind(pop.data.wide, age.cat = age.cat)
    
    contact.tab.c = contact_tab[[Country]]
    
    # Create a dataframe contact.data with n.age^2 rows.
    contact.data <- cbind(
      expand.grid(part.age = age, cont.age = age),
      matrix(contact.tab.c$m, nrow = n.age^2, ncol = 1, dimnames = list(NULL, c("m"))))
    
    # Add age categories to contact.data and pop.data
    contact.data <- cbind(contact.data, expand.grid(part.age.cat = age.cat, cont.age.cat = age.cat))
    
    # Aggregate population numbers
    pop.data.agg <- aggregate(cbind(pop) ~ age.cat, FUN = sum, data = pop.data.wide)
    
    # Aggegrate contact intensities over ages
    record.id.part <- match(x = contact.data$part.age, table = pop.data.wide$age)
    record.id.part.agg <- match(x = contact.data$part.age.cat, table = pop.data.agg$age.cat)
    contact.data.agg <- within(contact.data, {
      m   <- pop.data.wide[record.id.part, "pop" ]*m  /pop.data.agg[record.id.part.agg, "pop" ]
    })
    contact.data.agg <- aggregate(cbind(m) ~ part.age.cat + cont.age.cat, FUN = sum, data = contact.data.agg)
    
    # Reorder columns
    contact.data.agg <- contact.data.agg[, c("part.age.cat", "cont.age.cat", "m")] 
    
    # rename the age
    contact.data.agg = dplyr::rename(contact.data.agg, part.age = part.age.cat, cont.age = cont.age.cat)
    
    contact.tab.agg[[Country]] = contact.data.agg
  }
  
  return(contact.tab.agg)
}

map_contact_tab_to_matrix = function(countries, contact_tab){
  contact_tab_matrix = list()
  
  for(Country in countries){
    contact_tab_matrix.c <- pivot_wider(contact_tab[[Country]], id_cols= part.age, names_from = cont.age, values_from = m)
    stopifnot( all( colnames(contact_tab_matrix.c)[-1] == contact_tab_matrix.c$part.age ) )
    contact_tab_matrix[[Country]] <- unname(as.matrix(contact_tab_matrix.c[,-1]))
  }
  return(contact_tab_matrix)
}
