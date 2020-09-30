aggregate_ifr_data_by_age = function(path_to_file_pop_us, path_to_file_ifrbyage = NULL, ifr.by.age = NULL, max_age = 100)
{
  
  stopifnot(any(!is.null(c(path_to_file_ifrbyage, ifr.by.age))))
  
  if(!is.null(path_to_file_ifrbyage)){
    ifr.by.age <- as_tibble(read.csv(path_to_file_ifrbyage))
  }
  
  if(!is.null(ifr.by.age)){
    ifr.by.age <- as_tibble(ifr.by.age)
  }
  
  stopifnot( c('age','ifr_mean')%in%colnames(ifr.by.age) )
  ifr.by.age <- ifr.by.age %>% 
    dplyr::select(age, ifr_mean, ifr_cl, ifr_cu)
  #    aggregate data on contacts by 1-year bands to data by desired age bands
  pop_by_age <- readRDS(path_to_file_pop_us)
  age_bands <- dplyr::select(pop_by_age[which(!is.na(pop_by_age$code)),], -Total) %>%
    reshape2::melt(id.vars = c("Region", "code")) %>% 
    dplyr::rename(age = variable, pop = value, state = Region) %>% dplyr::select(age) %>% dplyr::distinct()
  age.cat <- sapply(strsplit(gsub("\\[|\\]|\\(|\\+", "", age_bands$age), "-"), as.numeric)
  if (is.na(age.cat[[length(age.cat)]][2])) age.cat[[length(age.cat)]][2] <- 99
  age.cat <- matrix(unlist(age.cat), ncol = 2, byrow = TRUE)
  
  ifr.by.age <- ifr.by.age[1:max_age,] %>% # 0 to 95
    dplyr::mutate(age.agg:= cut(age, breaks=c(age.cat[,1],max_age), right=FALSE, labels=seq_len(length(c(age.cat[,1],max_age))-1L))) %>%
    dplyr::group_by(age.agg) %>%
    dplyr::summarise(ifr_mean:= mean(ifr_mean), 
                     ifr_cl:= mean(ifr_cl), 
                     ifr_cu:= mean(ifr_cu)) %>%            
    ungroup() %>%
    dplyr::rename(age= age.agg)
  
  return(ifr.by.age)
}