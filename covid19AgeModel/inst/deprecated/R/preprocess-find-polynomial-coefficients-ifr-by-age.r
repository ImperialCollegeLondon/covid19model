library(tidyr)

find.logit.coef.ifr = function(path_to_ifr_by_age){
  
  ifr.by.age <- as_tibble(read.csv(path_to_ifr_by_age))
  stopifnot( c('age','ifr_mean')%in%colnames(ifr.by.age) )
  ifr.by.age <- ifr.by.age %>% 
    select(age, ifr_mean, ifr_cl, ifr_cu)
  
  x = seq(mean(c(0,9)),mean(c(80,89)),10)
  y.mean = unique(ifr.by.age$ifr_mean)
  
  # fit non linear regression
  mylogit <- glm(y.mean ~ log(x), family = quasibinomial)

  mean_sd_coef <- summary(mylogit)$coefficients[,1:2]
  
  # plot
  # logit = function(p) log(p/(1-p))
  # invlogit = function(x) 1/(1+exp(-x))
  # 
  # par(mfrow = c(1, 2))
  # plot(log(x), logit(y.mean))      # on the original scale
  # 
  # x.pred = 1:100
  # 
  # plot(x.pred, invlogit(glm_coef[1] + glm_coef[2]*log(x.pred)), col = "dodgerblue", lwd = 2, type = "l")
  # points(x, y.mean)
  # 
  
  return(mean_sd_coef)
}



find.polynomial.coef.ifr = function(path_to_ifr_by_age){
  
  ifr.by.age <- as_tibble(read.csv(path_to_ifr_by_age))
  stopifnot( c('age','ifr_mean')%in%colnames(ifr.by.age) )
  ifr.by.age <- ifr.by.age %>% 
    select(age, ifr_mean, ifr_cl, ifr_cu)
  
  x = seq(mean(c(0,9)),mean(c(80,89)),10)
  y.mean = unique(ifr.by.age$ifr_mean)
  
  ## check data
  # par(mfrow = c(1, 2))
  # plot(x, y.mean)      # on the original scale
  # plot(log_x + log_x2, log(y.mean))
  
  
  # fit non linear regression
  log_x = log(x)
  log_x2 = (log(x))^2
  
  mult_lm <- lm(log(y.mean) ~ log_x + log_x2)
  mean_coef <- mult_lm$coefficients
  mean_sd_coef <- summary(mult_lm)$coefficients[,1:2]
  
  # plot fit
  # x.pred = 0:100
  # plot(x.pred, exp(lm_coef[1] + lm_coef[2]*log(x.pred) + lm_coef[3]*log(x.pred)^2), col = "dodgerblue", lwd = 2, type = "l")
  # points(x, y.mean)
  return(mean_sd_coef)
}