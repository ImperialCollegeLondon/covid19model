library(forecast)
library(data.table)
library(ggplot2)
library(prophet)
library(dplyr)
library(fpp2)

# fips == 36005 Bronx 
# fips == 36047 Kings
# fips == 36061 New York
# fips == 36081 Queens
# fips == 36085 Richmond

mobility.ts.exploration.210113 <- function()
{
	require(data.table)
	require(ggplot2)
	require(ggsci)
	
	infile <- '~/Box/OR_Work/2020/2020_covid/data_examples/US_county_to_county_2020-01-30_2020-10-14_outtravel.rds'
	outdir <- '~/Box/OR_Work/2020/2020_covid/mobility_forecasts'
	
	mi <- readRDS(infile)
	mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	mi[, WDAY2 := strftime(DAY,'%A')]
	mi[, WDAY3 := strftime(DAY,'%u')]
	mi[, WEEK := as.integer(strftime(DAY,'%V'))]
	tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
	setkey(tmp, WDAY3)
	set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
	mi[, WDAY2:=NULL]
	mi <- merge(mi, tmp, by='WDAY3')
	mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
	set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
	setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
	
	#	select NYC 
	fips.select <- c('36005', '36047', '36061', '36081', '36085')
	mis <- subset(mi, FIPS%in%fips.select)
	
	p <- ggplot(mis, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
		geom_point() +
		scale_x_date(breaks='2 weeks') +
		ggsci::scale_color_lancet() +
		theme_bw() +
		facet_wrap(~FIPS, ncol=2, scales='free_y') +
		labs(x='', y='expected number travel journeys from county', colour='') +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nyc.pdf'), p, w=10, h=10)

	#	select NY State and Connecticut
	fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
	mis2 <- subset(mi, FIPS%in%fips.select2)	
	p <- ggplot(mis2, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
		geom_point() +
		scale_x_date(breaks='2 weeks') +
		ggsci::scale_color_lancet() +
		theme_bw() +
		facet_wrap(~FIPS, ncol=4, scales='free_y') +
		labs(x='', y='expected number travel journeys from county', colour='') +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	p <- ggplot(mis2, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
			geom_point() +
			scale_x_date(breaks='2 weeks') +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_wday.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	p <- ggplot(mis2, aes(x=DAY, y=log(CNT_TRAVELERS_ADJ_OUT), group=FIPS, colour=WDAY2)) +
			geom_point() +
			scale_x_date(breaks='2 weeks') +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	ggsave(file=file.path(outdir, 'fc_outofcounty_log_201014_nysct_wday.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	#	first observations:
	#	out travel follows the same overall trend across counties, 
	#	but overall magnitude differs
	#	there is nothing particular about weekends
	#	most counties do not have substantial weekday effects, but there may be some smaller ones

	
	#	ok
	#	let me just look at weekdays, and standardise to the same number of out travels at the start	
	mis2b <- subset(mis2, DAY>='2020-06-05')	
	tmp <- subset(mis2b, DAY=='2020-06-05', select=c(FIPS, CNT_TRAVELERS_ADJ_OUT))
	setnames(tmp, 'CNT_TRAVELERS_ADJ_OUT', 'CNT_TRAVELERS_ADJ_OUT_1')
	mis2b <- merge(mis2b, tmp, by='FIPS')
	mis2b[, CNT_TRAVELERS_ADJ_OUT_M := CNT_TRAVELERS_ADJ_OUT / CNT_TRAVELERS_ADJ_OUT_1]
	tmp <- subset(mis2b, WDAY==1)
	p <- ggplot(tmp, aes(x= DAY, y= CNT_TRAVELERS_ADJ_OUT_M, colour= FIPS, group=FIPS)) +
			geom_line() +
			scale_x_date(breaks='2 weeks') +
			theme_bw() +		
			labs(x='', y='expected number travel journeys from county', colour='county') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)	
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_multtrend.pdf'), p, w=12, h=12, limitsize = FALSE)
	
	#
	#	how strong are day of the week effects?
	mis3 <- subset(mis2b, select=c(FIPS, DAY, CNT_TRAVELERS_ADJ_OUT, WDAY2))
	mis3[, WEEK := strftime(DAY,'%V')]
	mis3 <- subset(mis3, WEEK>23)
	tmp <- subset(mis3, WDAY2=='Monday')
	setnames(tmp, 'CNT_TRAVELERS_ADJ_OUT', 'CNT_TRAVELERS_ADJ_OUT_1')
	tmp <- subset(tmp, select=c(FIPS, WEEK, CNT_TRAVELERS_ADJ_OUT_1))
	mis3 <- merge(mis3, tmp, by=c('FIPS','WEEK'))
	mis3[, CNT_TRAVELERS_ADJ_OUT_M := CNT_TRAVELERS_ADJ_OUT / CNT_TRAVELERS_ADJ_OUT_1]
	
	
	p <- ggplot(mis3, aes(x=WEEK, y= CNT_TRAVELERS_ADJ_OUT_M, colour=WDAY2)) +
		geom_boxplot() +		
		ggsci::scale_color_lancet() +
		labs(x='calendar week', y='relative number travel journeys from county\ncompared to Monday same week', colour='weekday') +
		theme_bw() +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_weekdaytrend.pdf'), p, w=15, h=8, limitsize = FALSE)
	#	second observations: 
	#	out travel follows the same overall trend across counties on a multiplicative scale
	#	weekday effects:
	#		typically there increases Monday - Friday and a drop Sat-Sun,
	#		but the magnitude differs by week
	#	it may not be worth to model weekday effects explicitly
	

	#	start with log normal model on outflows
	#	autocorrelated random effect for each week
}

mobility.ts.lognormal.ar.week.model.210113 <- function()
{
	require(data.table)
	require(ggplot2)
	require(ggsci)
	require(rstan)
	require(bayesplot)
	require(pammtools)
	
	infile <- '~/Box/OR_Work/2020/2020_covid/data_examples/US_county_to_county_2020-01-30_2020-10-14_outtravel.rds'
	outdir <- '~/Box/OR_Work/2020/2020_covid/mobility_forecasts'
	
	mi <- readRDS(infile)
	mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	mi[, WDAY2 := strftime(DAY,'%A')]
	mi[, WDAY3 := strftime(DAY,'%u')]
	mi[, WEEK := as.integer(strftime(DAY,'%V'))]
	tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
	setkey(tmp, WDAY3)
	set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
	mi[, WDAY2:=NULL]
	mi <- merge(mi, tmp, by='WDAY3')
	mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
	set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
	setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
		
	#	select NY State and Connecticut
	fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
	mis2 <- subset(mi, FIPS%in%fips.select2)	
	
	#	subset data to one FIPS
	df <- subset(mis2, FIPS=='36123')
	setkey(df, DAY)
	df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
	df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
	df[, DAY_IDX:= 1:nrow(df)]
	
	#	make forecast data.table
	dff <- data.table(
		STATE_NAME = df$STATE_NAME[1], 
		STATEFP = df$STATEFP[1],	
		FIPS = df$FIPS[1],
		DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
		DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
		)
	dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	dff[, WDAY3 := strftime(DAY,'%u')]
	dff[, WEEK := as.integer(strftime(DAY,'%V'))]
	dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
	setkey(dff, DAY)
	dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
	tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
	set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
	
	# define normal regression using Stan syntax
	lognormal_m1_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];	
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
}
parameters{
	real beta0;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;
		mu = beta0 + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	beta0 ~ normal( 0 , 5 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2:WK_FIT_M1] ~ normal(wk_effect[1:WK_FIT_M2],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	{	
		vector[WK_PRED] wk_effect_pred;
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		for(i in 2:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( wk_effect_pred[i-1], wk_effect_sigma );
		}
		mu_pred = beta0 + wk_effect_pred[ day_to_wk_idx_pred ];
	}
}
"
	# compile model
	lognormal_m1_c <- rstan::stan_model(
		model_name= 'lognormal_m1', 
		model_code = gsub('\t',' ',lognormal_m1_txt)
		)
	
	# prepare stan_data
	stan_data <- list()
	stan_data$N <- nrow(df)	
	stan_data$WK_FIT <- max(df$WEEK_IDX)
	stan_data$y <- df$Y
	stan_data$day_to_wk_idx <- df$WEEK_IDX
	stan_data$N_PRED <- nrow(dff)
	stan_data$WK_PRED <- max(dff$WEEK_IDX)	
	stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
	
	stan_inits <- list()
	stan_inits$beta0 <- round(stan_data$y[1])
	stan_inits$sigma <- .5
	stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
	stan_inits$wk_effect_sigma <- .5
	
	# run
	lognormal_m1_fit <- rstan::sampling(lognormal_m1_c, 
			data=stan_data, 
			warmup=5e2, iter=5e3, chains=4,
			init= list(stan_inits, stan_inits, stan_inits, stan_inits)
			)
	saveRDS(lognormal_m1_fit, file=file.path(outdir, 'lognormal_m1_fit_36123.rds'))
			
	# convergence and mixing
	m1_su <- summary(lognormal_m1_fit)$summary
	m1_su <- m1_su[grepl("beta|sigma|wk_effect|lp",rownames(m1_su)), ] 
	max(m1_su[, 'Rhat'])
	min(m1_su[, 'n_eff'])
	
	# trace plots
	po <- rstan:::extract(lognormal_m1_fit, inc_warmup = TRUE, permuted = FALSE)
	bayesplot:::color_scheme_set("mix-blue-pink")
	p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|lp", n_warmup = 5e2,
			facet_args = list(ncol = 1, labeller = label_parsed))
	pdf(file=file.path(outdir,'lognormal_m1_fit_36123_traces.pdf'), w=10, h=100)
	print(p)
	dev.off()
	
	# pair plot of first 10 parameters
	po <- as.array(lognormal_m1_fit)
	po <- po[,,1:10]
	p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
	ggsave(file=file.path(outdir,'lognormal_m1_fit_36123_pairplot.pdf'), p, w=20, h=20)
	
	# show fit
	po <- rstan::extract(lognormal_m1_fit)
	dfp <- as.data.table(reshape2::melt(po$mu))
	setnames(dfp, 2L, 'DAY_IDX')
	tmp <- as.data.table(reshape2::melt(po$mu_pred))
	setnames(tmp, 2L, 'DAY_IDX')
	set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
	dfp <- rbind(dfp, tmp)
	
	dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
	dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
	tmp <- rbind(df, dff, fill=TRUE)
	dfp <- merge(tmp, dfp, by='DAY_IDX')
	
	p <- ggplot(dfp) +
			pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
			geom_step( aes(x=DAY, y= MU_M), colour='black') +
			geom_point( data=subset(dfp, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
			scale_x_date(breaks='2 weeks', expand=c(0,0)) +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	ggsave(file=file.path(outdir, 'lognormal_m1_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)
	
	
	
	
	
}



infile <- 'data/US_county_to_county_adjusted.csv'
mi <- as.data.table(read.csv(infile, stringsAsFactors = FALSE))
mi <- set(mi,NULL,'X',NULL)


fit.ARIMA20201218 <- function(f)
{
  ny <- mi[fips == f]
  ny <- aggregate(cnt_travelers_adj ~ day, data = ny, sum)
  ny$cnt_travelers_adj <- log(ny$cnt_travelers_adj)
  ny <- as.data.table(ny)
  set(ny, NULL, 'day', ny[, as.Date(day, format = "%Y-%m-%d")])
  
  # change date here for different training and testing
  train <- ny[day >= '2020-06-01' & day <= '2020-08-30'] 
  test <- ny[day >= '2020-08-31' & day <= '2020-09-27']
  
  m <- nrow(train)/7
  
  train_ts <- ts(train$cnt_travelers_adj, start = c(2020, as.numeric(format(train[1,1], "%j"))))
  test_ts <- ts(test$cnt_travelers_adj, start = c(2020, as.numeric(format(test[1,1], "%j"))))
  
  weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  reg <- factor((weekdays(train$day) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('1', '0') )
  reg <- as.numeric(as.vector(reg))
  
  
  # ??? regressor not working, error: rank deficient
  #weekdays2 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')
  #reg <- matrix(nrow = nrow(train), ncol = 7)
  #for (i in 1:7){
  #for (j in 1:nrow(train)){
  #if (weekdays(train$day[j]) == weekdays2[i]){
  #reg[j,i] <- 1
  #}else {
  #reg[j,i] <- 0
  #}
  #}
  #}
  
  
  tsTrain <- ts(train_ts,frequency = 7)
  
  # fit model
  fit1 <- auto.arima(tsTrain, trace=TRUE) 
  fit2 <- auto.arima(tsTrain, xreg = reg, trace=TRUE)
  
  forecast1 <- forecast(fit1, h = 28)
  forecast2 <- forecast(fit2, 28, xreg = reg[1:28]) # reg[1:28,] if matrix
  
  # MAE & MAPE
  MAE1 <- sum(abs(test$cnt_travelers_adj - forecast1$mean))/nrow(test)
  MAE2 <- sum(abs(test$cnt_travelers_adj - forecast2$mean))/nrow(test)
  MAPE1 <- sum(abs((test$cnt_travelers_adj - forecast1$mean)/test$cnt_travelers_adj))/nrow(test)
  MAPE2 <- sum(abs((test$cnt_travelers_adj - forecast2$mean)/test$cnt_travelers_adj))/nrow(test)
  
  E <- data.frame(MAE = c(MAE1,MAE2), MAPE = c(MAPE1,MAPE2))
  row.names(E) <- c("fit1","fit2")
  
  # forecast for next 28 days
  plot(forecast1, PI = T, main = bquote("Forecast for "*.(f)))
  lines(forecast2$mean, col = "red", lwd = 2)
  lines(ts(test_ts,frequency = 7,start = m+1),col = "green", lwd = 2)
  legend("topleft", bty = "n", c("without regressor", "with regressor", "true value"),
         fill = c("blue", "red", "green"))
  
  print(E)
  
  autoplot(tsTrain) + 
    autolayer(fit2$fitted,series = 'with regressor') + 
    autolayer(fit1$fitted,series = 'without regressor')
}



# fit use prophet package ####
prophet.fit20201216 <- function(f){
  holidays <- data_frame(
    holiday = 'weekends',
    ds = as.Date(c('2020-06-06', '2020-06-13', '2020-06-20', '2020-06-27',
                   '2020-07-04', '2020-07-11', '2020-07-18', '2020-07-25',
                   '2020-08-01', '2020-08-08', '2020-08-15', '2020-08-22', '2020-08-29',
                   '2020-09-05', '2020-09-12', '2020-09-19', '2020-09-26')),
    lower_window = 0,
    upper_window = 1)
  
  df <- mi[fips == f]
  df <- aggregate(cnt_travelers_adj ~ day, data = df, sum)
  df$cnt_travelers_adj <- log(df$cnt_travelers_adj)
  df <- as.data.table(df)
  set(df, NULL, 'day', df[, as.Date(day, format = "%Y-%m-%d")])
  df <- df[day >= '2020-06-01' & day <= '2020-08-30'] 
  setnames(df,c('day','cnt_travelers_adj'),c('ds','y'))
  
  changepoint_prior_scale <- c(0.001, 0.01, 0.05, 0.1, 0.5)
  seasonality_prior_scale <- c(0.01, 0.1, 1.0, 5.0, 10.0)
  
  tuning.result <- c('changepoint_prior_scale','seasonality_prior_scale','MAPE')
  
  for (i in changepoint_prior_scale){
    for (j in seasonality_prior_scale){ 
      m <- prophet(changepoint.prior.scale = i, seasonality.prior.scale = j, holidays = holidays)
      m <- add_country_holidays(m, country_name = 'US')
      m <- fit.prophet(m, df)
      df.cv <- cross_validation(m, horizon = 14, units = "days")
      df.p <- performance_metrics(df.cv)
      tuning.result <- rbind(tuning.result,c(i,j,df.p$mape[1]))}
  }
  
  best_param <- as.numeric(tuning.result[which.min(tuning.result[,3]),])
  m <- prophet(changepoint.prior.scale = best_param[1],seasonality.prior.scale = best_param[2],
               holidays = holidays)
  m <- add_country_holidays(m, country_name = 'US')
  m <- fit.prophet(m, df)
  future <- make_future_dataframe(m, periods = 28)
  fcst <- predict(m, future)
  plot(m, fcst) + labs(title = bquote('County with fips'*.(f)))
  df.cv <- cross_validation(m, horizon = 14, units = "days")
  df.p <- performance_metrics(df.cv)
  
  print(df.p)
  print(best_param)
  print(tuning.result)
  plot_cross_validation_metric(df.cv, metric = 'mape')
  plot(m,fcst) + labs(title = bquote('County with fips ' *.(f)))
}



#
seasonal.naive20201216 <- function(f) # plottings good for richmond (fips=36085)
{
  df <- mi[fips == f]
  df <- aggregate(cnt_travelers_adj ~ day, data = df, sum)
  df$cnt_travelers_adj <- log(df$cnt_travelers_adj)
  df <- as.data.table(df)
  set(df, NULL, 'day', df[, as.Date(day, format = "%Y-%m-%d")])
  df <- df[day >= '2020-06-01' & day <= '2020-08-30'] 
  df <- ts(df$cnt_travelers_adj, start = c(2020, as.numeric(format(df[1,1], "%j"))),frequency = 356)
  
  
  wkrcm <- ts(df, frequency = 7)
  
  autoplot(wkrcm) +
    autolayer(naive(wkrcm, h=28)$mean, series="Na�ve") +
    autolayer(snaive(wkrcm, h=28)$mean, series="seasonal na�ve") +
    ggtitle("Richmond mobility forecast with frequency = 7") +
    xlab("week") + ylab("log_cnt_travelers_adj") +
    guides(colour=guide_legend(title="forecast")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  wkrcm2 <- window(wkrcm,start=1,end=c(9,7))
  
  mthrcm <- ts(df, frequency = 28)
  
  mthrcm2 <- window(mthrcm, start = 1, end = c(3,7))
  
  autoplot(window(mthrcm,start = 1))+
    autolayer(snaive(mthrcm2, h=28)$mean, series="Seasonal Naive") +
    xlab('month from 06-01') + ylab('log(cnt_travelers_adj') +
    ggtitle('richmond mobility 6-7') +
    theme(plot.title = element_text(hjust=0.5))
  
  autoplot(window(wkrcm, start=1)) +
    autolayer(naive(wkrcm2,h=28)$mean, series="Na�ve") +
    autolayer(snaive(wkrcm2,h=28)$mean, series="seasonal na�ve") +
    xlab("week") + ylab("log(cnt_travelers_adj") +
    ggtitle("Richmond mobility forecast 6-7") +
    guides(colour=guide_legend(title="forecast")) +
    theme(plot.title = element_text(hjust = 0.5))
}