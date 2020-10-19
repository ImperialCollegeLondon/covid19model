# this code requires you to install epidemia from 'exponential_new'; default
# branch uses logit link while we need 'exponential' link to mke things
# easy to switch. The command to be used is 
#devtools::install_github('ImperialCollegeLondon/epidemia', ref='exponential_new')
set.seed(1234)
library(here)
library(epidemia)
library(tidyverse)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(scales)
library(EnvStats)
library(gridExtra)
source(here("whatif/utils/plot-with-eta.r"))

o2d<-function(){
  i2o <- EuropeCovid$obs$deaths$i2o
  shape1 <- 5.807; scale1 <- 0.948; # infection to onset https://www.acpjournals.org/doi/10.7326/M20-0504
  shape2 <- 1.45404 ; scale2 <- 10.43547 # using estimated of onset to death from chess data
  x1 <- rgamma(1e6,shape=shape1,scale=scale1) # infection-to-onset distribution
  x2 <- rgamma(1e6,shape=shape2,scale=scale2) # infection-to-onset distribution
  
  f_cached <- ecdf(x1+x2) # empirical cumulative distribtion function
  convolution = function(u) f_cached(u)
  f = rep(NA,length(EuropeCovid$obs$deaths$i2o)) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0)) # first entry
  for(i in 2:length(EuropeCovid$obs$deaths$i2o)) { # all other entries
    f[i] = (convolution(i+.5) - convolution(i-.5))
  }
  return(f)
}

# use daily death data official stats fro DEnmark, Sweden, UK
dat <- read.csv(here("whatif/data/daily-death-data.csv"))
dat <- pivot_longer(dat, cols = -Date, names_to = 'country', values_to = 'deaths')
dat$deaths[is.na(dat$deaths)]=0 # to have all time series starting on the same day 30th Jan 2020
dat <- drop_na(dat) # to drop NAs just incase
dat <- mutate(dat, Date = as.Date(Date, format='%d/%m/%Y'))
dat <- rename(dat, date=Date)
dat <- filter(dat, date <= as.Date("2020-07-01")) %>% arrange(country, date) # only use data to July

# formatting data to be digested by epidemia
args <- EuropeCovid
args$data <- dat
i2o <- o2d() # note choice here

# deaths are related to infections by using infection to death distribution and ifr
deaths <- epiobs(
  formula = deaths(country, date) ~ 1,
  prior_intercept = rstanarm::normal(location=1,scale = 0.1),
  prior_aux = rstanarm::normal(location=10, scale=2),
  link="identity",
  i2o=i2o * 0.01,
)

# sampling parameters of chains, iterations, seed and samling related parameters
args$algorithm <- "sampling"
args$obs <- list(deaths=deaths)
args$sampling_args <- list(iter=1000, seed=12345, control=list(adapt_delta=0.95,max_treedepth=15))
# for rt just using a weekly random walk for each country with a separate R0
args$rt <- epirt(
  formula = R(country, date) ~ 0 + country + rw(time = week, gr=country, prior_scale = 0.1),
  prior = rstanarm::normal(log(3.5), 0.1)
)

args$prior_tau = rstanarm::exponential(rate = 1)
# make sure for peridod before week of 13th March has same weekly index (same Rt)
args$data$week <- format(args$data$date+3, "%V")
# start random walk on a given date
args$data <- mutate(
  args$data,
  week = replace(week, which(week <= 11), NA)
)
args$pop_adjust <- F
fit <- do.call(epim, args)

# original fit
fit$pop_adjust <- TRUE
fit_orig <- fit

# save Rt
orig_rt = plot_rt(fit,date_breaks = "1 month")
ggsave(here("whatif/figures/original-rt.pdf"),orig_rt,width=11,height=4)

# date from which we will change Rt
changeDate <- as.Date('2020-03-13')
# get index for random walks
nms <- colnames(as.matrix(fit_orig))
idx_swn <- grep("^R\\|rw.*Sweden", nms)
idx_uk <- grep("^R\\|rw.*United_Kingdom", nms)
idx_dnk <- grep("^R\\|rw.*Denmark", nms)
#get index for R0
idx_swn_R0 <- grep("^R\\|countrySweden", nms)
idx_uk_R0 <- grep("^R\\|countryUnited_Kingdom", nms)
idx_dnk_R0 <- grep("^R\\|countryDenmark", nms)
#get index for seeds
idx_swn_seeds <- grep("seeds\\[Sweden\\]", nms)
idx_uk_seeds <- grep("seeds\\[United_Kingdom\\]", nms)
idx_dnk_seeds <- grep("seeds\\[Denmark\\]", nms)
nchains <- length(fit_orig$stanfit@sim$samples)

mat <- as.matrix(fit)

# compute orderings for the draws
order_swn <- order(mat[, idx_swn_R0])
order_uk <- order(mat[, idx_swn_R0])
order_dnk <- order(mat[, idx_dnk_R0])

dnkSeeds = mat[,idx_dnk_seeds]
ukSeeds = mat[,idx_uk_seeds]
swnSeeds = mat[,idx_swn_seeds]

dnkR=exp(mat[,idx_dnk_R0])
swnR=exp(mat[,idx_swn_R0])
ukR=exp(mat[,idx_uk_R0])

pdf(here('whatif/figures/R0_ratios.pdf'))
par(mfrow=c(3,3))
hist(swnR,main="R0 Sweden")
hist(ukR,main="R0 UK")
hist(dnkR,main="R0 Denmark")
hist(swnR/ukR,main="R0 ratio Sweden/UK")
hist(ukR/swnR,main="R0 ratio UK/Sweden")
hist(dnkR/swnR,main="R0 ratio Denmark/Sweden")
hist(swnR/dnkR,main="R0 ratio Sweden/Denmark")
hist(ukR/dnkR,main="R0 ratio UK/Denmark")
hist(dnkR/ukR,main="R0 ratio Denmark/UK")
dev.off()

pdf(here('whatif/figures/PairPlot_R0_seeds.pdf'))
par(mfrow=c(1,3))
plot(dnkR,dnkSeeds,pch=16,main="Denmark",xlab="R0",ylab="Seeds")
plot(ukR,ukSeeds,pch=16,main="UK",xlab="R0",ylab="Seeds")
plot(swnR,swnSeeds,pch=16,main="Sweden",xlab="R0",ylab="Seeds")
dev.off()


pdf(here('whatif/figures/R0_prior.pdf'))
hist(exp(rnorm(1e6,log(3.5),0.1)),100,main="Prior distribution on R0",xlab="R0",ylab="Density",col='red')
dev.off()

# generate counterfactuals
e_orig <- posterior_linpred(fit)

## absolute
e1 <- e_orig
w <- e_orig$time >= changeDate

e1$draws[order_dnk, w & (e1$group == "Denmark")] <- e_orig$draws[order_swn, w & (e_orig$group == "Sweden")]
e1$draws[order_swn, w & (e1$group == "Sweden")] <- e_orig$draws[order_uk, w & (e_orig$group == "United_Kingdom")]
e1$draws[order_uk, w & (e1$group == "United_Kingdom")] <- e_orig$draws[order_swn, w & (e_orig$group == "Sweden")]

## absolute
e2 <- e_orig
w <- e_orig$time >= changeDate

e2$draws[order_dnk, w & (e2$group == "Denmark")] <- e_orig$draws[order_uk, w & (e_orig$group == "United_Kingdom")]
e2$draws[order_swn, w & (e2$group == "Sweden")] <- e_orig$draws[order_dnk, w & (e_orig$group == "Denmark")]
e2$draws[order_uk, w & (e2$group == "United_Kingdom")] <- e_orig$draws[order_dnk, w & (e_orig$group == "Denmark")]

## relative
fit3=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_swn <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_swn_R0]][-(1:warmup)])
  order_uk <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_uk_R0]][-(1:warmup)])
  order_dnk <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_dnk_R0]][-(1:warmup)])
  for (i in 1:length(idx_swn)) {
    fit3$stanfit@sim$samples[[chain]][[idx_swn[i]]][order_swn] <- fit_orig$stanfit@sim$samples[[chain]][[idx_uk[i]]][order_uk]
    fit3$stanfit@sim$samples[[chain]][[idx_uk[i]]][order_uk] <- fit_orig$stanfit@sim$samples[[chain]][[idx_swn[i]]][order_swn]
    fit3$stanfit@sim$samples[[chain]][[idx_dnk[i]]][order_dnk] <- fit_orig$stanfit@sim$samples[[chain]][[idx_swn[i]]][order_swn]
  }
}

fit_corr_plot <- fit_orig
for (chain in 1:nchains) {
  for (i in 1:length(idx_swn)) {
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_swn[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_uk[i]]] 
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_uk[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_swn[i]]]
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_dnk[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_swn[i]]] 
  }
}

pdf(here('whatif/figures/ordering.pdf'),width=8,height=6)
par(mfrow=c(2,2))
plot(swnR,ukR,pch=16,xlab='R0 UK', ylab='R0 Sweden', main="Posterior UK R0 against Sweden R0")
temp_samples=as.matrix(fit_orig)
plot(temp_samples[,idx_uk_R0],temp_samples[,idx_uk[1]],pch=16,xlab="R0 UK",ylab="UK first week random walk",main='Original samples')
temp_samples=as.matrix(fit_corr_plot)
plot(temp_samples[,idx_uk_R0],temp_samples[,idx_uk[1]],pch=16,xlab="R0 UK",ylab="UK first week random walk",main='Naive relative approach')
temp_samples=as.matrix(fit3)
plot(temp_samples[,idx_uk_R0],temp_samples[,idx_uk[1]],pch=16,xlab="R0 UK",ylab="UK first week random walk",main='Ordered relative approach')
dev.off()

#relative
fit4=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_swn <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_swn_R0]][-(1:warmup)])
  order_uk <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_uk_R0]][-(1:warmup)])
  order_dnk <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_dnk_R0]][-(1:warmup)])
  for (i in 1:length(idx_swn)) {
    fit4$stanfit@sim$samples[[chain]][[idx_swn[i]]][order_swn] <- fit_orig$stanfit@sim$samples[[chain]][[idx_dnk[i]]][order_dnk]
    fit4$stanfit@sim$samples[[chain]][[idx_uk[i]]][order_uk] <- fit_orig$stanfit@sim$samples[[chain]][[idx_dnk[i]]][order_dnk]
    fit4$stanfit@sim$samples[[chain]][[idx_dnk[i]]][order_dnk] <- fit_orig$stanfit@sim$samples[[chain]][[idx_uk[i]]][order_uk]
  }
}

## plotting rt
# get posterior infections
seed = 1234
rt <- posterior_rt(fit_orig, seed=seed)
rt_cf1 <- posterior_rt_(fit_orig, eta=e1$draws, seed=seed)
rt_cf2 <- posterior_rt_(fit_orig, eta=e2$draws, seed=seed)
rt_cf3 <- posterior_rt(fit3, seed=seed)
rt_cf4 <- posterior_rt(fit4, seed=seed)
deaths_obs <-args$data$deaths
deaths_fit <- posterior_predict(fit_orig, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf1 <- posterior_predict_(fit_orig, eta=e1$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf2 <- posterior_predict_(fit_orig, eta=e2$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf3 <- posterior_predict(fit3, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf4 <- posterior_predict(fit4, type="deaths",posterior_mean=TRUE, seed=seed)
infections <- posterior_infections(fit_orig,poster_mean=TRUE, seed=seed)
infections_cf1 <- posterior_infections_(fit_orig, eta=e1$draws, posterior_mean=TRUE, seed=seed)
infections_cf2 <- posterior_infections_(fit_orig, eta=e2$draws, posterior_mean=TRUE, seed=seed)
infections_cf3 <- posterior_infections(fit3, posterior_mean=TRUE, seed=seed)
infections_cf4 <- posterior_infections(fit4,posterior_mean=TRUE, seed=seed)

rt$group <- as.character(rt$group)
rt_cf1$group <- as.character(rt_cf1$group)
rt_cf2$group <- as.character(rt_cf2$group)
rt_cf3$group <- as.character(rt_cf3$group)
rt_cf4$group <- as.character(rt_cf4$group)

df <- data.frame(
  date = rt$time, 
  median = apply(rt$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections$draws, 2, function(x) quantile(x, 0.975)),
  group = rt$group
)
df_cf1 <- data.frame(
  date = rt_cf1$time, 
  median = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf1$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.975)),  
  group = rt_cf1$group
)
df_cf2 <- data.frame(
  date = rt_cf2$time, 
  median = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,  
  infections_median = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf2$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.975)),  
  group = rt_cf2$group
)
df_cf3 <- data.frame(
  date = rt_cf3$time, 
  median = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.975)),  
  deaths_median = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,
  infections_median = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf3$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.975)),    
  group = rt_cf3$group
)
df_cf4 <- data.frame(
  date = rt_cf4$time, 
  median = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.975)),    
  deaths_median = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,
  infections_median = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf4$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.975)),    
  group = rt_cf4$group
)



df$colour <- "Original"
df_cf1$colour <- "Scenario 1"
df_cf2$colour <- "Scenario 2"
df_cf3$colour <- "Scenario 3"
df_cf4$colour <- "Scenario 4"
df <- rbind(df, df_cf1, df_cf2, df_cf3, df_cf4)

# making sure we have renamed everything as per scenarios in papers, which is
# abs/real: donot->recpient
source(here("whatif/utils/transform-df-plotting.r"))

######################################################################################################################################################
# Rt
######################################################################################################################################################
date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right") 
p1 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df[df$country%in%c("Denmark") & !df$group%in%c("Fit Dmk"),], 
    size = 1,color="red",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    data = df_originals[df_originals$country%in%c("Denmark") & !df_originals$group%in%c("Fit Dmk"),], 
    size = 1,color='black',
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p1 <- p1 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Denmark")


p2 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df[df$country%in%c("Sweden") & !df$group%in%c("Fit Swe"),], 
    size = 1,color="red",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    data = df_originals[df_originals$country%in%c("Sweden") & !df_originals$group%in%c("Fit Swe"),], 
    size = 1,color='black',
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p2 <- p2 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired")  + ggtitle("Sweden")

p3 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df[df$country%in%c("United_Kingdom") & !df$group%in%c("Fit UK"),], 
    size = 1,color="red",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    data = df_originals[df_originals$country%in%c("United_Kingdom") & !df_originals$group%in%c("Fit UK"),], 
    size = 1,color='black',
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p3 <- p3 + ggplot2::labs(y = "Median Rt") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired")  + ggtitle("United Kingdom")


margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin

g_rt <- grid.arrange(p1,p2,p3,ncol=1)  # default settings
ggsave(here("whatif/figures/scenario-rt.pdf"),g_rt,width=8,height=11)

######################################################################################################################################################
# Deaths
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right") 

p1 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$country%in%c("Denmark"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = deaths_li,ymax=deaths_ui), 
    data = df[df$country%in%c("Denmark"),], 
    size = 1,fill=ci,alpha=0.8,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Denmark"),], 
    width = 0.5,fill='steelblue',alpha=0.3,
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = date, y = max),
    data = df_aux[df_aux$country%in%c("Denmark"),], 
    colour = "white") +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p1 <- p1 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Denmark")


p2 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$country%in%c("Sweden"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = deaths_li,ymax=deaths_ui), 
    data = df[df$country%in%c("Sweden"),], 
    size = 1,fill=ci,alpha=0.8,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("Sweden"),], 
    width = 0.5,fill='steelblue',alpha=0.3,
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = date, y = max),
    data = df_aux[df_aux$country%in%c("Sweden"),], 
    colour = "white") +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p2 <- p2 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("Sweden")


p3 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$country%in%c("United_Kingdom"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = deaths_li,ymax=deaths_ui), 
    data = df[df$country%in%c("United_Kingdom"),], 
    size = 1,fill=ci,alpha=0.8,
  ) +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
    data = df[df$country%in%c("United_Kingdom"),], 
    width = 0.5,fill='steelblue',alpha=0.3,
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = date, y = max),
    data = df_aux[df_aux$country%in%c("United_Kingdom"),], 
    colour = "white") +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p3 <- p3 + ggplot2::labs(y = "Deaths") + ggplot2::facet_wrap(~group,scales="free_y") + scale_color_brewer(palette="Paired") + ggtitle("United Kingdom")

margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin
g_deaths <- grid.arrange(p1,p2,p3,ncol=1)  # default settings
ggsave(here("whatif/figures/scenario-deaths.pdf"),g_deaths,width=8,height=11)


######################################################################################################################################################
# Infections
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme(legend.position = "right") 

p1 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$country%in%c("Denmark"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui), 
    data = df[df$country%in%c("Denmark"),], 
    size = 1,fill=ci,alpha=0.5,
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p1 <- p1 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Denmark")



p2 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$country%in%c("Sweden"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui), 
    data = df[df$country%in%c("Sweden"),], 
    size = 1,fill=ci,alpha=0.5,
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p2 <- p2 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("Sweden")

p3 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$country%in%c("United_Kingdom"),], 
    size = 1,color=mn,
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui), 
    data = df[df$country%in%c("United_Kingdom"),], 
    size = 1,fill=ci,alpha=0.5,
  ) +
  xlim(as.Date('2020-03-01'), as.Date('2020-07-01'))
p3 <- p3 + ggplot2::labs(y = "Infections") + ggplot2::facet_wrap(~group) + scale_color_brewer(palette="Paired") + ggtitle("United Kingdom")

margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin
g_infections <- grid.arrange(p1,p2,p3,ncol=1)  # default settings
ggsave(here("whatif/figures/scenario-infections.pdf"),g_infections,width=8,height=11)

saveRDS(list(fit=fit_orig,eta1 = e1$draws,eta2 = e2$draws, fit3=fit3, fit4 = fit4, args=args), here('whatif/results/whatif-experiments.rds'))

# getitng table
source(here('whatif/utils/run-summary.r'))
