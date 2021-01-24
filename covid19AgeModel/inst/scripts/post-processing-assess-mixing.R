# assess-mixing.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running assess-mixing.R \n \n -------------------------------- \n")

library(rstan)
library(data.table)
library(bayesplot)
library(ggplot2)
library(covid19AgeModel)

#	for dev purposes: olli
if(0)
{
	args_dir <- list()
	args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015e1_cmdstanv'
	args_dir[['out_dir']] <- '/Users/or105/Box/OR_Work/2020/2020_covid/age_renewal_usa/base_age_fsq_mobility_201015e1_cmdstanv-4states_AZCTFLNYC_Sep20_Levin'
	args_dir[['job_tag']] <- '4states_AZCTFLNYC_Sep20_Levin'	
}


args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
} 


## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/",
                      args_dir$stanModelFile , "-", args_dir$job_tag)


# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

file <- paste0(outfile.base,'-stanout-fit.RDS')
cat("\n read RDS:", file)
fit <- readRDS( file )

file <- paste0(outfile.base,'-stanout-transmission_pars.RDS')
cat("\n read RDS:", file)
transmission.pars <- readRDS( file )

# get number of loc and age cats
n.loc <- plot.pars.basic$stan_data$M
n.age.cat <- plot.pars.basic$stan_data$A

# make table of parameters
tmp <- fit@par_dims
fit.pars <- data.table(name= names(tmp))
fit.pars <- fit.pars[, {
			dim <- 1L
			if(length(tmp[[name]]))
				dim <- tmp[[name]]
			list(dim=dim)
		}, by='name']


cat("\n ----------- calculate pars with small neff: start ----------- \n")
#
# extract neff 
summary.par = summary(fit)$summary
neff <- as.numeric(summary.par[, which(colnames(summary.par) == "n_eff")])
Rhat <- summary.par[, which(colnames(summary.par) == "Rhat")]

bound <- 500
if(sum(neff < bound,na.rm = TRUE) >0){
  pars.with.small.neff <- summary.par[which(neff < bound),,drop=FALSE]
}else{
  pars.with.small.neff <- summary.par[order(neff)[1:10],]
}

cat("\n Write to file",paste0(outfile.base,'-pars-with-small-neff.rds'))
saveRDS(pars.with.small.neff, file=paste0(outfile.base,'-pars-with-small-neff.rds'),version = 2)

cat("\n ----------- calculate pars with small neff: end ----------- \n")

cat("\n ----------- report sampler diagnostics: start ----------- \n")
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
sampler_diagnostics <- data.table()
for (i in colnames(sampler_params[[1]])) {
  tmp <- data.table(t(sapply(sampler_params, function(x) quantile(x[, i],probs = c(0.025,0.5,0.975)))))
  tmp[, diagnostics:=i ]
  tmp[, chain:= seq_len(length(sampler_params))]
  sampler_diagnostics <- rbind(sampler_diagnostics, tmp)
}

saveRDS(sampler_diagnostics,file=paste0(outfile.base,'-sampler_diagnostics.rds'),version = 2)
cat("\n ----------- report sampler diagnostics: end ----------- \n")


cat("\n ----------- make trace plots: start ----------- \n")
# make trace plots
tryCatch({
	tmp <- subset(fit.pars, dim%in%c(n.loc, n.age.cat))
	for(x in tmp$name)
	{
		make_trace_plot(fit, x, paste0(outfile.base,'-HMC-',x), fig.type='.pdf')	
	}
	
	tmp <- subset(fit.pars, !dim%in%c(n.loc, n.age.cat))
	make_trace_plot(fit, tmp$name, paste0(outfile.base,'-HMC-other-pars'), fig.type='.pdf')		
})
cat("\n ----------- make trace plots: end ----------- \n")



# make some pair plots
cat("\n ----------- make pair plots: start ----------- \n")


# make pair plots for ifr_age and random effects
tryCatch({
	if(all(c('log_ifr_age_base','log_ifr_age_rnde_mid1','log_ifr_age_rnde_mid1','log_ifr_age_rnde_old')%in%fit.pars$name))
	{
		target.pars <- c('log_ifr_age_base','log_ifr_age_rnde_mid1','log_ifr_age_rnde_mid1','log_ifr_age_rnde_old')
		tmp <- data.table(name= names(fit)[ grepl(paste(paste0('^',target.pars),collapse = '|'),names(fit)) ])
		cat("\nMake pairs plot of pars:", tmp$name)
		make_pairs_plot(fit, tmp$name, paste0(outfile.base,'-HMC-log_ifr_age'), h=nrow(tmp)*3, w=nrow(tmp)*3, fig.type='.pdf')
	}
})

# make pair plots for beta, upswing_rnd, rho0 per state
tryCatch({
  if(all(c('beta',"dip_rnde",'upswing_rnde', 'rho0')%in%names(transmission.pars)))
  {
    target.pars <- c('beta',"dip_rnde",'upswing_rnde', 'rho0')
    for (i in 1:length(plot.pars.basic$regions)){
      cat("\nMake pairs plot of", target.pars,' for state ',plot.pars.basic$regions[i])
      tmp <- cbind(transmission.pars$beta,transmission.pars$dip_rnde[,i],transmission.pars$upswing_rnde[,i], transmission.pars$rho[,i])
      colnames(tmp) <- c(paste0('beta[',1:ncol(transmission.pars$beta),']'),target.pars[2:4])
      make_pairs_plot_from_samples(tmp, paste0(outfile.base,'-HMC-beta_upswing_rnde_rho0-',plot.pars.basic$regions[i]), h=ncol(tmp)*3, w=ncol(tmp)*3, fig.type='.pdf')
    }
  }
})

# make pair plots "R0", "e_cases_N0","dip_rnde","upswing_rnde", timeeff_shift_age_reduced
tryCatch({
  if(all(c("timeeff_shift_age_reduced", "R0", "e_cases_N0") %in%names(transmission.pars)) )
  {
    target.pars.state <- c("R0", "e_cases_N0")
    target.pars.state <- names(fit)[ grepl(paste(target.pars.state,collapse = '|'),names(fit))]
    target.pars.age <- "timeeff_shift_age_reduced"

    for (i in 1:length(plot.pars.basic$regions)){
      tmp <- grepl(paste0('\\[',i,'\\]'),target.pars.state)		
      tmp <- c( target.pars.age, target.pars.state[tmp])
      h = length(tmp) * 3
      w = length(tmp) * 3
      make_state_pairs_plot(fit, tmp, paste0(outfile.base,'-HMC-timeeff_shift_age_reduced-',plot.pars.basic$regions[i]), h, w, fig.type='.pdf')
    }
  }
})


# make statewise pair plots
tryCatch({
	target.pars.state <- c("R0", "e_cases_N0","dip_rnde","upswing_rnde","^log_ifr_age_rnde_mid1","^log_ifr_age_rnde_mid2","^log_ifr_age_rnde_old","^timeeff_shift_mid1","^timeeff_shift_mid2","^timeeff_shift_old")
	target.pars.state <- names(fit)[ grepl(paste(target.pars.state,collapse = '|'),names(fit))]
	target.pars.global <- c("beta","kappa","tau","log_relsusceptibility_age_reduced","log_reltransmissibility_age_reduced","_log_ifr_age_rnde_mid1","_log_ifr_age_rnde_mid2","_log_ifr_age_rnde_old","_timeeff_shift_mid1","_timeeff_shift_mid2","_timeeff_shift_old")
	target.pars.global <- names(fit)[ grepl(paste(target.pars.global,collapse = '|'),names(fit))]
	
	for (i in 1:length(plot.pars.basic$regions))
	{
		tmp <- grepl(paste0('\\[',i,'\\]'),target.pars.state)		
		tmp <- c( target.pars.global, target.pars.state[tmp])
		h = length(tmp) * 3
		w = length(tmp) * 3
		make_state_pairs_plot(fit, tmp, paste0(outfile.base,'-HMC-state-',plot.pars.basic$regions[i]), h, w, fig.type='.pdf')
	}   
})
cat("\n ----------- make pair plots: end ----------- \n")




cat("\n ----------- run diagnostics: start ----------- \n")
n_div <- check_all_diagnostics(fit,outfile.base)
if (n_div > 0){
  cat("\n ----------- make parallel coordinates plot: start ----------- \n")
  tryCatch({
    make_parcoord_plot(fit,outfile.base,'.pdf')})
  cat("\n ----------- make parallel coordinates plot: end ----------- \n")
}
cat("\n ----------- run diagnostics: end ----------- \n")



cat(" \n -------------------------------- \n \n End assess-mixing.R \n \n -------------------------------- \n")


