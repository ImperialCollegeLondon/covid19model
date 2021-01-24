# post-processing-state-pars.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-state-pars.R \n \n -------------------------------- \n")


suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(magick, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes: olli
if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015i3_cmdstanv'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results'
  args_dir[['job_tag']] <- '5states_Oct29_Levin7_schoolbound2,5states_Oct29_Levin7_schoolbound4,5states_Oct29_Levin7_schoolbound6'
  args_dir[['school_bound']] <- '2,4,6'
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-school_bound')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['school_bound']] <- args_line[[8]]
} 

args_dir$job_tag <- strsplit(args_dir$job_tag,',')[[1]]
args_dir$school_bound <- strsplit(args_dir$school_bound,',')[[1]]

## start script
cat(" \n -------------------------------- with post-processing arguments -------------------------------- \n")
str(args_dir)

for(i in 1:length(args_dir$school_bound)){
  assign(paste0("outfile.base", args_dir$school_bound[i]), paste0(args_dir$out_dir,"/", args_dir$stanModelFile , "-", args_dir$job_tag[i], "/",
                                                                args_dir$stanModelFile , "-", args_dir$job_tag[i]) )
}


# load inputs for this script
plot.pars.basic = vector('list',length(args_dir$school_bound))
for(i in args_dir$school_bound){
  file <- paste0(get(paste0("outfile.base", i)),'-stanout-basic.RDS')
  cat("\n read RDS:", file)
  plot.pars.basic[[i]] = readRDS(file)
}

# inferred
tmp1 = data.table(school_bound = rep(args_dir$school_bound,2),
                  loc = rep(c('FL','TX'), each = length(args_dir$school_bound)),
                  attack_rate = c(0.0133, 0.0133, 0.0132, 0.026, 0.0262, 0.026))

# clean stan data
x = seq(-0.1,0.2,0.0001)
tmp = list(); j = 1
for(m in seq_along(plot.pars.basic[[args_dir$school_bound[1]]]$regions)){
  #m = 1
  
  if(plot.pars.basic[[args_dir$school_bound[1]]]$stan_data$school_case_data[m,1] <= 0) next
  
  for(i in args_dir$school_bound){
    llik_par = plot.pars.basic[[i]]$stan_data$school_case_data[m,]
    tmp[[j]] = data.table(loc = plot.pars.basic[[i]]$regions[m],
                          x = x,
                          lik = pnorm(x, llik_par[1], llik_par[2], log = T) + pnorm(-x, -llik_par[3], llik_par[4], log = T),
                          school_bound = i)
    tmp1[loc == plot.pars.basic[[i]]$regions[m] & school_bound == i, eval_llik := pnorm(attack_rate, llik_par[1], llik_par[2], log = T) + pnorm(-attack_rate, -llik_par[3], llik_par[4], log = T)]
    
    j = j + 1
  }
}

tmp = do.call('rbind', tmp)


ggplot(tmp, aes(x = x, y = lik, col = school_bound)) + 
  geom_line() + 
  theme_bw() + 
  labs(x = 'school_attack_rate', y = 'log likelihood') + 
  facet_wrap(~loc, nrow = 2) + 
  geom_point(data = tmp1, aes(x = attack_rate, y = 0), shape = 4, size = 4) + 
  scale_x_continuous(limits = c(0, 0.05)) + 
  scale_y_continuous(limits = c(-100, 0))
ggsave('~/Downloads/log_lik_school_bound.png', w = 6, h = 6)


