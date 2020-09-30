# post-processing-sensititivy-ifr-age-prior-antibody.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-sensititivy-ifr-age-prior-antibody.R \n \n -------------------------------- \n")

suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(bayesplot, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(tidyverse, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(ggpubr, quietly = TRUE))
suppressMessages(library(gridExtra, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(1)
{
  args_dir <- list()
  args_dir[['stanModelFile']] <- "base_age_fsq_mobility_200821b4_cmdstanv"
  args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
  args_dir[['job_tag']] <- "37states_Sep2"
  args_dir[['dev_job_tag']] <- "37states_tau1_Levinprior_Sep2"
  args_dir[['overwrite']] <- 0
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-dev_job_tag')
  stopifnot(args_line[[9]]=='-overwrite')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['dev_job_tag']] <- args_line[[8]]
  args_dir[['overwrite']] <- as.integer(args_line[[10]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$dev_job_tag, "/",
                           args_dir$stanModelFile , "-", args_dir$dev_job_tag)

# defined model names
model_names = c("Central model", "Model with log IFR prior constructed \n from the Levin et al. meta-analysis")

#	load antibody estimate
file <- paste0(outfile.base,'-prop_antibody_validationtable.rds')
if(!file.exists(file)) stop("please run post-processing-validate-prop_antibody.R")
artable = readRDS(file)

file <- paste0(outfile.base.dev,'-prop_antibody_validationtable.rds')
if(!file.exists(file)) stop("please run post-processing-validate-prop_antibody.R")
artable.dev = readRDS(file)

da <- copy( artable[[1]] )
da[, model := model_names[1] ]
da.dev <- copy( artable.dev[[1]] )
da.dev[, model := model_names[2] ]
da = rbind(da, da.dev)

da <- subset(da, select=-c(L_est,L_obs))
da <- reshape2::melt(da, measure.vars=c('M_est','CL_est','CU_est','M_obs','CL_obs','CU_obs'))
set(da, NULL, 'value', da[, as.numeric(value)/100])	
da[, mtype:= gsub('est','Estimated', gsub('obs','Survey', gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\2',variable)))]
da[mtype == 'Estimated', mtype:= model]
da[, stat:= gsub('^([A-Za-z]+)_([A-Za-z]+)$','\\1',variable)]
da <- dcast.data.table(da, study+loc+loc_label+lag+date+age_band+mtype+model~stat, value.var='value')

tmp <- subset(da, age_band=='overall')
tmp[, L:= paste0(loc_label, ' (',study,'), ',as.character(date) )]

tmp[,src:='CDC']
tmp$src[tmp$study=='NY study'] <- 'NY study'
tmp <- tmp[order(-src,-loc_label,-date),]
tmp$L <- factor(tmp$L,levels=c(unique(tmp$L)))

tmp[, mtype := factor(mtype, levels = c("Survey", rev(model_names)))]

lags <- unique(tmp$lag)
for (i in 1:length(lags)) {
  tmp_s <- tmp[lag==lags[i]]
   g <- ggplot(tmp_s) +
    geom_point(aes(y=L, x=M, colour=mtype), position=position_dodge(width=0.7)) +
    geom_errorbarh(aes(y=L, xmin=CL, xmax=CU, colour=mtype), position=position_dodge(width=0.7)) +
    scale_x_continuous(labels=scales::percent, expand=c(0,0)) +
    coord_cartesian(xlim=c(0, max(tmp$CU)*1.1)) +
    labs(x='Estimated COVID-19 seroprevalence', y='', colour='') +
    theme_bw()  +
    scale_colour_manual(values = c("#3b0F70FF",  "#F76F5CFF", "gold" ))  +
    theme(legend.position=c(0.3,-0.1),
          plot.margin=unit(c(1,1,2.5,0.5),"cm"),
          legend.text = element_text(size = 10)) +
    guides(color=guide_legend(nrow=2,byrow=TRUE))
  file = paste0(outfile.base, "-sensitivity-ifr-prior","-","prop_antibody_validation_lag",lags[i],".png")
  cat("Write ", file)
  ggsave(file=file,g,w=7, h=10)
  }

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-ifr-age-prior-antibody.R \n \n -------------------------------- \n")
