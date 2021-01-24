# post-processing-sensititivy-antibody.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Start post-processing-sensititivy-antibody.R \n \n -------------------------------- \n")

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
  args_dir[['out_dir']] <- "~/Box\ Sync/2020/R0t/results"
  args_dir[['stanModelFile']] <- "base_age_fsq_mobility_201015f8_cmdstanv"
  args_dir[['job_tag']] <- "40states_tau10_Oct29_Levin"
  args_dir[['stanModelFileDev']] <- "base_age_fsq_mobility_201015e8_cmdstanv"
  args_dir[['dev_job_tag']] <- "40states_Oct29_Levin7"
  args_dir[['overwrite']] <- 0
  args_dir[['period_length']] <- 7
  args_dir[['prefix']] <- 'ifr-prior'
}

#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-stanModelFileDev')	
  stopifnot(args_line[[5]]=='-out_dir')
  stopifnot(args_line[[7]]=='-job_tag')
  stopifnot(args_line[[9]]=='-dev_job_tag')
  stopifnot(args_line[[11]]=='-overwrite')
  stopifnot(args_line[[13]]=='-prefix')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['stanModelFileDev']] <- args_line[[4]]
  args_dir[['out_dir']] <- args_line[[6]]
  args_dir[['job_tag']] <- args_line[[8]]
  args_dir[['dev_job_tag']] <- args_line[[10]]
  args_dir[['overwrite']] <- as.integer(args_line[[12]])
  args_dir[['prefix']] <- as.character(args_line[[14]])
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag, "/",
                       args_dir$stanModelFile , "-", args_dir$job_tag)
outfile.base.dev <- paste0(args_dir$out_dir, "/", args_dir$stanModelFileDev , "-", args_dir$dev_job_tag, "/",
                           args_dir$stanModelFileDev , "-", args_dir$dev_job_tag)

# defined model names
model_names <- c(paste0(args_dir$stanModelFile,'\n',args_dir$job_tag), paste0(args_dir$stanModelFileDev,'\n',args_dir$dev_job_tag))
if(args_dir$prefix == 'ifr-prior'){
  model_names = c("log IFR prior from the Levin et al meta-analysis, version 7", "log IFR prior from Levin et al meta-analysis, version 5")
}
if(args_dir$prefix == 'susceptibility-prior'){
  model_names = c("using relative susceptibility estimates from Zhang and colleagues", "using relative susceptibility estimates from Viner and colleagues")
}

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
da <- as.data.table( reshape2::melt(da, measure.vars=c('M_est','CL_est','CU_est','M_obs','CL_obs','CU_obs')) )
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

tmp[, mtype := factor(mtype, levels = c("Survey", model_names))]

#
# Save
file = paste0(outfile.base, "-sensitivity-", args_dir$prefix,"-","prop_antibody_validation",".rds")
cat("Write file ", file, '\n')
saveRDS(tmp, file = file)

#
# Plot
colors_viridis = viridis(n = 3, begin=0.05,end=0.85,direction=-1,option = "magma")
lags <- unique(tmp$lag)
for (i in 1:length(lags)) {
  tmp_s <- tmp[lag==lags[i]]
   g <- ggplot(tmp_s) +
    geom_point(aes(y=L, x=M, colour=mtype), position=position_dodge(width=0.7)) +
    geom_errorbar(aes(y=L, xmin=CL, xmax=CU, colour=mtype), position=position_dodge(width=0.7)) +
    scale_x_continuous(labels=scales::percent, expand=c(0,0)) +
    coord_cartesian(xlim=c(0, max(tmp$CU)*1.1)) +
    labs(x='Estimated COVID-19 seroprevalence', y='', colour='') +
    theme_bw()  +
    scale_colour_manual(values = colors_viridis)  +
    facet_grid(loc~., scales = "free", switch = "x", space = "free") +
    theme(legend.position='bottom',
          legend.text = element_text(size = 14),
          strip.background=element_blank(),
          strip.text=element_blank()) +
    guides(color=guide_legend(nrow=3,byrow=TRUE))
  file = paste0(outfile.base, "-sensitivity-", args_dir$prefix,"-","prop_antibody_validation_lag",lags[i],".png")
  cat("Write ", file, '\n')
  ggsave(file=file,g,w=8, h=11)
  ggsave(file=gsub('.png','.pdf',file),g,w=8, h=11,dpi=500)
}

cat(" \n -------------------------------- \n \n End post-processing-sensititivy-antibody.R \n \n -------------------------------- \n")
