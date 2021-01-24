# post-processing-make-figure-0.R
# 
###############################################################################

cat(" \n -------------------------------- \n \n Running post-processing-make-figure-0.R \n \n -------------------------------- \n")

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
suppressMessages(library(pdftools, quietly = TRUE))
suppressMessages(library(viridis, quietly = TRUE))
suppressMessages(library(covid19AgeModel, quietly = TRUE))

#	for dev purposes
if(1)
{	
  args_dir <- list()
  args_dir[['stanModelFile']] <- 'base_age_fsq_mobility_201015i3_cmdstanv'
  args_dir[['out_dir']] <- '/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_201015i3_cmdstanv-40states_Oct29_Levin7_schoolbound6'
  args_dir[['out_dir']] <- '~/Box\ Sync/2020/R0t/results/base_age_fsq_mobility_201015i3_cmdstanv-40states_Oct29_Levin7_schoolbound6'
  args_dir[['job_tag']] <- '40states_Oct29_Levin7_schoolbound6'
  args_dir[['overwrite']] <- 0
  args_dir[['with_forecast']] <- 0
}


#	for runtime
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-stanModelFile')	
  stopifnot(args_line[[3]]=='-out_dir')
  stopifnot(args_line[[5]]=='-job_tag')
  stopifnot(args_line[[7]]=='-overwrite')
  stopifnot(args_line[[9]]=='-with_forecast')
  args_dir <- list()
  args_dir[['stanModelFile']] <- args_line[[2]]
  args_dir[['out_dir']] <- args_line[[4]]
  args_dir[['job_tag']] <- args_line[[6]]
  args_dir[['overwrite']] <- as.integer(args_line[[8]])
  args_dir[['with_forecast']] <- as.integer(args_line[[10]])
  args_dir[["include_lambda_age"]] <- 0
} 

## start script
cat(" \n -------------------------------- \n with post-processing arguments \n -------------------------------- \n")
str(args_dir)

outfile.base <- paste0(args_dir$out_dir, "/", args_dir$stanModelFile , "-", args_dir$job_tag)

cat(" \n -------------------------------- \n summarise case samples: start \n -------------------------------- \n")

# load inputs for this script
file <- paste0(outfile.base,'-stanout-basic.RDS')
cat("\n read RDS:", file)
plot.pars.basic <- readRDS(file)

# map model age groups to report age groups
age_cat_map <- make_age_cat_map_7(plot.pars.basic$pop_info)

# Selected loc for figure 0
selected_loc = 'CA'


#
#	summarise effectively infectious cases by age
file <- paste0(outfile.base,'-summary-eff-infectious-cases-age.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
  file2 <- paste0(outfile.base,'-stanout-transmission_pars.RDS')
  cat("\n read RDS:", file2)
  plot.pars.trmspars <- readRDS(file2)
  file2 <- paste0(outfile.base,'-stanout-impact_intv-gqs.RDS')
  cat("\n read RDS:", file2)
  plot.pars.intv <- readRDS(file2)
  file2 <- paste0(outfile.base,'-stanout-E_effcasesByAge-gqs.RDS')
  cat("\n read RDS:", file2)
  E_effcasesByAge <- readRDS(file2)
  
  cat("\n ----------- summarise_e_acases_eff_byage_c ----------- \n")
  e_acases_eff_byage_c <- summarise_e_acases_eff_byage_c(E_effcasesByAge,
                                                         age_cat_map, 
                                                         plot.pars.basic$pop_info, 
                                                         plot.pars.basic$dates, 
                                                         plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(e_acases_eff_byage_c, file=file)
}
if(file.exists(file))
{
  e_acases_eff_byage_c <- readRDS(file)
}
if(nrow(subset(e_acases_eff_byage_c, loc == 'US')) > 0)
{
  e_acases_eff_byage_c = subset(e_acases_eff_byage_c, loc != 'US')
}

file <- paste0(outfile.base,'-contact_patterns_over_time.rds')
if(!file.exists(file) | args_dir[['overwrite']])
{
  cat(" \n -------------------------------- \n summarise contact matrices: start \n -------------------------------- \n")
  tryCatch({
    cnts <- make_contact_patterns_over_time_summaries(plot.pars.basic$pop_info,
                                                      plot.pars.basic$stan_data,
                                                      plot.pars.intv,
                                                      plot.pars.basic$regions,
                                                      plot.pars.basic$dates,
                                                      apply_eta_to_index_and_cnt, 
                                                      plot.pars.basic$with_contact_intensities_zhang,
                                                      plot.pars.trmspars$elt_school_intv_effect,
                                                      plot.pars.basic$mobility_data)
    cat("\nWrite ",file," ... ")
    saveRDS(cnts, file=file)		
  })
  cat(" \n -------------------------------- \n summarise contact matrices: end \n -------------------------------- \n")
}
if(file.exists(file))
{
  cnts <- readRDS(file)
}

#
#	summarise if cum prop flow by age for Oct
file <- paste0(outfile.base,'-summary-cum-prop-flow-age-oct.RDS')
if(!file.exists(file) | args_dir[['overwrite']])
{	
    file2 <- paste0(outfile.base,'-stanout-flows-gqs.RDS')
    cat("\n read RDS:", file2)
    flows_gqs <- readRDS(file2)
  
  cat("\n ----------- summarise_cumprop_flow_byage_c ----------- \n")
  cumpropflow_byage_c.oct <- summarise_cumprop_flow_byage_c(flows_gqs$reduced_flows, 
                                                            plot.pars.basic$stan_data$reduced_flows_Monday_idx,
                                                            age_cat_map, 
                                                            "2020-10-01",
                                                            plot.pars.basic$pop_info, 
                                                            plot.pars.basic$dates, 
                                                            plot.pars.basic$regions)
  cat("\nWrite ",file," ... ")
  saveRDS(cumpropflow_byage_c.oct, file=file)		
}
if(file.exists(file))
{
  cumpropflow_byage_c.oct <- readRDS(file)
}

flows_gqs <- NULL
E_effcasesByAge <- NULL

# do the summaries include the national average
with_national <- 0
if(nrow(subset(cumpropflow_byage_c.oct, loc == "US")) > 0 )
{
  with_national <- 1
}

#
#	handle if forecast period is to be included in plots
if(!args_dir$with_forecast)
{
  date.max <- max( as.Date( sapply( plot.pars.basic$dates, function(x) max(as.character(x)) ) ) )
  cat("\nExcluding forecast period from plotting, setting max date to ",as.character(date.max))
  e_acases_eff_byage_c <- subset(e_acases_eff_byage_c, date<=date.max)
  cnts <- subset(cnts, dates<=date.max)
}

# 
# Plot effective cases
p_eacases_eff_loc <- plot_par_byage_c(e_acases_eff_byage_c, 
                                     "e_acases_eff", 
                                     ylab='Total number of \n infectious people \n(posterior median by age band)',
                                     selected_loc,
                                     outfile.base=NULL) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))	+	
  theme_bw(base_size=14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.y=element_text(size=11),
        legend.position='bottom') + 
  ggtitle(unique(subset(e_acases_eff_byage_c, loc ==selected_loc)$loc_label))

# 
# Plot contact intensities
cnts[, month := format(dates, '%m')]
selected_dates = unique(subset(cnts, loc == selected_loc & month %in% unique(subset(cnts, loc == selected_loc)$month)[c(1,3)])$dates)[c(1,3)]
p_cnts_loc = plot_contact_patterns_over_time_specific_loc_date(subset(cnts, loc == selected_loc & dates %in% selected_dates))

#
# Plot contribution in October 
n.locs <- length(plot.pars.basic$regions)
tmp <- cumpropflow_byage_c.oct[, list( ALL_LOC=length(unique(loc))==(n.locs+with_national )), by='date'] # +1 for US: national 
tmp <- subset(tmp, ALL_LOC)
tmp <- tmp[, list(ALL_NON_FORECAST= all(sapply(plot.pars.basic$dates, function(x) any(x==date)))), by='date']
tmp <- subset(tmp, ALL_NON_FORECAST)
last.common.date <- max(tmp$date)
data <- subset(cumpropflow_byage_c.oct, date==last.common.date)
data <- subset(data,loc=='US')
# % population in age group c
tmp <- subset(plot.pars.basic$pop_info, loc %in% plot.pars.basic$regions, select=c(age.cat, pop,loc,loc_label))
tmp <- merge(tmp, subset(age_cat_map, select=c(age.cat, age.cat2)), by='age.cat')
tmp <- tmp[, list(pop= sum(pop)), by=c('age.cat2')]
tmp <- tmp[, list(age_cat=age.cat2,prop_pop= pop/sum(pop))]
data <- merge(data,tmp,by=c('age_cat'))

p_contribution = ggplot() +  
  geom_bar(data=data,aes(x=age_band, y=M,fill="Cumulated contribution"), stat='identity',alpha=0.7) +
  geom_bar(data=data,aes(x=age_band, y=prop_pop,fill='Proportion of population'), stat='identity',size=1.1,alpha=0,col="black") +
  geom_errorbar(data=data,aes(x=age_band, ymin = CL, ymax = CU), width=.2,
                position=position_dodge(0.7), col='black',show.legend=FALSE) +
  scale_y_continuous(expand=c(0,0),labels = label_percent(suffix="",accuracy=1)) + coord_cartesian(ylim=c(0, 0.5)) +
  labs(x="",y = "Percent of total \npopulation") +
  #facet_wrap(loc_label~., ncol=6) +
  theme_bw(base_size=30) +
  theme_bw(base_size=14) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
  theme(legend.position="right",
        axis.text.x=element_text(angle=45,hjust=1),
        strip.background = element_blank(),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        axis.title.x =element_blank() ) +
  scale_fill_manual(name="",values = c("steelblue3", "black"),labels=c("Proportion of SARS-Cov-2 infections \nthat originated from age group in \nOctober 2020","Proportion of age group in population"))

#
# Arrange and make left part
p = grid.arrange(p_eacases_eff_loc, p_cnts_loc,p_contribution, nrow = 4,
                 heights = c(0.05,0.7,1, 1),
                 widths = c(1.2,0.07,0.02, 1),
                 layout_matrix = rbind(c(NA,NA,NA,NA),
                                      c(NA,NA,3,3),
                                       c(NA, 1, 1,1),
                                        c(NA,NA,NA,2)))
ggsave(p, file = paste0(outfile.base, '-figure_0_right.png'), w = 14.5, h = 10)
p = grid.arrange(p_eacases_eff_loc, p_cnts_loc,p_contribution, nrow = 4,
                 heights = c(0.05,0.7,1, 1),
                 widths = c(0.07,0.02, 1),
                 layout_matrix = rbind(c(NA,NA,NA),
                                       c(NA,3,3),
                                       c( 1, 1,1),
                                       c(NA,NA,2)))
ggsave(p, file = paste0(outfile.base, '-figure_0_right.pdf'), w = 7, h = 10)

pkg.dir <- system.file(package = "covid19AgeModel" )
# pkg.dir = '~/git/R0t/covid19AgeModel/inst'
panel.right <- magick::image_read(paste0(outfile.base, '-figure_0_right.png'))
panel.left <- magick::image_read_pdf(file.path(pkg.dir,  'figures', 'model_diagram_v3.pdf'))


p2 = image_composite(panel.right, image_scale(panel.left, "2200"), offset = "+00+600") %>%
  image_annotate("A", font = 'Helvetica', size = 100, location = "+100+600") %>%
  image_annotate("B", font = 'Helvetica', size = 100, location = "+2300+0") %>%
  image_annotate("C", font = 'Helvetica', size = 100, location = "+2300+840") %>%
  image_annotate("D", font = 'Helvetica', size = 100, location = "+2300+1920") 


savepdf <- function(fname, width=16, height=10)
{
  pdf(fname, width=width/2.54, height=height/2.54)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))
}

savepdf(paste0(outfile.base, '-figure_0.pdf'), w = 14.5*2, h = 10*2)
plot(p2) 
dev.off()

cat(" \n -------------------------------- \n \n End post-processing-make-figure-0.R \n \n -------------------------------- \n")


