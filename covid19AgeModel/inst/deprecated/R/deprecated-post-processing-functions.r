postprocess_beta_age_if_aM_in_names <- function(aM,beta_age,age_band,outfile.base)
{
  cat("\n ----------- make beta_age figure when aM in names ----------- \n")
  tmp2 <- as.matrix(aM)
  tmp2 = reshape2::melt(tmp2) %>%
    rename(region = Var2, aM = value)
  
  tmp <- as.array(beta_age)
  tmp <-aperm(tmp, c(1, 3, 2))
  colnames(tmp) <- age_band
  tmp = reshape2::melt(tmp) %>%
    rename(age = Var2, region = Var3, beta_age = value) %>%
    merge(tmp2, by = "region") %>%
    mutate(beta_age_wre = beta_age-aM) %>%
    group_by(age) %>%
    summarise(M = mean(beta_age_wre),
              CL = quantile(beta_age_wre, prob = 0.025),
              CU = quantile(beta_age_wre, prob = 0.975))
  g <- ggplot(tmp, aes(x = age)) +
    geom_point(aes(y = M)) +
    geom_errorbar(aes(ymin = CL, ymax = CU)) +
    geom_hline(yintercept=1) +
    labs(x=expression(beta[a])) +
    theme_bw()
  ggsave(paste0(outfile.base,'-beta_age.png'),g,height=5,width=7)
}

make_parPerTimePerLoc_summaries <- function(samples, dates, regions, pop_info)
{
  ps <- c(0.5, 0.025, 0.975)
  p_labs <- c('M','CL','CU')
  
  stopifnot( dim(samples)[3]==length(regions) )
  
  p_loc <- unique(subset(pop_info, select=c(loc,pop_total)))
  p_loc <- subset(p_loc, loc %in% regions)
  p_loc <- p_loc[, pop_prop:=pop_total/sum(pop_total)]
  
  ans <- vector('list',length(regions))  
  ans_overall <- vector('list',length(regions)) 
  #	loop over locations for speed. very hard to take quantiles on such large data
  for(m in seq_along(regions))
  {
    
    #m <- 1	  
    cat('\nProcessing state',m,' ',regions[m])
    
    ds_m <- as.data.table( reshape2::melt( samples[,,m] ) )
    setnames(ds_m, 1:3, c('iteration','time','value'))
    
    # save time mapping for region m
    tmp1 <- unique(subset(ds_m, select=time))
    tmp1[, date:= dates[[m]][tmp1$time[1]] + tmp1$time - tmp1$time[1]]
    
    #	to compute national average: add prop_pop, time and save 	
    tmp <- ds_m[, pop_prop := subset(p_loc, loc==regions[m])$pop_prop]
    # add dates: we need to do here because the time index are different by state
    tmp <- merge(tmp, tmp1, by = 'time')
    tmp[, loc:= regions[m]]
    ans_overall[[m]] = copy(tmp)
    
    # summarise
    ds_m <- ds_m[, list( 	q= quantile(value, prob=ps),
                          q_label=p_labs), 
                 by=c('time')]		
    
    #	add loc 
    ds_m[, loc:= regions[m]]
    
    # add time index and date
    ds_m <- merge(ds_m, tmp1, by='time')
    
    # build new data object only after summarised
    ans[[m]] <- copy(ds_m)
  }
  ans <- do.call('rbind',ans)
  ans_overall <- do.call('rbind',ans_overall)
  
  ## National average
  # keep date that are common to every region
  tmp = unique(select(ans_overall, loc, date))
  dates_common = colnames(a<-table(tmp))[colSums(a>0)==nrow(a)]
  ds_m <- subset(ans_overall, date %in% as.Date(dates_common))
  #	summarise
  ds_m <- ds_m[, list(value= sum(value*pop_prop)), by=c('date','iteration')]
  ds_m <- ds_m[, list(q= quantile(value, prob=ps),
                      q_label= p_labs), 
               by=c('date')]
  
  #	make human readable loc labels
  tmp <- unique(subset(pop_info, select=c(loc, loc_label)))
  ans <- merge(ans, tmp, by='loc')
  ans <- select(ans, -time)
  ds_m[,loc:= "US"]
  ds_m[,loc_label := "United-States"]
  ans <- rbind(ans, ds_m)
  
  ans <- dcast.data.table(ans, loc + loc_label  + date ~ q_label, value.var='q')
  ans 
}

make_epidemiologic_scenarios_from_Rta <- function(Rt_byage_less_than_one,cutoff)
{
  Rt_byage_less_than_one[,date.type:=weekdays(date)]
  Rt_byage_less_than_one[,week:=floor_date(date,'week', week_start = 1)]
  Rt_byage_less_than_one_weekday <- Rt_byage_less_than_one[date.type %in% c('Monday','Tuesday','Wednesday','Thursday','Friday')]
  Rt_byage_less_than_one_weekend <- Rt_byage_less_than_one[date.type %in% c('Saturday','Sunday')]
  dt <-   Rt_byage_less_than_one_weekday[,list(greater_than_cutoff=sum(value>cutoff),
                                               total=length(value)),by=c('loc_label','age_band','week')]
  dt[,prop_greater_than_cutoff:=greater_than_cutoff/total]
  dt2 <-   Rt_byage_less_than_one_weekend[,list(greater_than_cutoff=sum(value>cutoff),
                                                total=length(value)),by=c('loc_label','age_band','week')]
  dt2[,prop_greater_than_cutoff:=greater_than_cutoff/total]
  clusters <- vector()
  
  for (state in unique(dt$loc_label)){
    tmp <- dt[loc_label==state]
    tmp.max.date <- tmp[total==5,max(week)]
    tmp2 <- dt2[loc_label==state]
    tmp2.max.date <- tmp2[total==2,max(week)]
    if(all(tmp[week==tmp.max.date &  age_band =='35-49' & total==5,prop_greater_than_cutoff==0.0])){
      if(any(tmp[ age_band =='35-49', prop_greater_than_cutoff >0.4])){
        clusters <- c(clusters, "recent rise in reproductive numbers > 1  in adults")
      }else{
        if(all(tmp[week==tmp.max.date&  age_band =='20-34' & total==5,prop_greater_than_cutoff==0.0])|
           all(tmp[week==tmp.max.date&  age_band =='10-19' & total==5,prop_greater_than_cutoff==0.0])|
           all(tmp[week==tmp.max.date&  age_band =='50-64' & total==5,prop_greater_than_cutoff==0.0])){
          clusters <- c(clusters, "sustained reproductive numbers > 1 across many age groups")
        }else{
          clusters <- c(clusters, "sustained reproductive numbers > 1 in adults")
        }
      }
    }else if(all(tmp[week==tmp.max.date&  age_band =='35-49' & total==5,prop_greater_than_cutoff==1.0])){
      if(tmp[week==tmp.max.date&  age_band =='35-49' & total==5,prop_greater_than_cutoff] > 
         tmp2[week==tmp2.max.date&  age_band =='35-49' & total==2,prop_greater_than_cutoff]){
        clusters <- c(clusters, 'unclear')
      }else{
        clusters <- c(clusters, "all age-specific reproductive numbers consistently < 1")
      }
    }else{
      clusters <- c(clusters, NA_character_)
    }
  }
  return(list(clusters,unique(dt$loc_label)))
}

#' @export
#' @keywords internal
#' @import tidyr forcats grid ggplot2 ggpubr
#' @importFrom scales percent
plot_Rt_less_than_one <- function(Rt_byage_less_than_one, plot_rebound_date=FALSE, rebound_date, outfile.base=NA_character_)
{	
  
  
  tmp <- data.table(y=runif(1e3 *  length(unique(Rt_byage_less_than_one$loc_label)), 0.5, 0.5 + length(unique(Rt_byage_less_than_one$age_band))),
                    loc_label=rep(unique(Rt_byage_less_than_one$loc_label),each=1e3))								
  tmp <- merge(tmp, unique(subset(Rt_byage_less_than_one, select=c(loc_label, epi_scenario_label))), by='loc_label')
  set(tmp, tmp[, which(is.na(epi_scenario_label) | epi_scenario_label=='-')], 'epi_scenario_label', 'unclear')
  set(tmp, NULL, 'epi_scenario_label', tmp[,factor(epi_scenario_label)])
  tmp[,epi_scenario_label:=factor(epi_scenario_label,
                                  levels= c("Rt<1 for all ages in last 4 weeks","Rt>1 after 4 week period of Rt<1 for all ages",
                                            "unclear",
                                            "Rt>1 for 35-49 in last 4 weeks and no prior period with sustained Rt<1",
                                            "Rt>1 for 35-49 and other age groups in last 4 weeks and no prior period with sustained Rt<1"),
                                  labels = c("Rt<1 for all ages in last 4 weeks",
                                             "Rt>1 after 4 week period of Rt<1 \n for all ages",
                                             "unclear",
                                             "Rt>1 for 35-49 in last 4 weeks \n and no prior period with \n sustained Rt<1",
                                             "Rt>1 for 35-49 and other age groups \n in last 4 weeks and no prior period \n with sustained Rt<1"))]
  p <- ggplot( Rt_byage_less_than_one) +
    scale_x_date(expand=c(0,0), date_breaks = "4 weeks", labels = date_format("%e %b")) +
    scale_y_discrete(expand=c(0,0)) +
    geom_tile(aes(x= date, y= age_band, fill=value)) +
    geom_rug(data=tmp,aes(y=y, color=epi_scenario_label),sides='r',outside=TRUE, show.legend=TRUE,size=2)+
    scale_colour_manual(values=c("Rt<1 for all ages in last 4 weeks"="deepskyblue",
                                 "Rt>1 after 4 week period of Rt<1 \n for all ages"="gold",
                                 "Rt>1 for 35-49 in last 4 weeks \n and no prior period with \n sustained Rt<1"="darkorchid1",
                                 "Rt>1 for 35-49 and other age groups \n in last 4 weeks and no prior period \n with sustained Rt<1"="firebrick1",
                                 "unclear"="pink"))+
    scale_fill_gradient(high = "azure1", low = "grey30", labels = scales::percent) +
    #scale_fill_gradient(high = c("lighskyblue2", "yellow", "blue"), labels = scales::percent) +
    #scale_fill_viridis(begin=0.1,end=1, alpha=1, direction=-1, option="inferno", labels = scales::percent) +
    #scale_fill_grey(labels = scales::percent) +
    #scale_fill_gradientn(colours = c("red", "yellow", "blue"), labels = scales::percent) +
    labs(x= 'Date', y='Age bands',color='Epidemiologic classification',
         fill='Posterior probability that age specific \n R[t] is below 1 \n \n \n ') +
    facet_wrap(loc_label~.,ncol=5) +
    theme_bw() +
    theme(#plot.margin = margin(1, 1, 1, 1, "cm"),
      legend.position='bottom',
      legend.box="horizontal",
      legend.direction='horizontal',
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.text.align = 0,
      legend.justification=c(0,0),
      plot.title = element_text(size = 10, face = "bold"),
      axis.text.x=element_text(angle=60, vjust = 0.9, hjust=1),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank()) +
    guides(col = guide_legend(ncol=2, byrow = TRUE, direction='vertical', title.position='top',order=1),
           fill = guide_colourbar(barheight = 0.5, barwidth = 10, direction='horizontal',title.position='top',order=2)) 
  if(plot_rebound_date){
    p = p+   geom_vline(data=rebound_date, aes(xintercept=rebound_date),color = "red")
  }
  h <- ceiling(length(unique(Rt_byage_less_than_one$loc))/5)
  
  ggsave(paste0(outfile.base,'-plot_Rt_less_than_one.png'),p, width = 5*1.7, height = h * 1.1 + 1, limitsize = FALSE)
}

