library(here)
library(ggplot2)
library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(gridExtra)

dir.create(here("tiers/figures"),recursive = TRUE)
dir.create(here("tiers/results"),recursive = TRUE)
USE_BACK_DATE = TRUE
#USE_BACK_DATE = FALSE

#USE_NESTED_PMAX_TIER_DEFS = TRUE
USE_NESTED_PMAX_TIER_DEFS = FALSE

if (!USE_BACK_DATE)
{
	# 	regular tiers version (data from 1st Sept)
	Day_0 = as.Date('01/09/2020', format = "%d/%m/%Y") 
	BackDate_Char = ""
	
} else 
{
	# 	backdated tiers version (data from 23 March, but only go as far back as 2nd May. This way all ltlas have same number of days/weeks of Rt estimates. )
	Day_0 = as.Date('01/07/2020', format = "%d/%m/%Y") 
	BackDate_Char = "_BD" 
}
if (USE_NESTED_PMAX_TIER_DEFS) Nested_Char = "_nested" else Nested_Char = ""

# build Tier characters (i.e. your covariates)
Tier_1_Char = paste0("tier_1", BackDate_Char, Nested_Char)
Tier_2_Char = paste0("tier_2", BackDate_Char, Nested_Char)
Tier_3_Char = paste0("tier_3", BackDate_Char, Nested_Char)
covar 		= c(Tier_1_Char, Tier_2_Char, Tier_3_Char) 

Day_Final 	= as.Date('04/11/2020', format = "%d/%m/%Y")

d <- readRDS(here("tiers/data/rt_covars_combined.rds"))
d = as.data.frame(d)

##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== 
##### ===== BEFORE/AFTER scatter plot

### isolate Data immeidiately before Tiers system introduced
Day_BeforeTiersIntroduced 		= as.Date('13/10/2020', format = "%d/%m/%Y")
Day_AfterTiersIntroduced 		= Day_BeforeTiersIntroduced + 15 # chosen 2 weeks after
Data_BeforeTiers 	= d[d$date == Day_BeforeTiersIntroduced,]
Data_AfterTiers 	= d[d$date == Day_AfterTiersIntroduced,]

length(exp(Data_BeforeTiers$Rt))
length(exp(Data_AfterTiers$Rt))

summary(exp(Data_BeforeTiers$Rt))
summary(exp(Data_AfterTiers$Rt))
quantile(exp(Data_BeforeTiers$Rt), c(0.025, 0.975), na.rm = T)
quantile(exp(Data_AfterTiers$Rt), c(0.025, 0.975), na.rm = T)

min(d$date[d[, Tier_3_Char] == 1])

Tier_3_indices = Data_AfterTiers[, Tier_3_Char] == 1
Tier_2_indices = Data_AfterTiers[, Tier_2_Char] == 1
Tier_1_indices = Data_AfterTiers[, Tier_1_Char] == 1

p_scatterBeforeAfter <- ggplot(data = data.frame(x = exp(Data_BeforeTiers$Rt), y = exp(Data_AfterTiers$Rt))) + 
		geom_point(data=data.frame(x = exp(Data_BeforeTiers$Rt), y = exp(Data_AfterTiers$Rt)), aes(x = x, y = y), color = 'black', alpha = 0.4) +
		geom_point(data=data.frame(x = exp(Data_BeforeTiers$Rt[Tier_1_indices]), y = exp(Data_AfterTiers$Rt[Tier_1_indices])), aes(x = x, y = y), color ='blue'	, alpha = 0.8) +
		geom_point(data=data.frame(x = exp(Data_BeforeTiers$Rt[Tier_2_indices]), y = exp(Data_AfterTiers$Rt[Tier_2_indices])), aes(x = x, y = y), color ='green', alpha = 0.8) +
		geom_point(data=data.frame(x = exp(Data_BeforeTiers$Rt[Tier_3_indices]), y = exp(Data_AfterTiers$Rt[Tier_3_indices])), aes(x = x, y = y), color ='red'	, alpha = 0.8) +
		xlim(0.9, 2) + ylim(0.75, 1.8) +
		geom_abline(x = 0, y = 1, col = 'orange') + theme_bw() + 
		xlab("Rt on 13th Oct (before Tier system introduced)") + ylab("Rt on 28th Oct (after Tier system introduced)")

pdf(here(paste0("tiers/figures/RtBeforeAfterTiers.pdf")), height = 4, width = 4)
grid.arrange(p_scatterBeforeAfter, ncol = 1)
dev.off()

png(here(paste0("tiers/figures/RtBeforeAfterTiers.png")), units = "in", res = 300, height = 4, width = 4)
grid.arrange(p_scatterBeforeAfter, ncol = 1)
dev.off()


##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== ##### ===== 




# omit everything before Day_0
d=d[d$date >= Day_0, ]
# omit everything after Day_Final
d=d[d$date <= Day_Final, ]
d = d[, c('date', 'Rt', 'ltla', 'Rt_se', covar)] ## subset relevant columns earlier - complete.cases removes too much that we care about because variables we don't care about are incomplete.
dim(d)

d = d[complete.cases(d), ]
dim(d)

## Aggregate data into weeks. 
d$week <- abs(as.numeric(floor((d$date - Day_0)/7)))
sum(d[, Tier_1_Char])
sum(d[, Tier_2_Char])
sum(d[, Tier_3_Char])

# Determine if each ltla under particular tier in each week.
un <- unique(d$ltla)
un2 <- unique(d$week)

for(i in 1:length(un)) ## loop over ltlas
	for(j in 1:length(un2)) ## loop over weeks
	{
		# subset by ltla and week
		Rows = which(d$ltla == un[i] & d$week == un2[j])
		
		# number days this week (in this region) where tiers 1,2,3 enacted 
		s1 = sum(d[Rows, Tier_1_Char], na.rm = T)
		s2 = sum(d[Rows, Tier_2_Char], na.rm = T)
		s3 = sum(d[Rows, Tier_3_Char], na.rm = T)
		
		# if any days were in tier X, assign 1 to tier X for weekly values. 
		if(s1 > 0)	d[Rows, Tier_1_Char] = 1	else	d[Rows, Tier_1_Char] = 0
		if(s2 > 0) 	d[Rows, Tier_2_Char] = 1 	else 	d[Rows, Tier_2_Char] = 0
		if(s3 > 0)	d[Rows, Tier_3_Char] = 1	else	d[Rows, Tier_3_Char] = 0
	}
rm(Rows)

sum(d[, Tier_1_Char])
sum(d[, Tier_2_Char])
sum(d[, Tier_3_Char])

## remove duplicates
d$dup = paste0(d$ltla,':', d$week)
d = d[!duplicated(d$dup), ]

## checks
#dim(d)
#length(un) * length(un2)
#dim(d)[1] - (length(un) * length(un2))
#
#NumWeeksByLTLA = rep(NA, length(un))
#for(i in 1:length(un)){ ## loop over ltlas
#
#	d_sub = d[d$ltla == un[i], ]
#	NumWeeksByLTLA[i] = length(unique(d_sub$week))
#} 
#NumWeeksByLTLA[which(NumWeeksByLTLA != 33)]
#sum(NumWeeksByLTLA[which(NumWeeksByLTLA != 33)])
#var(NumWeeksByLTLA) # should be zero

colnames(d)=gsub("-", "_", colnames(d))

un=unique(d$ltla)
un2=unique(d$region)

d$group = NA
d$group2 = NA
for(i in 1:length(un)){ ## loop over ltlas
	d$group[d$ltla==un[i]]=i
}
for(i in 1:length(un2 )){ ## loop over weeks
	d$group2[d$region==un2[i]]=i
}


tmp = d[,covar]
names(tmp[, sapply(tmp, function(v) var(v, na.rm = TRUE)==0)])
#remove constant columns
covar = covar[sapply(d[,covar], function(v) var(v, na.rm=TRUE) != 0)]
d = d[,c('date','Rt','ltla','group','group2','Rt_se','week', covar)]

data=list(
		y=d$Rt,
		yse=d$Rt_se,
		x=d[,covar],
		M=max(d$group),
		M2=max(d$group2),
		P=ncol(d[,covar]),
		N=nrow(d),
		group=d$group,
		group2=d$group2,
		Q=length(unique(d$date))
)


ModelChar 	= "2stage_LFM_non_centered"
StanModel 	= stan_model(here(paste0("tiers/stan_models/",ModelChar, ".stan")))
cat(paste0("Model compilation done\n"))

# create output string
OutputString = paste0(ModelChar, BackDate_Char, Nested_Char) ## add whatever is relevant here.

# create and write meta data
ModelMetaData 				= c()
ModelMetaData$iter 			= 2000
ModelMetaData$warmup 		= 500
ModelMetaData$thin 			= 1
ModelMetaData$chains 		= 10
ModelMetaData$adapt_delta 	= 0.9
ModelMetaData$max_treedepth = 15
ModelMetaData$ModelChar 	= ModelChar
ModelMetaData$BackDate_Char = BackDate_Char
ModelMetaData$Nested_Char 	= Nested_Char

ModelMetaData_dummy = as.data.frame(unlist(ModelMetaData))
colnames(ModelMetaData_dummy) = NULL

# use meta data as arguments in stan sampling
fit = sampling(StanModel, data = data, 
		iter 	= ModelMetaData$iter, 
		warmup 	= ModelMetaData$warmup, 
		thin 	= ModelMetaData$thin, 
		chains 	= ModelMetaData$chains, 
		pars 	= c("beta", "lp", "random_effects"), 
		control = list(adapt_delta = ModelMetaData$adapt_delta, max_treedepth = ModelMetaData$max_treedepth))

a=as.matrix(fit)
lp=exp(colMeans(a[,grep('^lp\\[',colnames(a))]))
library(matrixStats)
trend=data.frame(t=unique(d$date),
		m=exp(colMeans(a[,grep('^trend\\[',colnames(a))]))[1:length(unique(d$date))],
		ml=exp(colQuantiles(a[,grep('^trend\\[',colnames(a))],probs=0.025))[1:length(unique(d$date))],
		mh=exp(colQuantiles(a[,grep('^trend\\[',colnames(a))],probs=0.975))[1:length(unique(d$date))])

lpr=exp(colMeans(a[,grep('^random_effects\\[',colnames(a))]))
un=unique(d$ltla)
ms=lpr[d$ltla==un[1]]
for(i in 2:length(un))	ms = ms + lpr[d$ltla == un[i]]
ms=ms/length(un)

sm=matrix(nrow=3,ncol=3)
library(matrixStats)
sm[,1]=colMeans(-a[,grep('^beta\\[',colnames(a))])
sm[,2:3]=colQuantiles(-a[,grep('^beta\\[',colnames(a))],probs=c(0.025,0.975))

## Counterfactual: what if all regions in tier 3?
sub = d[d$date==max(d$date),]
sub$Rt = exp(sub$Rt)
# make duplicate Rt variable and amend values below.
sub$Rtd = sub$Rt 
if (USE_NESTED_PMAX_TIER_DEFS)
{
	sub$Rtd[sub[, Tier_1_Char] == 1] = sub$Rt[sub[, Tier_1_Char] == 1] * exp(sm[3,1] + sm[2,1]) # i.e. Turn on tiers 2 and 3
	sub$Rtd[sub[, Tier_2_Char] == 1] = sub$Rt[sub[, Tier_2_Char] == 1] * exp(sm[3,1]) # i.e. Turn on tier 3, (tier 1 should already be on)
	sub$Rtd[sub[, Tier_3_Char] == 1] = sub$Rt[sub[, Tier_3_Char] == 1]
	
} else 	
{
	sub$Rtd[sub[, Tier_1_Char] == 1] = sub$Rt[sub[, Tier_1_Char] == 1] * exp(sm[3,1] - sm[1,1]) # i.e. turn off Tier 1, turn on Tier 3
	sub$Rtd[sub[, Tier_2_Char] == 1] = sub$Rt[sub[, Tier_2_Char] == 1] * exp(sm[3,1] - sm[2,1]) # i.e. turn off Tier 2, turn on Tier 3
	sub$Rtd[sub[, Tier_3_Char] == 1] = sub$Rt[sub[, Tier_3_Char] == 1]
}


p<-ggplot() +   
		geom_histogram(data=sub, aes(x=Rt),color="black", fill="red",bins=20,alpha=0.5)+
		geom_histogram(data=sub, aes(x=Rtd),color="black", fill="blue",bins=20,alpha=0.5)+
		geom_vline(xintercept = 1,color = "black", size=1.5) +  theme_bw()

p2<-ggplot(data=data.frame(x=d$Rt,y=lp))+
		geom_point(data=data.frame(x=exp(d$Rt),y=lp),aes(x=x,y=y),color='black',alpha=0.4)+
		geom_point(data=data.frame(x=exp(d$Rt[d[, Tier_1_Char] == 1]),y=lp[d[, Tier_1_Char] == 1]), aes(x = x, y = y),color='blue',alpha=0.8)+
		geom_point(data=data.frame(x=exp(d$Rt[d[, Tier_2_Char] == 1]),y=lp[d[, Tier_2_Char] == 1]), aes(x = x, y = y),color='green',alpha=0.8)+
		geom_point(data=data.frame(x=exp(d$Rt[d[, Tier_3_Char] == 1]),y=lp[d[, Tier_3_Char] == 1]), aes(x = x, y = y),color='red',alpha=0.8)+
		xlim(0.5, 2)+ ylim(0.5, 2)+
		geom_abline(x=0,y=1,col='red')+theme_bw()+
		xlab("Observed")+ylab("Predicted")

R=seq(0.8,1.8,0.1)
p3<-ggplot()+
		geom_line(data=data.frame(x=R,y=R*exp(sm[1,1])),aes(x=x,y=y),col='blue')+
		geom_line(data=data.frame(x=R,y=R*exp(sm[2,1])),aes(x=x,y=y),col='green')+
		geom_line(data=data.frame(x=R,y=R*exp(sm[3,1])),aes(x=x,y=y),col='red')+
		geom_abline(x=0,y=1,col='black',linetype = "dashed")+
		geom_vline(xintercept = 1,linetype = "dashed")+geom_hline(yintercept = 1,linetype = "dashed")+
		theme_bw()+
		xlab("Starting Rt")+ylab("Ending Rt")

un=unique(d$ltla)
p4=ggplot()+geom_point(aes(x=x,y=y),data=data.frame(x=d$date[d$ltla==un[1]],y=lpr[d$ltla==un[1]]),alpha=0.5)+theme_bw()+
		xlab("Date")+ylab("Secular Effect Size")
for(i in 2:length(un)){
	p4=p4+geom_point(aes(x=x,y=y),data=data.frame(x=d$date[d$ltla==un[i]],y=lpr[d$ltla==un[i]]),alpha=0.5)
}
p4=p4+geom_point(aes(x=x,y=y),data=data.frame(x=d$date[d$ltla==un[i]],y=ms),alpha=1,col='red')
p4=p4+geom_vline(xintercept = as.Date('14/09/2020',format='%d/%m/%Y'),color = "purple", size=1,alpha=0.8) 
p4=p4+geom_vline(xintercept = as.Date('14/10/2020',format='%d/%m/%Y'),color = "purple", size=1,alpha=0.8) 


png(here(paste0("tiers/figures/Fig3_", OutputString, ".png")), units = "in", res = 300, height = 7.5, width = 7.5)

grid.arrange(p2,p,p3,p4,ncol=2)
dev.off()

pdf(here(paste0("tiers/figures/Fig3_", OutputString, ".pdf")), height = 7.5, width = 7.5)
grid.arrange(p2,p,p3,p4,ncol=2)
#grid.arrange(p2,p,p3,ncol=3)
dev.off()


## Do same as above for Neil's alternative scatter plot, but without subsetting data/estimates to final date. Don't change $Rt vals though as need them for plots.
DummyRt = exp(d$Rt)
d$RtD 	= DummyRt
if (USE_NESTED_PMAX_TIER_DEFS)
{
	d$Rtd[d[, Tier_1_Char] == 1] = DummyRt[d[, Tier_1_Char] == 1] * exp(sm[3,1] + sm[2,1]) # i.e. Turn on tiers 2 and 3
	d$Rtd[d[, Tier_2_Char] == 1] = DummyRt[d[, Tier_2_Char] == 1] * exp(sm[3,1]) # i.e. Turn on tier 3, (tier 1 should already be on)
	d$Rtd[d[, Tier_3_Char] == 1] = DummyRt[d[, Tier_3_Char] == 1]
	
} else 	
{
	d$Rtd[d[, Tier_1_Char] == 1] = DummyRt[d[, Tier_1_Char] == 1] * exp(sm[3,1] - sm[1,1]) # i.e. turn off Tier 1, turn on Tier 3
	d$Rtd[d[, Tier_2_Char] == 1] = DummyRt[d[, Tier_2_Char] == 1] * exp(sm[3,1] - sm[2,1]) # i.e. turn off Tier 2, turn on Tier 3
	d$Rtd[d[, Tier_3_Char] == 1] = DummyRt[d[, Tier_3_Char] == 1]
}

p_scatter_CF <- ggplot(data = data.frame(x = d$Rt, y = d$Rtd)) + 
		geom_point(data=data.frame(x = exp(d$Rt), y = d$Rtd),aes(x=x,y=y), color = 'black', alpha = 0.4) +
		geom_point(data=data.frame(x = exp(d$Rt[d[, Tier_3_Char] == 1]), y = d$Rtd[d[, Tier_3_Char] == 1]), aes(x = x, y = y), color='red',alpha = 0.8) +
		geom_point(data=data.frame(x = exp(d$Rt[d[, Tier_2_Char] == 1]), y = d$Rtd[d[, Tier_2_Char] == 1]), aes(x = x, y = y), color='green',alpha = 0.8) +
		geom_point(data=data.frame(x = exp(d$Rt[d[, Tier_1_Char] == 1]), y = d$Rtd[d[, Tier_1_Char] == 1]), aes(x = x, y = y), color='blue',alpha = 0.8) +
		xlim(0.5, 2)+ ylim(0.5, 2)+
		geom_abline(x=0,y=1,col='red')+theme_bw()+
		xlab("Observed")+ylab("Counterfactual")

png(here(paste0("tiers/figures/CF_Scatter_", OutputString, ".png")), units = "in", res = 200, height = 4, width = 4)
grid.arrange(p_scatter_CF, ncol = 1)
dev.off()


quantile(exp(d$Rt),probs=c(0.025,0.975))
mean(exp(d$Rt))
mean(d$Rt)
quantile((sub$Rtd),probs=c(0.025,0.975))
quantile((sub$Rt),probs=c(0.025,0.975))

sum(sub$Rt<1)/nrow(sub)
sum(sub$Rtd<1)/nrow(sub)

cor(d$Rt,lp)
mean(abs(exp(d$Rt)-lp))

print(exp(sm))

Results = exp(sm)
colnames(Results) = c("Mean"	, "Lower"	, "Upper")
rownames(Results) = c("Tier_1"	, "Tier_2"	, "Tier_3")

write.csv(Results, file = here(paste0("tiers/results/TierEffects_", OutputString, ".csv")))

ll=lm.fit(x=as.matrix(data$x),y=data$y)
mean(abs(exp(ll$fitted.values)-exp(data$y)))

#write Bayes plot
png(here(paste0("tiers/figures/TiersBayesPlot_", OutputString, ".png")), units = "in", res = 200, height = 3.5, width = 3.5)
bayesplot::mcmc_intervals(fit,regex_pars = "beta\\[")+ scale_y_discrete(labels = covar)
dev.off()

