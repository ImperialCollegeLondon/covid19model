library(matrixStats)
library(here)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())

onset_table <- readRDS(here("whatif/data/onset-table.rds"))
emf = as.numeric(onset_table)
x=as.numeric(names(onset_table))
xp=seq(0.001,max(x),0.1)


data=list(TOTAL=sum(emf),y=emf,x=x,N=length(x),xp=xp,N2=length(xp))


m1 <- stan_model(here('whatif/utils/distribution-gamma-bin.stan')) # Compile Stan code

fit <- sampling(m1, warmup=2000, iter=5000,  control = list(adapt_delta = 0.99, max_treedepth = 15), data=data, seed=1234)


ex=rstan::extract(fit)
data_pred1=data.frame(
  xp=xp,
  mean=colMeans(ex$lp),
  low=colQuantiles(ex$lp,probs=0.025),
  high=colQuantiles(ex$lp,probs=0.975)
)


p1 <- ggplot() + 
  geom_bar(data=data.frame(x=x,y=emf),aes(x=x,y=y),alpha=0.7,stat="identity",fill='black',color='white')+
  geom_line(data=data_pred1,aes(x=xp,y=mean),alpha=0.8,color='red')+
  geom_ribbon(data=data_pred1,aes(x=xp,ymin=low,ymax=high),alpha=0.25,fill='red')+
  xlab("Number of days since onset of symptoms") +
  ylab("Count") +
  theme_bw() + 
  theme(legend.position = "none")
p1
ggsave(here('whatif/figures/onset_death.pdf'),p1)

ex=rstan::extract(fit)
shape=mean(ex$shape)
scale=1/mean(ex$scale)
quantile(ex$shape,probs=c(0.025,0.975))

quantile(1/ex$scale,probs=c(0.025,0.975))

mu=shape*scale
cv=shape^(-0.5)

cat(sprintf('Shape is %s\n Scale is %s\n Mu is %s\n CV is %s\n', shape, scale, mu, cv))
