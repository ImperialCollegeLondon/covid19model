library(data.table)
library(rstan)
library(lognorm)
library(dplyr)

args = list(
        seed = 42, 
        chain = 1,
        outdir= "~/Box\ Sync/2020/R0t/results/ifr_by_age_prior",
        indir = "~/git/R0t",
        cmdstan = 1,
        #stanModelFile = 'base_age_prior_ifr_200820a7_cmdstanv',
        stanModelFile = 'base_age_prior_ifr_200820b5_cmdstanv',
        job_tag= 'new_data_spain_England',
        job_id='41286'
)

args$job_dir <- file.path(args$outdir,paste0(args$stanModelFile,'-',args$job_tag,'-',args$job_id)) 

outfile.base <- paste0(args$job_dir, "/",  args$stanModelFile , "-", args$job_tag)

# Outputs of the log normal fit from 28/08/2020 
load(file.path(args$job_dir, paste0(basename(args$job_dir), '_stanout.RData')) )

fit = rstan:::sflist2stanfit(list(fit))

# divergences
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
sampler_params_chain1 <- sampler_params[[1]]
sum(sampler_params_chain1[,"divergent__"])

file <- paste0(outfile.base, "-convergence_diagnostics.rds")
cat("write ", file)
summary.par <- summary(fit)$summary
neff <- as.numeric(summary.par[, which(colnames(summary.par) == "n_eff")])
neff_range = prettyNum(round(range(neff)),big.mark=",")
Rhat <- summary.par[, which(colnames(summary.par) == "Rhat")]
Rhat_range <- round(range(Rhat), digits = 4)
saveRDS(list(neff_range, Rhat_range), file = file, version = 2)

age_predict <- c(0, seq(2.5, 97.5, 5))
fits <- as.data.table(summary.par)
fits[, variable:= rownames(summary.par)]
colnames(fits)[4:8] <-  c('CL','IL','M','IU','CU')
fits[, age.idx:= gsub('.*\\[([0-9]+)\\].*','\\1',variable)]
tmp <- fits[, which(variable==age.idx)]
set(fits, tmp, 'age.idx', NA_character_)
set(fits, NULL, 'age.idx', fits[, as.integer(age.idx)])
fits <- merge(fits, data.table(age.idx=seq_along(age_predict), age=age_predict), by='age.idx', all.x=TRUE)

#	make trace plot
color_scheme_set("mix-blue-red")
target.pars <- c('gp','alpha','alpha0','alpha1','beta','beta0','beta1','obs_rnd','study_rnd','age_rnd','sero_p')
target.pars <- names(fit)[ grepl(paste(paste0('^',target.pars),collapse = '|'),names(fit)) ]
p <- rstan::traceplot(fit, pars= target.pars, inc_warmup=TRUE, ncol = 1)
outfile <- file.path(args$outdir, paste0(basename(args$outdir), '_traceplot.pdf'))  
cat('\n write pairs plot', outfile)  
ggsave(file=outfile,p,w=20, h=length(target.pars)*3,limitsize = FALSE)

# make pair plot 
target.pars <- c('gp','alpha','alpha0','alpha1','beta','beta0','beta1','obs_rnd','age_rnd','study_rnd')
target.pars <- names(fit)[ grepl(paste(paste0('^',target.pars),collapse = '|'),names(fit)) ]
color_scheme_set("mix-blue-red")		
tmp <- rstan::extract(fit, pars=target.pars )
tmp <- unlist(tmp)	
tmp <- matrix(tmp, ncol=length(target.pars), byrow=FALSE)
colnames(tmp) <- target.pars  
#p <- mcmc_pairs(tmp, diag_fun = "dens", off_diag_fun = "hex")
p <- mcmc_pairs(tmp, diag_fun = "dens")
outfile <- file.path(args$job_dir, paste0(basename(args$job_dir), '_pairsplot.pdf'))  
cat('\n write pairs plot', outfile)
ggsave(file=outfile, p, w=20, h=20, limitsize = FALSE)


# divergent chains
divergent <- get_sampler_params(fit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

params_cp9 <- as.data.frame(extract(fit, permuted=FALSE))
names(params_cp9) <- gsub("chain:1.", "", names(params_cp9), fixed = TRUE)
names(params_cp9) <- gsub("[", ".", names(params_cp9), fixed = TRUE)
names(params_cp9) <- gsub("]", "", names(params_cp9), fixed = TRUE)
params_cp9$iter <- 1:9000

params_cp9$divergent <- divergent

div_params_cp <- params_cp9[params_cp9$divergent == 1,]
nondiv_params_cp <- params_cp9[params_cp9$divergent == 0,]

pdf(file = file.path(args$outdir, "pairs_plot_study_rnde.1_study_rnde_sd_9.pdf"))
plot(nondiv_params_cp$study_rnde.1, log(nondiv_params_cp$study_rnde_sd),
     col="#8F2727", pch=16, cex=0.8, xlab="study_rnde.2", ylab="log(study_rnde_sd)")
points(div_params_cp$study_rnde.1, log(div_params_cp$study_rnde_sd),
       col="green", pch=16, cex=0.8)
dev.off()

pdf(file = file.path(args$outdir, "pairs_plot_study_rnde1.2_study_rnde_sd2_9.pdf"))
plot(nondiv_params_cp$study_rnde2.1, log(nondiv_params_cp$study_rnde_sd2),
     col="#8F2727", pch=16, cex=0.8, xlab="study_rnde2.2", ylab="log(study_rnde_sd2)")
points(div_params_cp$study_rnde2.1, log(div_params_cp$study_rnde_sd2),
       col="green", pch=16, cex=0.8)
dev.off()

pdf(file = file.path(args$outdir, "pairs_plot_study_rnde_sd-study_rnde_sd2-9.pdf"))
plot(log(nondiv_params_cp$study_rnde_sd), log(nondiv_params_cp$study_rnde_sd2),
     col="#8F2727", pch=16, cex=0.8, xlab="log(study_rnde_sd)", ylab="log(study_rnde_sd2)")
points(log(div_params_cp$study_rnde_sd), log(div_params_cp$study_rnde_sd2),
       col="green", pch=16, cex=0.8)
dev.off()
