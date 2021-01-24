require(data.table)
require(covid19AgeModel)

pkg.dir <- system.file(package = "covid19AgeModel" )
cmdstan_dir <- '~/git/cmdstan'
out_dir <- '~/short_run_test'
report_dir <- '~/short_run_test/reports'
hmc_chains_n <- 8

## important note:
# the combination of stanModelFile and job_tag should be unique for each analysis
# all outputs with stanModelFile-job_tag are assumed to be several HMC chains run in parallel

## Final version for publication central analysis
if(1)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau10_schoolbound6_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 1,
    with_forecasts = 1, 
    ifr_by_age_prior = "Levin_meta_analysis_v7",
    school.cases.upper.bound.multiplier = 6
  )	
}

## Final version for publication sensitivity analysis, assume 50% lower contacts compared to Zhang et al. during school period from/to 0-14
if(0)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau05_schoolbound6_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 0.5,
    with_forecasts = 0,
    ifr_by_age_prior = "Levin_meta_analysis_v7",
    school.cases.upper.bound.multiplier = 6
  )	
}

## Final version for publication sensitivity analysis, assume 100% more contacts compared to Zhang et al. during school period from/to 0-14
if(0)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau20_schoolbound6_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 2,
    with_forecasts = 0,
    ifr_by_age_prior = "Levin_meta_analysis_v7",
    school.cases.upper.bound.multiplier = 6
  )	
}

## Final version for publication sensitivity analysis, using Levin et al. prior version 5
if(0)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau10_schoolbound6_Levinv5_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 1,
    with_forecasts = 0,
    ifr_by_age_prior = "Levin_meta_analysis",
    school.cases.upper.bound.multiplier = 6
  )	
}

## Final version for publication sensitivity analysis, assume attack rates among school-aged children and teens (xi) of 2
if(0)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau10_schoolbound2_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 1,
    with_forecasts = 1,
    ifr_by_age_prior = "Levin_meta_analysis_v7",
    school.cases.upper.bound.multiplier = 2
  )	
}

## Final version for publication sensitivity analysis, assume attack rates among school-aged children and teens (xi) of 10
if(0)
{
  countries <- "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,MI,MN,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries	
  args <- data.table(
    source_dir= pkg.dir,
    cmdstan_dir = cmdstan_dir,
    out_dir= out_dir,
    report_dir = report_dir,
    script_file= 'scripts/stan-make-data-usa-v120.r',
    script_converting_file = "scripts/stan-convert-csv-to-rda.r",
    script_generate_quantities_file = "scripts/generate-quantities.r",
    script_rmd_file = "scripts/post-processing-make-report.Rmd",
    stanModelFile= 'covid19AgeModel_v120_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 1500,
    hmc_num_warmup= 1000,			
    seed= 42,
    chain= 1,
    job_tag= '40states_tau10_schoolbound10_v120',
    countries = countries,			
    cmdstan = 1L,
    multiplier_cntct_school_closure = 1,
    with_forecasts = 1,
    ifr_by_age_prior = "Levin_meta_analysis_v7",
    school.cases.upper.bound.multiplier = 10
  )	
}

if(exists('hpc.nproc.cmdstan'))
{
  stopifnot( hpc.nproc.cmdstan<=length(unlist(strsplit(args$countries, split=','))) )	
}

if(1)
{
  tmp <- data.table(chain=1:hmc_chains_n)		
  tmp[, seed:= round(runif(seq_len(nrow(tmp)))*1e6)]		
  set(args, NULL, colnames(tmp), NULL)
  tmp[, dummy:= 1L]
  args[, dummy:= 1L]
  args <- merge(args, tmp, by='dummy')
  set(args, NULL, 'dummy', NULL)	
  JOBID = as.character(abs(round(rnorm(1) * 1e6)))
}

# make commands
cmds <- vector('list', nrow(args))
for(i in seq_len(nrow(args)))
{
  # allow for multi-threadings
  cmd <- paste0("export STAN_NUM_THREADS=",hpc.nproc.cmdstan,"\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE\n")
  #	general housekeeping
  cmd				<- paste0(cmd, "CWD=",tempfile(),"\n")
  cmd				<- paste0(cmd,"echo $CWD\n")	
  tmpdir.prefix	<- paste0('cvd_',format(Sys.time(),"%y-%m-%d-%H-%M-%S"))
  tmpdir			<- paste0("$CWD/",tmpdir.prefix)
  cmd				<- paste0(cmd,"mkdir -p ",tmpdir,'\n')
  cmd <- paste0(cmd, 'cd $CWD\n')
  # generate job id identifier
  cmd				<- paste0(cmd,'touch .Renviron\n')
  cmd				<- paste0(cmd,'echo PBS_JOBID=',JOBID, '[', i, ']',' > .Renviron\n')
  #	generate data set and run if not using cmdstan
  cmd 			<- paste0( cmd, 'echo "----------- Generating input data: ------------"\n')
  tmp 			<- paste0('Rscript ', file.path(args$source_dir[i],args$script_file[i]), 
                   ' -stanModelFile "', args$stanModelFile[i],'"',
                   ' -seed ', args$seed[i],
                   ' -chain ', args$chain[i],							
                   ' -outdir ', tmpdir,'',
                   ' -jobtag "', args$job_tag[i],'"',
                   ' -countries "', args$countries[i],'"',							
                   ' -cmdstan ', args$cmdstan[i],
                   ' -multiplier_cntct_school_closure ', args$multiplier_cntct_school_closure[i],
                   ' -ifr_by_age_prior "', args$ifr_by_age_prior[i],'"',
                   ' -school.cases.upper.bound.multiplier ', args$school.cases.upper.bound.multiplier[i]
  )
  cmd				<- paste0(cmd, tmp, '\n')
  #	if using cmdstan  
  if(args$cmdstan[i]==1) 
  {
    cmd <- paste0(cmd, 'echo "----------- Building Stan model file: ------------"\n')
    #	clean up any existing model code
    cmd <- paste0(cmd, 'rm ', file.path('$CWD',paste0(args$stanModelFile[i],'.*')), ' \n')
    #	copy stan model file
    cmd	<- paste0(cmd, 'cp -R ',file.path(args$source_dir[i], 'stan-models',paste0(args$stanModelFile[i],'.stan')), ' ./\n')
    #	build model		
    cmd <- paste0(cmd, 'cd ', args$cmdstan_dir[i], '\n')
    cmd <- paste0(cmd, 'make STAN_THREADS=TRUE ', file.path('$CWD',args$stanModelFile[i]), ' \n')
    cmd <- paste0(cmd, 'cd $CWD\n')
    #	set up env variables
    cmd <- paste0( cmd, 'JOB_DIR=$(ls -d "',tmpdir,'"/*/)\n')
    cmd <- paste0( cmd, 'JOB_DIR=${JOB_DIR%?}\n')
    cmd <- paste0( cmd, 'JOB_DIR_NAME=${JOB_DIR##*/}\n')
    cmd <- paste0( cmd, 'STAN_DATA_FILE=$(find ', tmpdir, ' -name "*cmdstanin.R")\n')
    cmd <- paste0( cmd, 'STAN_INIT_FILE=$(find ', tmpdir, ' -name "*cmdstaninit.R")\n')
    cmd <- paste0( cmd, 'STAN_OUT_FILE=', file.path('$JOB_DIR','${JOB_DIR##*/}_stanout.csv'),' \n')
    #	run model
    cmd <- paste0( cmd, 'echo "----------- env variables are: ------------"\n')
    cmd <- paste0( cmd, 'echo $JOB_DIR\n')
    cmd <- paste0( cmd, 'echo $JOB_DIR_NAME\n')
    cmd <- paste0( cmd, 'echo $STAN_DATA_FILE\n')
    cmd <- paste0( cmd, 'echo $STAN_OUT_FILE\n')
    cmd <- paste0( cmd, 'echo "----------- Starting Stan sampling: ------------"\n')
    #	
    tmp <- paste0( './',args$stanModelFile[i],' ',
                   'sample num_samples=',args$hmc_num_samples[i],' num_warmup=',args$hmc_num_warmup[i],' save_warmup=0 thin=1 ',
                   'adapt delta=0.95 ',
                   'algorithm=hmc engine=nuts max_depth=15 stepsize=',args$hmc_stepsize[i],' ',
                   'data file=$STAN_DATA_FILE ',
                   'init=$STAN_INIT_FILE ',
                   'random seed=',args$seed[i],' ',
                   'output file=$STAN_OUT_FILE' )
    cmd <- paste0(cmd, tmp, '\n')
    # convert csv to rdata
    cmd		<- paste0( cmd, 'echo "----------- Converting Stan output to RDA file: ------------"\n')
    tmp		<- paste0('Rscript ', file.path(args$source_dir[i],args$script_converting_file[i]), 
                   ' -csv_file "', "$STAN_OUT_FILE",'"',
                   ' -rda_file "', file.path('$JOB_DIR','${JOB_DIR##*/}_stanout.RData'),'"'
    )
    cmd		<- paste0(cmd, tmp, '\n')		
  }			
  
  #	general housekeeping
  cmd 	<- paste0( cmd, 'echo "----------- Copy files to out directory: ------------"\n')
  tmpdir2	<- file.path(args$out_dir[i], paste0(args$stanModelFile[i],'-',args$job_tag[i]))
  if(i==1)
  {
    dir.create(tmpdir2)		  		
  }
  cmd		<- paste0(cmd,"mkdir -p ",tmpdir2,'\n')
  cmd		<- paste0(cmd, 'cp -R "', tmpdir,'"/* ', tmpdir2,'\n')
  cmd		<- paste0(cmd, 'chmod -R g+rw ', tmpdir2,'\n')
  
  #	generate quantities 
  cmd <- paste0( cmd, 'echo "----------- Generating quantities: ------------"\n')
  cmd <- paste0( cmd, 'JOB_DIR2="',tmpdir2,'"/"$JOB_DIR_NAME" \n')
  cmd <- paste0( cmd, 'echo $JOB_DIR2\n')
  tmp <- length(unlist(strsplit(args$countries[i], split=',')))
  cmd <- paste0( cmd, paste0("echo {1..",tmp,"} | tr ' ' '\\n' | ") )
  tmp <- ifelse(args$cmdstan[i]==1, hpc.nproc.cmdstan, 1)
  stopifnot(is.numeric(tmp))
  cmd <- paste0( cmd, paste0('xargs -P ',tmp,' -n 1 -I {} ') )
  tmp <- paste0('Rscript ', file.path(args$source_dir[i],args$script_generate_quantities_file[i]),
                ' -indir.results "$JOB_DIR2"',
                ' -location.index {}',
                ' -with.flow 1')		
  cmd <- paste0(cmd, tmp,'\n')
  
  #	generate quantities for forecasts
  if(args$with_forecasts[i]==1) 
  {		 
    cmd <- paste0( cmd, 'echo "----------- Generating quantities for forecast: ------------"\n')
    cmd <- paste0( cmd, 'FORECAST_PERIOD=',90,'\n')
    
    Level = 12 # school level, until K12
    
    # school closure
    tmp <- length(unlist(strsplit(args$countries[i], split=',')))
    cmd <- paste0( cmd, paste0("echo {1..",tmp,"} | tr ' ' '\\n' | ") )
    tmp <- ifelse(args$cmdstan[i]==1, hpc.nproc.cmdstan, 1)
    stopifnot(is.numeric(tmp))
    cmd <- paste0( cmd, paste0('xargs -P ',tmp,' -n 1 -I {} ') )
    tmp <- paste0('Rscript ', file.path(args$source_dir[i],paste0(gsub('.r','',args$script_generate_quantities_file[i],fixed=TRUE),'-forecast-v120.r')),
                  ' -indir.results "$JOB_DIR2"',
                  ' -location.index {}',
                  ' -with.flow 1',
                  ' -forecast.period $FORECAST_PERIOD',
                  ' -school.reopen 0',
                  ' -multiplier_cntct_school_opening 1',
                  ' -school_level K', Level, 
                  ' -counterfactual.scenario ', 0)		
    cmd <- paste0(cmd, tmp,'\n')
    
    # school reopening forecast
    tmp <- length(unlist(strsplit(args$countries[i], split=',')))
    cmd <- paste0( cmd, paste0("echo {1..",tmp,"} | tr ' ' '\\n' | ") )
    tmp <- ifelse(args$cmdstan[i]==1, hpc.nproc.cmdstan, 1)
    stopifnot(is.numeric(tmp))
    cmd <- paste0( cmd, paste0('xargs -P ',tmp,' -n 1 -I {} ') )
    tmp <- paste0('Rscript ', file.path(args$source_dir[i],paste0(gsub('.r','',args$script_generate_quantities_file[i],fixed=TRUE),'-forecast-v120.r')),
                  ' -indir.results "$JOB_DIR2"',
                  ' -location.index {}',
                  ' -with.flow 1',
                  ' -forecast.period $FORECAST_PERIOD',
                  ' -school.reopen 1',
                  ' -multiplier_cntct_school_opening 1',
                  ' -school_level K', Level, 
                  ' -counterfactual.scenario ', 0)		
    cmd <- paste0(cmd, tmp,'\n')
    
  }
  
  # create post-processing shell script for central analyses
  if(i==1)
  {
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n'
    )
    # save posterior samples
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','save-posterior-samples.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # start of remaining postprocessing files
    post.processing.file <- file.path(tmpdir2, 'post_processing_assess_mixing.sh')
    cmd2 	<- paste0(cmd2, 'cd ', dirname(post.processing.file),'\n')
    cmd2 	<- paste0(cmd2,'\\. ', basename(post.processing.file),'\n')
    post.processing.file <- file.path(tmpdir2, 'post_processing_etas.sh')
    cmd2 	<- paste0(cmd2,'\\. ', basename(post.processing.file),'\n')
    post.processing.file <- file.path(tmpdir2, 'post_processing.sh')
    cmd2 	<- paste0(cmd2,'\\. ', basename(post.processing.file),'\n')
    # write submission file	
    post.processing.file <- file.path(tmpdir2, 'post_processing_save_posterior_samples.sh')
    cat(cmd2, file=post.processing.file)
    # set permissions
    Sys.chmod(post.processing.file, mode='775')	
    # assess mixing subsmission script
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n'
    )
    # assess mixing
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-assess-mixing.R'), 
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # write submission file	
    post.processing.file <- file.path(tmpdir2, 'post_processing_assess_mixing.sh')
    cat(cmd2, file=post.processing.file)
    # set permissions
    Sys.chmod(post.processing.file, mode='775')	
    
    # make etas subsmission script
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n'
    )
    # postprocessing  etas
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-etas.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')		
    cmd2 <- paste0(cmd2,tmp,'\n')
    # write submission file	
    post.processing.file <- file.path(tmpdir2, 'post_processing_etas.sh')
    cat(cmd2, file=post.processing.file)
    # set permissions
    Sys.chmod(post.processing.file, mode='775')	
    
    # wrap remaing postprocessing in a subsmission script
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n'
    )
    # postprocessing  state-pars
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-state-pars.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG')		
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # postprocessing  age-pars
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-age-pars.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG')		
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing  flows
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-flows.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG')		
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make flow by age table
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-flow-byage-table.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing  mobility analysis
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-fsq-mobility-analysis.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')		
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # postprocessing  make cases-deaths-Rt plot
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-newcases-overalldeaths-Rt-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')		
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make Rt by age table
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-Rt-byage-table.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -period_length $PERIOD_LENGTH')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make Rt less than one classifications
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-Rtlessthan1-classifications.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # post processing ifr table
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-ifr-table.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')	
    # postprocessing make attack rates table
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-attackrate-tables.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # post processing antibody validation table
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-summarise-antibody.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')	
    # post processing validate attack rate
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-validate-attackrates.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG')
    cmd2 <- paste0(cmd2,tmp,'\n')	
    # postprocessing make prop-susceptible-byage-plot
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-prop-susceptible-byage-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # postprocessing make age-cases-plots
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-plot-age-newcases.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # postprocessing make deaths panel plots
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-deaths-panel-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make deaths-mobility-contacts panel plot
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-deaths-mobility-contacts-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make five panel plot of pars-gqs by report age groupings
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-five-panel-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # postprocessing make summary of deaths-eacases-Rt for 3 states
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-deaths-eacases-Rt-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make splot of expected death vs observed death
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-daily-weekly-deaths-expected-observed-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make panel plot of obs death and cases share
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-deathsage-casesage-plot-v120.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make plot of Rmta for all states
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-aRt-allstates-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing make plot of eacases over time for 20-49yo
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-make-eacases-eff-over-time-plot.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # postprocessing knit report
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-knit-report.R'),
                  ' -rmd_file "', args$script_rmd_file[i],'" -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG',				
                  ' -report_dir "', args$report_dir[i],'"'						
    )
    cmd2 <- paste0(cmd2,tmp,'\n')
    # write submission file	
    post.processing.file <- file.path(tmpdir2, 'post_processing.sh')
    cat(cmd2, file=post.processing.file)
    # set permissions
    Sys.chmod(post.processing.file, mode='775')		
  }
  
  if(i==1 & args$with_forecasts[i]==1) 
  {
    #
    # create postprocessing shell script for reopen=0 forecast 
    
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n',
                   'FORECAST_PERIOD=',90,'\n',
                   'COUNTERFACTUAL_SCENARIO=',0,'\n',
                   'MULTIPLIER_CNTCT_OPENING=',1.0,'\n',
                   'SCHOOL_LEVEL=','K',Level,'\n'
    )
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','save-posterior-samples-forecast.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS -school.reopen 0 -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING  -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')		
    # write submission file	 
    post.processing.file.forecast <- file.path(tmpdir2, paste0('post_processing_forecast_reopen_0','_level_K',Level,'.sh'))
    cat(cmd2, file=post.processing.file.forecast)
    # set permissions
    Sys.chmod(post.processing.file.forecast, mode='775')	
    
    #
    # create postprocessing shell script for reopen=1 forecast 
    cmd2 <- '#!/bin/sh\n'
    cmd2 <- paste0(cmd2,'\n')
    cmd2 <- paste0(cmd2,'CWD=$(pwd)\n')
    cmd2	<- paste0(cmd2,"echo $CWD\n")	
    # set up env variables
    cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
                   'OUT_DIR=',tmpdir2,'\n',
                   'JOB_TAG=',args$job_tag[i],'\n',
                   'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
                   'NUMB_CHAINS=', max(args$chain),'\n',
                   'OVERWRITE=0\n',
                   'PERIOD_LENGTH=7\n',
                   'WITH_FORECAST=',0,'\n',
                   'COUNTERFACTUAL_SCENARIO=',0,'\n',
                   'MULTIPLIER_CNTCT_OPENING=',1.0,'\n',
                   'SCHOOL_LEVEL=K',Level,'\n'
    )
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','save-posterior-samples-forecast.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS -school.reopen 1 -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-contributions.r'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-increase-deaths-cases.r'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-plot-deaths.r'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-plot-cases.R'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-increase-Rt.r'),
                  ' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -school_level $SCHOOL_LEVEL -period_length $PERIOD_LENGTH -counterfactual.scenario $COUNTERFACTUAL_SCENARIO')
    cmd2 <- paste0(cmd2,tmp,'\n')
    tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-knit-report-forecast.r'),
                  ' -rmd_file scripts/post-processing-make-report-forecast.Rmd -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -report_dir "', args$report_dir[i],'"')
    cmd2 <- paste0(cmd2,tmp,'\n')
    # write submission file	 
    post.processing.file.forecast <- file.path(tmpdir2, paste0('post_processing_forecast_reopen_1_school_counterfactual_0_multiplier_',as.integer(1.0*100),'_level_K',Level,'.sh'))
    cat(cmd2, file=post.processing.file.forecast)
    # set permissions
    Sys.chmod(post.processing.file.forecast, mode='775')	
    
  }
  cmds[[i]]	<- cmd	
}	

#	make bash scripts
for(i in seq_len(nrow(args)))
{
  tmpdir2	<- file.path(args$out_dir[i], paste0(args$stanModelFile[i],'-',args$job_tag[i]))
  tmp <- file.path(tmpdir2,paste0('startme-',args$stanModelFile[i],'-',args$job_tag[i],'-hmc',i,'.sh'))
  cat('\nWriting shell script to ',tmp)
  cat(cmds[[i]], file=tmp)	
  # set permissions
  Sys.chmod(tmp, mode='775')
}

