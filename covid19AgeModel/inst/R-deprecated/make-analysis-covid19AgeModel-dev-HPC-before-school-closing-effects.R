require(data.table)
require(covid19AgeModel)

pkg.dir <- system.file(package = "covid19AgeModel" )

## important note:
# the combination of stanModelFile and job_tag should be unique for each analysis
# all outputs with stanModelFile-job_tag are assumed to be several HMC chains run in parallel

#	function to make PBS header
make.PBS.header <- function(hpc.walltime=47, hpc.select=1, hpc.nproc=1, hpc.mem= "6gb", hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", hpc.q="pqcovid19c", hpc.array=1 )
{	
	pbshead <- "#!/bin/sh"
	tmp <- paste("#PBS -l walltime=", hpc.walltime, ":59:00", sep = "")
	pbshead <- paste(pbshead, tmp, sep = "\n")
	tmp <- paste("#PBS -l select=", hpc.select, ":ncpus=", hpc.nproc,":ompthreads=", hpc.nproc,":mem=", hpc.mem, sep = "")	
	pbshead <- paste(pbshead, tmp, sep = "\n")
	pbshead <- paste(pbshead, "#PBS -j oe", sep = "\n")
	if(hpc.array>1)
	{
		pbshead	<- paste(pbshead, "\n#PBS -J 1-", hpc.array, sep='')
	}				
	if(!is.na(hpc.q))
	{
		pbshead <- paste(pbshead, paste("#PBS -q", hpc.q), sep = "\n")
	}		
	pbshead	<- paste(pbshead, hpc.load, sep = "\n")
	pbshead
}

## Xiaoyue args 
if(0)
{	
  countries <- "AL,AZ,CA,CO,CT,FL,GA,IL,IN,LA,MA,MD,MI,MS,NC,NJ,NYC,TN,WA"
  countries <- "CA,FL,GA,IL,MI,PA"
  n_countries <- length(unlist(strsplit(countries,',')))
  hpc.nproc.cmdstan <- n_countries
  args <- data.table(
    source_dir= '~/R0t',
    cmdstan_dir = '/apps/cmdstan/2.33.0',
    out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa',	
    report_dir = '/rds/general/project/ratmann_covid19/live/age_renewal_usa/reports',	
	script_file= 'scripts/stan-make-data-usa-dev.r',
	script_converting_file = "scripts/stan-convert-csv-to-rda.r",
	script_generate_quantities_file = "scripts/generate-quantities.r",
	script_rmd_file = "scripts/post-processing-make-report.Rmd",
	stanModelFile= 'base_age_fsq_mobility_200703n3_cmdstanv',
    hmc_stepsize= 0.02,
    hmc_num_samples= 2000,
    hmc_num_warmup= 1500,
    seed= 42,
    chain= 1,
    job_tag= '19states_devcntct_stepsize002',
    countries = countries,	
    cmdstan = 1L,
    multiplier_cntct_school_closure = 1,
	with_forecasts = 1,
    forecast_with_schools_reopened = 0, 
	ifr_by_age_prior = "BetaBinomial"
  )
}

## Melodie args 
if(1)
{
	countries <- "CO,CT,FL,NYC"
	countries <- "AL,AZ,CA,CO,CT,FL,GA,IL,IN,LA,MA,MD,MI,MS,NC,NJ,NYC,TN,WA"
	n_countries <- length(unlist(strsplit(countries,',')))
	hpc.nproc.cmdstan <- n_countries	
	args <- data.table(
			source_dir= pkg.dir,
			cmdstan_dir = '/apps/cmdstan/2.33.0',
			out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa',
			report_dir = '/rds/general/project/ratmann_covid19/live/age_renewal_usa/reports',
			script_file= 'scripts/stan-make-data-usa-dev.r',
			script_converting_file = "scripts/stan-convert-csv-to-rda.r",
			script_generate_quantities_file = "scripts/generate-quantities.r",
			script_rmd_file = "scripts/post-processing-make-report.Rmd",
			stanModelFile= 'base_age_fsq_mobility_200821b9_cmdstanv',
			hmc_stepsize= 0.02,
			hmc_num_samples= 15,
			hmc_num_warmup= 10,			
			seed= 42,
			chain= 1,
			job_tag= 'test_newsubmitscript',
			countries = countries,			
			cmdstan = 1L,
			multiplier_cntct_school_closure = 1,
			with_forecasts = 1,
			forecast_with_schools_reopened = 0, 
			ifr_by_age_prior = "BetaBinomial"
			)	
}

## Alex args 
if(0)
{	
	countries <- "CO,CT,FL,NYC"
	n_countries <- length(unlist(strsplit(countries,',')))
	hpc.nproc.cmdstan <- ifelse(n_countries>=20,floor(n_countries/2),n_countries) 
	args <- data.table(
		source_dir= '/rdsgpfs/general/user/ablenkin/home/git/R0t',
		cmdstan_dir = '/apps/cmdstan/2.33.0',
		out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa',	
		report_dir = '/rds/general/project/ratmann_covid19/live/age_renewal_usa/reports',	
		script_file= 'scripts/stan-make-data-usa-dev.r',
		script_converting_file = "scripts/stan-convert-csv-to-rda.r",
		script_generate_quantities_file = "scripts/generate-quantities.r",
		script_rmd_file = "scripts/post-processing-make-report.Rmd",
		stanModelFile= 'base_age_fsq_mobility_200730a2_cmdstanv',
		hmc_stepsize= 0.02,
		hmc_num_samples= 1500,
		hmc_num_warmup= 1000,
		seed= 42,
		chain= 1,
		job_tag= '4states_mobtrendsinto3_2betas',
		countries = countries,	
		cmdstan = 1L,
		multiplier_cntct_school_closure = 1,
		with_forecasts = 1,
		forecast_with_schools_reopened = 0, 
		ifr_by_age_prior = "BetaBinomial"
	)
}

## olli s args cmdstan
if(1)
{	
	countries <- "CO,CT,FL,NYC"
	#countries <- "DC,CO,CT,ID,FL,NYC"
	#countries <- "AK,AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MO,MS,NC,ND,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,UT,VA,VT,WA,WI"
	#countries <-  "AL,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KY,LA,MA,MD,MI,MO,MS,NC,NH,NJ,NM,NV,NYC,OK,OR,PA,RI,SC,TN,TX,UT,VA,WA,WI"
	n_countries <- length(unlist(strsplit(countries,',')))
	hpc.nproc.cmdstan <- ifelse(n_countries>=20,floor(n_countries/2),n_countries)
	hpc.nproc.cmdstan <- n_countries
	args <- data.table(			
			source_dir= pkg.dir,
			#cmdstan_dir = '~/sandbox/cmdstan-2.23.0',
			#out_dir= '~/sandbox',						
			#report_dir = '~/sandbox',			
			cmdstan_dir = '/apps/cmdstan/2.33.0',
			out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa',	
			report_dir = '/rds/general/project/ratmann_covid19/live/age_renewal_usa/reports',	
			script_file= 'scripts/stan-make-data-usa-dev.r',
			script_converting_file = "scripts/stan-convert-csv-to-rda.r",
			script_generate_quantities_file = "scripts/generate-quantities.r",
			script_rmd_file = "scripts/post-processing-make-report.Rmd",
			stanModelFile= 'base_age_fsq_mobility_200923b9_cmdstanv',
			hmc_stepsize= 0.02,
			hmc_num_samples= 1500,
			hmc_num_warmup= 1000,
			seed= 42,
			chain= 1,
			job_tag= '4states_tau10_Sep20',			
			countries = countries,	
			cmdstan = 1L,
			multiplier_cntct_school_closure = 1,
			with_forecasts = 0,
			forecast_with_schools_reopened = 1, 
			ifr_by_age_prior = "BetaBinomial"		
			)
}



if(exists('hpc.nproc.cmdstan'))
{
	stopifnot( hpc.nproc.cmdstan<=length(unlist(strsplit(args$countries, split=','))) )	
}

if(1)
{
  tmp <- data.table(chain=1:8)		
  tmp[, seed:= round(runif(seq_len(nrow(tmp)))*1e6)]		
  set(args, NULL, colnames(tmp), NULL)
  tmp[, dummy:= 1L]
  args[, dummy:= 1L]
  args <- merge(args, tmp, by='dummy')
  set(args, NULL, 'dummy', NULL)	
}

# make commands
cmds <- vector('list', nrow(args))
for(i in seq_len(nrow(args)))
{
	cmd				<- ''			
	#	general housekeeping
	cmd				<- paste0(cmd,"CWD=$(pwd)\n")
	cmd				<- paste0(cmd,"echo $CWD\n")	
	tmpdir.prefix	<- paste0('cvd_',format(Sys.time(),"%y-%m-%d-%H-%M-%S"))
	tmpdir			<- paste0("$CWD/",tmpdir.prefix)
	cmd				<- paste0(cmd,"mkdir -p ",tmpdir,'\n')	
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
							' -forecast_with_schools_reopened ', args$forecast_with_schools_reopened[i],
							' -ifr_by_age_prior "', args$ifr_by_age_prior[i], '"'
							)
	cmd				<- paste0(cmd, tmp, '\n')
	#	if using cmdstan  
	if(args$cmdstan[i]==1) 
	{
		cmd <- paste0(cmd, 'echo "----------- Building Stan model file: ------------"\n')
		#	clean up any existing model code
		cmd <- paste0(cmd, 'rm ', file.path('$CWD',paste0(args$stanModelFile[i],'.*')), ' \n')
		#	copy stan model file
		cmd	<- paste0(cmd, 'cp -R ',file.path(args$source_dir[i], 'stan-models',paste0(args$stanModelFile[i],'.stan')),' .\n')
		#	build model		
		cmd <- paste0(cmd, 'cd ', args$cmdstan_dir[i], '\n')
		cmd <- paste0(cmd, 'make ', file.path('$CWD',args$stanModelFile[i]), ' \n')
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
	cmd		<- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/* ', tmpdir2,'\n')
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
		# school closure
		cmd <- paste0( cmd, 'FORECAST_PERIOD=',90,'\n')
		tmp <- length(unlist(strsplit(args$countries[i], split=',')))
		cmd <- paste0( cmd, paste0("echo {1..",tmp,"} | tr ' ' '\\n' | ") )
		tmp <- ifelse(args$cmdstan[i]==1, hpc.nproc.cmdstan, 1)
		stopifnot(is.numeric(tmp))
		cmd <- paste0( cmd, paste0('xargs -P ',tmp,' -n 1 -I {} ') )
		tmp <- paste0('Rscript ', file.path(args$source_dir[i],paste0(gsub('.r','',args$script_generate_quantities_file[i],fixed=TRUE),'-forecast.r')),
		              ' -indir.results "$JOB_DIR2"',
		              ' -location.index {}',
		              ' -with.flow 1',
		              ' -forecast.period $FORECAST_PERIOD',
		              ' -school.reopen 0',
		              ' -multiplier_cntct_school_opening 1')		
		cmd <- paste0(cmd, tmp,'\n')
		# school reopening
		for(multiplier in c(0.2, 0.33, 0.5, 1.0)){
		  tmp <- length(unlist(strsplit(args$countries[i], split=',')))
		  cmd <- paste0( cmd, paste0("echo {1..",tmp,"} | tr ' ' '\\n' | ") )
		  tmp <- ifelse(args$cmdstan[i]==1, hpc.nproc.cmdstan, 1)
		  stopifnot(is.numeric(tmp))
		  cmd <- paste0( cmd, paste0('xargs -P ',tmp,' -n 1 -I {} ') )
		  tmp <- paste0('Rscript ', file.path(args$source_dir[i],paste0(gsub('.r','',args$script_generate_quantities_file[i],fixed=TRUE),'-forecast.r')),
		                ' -indir.results "$JOB_DIR2"',
		                ' -location.index {}',
		                ' -with.flow 1',
		                ' -forecast.period $FORECAST_PERIOD',
		                ' -school.reopen 1',
		                ' -multiplier_cntct_school_opening ', multiplier)		
		  cmd <- paste0(cmd, tmp,'\n')
		}
	}

	# create post-processing shell script for central analyses
	if(i==1)
	{
		cmd2 <- make.PBS.header(	hpc.walltime=23, 
		    	hpc.select=1, 
				hpc.nproc=1, 
		        hpc.mem= "550gb", 
		        hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", 
		        hpc.q="pqcovid19c", 
		        hpc.array= 1)
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
		# assess mixing
		tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-assess-mixing.R'), 
				' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS')
		cmd2 <- paste0(cmd2,tmp,'\n')
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
		# postprocessing  etas
		tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-etas.R'),
				' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST')		
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
		Sys.chmod(post.processing.file, mode='644')		
	}
	# create postprocessing shell script for reopen=0 forecast 
	if(i==1 & args$with_forecasts[i]==1) 
	{
		cmd2 <- make.PBS.header(	hpc.walltime=23, 
					hpc.select=1, 
					hpc.nproc=1, 
					hpc.mem= "550gb", 
					hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", 
					hpc.q="pqcovid19c", 
					hpc.array= 1)
		cmd2 <- paste0(cmd2,'\n')
		# set up env variables
		cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
				'OUT_DIR=',tmpdir2,'\n',
				'JOB_TAG=',args$job_tag[i],'\n',
				'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
				'NUMB_CHAINS=', max(args$chain),'\n',
				'OVERWRITE=0\n',
				'PERIOD_LENGTH=7\n',
				'WITH_FORECAST=',1,'\n',
				'FORECAST_PERIOD=',90,'\n',
				'MULTIPLIER_CNTCT_OPENING=',0.5,'\n'
		)
		tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','save-posterior-samples-forecast.R'),
				' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS -school.reopen 0 -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING')
		cmd2 <- paste0(cmd2,tmp,'\n')		
		# write submission file	 
		post.processing.file.forecast <- file.path(tmpdir2, 'post_processing_forecast_reopen_0_multiplier_50.sh')
		cat(cmd2, file=post.processing.file.forecast)
		# set permissions
		Sys.chmod(post.processing.file.forecast, mode='644')	
		
	}
	# create postprocessing shell script for reopen=1 forecast 
	if(i==1 & args$with_forecasts[i]==1) 
	{
		for(multiplier in c(0.2, 0.33, 0.5, 1.0))
		{
			cmd2 <- make.PBS.header(	hpc.walltime=23, 
				hpc.select=1, 
				hpc.nproc=1, 
				hpc.mem= "550gb", 
				hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", 
				hpc.q="pqcovid19c", 
				hpc.array= 1)
			cmd2 <- paste0(cmd2,'\n')
			# set up env variables
			cmd2 <- paste0(cmd2,'SCRIPT_DIR=',args$source_dir[i],'\n',			
				'OUT_DIR=',tmpdir2,'\n',
				'JOB_TAG=',args$job_tag[i],'\n',
				'STAN_MODEL_FILE=',args$stanModelFile[i],'\n',
				'NUMB_CHAINS=', max(args$chain),'\n',
				'OVERWRITE=0\n',
				'PERIOD_LENGTH=7\n',
				'WITH_FORECAST=',1,'\n',
				'MULTIPLIER_CNTCT_OPENING=',multiplier,'\n'
				)
			tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','save-posterior-samples-forecast.R'),
					' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -numb_chains $NUMB_CHAINS -school.reopen 1 -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING')
			cmd2 <- paste0(cmd2,tmp,'\n')
			tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-contributions.r'),
					' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING')
			cmd2 <- paste0(cmd2,tmp,'\n')
			tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-increase-deaths-cases.r'),
					' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING')
			cmd2 <- paste0(cmd2,tmp,'\n')
			tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-plot-deaths.r'),
					' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING')
			cmd2 <- paste0(cmd2,tmp,'\n')
			tmp <- paste0('Rscript ', file.path('$SCRIPT_DIR','scripts','post-processing-forecast-increase-Rt.r'),
					' -stanModelFile $STAN_MODEL_FILE -out_dir $OUT_DIR -job_tag $JOB_TAG -overwrite $OVERWRITE -with_forecast $WITH_FORECAST -multiplier_cntct_school_opening $MULTIPLIER_CNTCT_OPENING -period_length $PERIOD_LENGTH')
			cmd2 <- paste0(cmd2,tmp,'\n')
			# write submission file	 
			post.processing.file.forecast <- file.path(tmpdir2, paste0('post_processing_forecast_reopen_1_multiplier_',as.integer(multiplier*100),'.sh'))
			cat(cmd2, file=post.processing.file.forecast)
			# set permissions
			Sys.chmod(post.processing.file.forecast, mode='644')	
		}
	}
		
	#	schedule post-processing	
	cmd		<- paste0( cmd, 'echo "----------- Post-processing: ------------"\n')
	tmp		<- paste("if [ $(find ",tmpdir2," -name '*_stanout.RData' | wc -l) -ge ",max( args$chain )," ]; then\n",sep='')
	cmd		<- paste(cmd,tmp,sep='')	
	post.processing.file <- file.path(tmpdir2, 'post_processing.sh')
	cmd 	<- paste0(cmd, '\tcd ', dirname(post.processing.file),'\n')
	cmd 	<- paste0(cmd,'\tqsub ', basename(post.processing.file),'\n')
	if(args$with_forecasts[i]==1) 
	{	  
	  cmd 	<- paste0(cmd, '\tqsub post_processing_forecast_reopen_0_multiplier_50.sh\n')
	  cmd 	<- paste0(cmd, '\tqsub post_processing_forecast_reopen_1_multiplier_20.sh\n')
	  cmd 	<- paste0(cmd, '\tqsub post_processing_forecast_reopen_1_multiplier_33.sh\n')
	  cmd 	<- paste0(cmd, '\tqsub post_processing_forecast_reopen_1_multiplier_50.sh\n')
	  cmd 	<- paste0(cmd, '\tqsub post_processing_forecast_reopen_1_multiplier_100.sh\n')
	}
	cmd		<- paste0(cmd,"fi\n")
	cmd		<- paste(cmd, "rm -rf $CWD/", basename(args$source_dir[i]),'\n',sep='')
	cat(cmd)
	cmds[[i]]	<- cmd	
}	


if(args$cmdstan[1]==0)
{
	pbshead <- make.PBS.header(	hpc.walltime=63, 
			hpc.select=1, 
			hpc.nproc=1, 
			hpc.mem= "6gb", 
			hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", 
			hpc.q="pqcovid19c", 
			hpc.array= length(cmds) )
}
if(args$cmdstan[1]==1)
{ 
	pbshead <- make.PBS.header(	hpc.walltime=63, 
			hpc.select=1, 
			hpc.nproc=hpc.nproc.cmdstan, 
			hpc.mem= paste0(hpc.nproc.cmdstan*9,'gb'), 
			hpc.load= paste0("module load cmdstan/2.33.0 anaconda3/personal\nsource activate covid19AgeModel\nexport STAN_NUM_THREADS=",hpc.nproc.cmdstan,"\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE"), 
			hpc.q="pqcovid19c", 
			hpc.array= length(cmds) )
}


#	make array job
for(i in seq_len(nrow(args)))
{
  cmds[[i]] <- paste0(i,')\n',cmds[[i]],';;\n')
}
cmd		<- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse=''),'esac')			
cmd		<- paste(pbshead,cmd,sep='\n')

#	submit job
outfile		<- gsub(':','',paste("cvd",paste(strsplit(date(),split=' ')[[1]],collapse='_',sep=''),'sh',sep='.'))
outfile		<- file.path(args$out_dir[1], outfile)
cat(cmd, file=outfile)
cmd 		<- paste("qsub", outfile)
cat(cmd)
cat(system(cmd, intern= TRUE))	
