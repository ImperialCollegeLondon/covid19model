require(data.table)

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

## Melodie args 
if(0)
{
  hpc.nproc.cmdstan <- 2	
  args <- data.table(
    source_dir= '~/git/R0t',
    cmdstan_dir = '/apps/cmdstan/2.33.0',
    out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa/ifr_by_age_prior',
    script_file= 'usa/code/base-ages-find-ifr-by-age-prior.r',
    script_converting_file = "utils/convert_csv_to_rda.r",
    stanModelFile= 'base_age_prior_ifr_200817_cmdstanv',
    hmc_num_samples= 15,
    hmc_num_warmup= 10,			
    job_tag= 'first_run',
    dummy= 1L,
    ovrcnt = 1L,
    cmdstan = 1L,
    seed= 42,
    chain= 1
  )	
}

## olli args 
if(0)
{
	hpc.nproc.cmdstan <- 34/2	
	args <- data.table(
		source_dir= '/rds/general/user/or105/home/libs/R0t',
		cmdstan_dir = '/apps/cmdstan/2.33.0',
		out_dir= '/rds/general/project/ratmann_covid19/live/age_renewal_usa/ifr_by_age_prior',
		script_file= 'usa/code/base-ages-find-ifr-by-age-prior.r',
		script_converting_file = "utils/convert_csv_to_rda.r",
		stanModelFile= 'base_age_prior_ifr_200820a5_cmdstanv',
		hmc_num_samples= 1000,
		hmc_num_warmup= 500,			
		job_tag= 'first_run',
		dummy= 1L,
		ovrcnt = 1L,
		cmdstan = 1L,
		seed= 42,
		chain= 1
		)	
}

## olli args 
if(1)
{
	hpc.nproc.cmdstan <- 6	
	args <- data.table(
			source_dir= '/Users/or105/git/R0t',
			cmdstan_dir = '/Users/or105/sandbox/cmdstan-2.23.0',
			out_dir= '/Users/or105/sandbox',
			script_file= 'usa/code/base-ages-find-ifr-by-age-prior.r',
			script_converting_file = "utils/convert_csv_to_rda.r",
			stanModelFile= 'base_age_prior_ifr_200820a4_cmdstanv',
			hmc_num_samples= 1000,
			hmc_num_warmup= 500,			
			job_tag= 'first_run',
			dummy= 1L,
			ovrcnt = 1L,
			cmdstan = 1L,
			seed= 42,
			chain= 1
	)	
}

if(1)
{
  tmp <- data.table(chain=1:2)		
  tmp[, seed:= round(runif(seq_len(nrow(tmp)))*1e6)]		
  set(args, NULL, colnames(tmp), NULL)
  tmp[, dummy:= 1L]
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
  cmd				<- paste0(cmd,'cp -R ',args$source_dir[i],' .\n')
  #	generate data set and run if not using cmdstan
  cmd 			<- paste0( cmd, 'echo "----------- Generating input data: ------------"\n')
  tmp 			<- paste0('Rscript ', file.path('$CWD',basename(args$source_dir[i]),args$script_file[i]), 
                   ' -seed ', args$seed[i],
                   ' -chain ', args$chain[i],
                   ' -indir ', file.path('$CWD',basename(args$source_dir[i])),'',
                   ' -outdir ', tmpdir,'',
                   ' -cmdstan ', args$cmdstan[i],
                   ' -stanModelFile "', args$stanModelFile[i],'"',
                   ' -jobtag "', args$job_tag[i],'"'
  )
  cmd				<- paste0(cmd, tmp, '\n')
  #	if using cmdstan
  if(args$cmdstan[i]==1)
  {
    #	clean up any existing model code
    cmd <- paste0(cmd, 'rm ', file.path(basename(args$source_dir[i]),'stan-models',args$stanModelFile[i]), ' \n')
    cmd <- paste0(cmd, 'rm ', file.path(basename(args$source_dir[i]),'stan-models',paste0(args$stanModelFile[i], '.d')),' \n')
    cmd <- paste0(cmd, 'rm ', file.path(basename(args$source_dir[i]),'stan-models',paste0(args$stanModelFile[i], '.hpp')),' \n')
    cmd <- paste0(cmd, 'rm ', file.path(basename(args$source_dir[i]),'stan-models',paste0(args$stanModelFile[i], '.o')),' \n')
    #	build model
    cmd <- paste0( cmd, 'echo "----------- Building Stan model file: ------------"\n')
    cmd <- paste0(cmd, 'cd ', args$cmdstan_dir[i], '\n')
    cmd <- paste0(cmd, 'make ', file.path('$CWD',basename(args$source_dir[i]),'stan-models',args$stanModelFile[i]), ' \n')
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
    tmp <- paste0( file.path(basename(args$source_dir[i]),'stan-models',args$stanModelFile[i]),' ',
                   'sample num_samples=',args$hmc_num_samples[i],' num_warmup=',args$hmc_num_warmup[i],' save_warmup=0 thin=1 ',
                   'adapt delta=0.95 ',
                   'algorithm=hmc engine=nuts max_depth=15 ',
                   'data file=$STAN_DATA_FILE ',
				   'init=$STAN_INIT_FILE ',
                   'random seed=',args$seed[i],' ',
                   'output file=$STAN_OUT_FILE' )
    cmd <- paste0(cmd, tmp, '\n')
    # convert csv to rdata
    cmd		<- paste0( cmd, 'echo "----------- Converting Stan output to RDA file: ------------"\n')
    tmp		<- paste0('Rscript ', file.path('$CWD',basename(args$source_dir[i]),args$script_converting_file[i]), 
                   ' -csv_file "', "$STAN_OUT_FILE",'"',
                   ' -rda_file "', file.path('$JOB_DIR','${JOB_DIR##*/}_stanout.RData'),'"'
    )
    cmd		<- paste0(cmd, tmp, '\n')
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
    cmd		<- paste(cmd, "rm -rf ", tmpdir,'\n',sep='')
    
    cat(cmd)
    cmds[[i]]	<- cmd
  }
}
  

if(args$cmdstan[1]==0)
{
  pbshead <- make.PBS.header(	hpc.walltime=47, 
                              hpc.select=1, 
                              hpc.nproc=1, 
                              hpc.mem= "6gb", 
                              hpc.load= "module load anaconda3/personal\nsource activate covid19AgeModel", 
                              hpc.q="pqcovid19c", 
                              hpc.array= length(cmds) )
}
if(args$cmdstan[1]==1)
{ 
  pbshead <- make.PBS.header(	hpc.walltime=47, 
                              hpc.select=1, 
                              hpc.nproc=hpc.nproc.cmdstan, 
                              hpc.mem= paste0(hpc.nproc.cmdstan*2,'gb'), 
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
  