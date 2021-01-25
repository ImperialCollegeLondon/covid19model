**UPDATE:** 

**25 January 2021 update** package version 1.2.0 <br/>

- Added data and code for analysis of the final revision for publication 

**07 January 2021 update** package version 1.1.0 <br/>

- Added data and code for analysis of Imperial Report 32 v2


---
---

# covid19AgeModel: COVID19 contact and infection model of the Imperial College London COVID-19 Response Team

## Overview
This repository contains the code for the contact-and-infection model of the Imperial College London COVID-19 Response Team, which is an extension of the Bayesian semi-mechanistic model of [Flaxman et al](https://www.nature.com/articles/s41586-020-2405-7).

The model and associated Bayesian analysis via [Stan](https://mc-stan.org/users/interfaces/cmdstan) have been used in 
* M Monod, A Blenkinsop, X Xi et al. [Report 32: Age groups that sustain resurging COVID-19 epidemics in the United States](https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-09-17-COVID19-Report-32.pdf), version 1, Imperial College London (17-09-2020), doi: https://doi.org/10.25561/82551.
* M Monod, A Blenkinsop, X Xi et al. [Report 32: Age groups that sustain resurging COVID-19 epidemics in the United States](https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-09-17-COVID19-Report-32.pdf), version 2, Imperial College London (07-01-2021), doi: https://doi.org/10.25561/82551.

## Data
The data in ```covid19AgeModel/inst/data-Report32-200917``` contain
* a snapshot of the age-specific COVID-19 mortality data used in Imperial Report 32 v1, in file ```DeathsByAge_US_200909.csv```. [Please see the links at this location for a complete list of data sources from US state DoH](https://github.com/ImperialCollegeLondon/US-covid19-agespecific-mortality-data).
* a snapshot of the age-specific mobility data from Foursquare Inc used in in Imperial Report 32 v1, in file ```fsq_visit_data_aug_refresh_200829.csv```
* a snapshot of the age-specific mobility data from Emodo Inc used in Imperial Report 32 v1, in file ```chi_emodo_mobility_trends.rds```

The data in ```covid19AgeModel/inst/data-v110``` contain
* a snapshot of the age-specific COVID-19 mortality data used in Imperial Report 32 v2, in file ```DeathsByAge_US_201029.csv```. [Please see the links at this location for a complete list of data sources from US state DoH](https://github.com/ImperialCollegeLondon/US-covid19-agespecific-mortality-data).
* a snapshot of the age-specific mobility data from Foursquare Inc used in Imperial Report 32 v2, in file ```fsq_visit_data_nov_refresh_201112.csv```
* a snapshot of the age-specific mobility data from Emodo Inc used in Imperial Report 32 v2, in file ```chi_emodo_mobility_trends.rds```

The data in ```covid19AgeModel/inst/data-v120``` contain
* a snapshot of the age-specific COVID-19 mortality data used in the final revision for publication, in file ```DeathsByAge_US_201029_cured.csv```. [Please see the links at this location for a complete list of data sources from US state DoH](https://github.com/ImperialCollegeLondon/US-covid19-agespecific-mortality-data).
* a snapshot of the age-specific mobility data from Foursquare Inc used in the final revision for publication, in file ```fsq_visit_data_dec_refresh_201029.csv```
* a snapshot of the age-specific mobility data from Emodo Inc used in the final revision for publication, in file ```chi_emodo_mobility_trends.rds```

The data in ```covid19AgeModel/inst/data``` contain
* updated age-specific COVID-19 mortality data. These data were taken from the [US Covid-19 age-specific mortality data set](https://github.com/ImperialCollegeLondon/US-covid19-agespecific-mortality-data) of the Imperial College London COVID-19 Response Team.
* updated mobility data from Foursquare Inc

## License
- The code in this repository is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) by Imperial College London on behalf of its COVID-19 Response Team. Copyright Imperial College London 2020. 
- The mobility data set from Foursquare Inc. in this repository are re-distributed under [CC-BY-NC-ND-4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/). This comprises files starting with ```fsq``` in the data directories. 
- The mobility data set from Emodo Inc. in this repository are re-distributed under [CC-BY-NC-4.0](https://creativecommons.org/licenses/by-nc/4.0/). This comprises files starting with ```emodo``` in the data directories. 
- The age-specific COVID-19 mortality data alone are available under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) from [US Covid-19 age-specific mortality data set](https://github.com/ImperialCollegeLondon/US-covid19-agespecific-mortality-data).

## Warranty

Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.

## Cite

Please cite 
* M Monod, A Blenkinsop, X Xi et al. [Report 32: Age groups that sustain resurging COVID-19 epidemics in the United States](https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-09-17-COVID19-Report-32.pdf), version 2, Imperial College London (07-01-2021), doi: https://doi.org/10.25561/82551.

## Acknowledgements

This work was supported by the NIHR HPRU in Modelling and Health Economics, a partnership between PHE, Imperial College London and LSHTM (grant code NIHR200908) and the Imperial College Research Computing Service DOI:10.14469/hpc/2232; and acknowledges funding from the Imperial College COVID-19 Response Fund, the Bill & Melinda Gates Foundation, the EPSRC through the EPSRC Centre for Doctoral Training in Modern Statistics and Statistical Machine Learning at Imperial and Oxford, the MRC Centre for Global Infectious Disease Analysis (reference MR/R015600/1), jointly funded by the UK Medical Research Council (MRC) and the UK Foreign, Commonwealth & Development Office (FCDO), under the MRC/FCDO Concordat agreement and is also part of the EDCTP2 programme supported by the European Union. We would like to thank Microsoft and Amazon for providing cloud computing services. We acknowledge the data support of [Foursquare Inc.](http://foursquare.com/), and US state Departments of Health that publish daily updates of COVID-19 attributable death data.

## Funding

This research was partly funded by the The Imperial College COVID-19 Research Fund.

## System Requirements
- macOS or UNIX, the code was developed on macOS Mojave 10.14
- [R](https://www.r-project.org/) version >= 3.6.1
- [cmdstan](https://mc-stan.org/users/interfaces/cmdstan) version >= 2.23. To allow for multi-threading, please add the necessary flags: 
1. On a Linux machine:
```bash
$ cd cmdstan_dir
$ echo "CXXFLAGS += -DSTAN_THREADS" > make/local
$ echo "CXXFLAGS += -pthread" >> make/local
```
2. On a Macintosh machine
```bash
$ cd cmdstan_dir
$ echo "CXXFLAGS += -DSTAN_THREADS" > make/local
```
where ```cmdstan_dir``` is where cmdstan has been installed. Then you can check your make file with
```bash
$ cat make/local
```


## Installation 
The code is structured as an R package. A ```yml``` file is provided and can be used to build a conda virtual environment containing all R dependencies. Create the environment using:
```bash
$ cd covid19AgeModel
$ conda env create -f covid19AgeModel.yml
```
Then activate the environment for use:
```bash
$ source activate covid19AgeModel
```
Then build and install the R package with
```bash
$ cd ..
$ R CMD build covid19AgeModel
$ R CMD INSTALL covid19AgeModel_x.y-z.tar.gz
```

## Usage
### Overview
To run the age-specific COVID-19 analysis on the US data, you need to 
* first run an R script, which generates a series of bash scripts; 
* each bash script corresponds to fitting the contact-and-infection model to the data via one Hameltonian Monte Carlo chain with Stan; 
* by default, we typically run 8 HMC chains in parallel, though 2 or 4 can also be specified;
* the bash scripts need to be executed in a UNIX environment.

### Package versions
* Results of the most recent analysis can be reproduced using the package version 1.2.0, 
* Results of Imperial Report 32 v2 can be reproduced using the package version 1.1.0
* Results of Imperial Report 32 v1 can be reproduced using the package version 1.0.0

<details>
<summary> Reproduce results of the final revision for publication (package version 1.2.0) </summary>
	
## Usage: example analysis for 4 states, 2 HMC chains
We provide a demo analysis that can be run on a laptop. The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v120-demo-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We advise to run 2 chains for the demo, but more can be run as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 2
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend to use 4 for the demo:
```R
hpc.nproc.cmdstan <- 4
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. The default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used in the final version for publication:
```R
stanModelFile= 'covid19AgeModel_v120_cmdstanv'
```
Lastly, the demo analysis is performed for 4 locations: Colorado, Connecticut, Florida and New York City. The selected locations can be changed using:
```R
countries <-  "CO,CT,FL,NYC"
```
Please be careful not to leave spaces between commas and location's abbreviations.

Then, run the Rscript from the terminal console
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-v120-demo-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain and the post-processing scripts. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
When all the chains are finished, run the postprocessing with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_save_posterior_samples.sh
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results of the central analysis is stored under:
```
report_dir/report_stanModelFile-job_tag.html
```

## Usage: full analysis, 40 states 8 HMC chains (as in the final version for publication)
The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v120-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We typically run 8 chains, but 2 or 4 can also be used as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 8
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend at least 4, and by default use as many CPU cores as states in the analysis:
```R
hpc.nproc.cmdstan <- n_countries
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. Aim for 4000 to 5000 total samples after warmup across all HMC chains. For 8 HMC chains, the default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used in the final version for publication:
```R
stanModelFile= 'covid19AgeModel_v120_cmdstanv'
```
			
Then, run the Rscript from a Terminal console:
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-v120-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain and the post-processing scripts. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
where ```out_dir```, ```stanModelFile```, ```job_tag``` are specified above. 
When all the chains are finished, run the postprocessing of the central analysis with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_save_posterior_samples.sh
```
and the postprocessing of the schools counterfactual analysis with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_forecast_reopen_0_level_K12.sh
$ ./out_dir/stanModelFile-job_tag/post_processing_forecast_reopen_1_school_counterfactual_0_multiplier_100_level_K12.sh
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results of the central analysis is stored under:
```
report_dir/report_stanModelFile-job_tag.html
```
An html report summarising the results of the schools counterfactual analysis is stored under:
```
report_dir/report_forecast_stanModelFile-job_tag.html
```

Please note the full analysis on 40 US states or more is computationally intensive. The run time for 40 locations with 1500 iterations, including 1000 considered as warmup, is approximately 120 hours per chain, when one CPU is allocated to each chain. The age-specific outputs are also memory expensive, with approximately ~1TB memory for a full analysis on 40 US states or more with various forecast scenarios included.


## Usage: full analysis for 40 states, 8 HMC chains, high-throughput computing (as in the final version for publication)
Follow the above steps but use the Rscript
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v120-HPC.R
```
You will need to modify the PBS header function at the start of this Rscript:
```R
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
```
</details>

<details>
<summary> Reproduce results of Imperial Report 32 v2 (package version 1.1.0) </summary>
	
## Usage: example analysis for 4 states, 2 HMC chains
We provide a demo analysis that can be run on a laptop. The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v110-demo-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We advise to run 2 chains for the demo, but more can be run as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 2
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend to use 4 for the demo:
```R
hpc.nproc.cmdstan <- 4
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. The default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used in Imperial Report 32 (version 2):
```R
stanModelFile= 'covid19AgeModel_v110_cmdstanv'
```
Lastly, the demo analysis is performed for 4 locations: Colorado, Connecticut, Florida and New York City. The selected locations can be changed using:
```R
countries <-  "CO,CT,FL,NYC"
```
Please be careful not to leave spaces between commas and location's abbreviations.

Then, run the Rscript from the terminal console
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-v110-demo-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain and the post-processing scripts. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
When all the chains are finished, run the postprocessing with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_save_posterior_samples.sh
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results of the central analysis is stored under:
```
report_dir/report_stanModelFile-job_tag.html
```

## Usage: full analysis, 40 states 8 HMC chains (as in Imperial Report 32 v2)
The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v110-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We typically run 8 chains, but 2 or 4 can also be used as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 8
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend at least 4, and by default use as many CPU cores as states in the analysis:
```R
hpc.nproc.cmdstan <- n_countries
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. Aim for 4000 to 5000 total samples after warmup across all HMC chains. For 8 HMC chains, the default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used in Imperial Report 32 (version 2):
```R
stanModelFile= 'covid19AgeModel_v110_cmdstanv'
```
			
Then, run the Rscript from a Terminal console:
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-v110-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain and the post-processing scripts. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
where ```out_dir```, ```stanModelFile```, ```job_tag``` are specified above. 
When all the chains are finished, run the postprocessing of the central analysis with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_save_posterior_samples.sh
```
and the postprocessing of the schools counterfactual analysis with, 
```
$ ./out_dir/stanModelFile-job_tag/post_processing_forecast_reopen_0_level_K12.sh
$ ./out_dir/stanModelFile-job_tag/post_processing_forecast_reopen_1_school_counterfactual_0_multiplier_100_level_K12.sh
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results of the central analysis is stored under:
```
report_dir/report_stanModelFile-job_tag.html
```
An html report summarising the results of the schools counterfactual analysis is stored under:
```
report_dir/report_forecast_stanModelFile-job_tag.html
```

Please note the full analysis on 40 US states or more is computationally intensive. The run time for 40 locations with 1500 iterations, including 1000 considered as warmup, is approximately 120 hours per chain, when one CPU is allocated to each chain. The age-specific outputs are also memory expensive, with approximately ~1TB memory for a full analysis on 40 US states or more with various forecast scenarios included.


## Usage: full analysis for 40 states, 8 HMC chains, high-throughput computing (as in Imperial Report 32 v2)
Follow the above steps but use the Rscript
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-v110-HPC.R
```
You will need to modify the PBS header function at the start of this Rscript:
```R
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
```
</details>



<details>
<summary> Reproduce results of Imperial Report 32 v1 (package version 1.0.0) </summary>
	
## Usage: example analysis for 4 states, 2 HMC chains
We provide a demo analysis that can be run on a laptop. The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-report32-demo-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We advise to run 2 chains for the demo, but more can be run as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 2
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend to use 4 for the demo:
```R
hpc.nproc.cmdstan <- 4
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. The default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used for Report 32:
```R
stanModelFile= 'covid19AgeModel_report32_cmdstanv'
job_tag= '37states_central_analysis'
```
Lastly, the demo analysis is performed for 4 locations: Colorado, Connecticut, Florida and New York City. The selected locations can be changed using:
```R
countries <-  "CO,CT,FL,NYC"
```
Please be careful not to leave spaces between commas and locations' code.

Then, run the Rscript from the terminal console
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-report32-demo-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
When all the chains are finished, run the postprocessing with, 
```bash
$ ./postprocessing.sh 
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results is stored under:
```
report_dir/stanModelFile-job_tag-report.html
```

## Usage: full analysis for 37 states, 8 HMC chains (as in Imperial Report 32 v1)
The following modifications need to be done to the start of the R script
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-report32-bash.R
```
First, set the directory to where ```cmdstan``` has been installed to, as well as the output directory, and the directory in which the ```html``` summary are stored:
```R
cmdstan_dir <- '~/sandbox/cmdstan-2.23.0'
out_dir <- '~/sandbox'
report_dir <- '~/sandbox'
```
Second, set the number of HMC chains that are to be run in parallel. We typically run 8 chains, but 2 or 4 can also be used as long as the number of HMC iterations below are adjusted:
```R
hmc_chains_n <- 8
```
Third, set the variable ```hpc.nproc.cmdstan``` which specifies how many CPU cores are used to generate one HMC chain. We recommend at least 4, and by default use as many CPU cores as states in the analysis:
```R
hpc.nproc.cmdstan <- n_countries
```
Fourth, set the number of HMC warmup step and total number of HMC iterations. Aim for 4000 to 5000 total samples after warmup across all HMC chains. For 8 HMC chains, the default choice is: 
```R
hmc_num_samples= 1500
hmc_num_warmup= 1000
```
Fifth, specify the Stan model that you wish to use, and a job tag that may contain alphanumeric characters and the underscore character. By default the Stan model is set to that used for Report 32:
```R
stanModelFile= 'covid19AgeModel_report32_cmdstanv'
job_tag= '37states_central_analysis'
```
Then, run the Rscript from a Terminal console:
```bash
$ cd covid19AgeModel/inst
$ Rscript make-analysis-covid19AgeModel-report32-bash.R
```
This will generate in the specified output directory one bash script for each HMC chain. Execute these bash script one by one, or alternatively run them in the background,
```bash
$ cd out_dir/stanModelFile-job_tag
$ ./startme-stanModelFile-job_tag-hmc1.sh 
```
where ```out_dir```, ```stanModelFile```, ```job_tag``` are specified above. After all the HMC chains are completed, run the postprocessing with,  
```
out_dir/stanModelFile-job_tag/postprocessing.sh
```
and the postprocessing of the schools counterfactual analysis with, 
```
$ ./out_dir/post_processing_forecast_reopen_0_multiplier_50.sh
$ ./out_dir/post_processing_forecast_reopen_1_multiplier_20.sh
$ ./out_dir/post_processing_forecast_reopen_1_multiplier_33.sh
$ ./out_dir/post_processing_forecast_reopen_1_multiplier_50.sh
$ ./out_dir/post_processing_forecast_reopen_1_multiplier_100.sh
```
The HMC samples, figures and tables are stored in one folder:
```
out_dir/stanModelFile-job_tag
```
An html report summarising the results is stored under:
```
report_dir/stanModelFile-job_tag-report.html
```
An html report summarising the results of the schools counterfactual analysis is stored under:
```
report_dir/report_forecast_stanModelFile-job_tag.html
```

Please note the full analysis on 37 US states or more is computationally intensive. The run time for 37 locations with 1500 iterations, including 1000 considered as warmup, is approximately 48 hours per chain, when one CPU is allocated to each chain. The age-specific outputs are also memory expensive, with approximately ~1TB memory for a full analysis on 37 US states or more with various forecast scenarios included.


## Usage: full analysis for 37 states, 8 HMC chains, high-throughput computing (as in Imperial Report 32 v1)
Follow the above steps but use the Rscript
```bash
covid19AgeModel/inst/make-analysis-covid19AgeModel-report32-HPC.R
```
You will need to modify the PBS header function at the start of this Rscript:
```R
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
```
</details>
