#!/bin/bash

# Open the branch for french region calculations
#git clone git@github.com:payoto/covid19model.git 
#https://github.com/payoto/covid19model.git
#cd covid19model

# if [ "`git branch --show-current`" != "france-regions" ]; then
#     #git checkout france-regions
#     echo "nothing"
# fi;

# if [ "$CONDA_DEFAULT_ENV" != "covid19model" ]; then
#     if [ "`conda env list | grep covid19model`" != "" ]; then
#         echo "activate conda env covid19model"
#         conda activate covid19model
#     else
#         echo "create conda env covid19model"
#         conda create -n covid19model python=3.6 pandas numpy pyyaml pandas scipy  pystan r-caret r-rstan r-gdata r-httr r-tidyr r-dplyr r-optparse
#         conda activate covid19model
#         conda install -c conda-forge  r-bayesplot r-data.table r-envstats r-ggpubr  
#     fi;
# else
#     echo "already in covid19model conda env"
# fi;

# Update data
# Update Europe wide data from ECDC and process to RDS
echo "RUN data/fetch-ecdc.r"
Rscript data/fetch-ecdc.r
# Download and format data from opencovid19-fr
#  - Death and case data 
#  - Population data for france 
# (Requires python>=3.6, and pandas) 
echo "RUN data/update-french-regional-data.sh"
bash data/update-french-regional-data.sh
#data/update-french-regional-data.sh
# Process data to RDS format (for the runs)
echo "RUN data/fetch-region-fr.r"
Rscript data/fetch-region-fr.r
# Run model
echo "RUN base-region-france.r"
#Rscript base-region-france.r --debug > results/stdout.txt 2> results/stderr.txt 
Rscript base-region-france.r --full > results/stdout.txt 2> results/stderr.txt 
