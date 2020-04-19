#!/bin/bash

# Open the branch for french region calculations
git clone https://github.com/payoto/covid19model.git
git checkout france-regions

# Update data
# Update Europe wide data from ECDC and process to RDS
Rscript data/fetch-ecdc.r
# Download and format data from opencovid19-fr
#  - Death and case data 
#  - Population data for france 
# (Requires python>=3.6, and pandas) 
data/update-french-regional-data.sh
# Process data to RDS format (for the runs)
Rscript data/fetch-region-fr.r

# Run model
Rscript base-region-france.r --full > results/stdout.txt 2> results/stderr.txt &
