#!/bin/sh
#PBS -l walltime=23:59:00
#PBS -l select=1:ncpus=1:ompthreads=1:mem=100gb
#PBS -j oe
#PBS -J 1-6
#PBS -q pqcovid19c
module load anaconda3/personal
source activate covid19AgeModel

# set default inputs
CODE_DIR=~/git/R0t
OUTPUT_DIR=/rds/general/project/ratmann_covid19/live/age_renewal_usa
SCHOOL_MULTIPLIER=(0.2 0.33 0.5 1.0)
NUM_CHAIN=8

# set job inputs
FORECAST_PERIOD=90
JOB_TAG=37states_Sep2
STAN_MODEL_FILE=base_age_fsq_mobility_200821b4_cmdstanv
PERIOD_LENGTH=7
JOB_BASE_DIR=$OUTPUT_DIR/$STAN_MODEL_FILE-$JOB_TAG

# echo "----------- collect directories to process: ------------"
# echo "input dir: "$JOB_DIR_BASE
# OUTPUT_DIRS_FORECAST_SCHOOL1_ARRAY=($JOB_DIR_FORECAST_SCHOOL1*)
# JOBS_NUM=${#OUTPUT_DIRS_FORECAST_SCHOOL1_ARRAY[@]}
# echo "----------- select directory to process: ---------------"
# #PBS_ARRAY_INDEX=1
# echo $JOBS_NUM
# echo $PBS_ARRAY_INDEX
# # Check that the current array index is not larger than the number of input files.
# if [ "$PBS_ARRAY_INDEX" -gt "$JOBS_NUM" ]; then
#   echo 'Error: job array index' "$PBS_ARRAY_INDEX" 'is greater than the number'\
#   'of lines in' "$JOB_TO_PROCESS_FILE"'. Quitting.' >&2
#   exit 1
# fi
# OUTPUT_DIR_FORECAST_SCHOOL1=${OUTPUT_DIRS_FORECAST_SCHOOL1_ARRAY[$PBS_ARRAY_INDEX-1]}

echo "----------- Start postprocessing: ------------"
#for OUTPUT_DIR_FORECAST_SCHOOL1 in "${OUTPUT_DIRS_FORECAST_SCHOOL1_ARRAY[@]}"
#do
echo "post-processing forecast results in "$JOB_BASE_DIR

# run postprocessing for school closure scenario
Rscript "$CODE_DIR"/usa/code/postprocessing/save-posterior-samples-forecast.R -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -numb_chains "$NUM_CHAIN" -school.reopen 0 -multiplier_cntct_school_opening "1"

# run postprocessing for school opening scenario
for i in "${SCHOOL_MULTIPLIER[@]}"; do
  Rscript "$CODE_DIR"/usa/code/postprocessing/save-posterior-samples-forecast.R -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -numb_chains "$NUM_CHAIN" -school.reopen 1 -multiplier_cntct_school_opening "$i"

  Rscript "$CODE_DIR"/usa/code/postprocessing/post-processing-forecast-contributions.r -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -overwrite 0 -with_forecast 1 -multiplier_cntct_school_opening "$i"

  Rscript "$CODE_DIR"/usa/code/postprocessing/post-processing-forecast-increase-deaths-cases.r -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -overwrite 0 -with_forecast 1 -multiplier_cntct_school_opening "$i"

  Rscript "$CODE_DIR"/usa/code/postprocessing/post-processing-forecast-plot-deaths.r -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -overwrite 0 -with_forecast 1 -multiplier_cntct_school_opening "$i"

  Rscript "$CODE_DIR"/usa/code/postprocessing/post-processing-forecast-increase-Rt.r -stanModelFile "$STAN_MODEL_FILE" -out_dir "$JOB_BASE_DIR" -job_tag "$JOB_TAG" -overwrite 0 -with_forecast 1 -period_length $PERIOD_LENGTH -multiplier_cntct_school_opening "$i"

done
#done

echo "----------- End postprocessing: ------------"