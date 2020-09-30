#!/bin/sh
#PBS -l walltime=23:59:00
#PBS -l select=1:ncpus=37:ompthreads=37:mem=333gb
#PBS -j oe
#PBS -J 1-8
#PBS -q pqcovid19c
module load cmdstan/2.33.0 anaconda3/personal
source activate covid19model

# set default inputs
CODE_DIR=~/git/R0t

# set job inputs
FORECAST_PERIOD=90
SCHOOL_MULTIPLIER=(0.2 0.33 0.5 1.0)
JOB_DIR_BASE=/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b9_cmdstanv-37states_tau10_sameLastDate
#JOB_DIR_BASE=/rds/general/project/ratmann_covid19/live/age_renewal_usa/base_age_fsq_mobility_200821b4_cmdstanv-37states_tau20_Sep2

echo "----------- collect directories to process: ------------"
echo "input dir: "$JOB_DIR_BASE
JOB_TO_PROCESS_FILE=$JOB_DIR_BASE"_generate_forecasts.txt"
echo "input file with directories to process is: "$JOB_TO_PROCESS_FILE
if [ ! -f "$JOB_TO_PROCESS_FILE" ]; then
  echo "writing directories to file: "$JOB_TO_PROCESS_FILE  
  JOB_DIR1=$JOB_DIR_BASE
  ls -d $JOB_DIR1/*/ > $JOB_TO_PROCESS_FILE
fi

echo "----------- collect directories to process: ------------"
# Each line in the input file is to be analysed, so count the number of lines.
JOBS_NUM=$(cat "$JOB_TO_PROCESS_FILE" | wc -l)
#PBS_ARRAY_INDEX=20
echo $JOBS_NUM
echo $PBS_ARRAY_INDEX
# Check that the current array index is not larger than the number of input files.
if [ "$PBS_ARRAY_INDEX" -gt "$JOBS_NUM" ]; then
  echo 'Error: job array index' "$PBS_ARRAY_INDEX" 'is greater than the number'\
  'of lines in' "$JOB_TO_PROCESS_FILE"'. Quitting.' >&2
  exit 1
fi
# Find input directory corresponding to this array index.
PBS_ARRAY_INPUT=$(sed -n "$PBS_ARRAY_INDEX"'p' "$JOB_TO_PROCESS_FILE")
echo "PBS input arg is: "$PBS_ARRAY_INPUT

echo "----------- make forecasts: ------------"
PBS_ARRAY_INPUT=${PBS_ARRAY_INPUT%?}
JOB_PAR_DIR="${PBS_ARRAY_INPUT%/*}"
JOB_PAR_DIR=${JOB_PAR_DIR##*/}
echo $JOB_PAR_DIR
echo $PBS_ARRAY_INPUT

echo {1..37} | tr ' ' '\n' | xargs -P 37 -n 1 -I {} Rscript "$CODE_DIR"/base-ages-generate-quantities-forecast.R -indir.code "$CODE_DIR" -indir.results "$PBS_ARRAY_INPUT" -location.index {} -with.flow 1 -forecast.period $FORECAST_PERIOD -school.reopen "0" -multiplier_cntct_school_opening "1"

for i in "${SCHOOL_MULTIPLIER[@]}"; do
  echo {1..37} | tr ' ' '\n' | xargs -P 37 -n 1 -I {} Rscript "$CODE_DIR"/base-ages-generate-quantities-forecast.R -indir.code "$CODE_DIR" -indir.results "$PBS_ARRAY_INPUT" -location.index {} -with.flow 1 -forecast.period $FORECAST_PERIOD -school.reopen "1" -multiplier_cntct_school_opening "$i"
done



