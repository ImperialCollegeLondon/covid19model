#!/bin/sh
#PBS -l walltime=23:59:00
#PBS -l select=1:ncpus=1:ompthreads=1:mem=100gb
#PBS -j oe
#PBS -q pqcovid19c
module load anaconda3/personal
source activate covid19AgeModel

# set default inputs
SCRIPT_DIR=~/git/R0t
OUTPUT_DIR=/rds/general/project/ratmann_covid19/live/age_renewal_usa
OVERWRITE=0
PERIOD_LENGTH=7

# set base model inputs
STAN_MODEL_FILE=base_age_fsq_mobility_200821b4_cmdstanv
JOB_TAG=37states_Sep2

# set alternative models inputs
JOB_TAG_DEV="37states_tau1_Levinprior_Sep2"


echo "----------- Start postprocessing: ------------"

Rscript $SCRIPT_DIR/usa/code/postprocessing/post-processing-sensitivity-ifr-age-prior-Rt-contribution.R -script_dir $SCRIPT_DIR -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE -period_length $PERIOD_LENGTH

Rscript $SCRIPT_DIR/usa/code/postprocessing/post-processing-sensitivity-ifr-age-prior-attackrate.R -script_dir $SCRIPT_DIR -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE 

Rscript $SCRIPT_DIR/usa/code/postprocessing/post-processing-sensitivity-ifr-age-prior-antibody.R -script_dir $SCRIPT_DIR -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE 

echo "----------- End postprocessing: ------------"