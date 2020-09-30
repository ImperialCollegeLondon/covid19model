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
PERIOD_LENGHT=7

# set base model inputs
STAN_MODEL_FILE=base_age_fsq_mobility_200730j2_cmdstanv
JOB_TAG=37states_Sep8

# set alternative models inputs
STAN_MODEL_FILE_DEV="base_age_fsq_mobility_200821b4_cmdstanv,base_age_fsq_mobility_200821b9_cmdstanv,base_age_fsq_mobility_200821b4_cmdstanv"
JOB_TAG_DEV="37states_tau05_Sep2,37states_tau10_sameLastDate,37states_tau1.5_Sep2"
NUMB_DEV_MODELS=3
DEV_MODELS_TAU="0.5,1,1.5"


echo "----------- Start postprocessing: ------------"

Rscript $SCRIPT_DIR/usa/code/postprocessing/post-processing-sensitivity-contacts-0-14-Rt-contribution.r -script_dir $SCRIPT_DIR -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -num_dev_mod $NUMB_DEV_MODELS -dev_models_tau $DEV_MODELS_TAU -dev_stanModelFile $STAN_MODEL_FILE_DEV -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE -period_length $PERIOD_LENGHT

Rscript $SCRIPT_DIR/usa/code/postprocessing/post-processing-sensitivity-contacts-0-14-contact_patterns.r -script_dir $SCRIPT_DIR -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -num_dev_mod $NUMB_DEV_MODELS -dev_models_tau $DEV_MODELS_TAU -dev_stanModelFile $STAN_MODEL_FILE_DEV -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE 

echo "----------- End postprocessing: ------------"