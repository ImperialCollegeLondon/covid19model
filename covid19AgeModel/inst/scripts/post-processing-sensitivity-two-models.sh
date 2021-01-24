#!/bin/sh
#PBS -l walltime=23:59:00
#PBS -l select=1:ncpus=1:ompthreads=1:mem=100gb
#PBS -j oe
#PBS -q pqcovid19c
module load anaconda3/personal
source activate covid19AgeModel

# set default inputs
SCRIPT_DIR=/rds/general/user/mm3218/home/anaconda3/envs/covid19AgeModel/lib/R/library/covid19AgeModel
OUTPUT_DIR=/rds/general/project/ratmann_covid19/live/age_renewal_usa
OVERWRITE=0
PERIOD_LENGTH=7

# set base model inputs
STAN_MODEL_FILE=base_age_fsq_mobility_201015f8_cmdstanv
JOB_TAG=40states_tau10_Oct29_Levin

# set alternative models inputs
STAN_MODEL_FILE_DEV=base_age_fsq_mobility_201015e8_cmdstanv
JOB_TAG_DEV="40states_Oct29_Levin7"

# name of the sensitivity analysis
PREFIX='ifr-prior'
#PREFIX='susceptibility-prior'

echo "----------- Start postprocessing: ------------"

Rscript $SCRIPT_DIR/scripts/post-processing-sensitivity-Rt-contribution.R -stanModelFile $STAN_MODEL_FILE -stanModelFileDev $STAN_MODEL_FILE_DEV -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE -period_length $PERIOD_LENGTH -prefix $PREFIX

Rscript $SCRIPT_DIR/scripts/post-processing-sensitivity-attackrate.R -stanModelFile $STAN_MODEL_FILE -stanModelFileDev $STAN_MODEL_FILE_DEV -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE -prefix $PREFIX

Rscript $SCRIPT_DIR/scripts/post-processing-sensitivity-antibody.R -stanModelFile $STAN_MODEL_FILE -stanModelFileDev $STAN_MODEL_FILE_DEV -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -dev_job_tag $JOB_TAG_DEV -overwrite $OVERWRITE -prefix $PREFIX

echo "----------- End postprocessing: ------------"