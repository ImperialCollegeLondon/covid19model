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
PERIOD_LENGHT=7

# set base model inputs
STAN_MODEL_FILE="base_age_fsq_mobility_200821b4_cmdstanv,base_age_fsq_mobility_200821b9_cmdstanv,base_age_fsq_mobility_200821b9_cmdstanv"
JOB_TAG="37states_tau05_Sep2,37states_tau10_sameLastDate,37states_tau10_sameLastDate"
NUMB_MODELS=3
MODELS_TAU="0.5,1,2"



echo "----------- Start postprocessing: ------------"

Rscript $SCRIPT_DIR/scripts/post-processing-sensitivity-mltp_cntct_school_close-Rt-contribution.r -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -num_mod $NUMB_MODELS -models_tau $MODELS_TAU  -overwrite $OVERWRITE -period_length $PERIOD_LENGHT

Rscript $SCRIPT_DIR/scripts/post-processing-sensitivity-mltp_cntct_school_close-contact_patterns.r -stanModelFile $STAN_MODEL_FILE -out_dir $OUTPUT_DIR -job_tag $JOB_TAG -num_mod $NUMB_MODELS -models_tau $MODELS_TAU  -overwrite $OVERWRITE 

echo "----------- End postprocessing: ------------"