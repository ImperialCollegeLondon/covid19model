#!/bin/bash

if [ "$#" -ne 2 ]
then
    echo "Usage: sh run_model_script.sh UID GID"
    echo "Values for UID and GID are required to set the file ownerships"
    echo "of model outputs."
    exit 1
fi

# Run the code using the required UID/GID
sudo --preserve-env -u \#"$1" -g \#"$2" Rscript base.r base
