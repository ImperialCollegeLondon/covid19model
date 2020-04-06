![](https://github.com/ImperialCollegeLondon/covid19model/workflows/CI/badge.svg)

# covid19model
Code for modelling estimated deaths and cases for COVID19 from Report 13 published by MRC Centre for Global Infectious Disease Analysis, Imperial College London: [Estimating the number of infections and the impact of nonpharmaceutical interventions on COVID-19 in 11 European countries](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/) 

This repository has code for replication purposes. The bleeding edge code and advancements are done in a private repository. Ask report authors for any collaborations. 

## Contributing

We welcome all potential collaborators and contributors from the wider community. Please see [contributing](contributing.md) for more details.

# Installing dependencies

## Using Conda

An `environment.yml` file is provided and can be used to build a virtual
environment containing all model dependencies. Create the environment using:
```
conda env create -f environment.yml
```

Then activate the environment for use:
```
conda activate covid19model
```

## Using Docker

A [Docker][] image providing all model dependencies is available. See
[docker/README.md](docker/) for details of running the model with Docker.

[Docker]: https://www.docker.com/

## Other

If you wish to install packages into your native R environment or with a system
package manager please see `environment.yml` for a full list of dependencies.

# How to run the code

There are two ways to run our code:-
* Open the rstudio project covid19model.Rproj file in rstudio and run/source base.r file
* To run from commandline please enter the cloned directory and type 'Rscript base.r base' in terminal
* The results are stored in two folders results and figures.
* Results has the stored stan fits and data used for plotting
* Figures have the images with daily cases, daily death and Rt for all countries.

## Please note to not make you wait for long we have by default run sampling for short period. To be comparable with report please uncomment the line 212 and comment out line 213. This will run sampling for 4000 iterations with 2000 warmups and 4 chains.
