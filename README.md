![](https://github.com/ImperialCollegeLondon/covid19model/workflows/CI/badge.svg)

# covid19model
Code for modelling estimated deaths and cases for COVID19 from Report 13 published by MRC Centre for Global Infectious Disease Analysis, Imperial College London: [Estimating the number of infections and the impact of nonpharmaceutical interventions on COVID-19 in 11 European countries](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/) 

If you are looking for the individual based model used in [Report 9](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-9-impact-of-npis-on-covid-19/), please look [here](https://github.com/mrc-ide/covid-sim)

## Version 3 Release
In this update, we first extended our model from version 2 to have 'partial-pooling' for lockdown across all countries. This means now we have a global effect of lockdown along with each country having its own different lockdown effect. We also made our code modular, stan code faster (with help from the community) and now we create CSV outputs too for usage. 

You can directly get csv files [here](https://mrc-ide.github.io/covid19estimates/#/download ) and new model description [here](https://arxiv.org/abs/2004.11342)

## Notice
 :warning: Python code is right now not updated and won't work. Python code is good for only version 1 model and data.
 
 :warning: base_general.r and base_general.stan, base_general_speed.stan and  	base_general_speed2.stan are now valid models for only version2

:warning: This code is released with no support. We try our best to look at issues and pull request but can't help people with setup most of the time. We have docker images and conda enviornment file to make it easy for you to get started with the setup, any other approach assumes user can handle their computing enviornments approriately.

:warning: This model is in active development and so parameter name and behaviours, and output file formats will change without notice.

:warning: As with any mathematical model, it is easy to misconfigure inputs and therefore get meaningless outputs. The development team only endorses outputs it has itself generated.

## Version 2 Release
In this update we extend our original [model](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/)  to include (a) population saturation effects, (b) prior uncertainty on the infection fatality ratio and (c) a more balanced prior on intervention effects.  We also (d) included another 3 countries (Greece, the Netherlands and Portugal). The updated technical detail is available [here](https://github.com/ImperialCollegeLondon/covid19model/blob/master/Technical_description_of_Imperial_COVID_19_Model.pdf).

You can directly look at our results [here](https://imperialcollegelondon.github.io/covid19estimates)


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
* To run from commandline please enter the cloned directory and type `Rscript base.r base` in terminal

Please note to not make you wait for long we have by default set run sampling to a short period. For proper estimates please run it in FULL mode either by setting the flag `--full` or the environment variable `FULL=TRUE`. This will run sampling for 4000 iterations with 2000 warmups and 4 chains. The run time for 14 countries using new faster code is around 50 mins/1hr for the version 3 code.

## Run mode settings 
Three different run modes are supported:

* DEBUG which can either be enabled by setting the flag `--debug` when running the base.r file as such: 
  * `Rscript base.r base --debug` or by setting the environment variable `DEBUG` to `TRUE`.
* DEFAULT which will run if neither full nor debug are set. Please note that for proper estimates FULL should always be set.
* FULL which must always be used if you want to obtain reliable results and can be enabled by setting the flag `--full` on the command line: 
  * `Rscript base.r base --full` or by setting the environment variable `FULL` to `TRUE`. 

# Results 
* The results are stored in two folders results and figures.
* Results has the stored stan fits and data used for plotting
* Figures have the images with daily cases, daily death and Rt for all countries.
