![](https://github.com/ImperialCollegeLondon/covid19model/workflows/CI/badge.svg)

# covid19model
Code for modelling estimated deaths and infections for COVID-19 from ["Estimating the effects of non-pharmaceutical interventions on COVID-19 in Europe"](https://www.nature.com/articles/s41586-020-2405-7), Flaxman, Mishra, Gandy et al, Nature, 2020.

If you are looking for the individual based model used in Imperial's [Report 9, Ferguson, Laydon, Nedjati-Gilani et al](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-9-impact-of-npis-on-covid-19/), please look [here](https://github.com/mrc-ide/covid-sim).

## Version 7 Release

This code is the exact code that was used in Flaxman, Mishra, Gandy et al. "Estimating the effects of non-pharmaceutical interventions on COVID-19 in Europe," Nature, 2020.[https://www.nature.com/articles/s41586-020-2405-7](https://www.nature.com/articles/s41586-020-2405-7)

To run the code from the main folder in rstudio source base-nature.r or from terminal after reaching the root of the repository type Rscript base-nature.r.

The code should be run in full mode to obtain any results. Not running full model to estimate anything is not recommended and discouraged. Only full run should be used to get results.

The repository with posterior draws of the model in Flaxman, Mishra, Gandy et al. "Estimating the effects of non-pharmaceutical interventions on COVID-19 in Europe," Nature, 2020. [https://www.nature.com/articles/s41586-020-2405-7](https://www.nature.com/articles/s41586-020-2405-7) is [here](https://github.com/ImperialCollegeLondon/covid19modelnaturedraws).

This code doesn't supersede our earlier model, it is here for everyone to have direct access to code used in Flaxman, Mishra, Gandy et al. "Estimating the effects of non-pharmaceutical interventions on COVID-19 in Europe," Nature, 2020.[https://www.nature.com/articles/s41586-020-2405-7](https://www.nature.com/articles/s41586-020-2405-7).

The instructions for European, Italy, Brazil, and USA code are the same as earlier (Look at version 3, version 4, version 5, version 6).

## Version 6 Release

This is the release related to [report 23](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-23-united-states/), where we use mobility data to estimate situation in all states of the USA. All other code is still the same.

To run this code you can directly run the base-usa.r file or from command line after seting the current directory as the repository directory run the following command `Rscript base-usa.r`

The code shold be run in full mode to obtain any results. Not running full model to estimate anything is not recommended and discouraged. Only full run should be used to get results.

The instructions for European, Italy and Brazil code are same as earlier (Look at version 3, version 4 and version 5). This release is specific to [USA report](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-23-united-states/)

## Version 5 Release

This is the release related to [report 21](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-21-brazil/), where we use mobility data to estimate situation in Brazil. All other code is still the same.

To run this code you can directly run the base-Brazil.r file or from command line after seting the current directory as the repository directory run the following command `Rscript base-Brazil.r`

The code shold be run in full mode to obtain any results. Not running full model to estimate anything is not recommended and discouraged. Only full run should be used to get results.

The instructions for European and Italy code are same as earlier (Look at version 3 and version 4). This release is specific to [Brazil report](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-21-brazil/)

## Version 4 Release

This is the release related to [report 20](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-20-italy/), where we use mobility data to estimate situation in Italy. All other code is still the same.

To run this code you can directly source the base-italy.r file in rstudio inside the project or from command line after setting the current directory as the repository directory run the following command `Rscript base-italy.r base-italy google interventions '~ -1 + residential + transit + averageMobility' '~ -1 + residential + transit + averageMobility'`

The code for scenarios runs only in full mode not in short run or debug mode. Not running full model to estimate anything is not recommended and discouraged. Only full run should be used to get results.

The instructions for European code are below. This release is specific to [Italy report](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-20-italy/)


## Version 3 Release
In this update, we first extended our model from version 2 to have 'partial-pooling' for lockdown across all countries. This means now we have a global effect of lockdown along with each country having its own different lockdown effect. We also made our code modular, stan code faster (with help from the community) and now we create CSV outputs too for usage. 

You can directly get csv files [here](https://mrc-ide.github.io/covid19estimates/#/download ) and new model description [here](https://arxiv.org/abs/2004.11342)

## Notice
 :warning: Python code is right now not updated and won't work. Python code is good for only version 1 model and data.
 
 :warning: base_general.r and base_general.stan, base_general_speed.stan and  	base_general_speed2.stan are now valid models for only version2

:warning: This code is released with no support. We try our best to look at issues and pull request but can't help people with setup most of the time. We have docker images and conda environment file to make it easy for you to get started with the setup, any other approach assumes user can handle their computing environments approriately.

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
* Results have the stored stan fits and data used for plotting
* Figures have the images with daily cases, daily death and Rt for all countries.
