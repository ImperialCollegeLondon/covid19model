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

## There are two ways to run our code
1. Open the rstudio project covid19model.Rproj file in rstudio and run/source `base.r` file
2. To run from commandline please enter the cloned directory and type 'Rscript base.r base' in terminal
## Results
* The results are stored in two folders results and figures.
* Results has the stored stan fits and data used for plotting
* Figures have the images with daily cases, daily death and Rt for all countries.

## Notice
Please note to not make you wait for long, we have by default run sampling for a short period. 
 
To be comparable with report, in [`base.r`](base.r), uncomment the first line shown below and comment out the second line. Click [here](https://github.com/ImperialCollegeLondon/covid19model/blob/7c35e25340a8067706d548a104eca86169d50b67/base.r#L212-L213) to navigate to the area of interest.

```python
# fit = sampling(m,data=stan_data,iter=4000,warmup=2000,chains=8,thin=4,control = list(adapt_delta = 0.90, max_treedepth = 10))
fit = sampling(m,data=stan_data,iter=200,warmup=100,chains=4,thin=4,control = list(adapt_delta = 0.90, max_treedepth = 10))
```
This will run sampling for 4000 iterations with 2000 warmups and 8 chains.

You can also limit the number of countries studied by replacing `countries` as shown [here](https://github.com/ImperialCollegeLondon/covid19model/blob/7c35e25340a8067706d548a104eca86169d50b67/base.r#L63).