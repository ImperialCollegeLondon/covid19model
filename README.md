# covid19model

![CI status](https://github.com/payoto/covid19model/workflows/CI/badge.svg)

This code is derivative work from the Imperial College Study [Estimating the number of infections and the impact of nonpharmaceutical interventions (NPI) on COVID-19 in 11 European countries](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/).
This repository extends applies the same NPI effect fitting and modelling to the regions (and possibly departements) in France.
The modifications to the code are made:

- To make it more modular and facilitate reuse.
- To handle additional data streams beyond the ECDC.

This code is part of the [data against covid-19](https://opencovid19.fr/)
citizens' initiative for open data and open source code around the COVID-19
pandemic.

Looking to contribute? Check the [projects page](https://github.com/payoto/covid19model/projects)!

## Motivation for regional predictions

Mid-March conversations between the
[data against covid-19](https://opencovid19.fr/) initiative and
hospital managers revealed a need for local predictions of the evolution of the
COVID-19 pandemic. France has been unevenly hit by the spread of
the novel coronavirus, and in order to most effectively allocate resources on a
national level, an understanding of local progression is critical.

## Information about the model from Imperial College London
>
> Code for modelling estimated deaths and cases for COVID19 from Report 13
published by MRC Centre for Global Infectious Disease Analysis, Imperial
College London:
[Estimating the number of infections and the impact of nonpharmaceutical interventions on COVID-19 in 11 European countries](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/).
>
> ## Version 2 Release
>
>In this update we extend our original [model](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-13-europe-npi-impact/)  to include (a) population saturation effects, (b) prior uncertainty on the infection fatality ratio and (c) a more balanced prior on intervention effects.  We also (d) included another 3 countries (Greece, the Netherlands and Portugal). The updated technical detail is available [here](https://github.com/ImperialCollegeLondon/covid19model/blob/master/Technical_description_of_Imperial_COVID_19_Model.pdf).
>
> You can directly look at our results [here](https://imperialcollegelondon.github.io/covid19estimates)
>
> This repository has code for replication purposes. The bleeding edge code and advancements are done in a private repository. Ask report authors for any collaborations.

To see the full readme from the original repository please consult either
the [readme on upstram-master](https://github.com/payoto/covid19model/tree/upstream-master) or the [ICL readme](https://github.com/ImperialCollegeLondon/covid19model/blob/master/README.md).

The original readme includes more details on configuring and running the model.

## Method

In an attempt to analyse and predict progression of the epidemic in France,
the model from the study of non-pharmaceutical interventions on the basis of death data produced and published by Imperial is used in conjunction with the
latest available French regional data from [opencovid19-fr/data](https://github.com/opencovid19-fr/data).

### Data sources

'Live' data sources updated regularly:

- [opencovid19-fr/data](https://github.com/opencovid19-fr/data) French regions and departments covid-19 death data;
- [ECDC](https://www.ecdc.europa.eu/en) European countries death data;
- INSEE data on the breakdown of French population by age preprocessed in [scrouzet/covid19-incrementality](https://raw.githubusercontent.com/scrouzet/covid19-incrementality/master/data/INSEE%20-%20year%20x%20dept%20x%20sex%20x%20age%20-%20population.csv).

'Static' sources not updated:

- EHPAD population age breakdown from the [DREES](http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx);
- Infection Fatality Ration (IFR) provided by original repository, calculation
by the ICL MRC Centre for Global Infectious Disease Analysis in [report 12](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-12-global-impact-covid-19/).

### Data processing

The original code is developed on `.csv` files downloaded from the
ECDC. These are then converted to `.rds`.

The processing of French regional and departmental data is performed in 3 steps:

- Download and pre-process CSV: [`data/update-french-regional-data.sh`](data/update-french-regional-data.sh)
  - Process to ECDC format CSV: [`data/extract_opencovidfr_2_ICL.py`](data/extract_opencovidfr_2_ICL.)
- Format pre-processed data to RDS: [`data/fetch-region-france.r`](data/fetch-region-france.r)

#### Notes about data from France

- The data from opencovid19-fr are deaths since the epidemic start.
- Deaths in nursing homes (EHPAD) are reported separately to those in hospitals.
- For all French regions only the hospital deaths are available.

These observations led to the following choices in the processing of the
opencovid19-fr data:

- Geographical "Regions" and "departements" only consider deaths at hospital.
- Three additional regions are defined:
  - `France-OC19` : France's death data in hospitals and EHPAD as provided by `opencovid19-fr/data`;
  - `France-Hopitaux` : France's death data from hospitals;
  - `France-EHPAD` : France's death data from nursing homes (EHPAD).

The separation between the hospital and EHPAD data is done, permit an
acceptable fit on the French data despite the change in data reporting half-way
through the period.

## Organisation

Much of the discussion is done in the data against covid-19 slack that you can [join here](https://opencovid19.fr/). If you are not part of it feel free to
submit an issue on this repository.

### Git Branches

- [`master`](https://github.com/payoto/covid19model/tree/master) is the production branch, modelling for prediction is run on this
code.
- [`france-regions`](https://github.com/payoto/covid19model/tree/france-regions) is a development branch for features looking to improve
modelling and processing of French regions.
- [`upstream-master`](https://github.com/payoto/covid19model/tree/upstream-master) an exact mirror of the [original model](https://github.com/ImperialCollegeLondon/covid19model).
- [`community-contribs`](https://github.com/payoto/covid19model/tree/community-contribs) Branch to pull in contributions from the rest of the
community that is actively developing this `covid19model`.
- [`modularisation`](https://github.com/payoto/covid19model/tree/modularisation) development branch of features which can be useful to
other community projects.

### Contributing

To contribute:

- Fork the repository;
- Check the todo items in the [projects page](https://github.com/payoto/covid19model/projects);
- Submit a pull request against the more appropriate branch, depending on what you are trying to do.

## References

- Original ICL report:

Seth Flaxman, Swapnil Mishra, Axel Gandy et al. Estimating the number of infections and the impact of nonpharmaceutical interventions on COVID-19 in 11 European countries. Imperial College London (30-03-2020)
doi: https://doi.org/10.25561/77731