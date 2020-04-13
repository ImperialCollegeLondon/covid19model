#!/bin/bash

datadir="data/FRA/"
source_opencovid="https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv"
source_INSEE_reg="https://raw.githubusercontent.com/scrouzet/covid19-incrementality/master/data/departements.csv"
source_INSEE_dep="https://raw.githubusercontent.com/scrouzet/covid19-incrementality/master/data/INSEE%20-%20year%20x%20dept%20x%20sex%20x%20age%20-%20population.csv"
wget -O ${datadir}/opencovid19-fr-chiffres-cles.csv ${source_opencovid}
wget -O ${datadir}/population-fra-INSEE-region-departement.csv ${source_INSEE_reg}
wget -O ${datadir}/population-fra-INSEE-departement.csv ${source_INSEE_dep}