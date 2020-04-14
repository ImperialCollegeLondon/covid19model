#!/bin/bash
# Requires python 3


datadir="data/FRA/"
source_opencovid="https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv"
source_INSEE_reg="https://raw.githubusercontent.com/scrouzet/covid19-incrementality/master/data/departements.csv"
source_INSEE_dep="https://raw.githubusercontent.com/scrouzet/covid19-incrementality/master/data/INSEE%20-%20year%20x%20dept%20x%20sex%20x%20age%20-%20population.csv"

# EHPAD source http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx

wget -O ${datadir}/opencovid19-fr-chiffres-cles.csv ${source_opencovid}
wget -O ${datadir}/population-fra-INSEE-region-departement.csv ${source_INSEE_reg}
wget -O ${datadir}/population-fra-INSEE-departement.csv ${source_INSEE_dep}

python data/extract_opencovidfr_2_ICL.py data/FRA/opencovid19-fr-chiffres-cles.csv all-france
python data/extract_opencovidfr_2_ICL.py data/FRA/opencovid19-fr-chiffres-cles.csv REG
python data/extract_opencovidfr_2_ICL.py data/FRA/opencovid19-fr-chiffres-cles.csv DEP