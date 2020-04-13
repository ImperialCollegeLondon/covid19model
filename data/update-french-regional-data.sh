#!/bin/bash

source_url="https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv"

wget -O data/opencovid19-fr-chiffres-cles.csv ${source_url}