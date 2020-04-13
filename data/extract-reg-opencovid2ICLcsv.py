# -*- coding: utf-8 -*-
"""
Provides a command line program which takes in arguments:

extract-reg-opencovid2ICLcsv.py <opencovid csv file>  <maille_code to extract>
"""

import sys
import pandas as pd
from datetime import datetime
import re

import pdb

# Module wide variables
data_dir = 'data/'

# TODO get population data for all regions and departments
# look at https://github.com/scrouzet/covid19-incrementality
pop_per_region = {
    "REG-93": 5059473,
}


def dt_to_dec(dt):
    """Convert a datetime to decimal year.
    Thanks to https://github.com/ImperialCollegeLondon/covid19model/blob/
    master/Python/src/util.py
    """
    year_start = datetime(dt.year, 1, 1)
    year_end = year_start.replace(year=dt.year+1)
    # year value + seconds so far / seconds in year
    return dt.year + ((dt - year_start).total_seconds() /
                      float((year_end - year_start).total_seconds()))


def convert_opencovidfr_to_ICL_model(srcReg, pop_per_region):
    """
    FUnction that convertsfrom the tabular headers of opencovid19-fr to those
    needed by the covid19 model.

    Maps:
        date, cas_confirmes, deces, maille_nom, maille_code, maille_code, date
    to:
        dateRep, cases, deaths, countriesAndTerritories, geoId,
        countryterritoryCode, t

    and population_region -> popData2018

    :param      srcReg:             The opencovid format data frame for a
    region.
    :type       srcReg:             pd.DataFrame()
    :param      population_region:  The population of the region.
    :type       population_region:  double
    """
    dst = pd.DataFrame()
    # mapping attributs src -> dst

    dst['dateRep'] = srcReg['date'].values
    # conversion vers des vraies dates
    dst['dateRep'] = dst['dateRep'].apply(pd.to_datetime, format="%Y-%m-%d")

    # valeurs bidon pour le moment
    dst['day'] = 1
    dst['month'] = 1
    dst['year'] = 2020

    dst['cases'] = srcReg['cas_confirmes'].values

    # avec ou sans les ehpads ?
    dst['deaths'] = srcReg['deces'].values

    dst['countriesAndTerritories'] = srcReg['maille_nom'].values
    dst['geoId'] = srcReg['maille_code'].values
    dst['countryterritoryCode'] = srcReg['maille_code'].values

    # valeurs 2020
    dst['popData2018'] = [pop_per_region[regId] for regId in dst['geoId']]

    dst["t"] = dst["dateRep"].apply(lambda v: dt_to_dec(v))

    # et pour finir on re-sérialise la date sous un autre format
    dst['dateRep'] = dst['dateRep'].apply(lambda x: x.strftime('%d/%m/%Y'))
    return dst


def find_active_regions(src, reg):

    # Extraire les IDs de regions qui match reg
    available_region_list = src["maille_code"].unique()
    re_expression = re.compile(reg)
    active_regions = []  # Les regions a ajoutees au fichier
    for region in available_region_list:
        if re_expression.search(region):
            active_regions.append(region)

    print(f"{reg} pattern matched the following {len(active_regions)} IDs:")
    print(active_regions)
    return active_regions


def process_from_cmd():

    if len(sys.argv) != 3:
        print(
            "Usage : extract-reg-opencovid2ICLcsv.py <opencovid csv file>  "
            + " <maille_code to extract>")
        print(
            "   Will read opencovid csv file, extract maille_code data and output"
            + " them in maille_code.csv with columns needed by base.r")
        sys.exit(1)

    fic = sys.argv[1]
    reg = sys.argv[2]

    src = pd.read_csv(fic)

    active_regions = find_active_regions(src, reg)

    # filtrage sur maille_code dans "active_regions"
    srcReg = src.loc[
        lambda df: [code in active_regions for code in df.maille_code], :]

    # notre intérêt est sur la colonne décès, on supprime donc les lignes où
    # cette valeur n'est pas connue
    srcReg = srcReg.dropna(how='all', subset=["deces"])
    print(reg + " : " + str(srcReg.shape[0]) + " lignes")
    # Nous avons besoins de remplir les cas_confirmes manquant avec des 0
    srcReg["cas_confirmes"] = srcReg["cas_confirmes"].fillna(0)

    # Il y a des doublons - on les élimines, celui qui reste est indéterminé
    srcReg = srcReg.drop_duplicates(subset=["date", "maille_code"])

    dst = convert_opencovidfr_to_ICL_model(srcReg, pop_per_region)

    # et voilà
    dst.to_csv(data_dir + reg + '.csv', index=False)


if __name__ == "__main__":
    process_from_cmd()
