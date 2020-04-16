# -*- coding: utf-8 -*-
"""
Provides a command line program which takes in arguments:

extract-reg-opencovid2ICLcsv.py <opencovid csv file>  <maille_code to extract>
"""

import sys
import re
from datetime import datetime

import pandas as pd
import numpy as np

import pdb

# Module wide variables
data_dir = 'data/'
special_regions = [
    "FRA-EHPAD", "FRA-HOP",
]

# Population processed for:
#  Gets data from INSEE via https://github.com/scrouzet/covid19-incrementality
def read_pop_region(
    pop_file_region=None
):
    if pop_file_region is None:
        pop_file_region = data_dir + 'FRA/french_population_age_regional.csv'
    pop_fra_df = pd.read_csv(pop_file_region)
    pop_per_region = {}
    for ind in pop_fra_df.index:
        pop_per_region[pop_fra_df.loc[ind,"fra_code"]] = pop_fra_df.loc[ind,"total"]
    return  pop_per_region

try:
    pop_regions = read_pop_region()
except FileNotFoundError as pop_file_err:
    pop_regions = {}
    print("Population .csv file not found is the direction to dir data correct?")
    print("Please set data_dir and run `pop_regions = read_pop_region()`")

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

def calculate_daily_change(df, region_id, cumulated_field, field):
    """calculates the interval change in field cumulated_field and stores it in
    field.
    """
    reg_logicind = df["geoId"] == region_id
    reg_deaths = np.array(df.loc[reg_logicind, cumulated_field])
    for i, deaths in enumerate(reg_deaths):
        if i>0 and deaths < reg_deaths[i-1]:
            reg_deaths[i] = reg_deaths[i-1]
    reg_deaths = reg_deaths - [0, *reg_deaths[:-1]]
    df.loc[reg_logicind, field] = reg_deaths

def prepare_special_regions(srcReg):
    # Re-add france, rename france as hopitaux
    # Define EHPAD as death with ehpad deaths
    df_to_add_france = pd.DataFrame(srcReg[srcReg['maille_nom'] == "France"])
    df_to_add_france["deces"] = df_to_add_france.loc[:, "deces"].add(
        df_to_add_france.loc[:, "deces_ehpad"])

    df_to_add_ehpad = pd.DataFrame(srcReg[srcReg['maille_nom'] == "France"])
    df_to_add_ehpad["deces"] = df_to_add_ehpad["deces_ehpad"]
    df_to_add_ehpad["maille_nom"] = "France-EHPAD"
    df_to_add_ehpad["maille_code"] = "FRA-EHPAD"

    srcReg.loc[srcReg['maille_nom'] == "France",'maille_code'] = "FRA-HOP"
    srcReg.loc[srcReg['maille_nom'] == "France",'maille_nom'] = "France-hopitaux"

    df_to_add = df_to_add_france.append(df_to_add_ehpad)
    return srcReg.append(df_to_add)

def convert_opencovidfr_to_ICL_model(srcReg, pop_per_region=None):
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
    if pop_per_region is None:
        pop_per_region = pop_regions
    dst = pd.DataFrame()

    # Prepare special regions : France, Hopital, EHPAD
    srcReg = prepare_special_regions(srcReg)
    # mapping attributs src -> dst
    dst['dateRep'] = srcReg['date'].values
    # conversion vers des vraies dates
    dst['dateRep'] = dst['dateRep'].apply(pd.to_datetime, format="%Y-%m-%d")

    # valeurs bidon pour le moment
    dst['day'] = 1
    dst['month'] = 1
    dst['year'] = 2020

    dst['cumulated_cases'] = srcReg['cas_confirmes'].values
    # avec ou sans les ehpads ?
    dst['cumulated_deaths'] = srcReg['deces'].values

    dst['countriesAndTerritories'] = srcReg['maille_nom'].values
    dst['geoId'] = srcReg['maille_code'].values
    dst['countryterritoryCode'] = srcReg['maille_code'].values

    # valeurs 2020
    dst['popData2018'] = [pop_per_region[regId] for regId in dst['geoId']]

    dst["t"] = dst["dateRep"].apply(lambda v: dt_to_dec(v))

    # et pour finir on re-sérialise la date sous un autre format
    dst['dateRep'] = dst['dateRep'].apply(lambda x: x.strftime('%d/%m/%Y'))
    
    # Need to compute the new deaths per day as required by the format
    dst.sort_values("t", inplace=True)
    active_regions = dst["geoId"].unique()
    dst["deaths"] = 0
    for region in active_regions:
        calculate_daily_change(dst, region, "cumulated_deaths", "deaths")
        calculate_daily_change(dst, region, "cumulated_cases", "cases")

    return dst.drop(["cumulated_deaths", "cumulated_cases"], axis=1)


def find_active_regions(src, reg):

    # Extraire les IDs de regions qui match reg
    available_region_list = np.append(src["maille_code"].unique(), special_regions)
    if reg == "all-france":
        active_regions = [a for a in available_region_list if a in pop_regions]
    else:
        re_expression = re.compile(reg)
        active_regions = []  # Les regions a ajoutees au fichier
        for region in available_region_list:
            if re_expression.search(region):
                active_regions.append(region)

    print("{} pattern matched the following {} IDs:".format(reg,len(active_regions)))
    print(active_regions)
    return active_regions


def clean_region_data(src, active_regions):
    # filtrage sur maille_code dans "active_regions"
    srcReg = src.loc[
        lambda df: [code in active_regions for code in df.maille_code], :]

    # notre intérêt est sur la colonne décès, on supprime donc les lignes où
    # cette valeur n'est pas connue
    srcReg = srcReg.dropna(how='all', subset=["deces"])
    # Nous avons besoins de remplir les cas_confirmes manquant avec des 0
    srcReg["cas_confirmes"] = srcReg["cas_confirmes"].fillna(0)
    # Il y a des doublons - on les élimines, on conserve celui qui a de
    # preferences les deces avec ehpad
    srcReg.sort_values(
        ["date", "maille_code", "deces", "deces_ehpad"],
        ascending=True, na_position="last")
    srcReg = srcReg.drop_duplicates(
        subset=["date", "maille_code"], keep='first')
    # Nous avons besoins de remplir les cas_confirmes manquant avec des 0
    srcReg["deces_ehpad"] = srcReg["deces_ehpad"].fillna(0)
    return srcReg

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
    srcReg = clean_region_data(src, active_regions)

    dst = convert_opencovidfr_to_ICL_model(srcReg)
    print(reg + " : " + str(dst.shape[0]) + " lignes")

    # et voilà
    dst.to_csv(
        data_dir + reg + '.csv', index=False, columns=[
            "dateRep","day","month","year","cases","deaths",
            "countriesAndTerritories","geoId","countryterritoryCode",
            "popData2018"
        ]
        )


if __name__ == "__main__":
    process_from_cmd()
