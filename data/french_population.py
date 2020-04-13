"""
Preprocesses raw population data for French regions.

Generates: `ages-french-regions.csv` a file with stratified age ranges:
,0-9,10-19,20-29,30-39,40-49,50-59,60-69,70-79,80+

"""


import sys
import pandas as pd
import pdb
from path import Path

# Step 1 Load population data from "population-fra-INSEE-departement.csv"
datadir = Path('data/FRA/')
departement_csv = 'population-fra-INSEE-departement.csv'
region_to_departement_csv = 'population-fra-INSEE-region-departement.csv'
# Step 2 define output format
# Maps ages from INSEE breakdown to desired breakdown
age_map = {
    "0-9": ["00_04", "05_09"],
    "10-19": ["10_14", "15_19"],
    "20-29": ["20_24", "25_29"],
    "30-39": ["30_34", "35_39"],
    "40-49": ["40_44", "45_49"],
    "50-59": ["50_54", "55_59"],
    "60-69": ["60_64", "65_69"],
    "70-79": ["70_74", "75_79"],
    "80+": ["80_84", "85_89", "90_94", "95_130"],
}
reversed_age_map = {}
for age_table in age_map:
    for age_source in age_map[age_table]:
        reversed_age_map[age_source] = age_table


def df_source_2_table(row, target_table):
    new_col = reversed_age_map[row["classe_age_5"]]
    new_row = row["departement_code"]
    target_table[new_col][new_row] += row["population"]


def process_department_data(datafile_departement):
    src = pd.read_csv(datafile_departement, sep=";")
    # Select last non estimated year of data
    src = src.loc[lambda df: df.year == 2018]
    for field in src:
        print(f"{field} has the following unique elements:")
        print(src[field].unique())

    age_table = pd.DataFrame(
        0,
        index=src["departement_code"].unique(),
        columns=[key for key in age_map]
    )
    src.apply(lambda x: df_source_2_table(x, age_table), axis=1)

    age_table["total"] = age_table.sum(axis=1)
    age_table["fra_code"] = age_table.index
    age_table["fra_code"].map('DEP-{}')

    return age_table


def main():
    datafile_departement = datadir + departement_csv
    
    age_table_department = process_department_data(datafile_departement)

    pdb.set_trace()
    return len(sys.argv)


if __name__ == '__main__':
    main()
