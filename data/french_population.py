"""
Preprocesses raw population data for French regions.

Generates: `ages-french-regions.csv` a file with stratified age ranges:
,0-9,10-19,20-29,30-39,40-49,50-59,60-69,70-79,80+

"""


import sys
import pandas as pd
import pdb
import traceback
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

def df_department_table_to_region(row, source_table, target_table):
    for age in target_table:
        target_table.loc[row["region_code"], age] += \
            source_table.loc[row["departement_code"], age]

def add_names_to_age_tables(
    age_table, codes_to_names, name_field="", code_field="code"):
    age_table["name"] = 0
    for row in age_table.index:
        name = codes_to_names[(codes_to_names[code_field]==row)][name_field]
        age_table.loc[row, "name"] = name.iloc[0]

def new_age_table(index=[]):
    return pd.DataFrame(
        0,
        index=index,
        columns=[key for key in age_map]
    )

def process_department_data(datafile_departement):
    src = pd.read_csv(datafile_departement, sep=";")
    # Select last non estimated year of data
    src = src.loc[lambda df: df.year == 2019]
    for field in src:
        print(f"{field} has the following unique elements:")
        print(src[field].unique())

    age_table = new_age_table(index=src["departement_code"].unique())
    
    src.apply(lambda x: df_source_2_table(x, age_table), axis=1)

    age_table["total"] = age_table.sum(axis=1)
    age_table["fra_code"] = age_table.index
    age_table["fra_code"] = age_table["fra_code"].map('DEP-{}'.format)

    return age_table

def department_to_region(departement_age_table):
    datafile_region =  datadir + region_to_departement_csv
    src = pd.read_csv(datafile_region, sep=";")
    src.set_index(["departement_code"])
    region_age_table = new_age_table(src["region_code"].unique())

    src.apply(lambda x: df_department_table_to_region(
        x, departement_age_table, region_age_table),
        axis=1)
    region_age_table["total"] = region_age_table.sum(axis=1)
    region_age_table["fra_code"] = region_age_table.index
    region_age_table["fra_code"] = \
        region_age_table["fra_code"].map('REG-{:02d}'.format)
    return region_age_table

def main():
    datafile_departement = datadir + departement_csv
    
    departement_age_table = process_department_data(datafile_departement)
    region_age_table = department_to_region(departement_age_table)

    check_sum = (departement_age_table.sum()==region_age_table.sum()
    ).drop("fra_code")
    if not check_sum.all():
        raise ArithmeticError(
            "Region and departement total populations do not match."
            + " Check failed"
        )
    datafile_region =  datadir + region_to_departement_csv
    codes_to_names = pd.read_csv(datafile_region, sep=";")
    add_names_to_age_tables(
        region_age_table, codes_to_names,
        name_field="region", code_field="region_code")
    add_names_to_age_tables(
        departement_age_table, codes_to_names,
        name_field="departement", code_field="departement_code")

    age_table = region_age_table.append(
        departement_age_table, ignore_index=True)
    print(age_table)
    pdb.set_trace()
    return len(sys.argv)


if __name__ == '__main__':
    try:
        main()
    except Exception as e:
        traceback.print_exc()
        pdb.post_mortem()
    
