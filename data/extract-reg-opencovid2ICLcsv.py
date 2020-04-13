# -*- coding: utf-8 -*-

import pandas as pd
from datetime import datetime
import sys


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

# TODO…
popreg = {
    "REG-93": 5059473,
}

src = pd.read_csv(fic)

# filtrage sur maille_code
srcReg = src.loc[lambda df: df.maille_code == reg, :]

# notre intérêt est sur la colonne décès, on supprime donc les lignes où cette
# valeur n'est pas connue
srcReg = srcReg.dropna(how='all', subset=["deces"])
print(reg + " : " + str(srcReg.shape[0]) + " lignes")

# Attn il y a des doublons - on les élimines, celui qui reste est indéterminé
# ATTN ne fonctionne que si on est MONO région
srcReg = srcReg.drop_duplicates(subset=["date"])

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
dst['popData2018'] = popreg[reg]

dst["t"] = dst["dateRep"].apply(lambda v: dt_to_dec(v))

# et pour finir on re-sérialise la date sous un autre format
dst['dateRep'] = dst['dateRep'].apply(lambda x: x.strftime('%d/%m/%Y'))

# et voilà
dst.to_csv(reg,index=False)
