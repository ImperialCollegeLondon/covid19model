import yaml
import pandas as pd
import numpy as np
from src.util import poly, dt_to_dec
from scipy.stats import gamma as gamma_scipy
from numpy.random import gamma as gamma_np
from statsmodels.distributions.empirical_distribution import ECDF


class HierarchicalDataset:
    """Base Dataset class containing attributes relating to the datasets used for the modelling and methods
    for data wrangling

        Args:
            - config_dir
            - cases_dir
            - ifr_dir
            - serial_interval_dir
            - interventions_dir
            - num_countries
            - num_covariates
            - N2: number of days including forecast
            - DEBUG: flag for debugging setting


        Attributes:
            - countries
            - cases
            - serial_interval
            - num_countries
            - num_covariates
            - DEBUG
            - ifr
            - covariate_names
            - covariates
    """

    def __init__(
        self,
        config_dir="../../data/catalog.yml",
        cases_dir="../../data/COVID-19-up-to-date.csv",
        ifr_dir="../../data/weighted_fatality.csv",
        serial_interval_dir="../../data/serial_interval.csv",
        interventions_dir="../../data/interventions.csv",
        num_countries=11,
        num_covariates=6,
        N2=75,
        DEBUG=False,
    ):
        with open(config_dir, "r") as stream:
            # merci https://stackoverflow.com/questions/1773805/how-can-i-parse-a-yaml-file-in-python
            try:
                config = yaml.safe_load(stream)
            except yaml.YAMLError as exc:
                print(exc)

        # read in all the datasets
        self.countries = config["countries"]
        self.cases = pd.read_csv(cases_dir, encoding="ISO-8859-1")
        self.serial_interval = pd.read_csv(serial_interval_dir)
        covariates = pd.read_csv(interventions_dir)
        self.num_countries = num_countries
        self.num_covariates = num_covariates
        # whether to use smaller dataset for debugging
        self.DEBUG = DEBUG

        # process the datasets
        # remaing column and the UK in particular
        ifr = pd.read_csv(ifr_dir)
        # inefficient bit but couldn't figure out why .rename() doesn't work
        ifr["country"] = ifr.iloc[:, 1]
        # rename the UK
        ifr["country"][ifr["country"] == "United Kingdom"] = "United_Kingdom"
        self.ifr = ifr
        # pick out the covariates for the countries (11 by default, 8 interventions)
        # num_covariates+1 because we need the Country index column too
        covariates = covariates.iloc[:num_countries, : num_covariates + 1]
        self.covariate_names = list(covariates.columns)[1:]
        # convert the dates to datetime
        for covariate_name in self.covariate_names:
            covariates[covariate_name] = covariates[covariate_name].apply(
                pd.to_datetime, format="%Y-%m-%d"
            )

        # making all covariates that happen after lockdown to have same date as lockdown
        non_lockdown_covariates = self.covariate_names.copy()
        non_lockdown_covariates.remove("lockdown")
        for covariate_name in non_lockdown_covariates:
            ind = covariates[covariate_name] > covariates["lockdown"]
            covariates[covariate_name][ind] = covariates["lockdown"][ind]

        self.covariates = covariates

    def get_stan_data(self, N2):
        """Returns a dictionary object containing data to be fed into the Stan compiler

        Args:

        N2: number of days including forecast

        """
        stan_data = {}

        # M, number of countries
        stan_data["M"] = self.num_countries
        stan_data["p"] = self.num_covariates
        stan_data["x1"] = poly(np.linspace(0, N2 - 1, N2), 2)[:, 0]
        # for some reason it is negative, check util.py
        stan_data["x2"] = -poly(np.linspace(0, N2 - 1, N2), 2)[:, 1]
        # TODO: this is hardcoded in base.r, beware
        stan_data["N0"] = self.num_covariates
        stan_data["N2"] = N2
        stan_data["SI"] = self.serial_interval["fit"][:N2]
        stan_data["x"] = np.linspace(1, N2, N2)

        # TODO: we will use lists, but we need to be careful of stack memory in the future
        stan_data["EpidemicStart"] = []
        stan_data["y"] = []
        stan_data["N"] = []
        # initialise with number of covariates
        for i in range(1, self.num_covariates+1):
            stan_data["covariate{}".format(i)] = np.zeros((N2, self.num_countries))

        # store the covariates in a numpy array, initialised
        stan_data["deaths"] = np.ones((N2, self.num_countries)) * (-1)
        stan_data["cases"] = np.zeros((N2, self.num_countries)) * (-1)
        stan_data["f"] = np.zeros((N2, self.num_countries))

        # we will generate the dataset in this country order. Could also use a pandas dataframe, but not necessary in my opinion
        for country_num, country in enumerate(self.countries):
            ifr = self.ifr["weighted_fatality"][self.ifr["country"] == country]
            covariates1 = self.covariates.loc[
                self.covariates["Country"] == country, self.covariate_names
            ]
            cases = self.cases[self.cases["countriesAndTerritories"] == country]
            cases["date"] = cases["dateRep"].apply(pd.to_datetime, format="%d/%m/%Y")

            cases["t"] = cases["date"].apply(lambda v: dt_to_dec(v))
            cases = cases.sort_values(by="t")
            cases = cases.reset_index()

            # where the first case occurs
            index = cases[(cases["cases"] > 0)].index[0]
            # where the cumulative deaths reaches 10
            index_1 = cases[(cases["deaths"].cumsum() >= 10)].index[0]
            # 30 days before 10th death
            index_2 = index_1 - 30

            # TODO: what is the latter?
            print(
                "First non-zero cases is on day {}, and 30 days before 5 days is day {}".format(
                    index, index_2
                )
            )

            # # only care about this timeframe
            cases = cases[index_2 : cases.shape[0]]

            # update Epidemic Start day for each country
            stan_data["EpidemicStart"].append(index_1 + 1 - index_2)
            # turn intervention dates into boolean
            for covariate in self.covariate_names:
                cases[covariate] = (
                    cases["date"] > covariates1[covariate].values[0]
                ) * 1

            # record dates for cases in the country
            cases[country] = cases["date"]

            # Hazard estimation
            N = cases.shape[0]
            print("{} has {} of data".format(country, N))

            # number of days to forecast
            forecast = N2 - N

            if forecast < 0:
                raise ValueError("Increase N2 to make it work. N2=N, forecast=N2-N")

            # discrete hazard rate from time t=0,...,99
            h = np.zeros(forecast + N)

            if self.DEBUG:
                mean = 18.8
                cv = 0.45

                loc = 1 / cv ** 2
                scale = mean * cv ** 2
                for i in range(len(h)):
                    h[i] = (
                        ifr * gamma_scipy.cdf(i, loc=loc, scale=scale)
                        - ifr * gamma_scipy.cdf(i - 1, loc=loc, scale=scale)
                    ) / (1 - ifr * gamma_scipy.cdf(i - 1, loc=loc, scale=scale))

            else:
                # infection to onset
                mean_1 = 5.1
                cv_1 = 0.86
                loc_1 = 1 / cv_1 ** 2
                scale_1 = mean_1 * cv_1 ** 2
                # onset to death
                mean_2 = 18.8
                cv_2 = 0.45
                loc_2 = 1 / cv_2 ** 2
                scale_2 = mean_2 * cv_2 ** 2
                # assume that IFR is probability of dying given infection
                x1 = gamma_np(shape=loc_1, scale=scale_1, size=int(5e6))
                # infection-to-onset ----> do all people who are infected get to onset?
                x2 = gamma_np(shape=loc_2, scale=scale_2, size=int(5e6))

                # CDF of sum of 2 gamma distributions
                gamma_cdf = ECDF(x1 + x2)

                # probability distribution of the infection-to-death distribution \pi_m in the paper
                def convolution(u):
                    return ifr * gamma_cdf(u)

                h[0] = convolution(1.5) - convolution(0)

                for i in range(1, len(h)):
                    h[i] = (convolution(i + 0.5) - convolution(i - 0.5)) / (
                        1 - convolution(i - 0.5)
                    )

            # TODO: Check these quantities via tests
            s = np.zeros(N2)
            s[0] = 1
            for i in range(1, N2):
                s[i] = s[i - 1] * (1 - h[i - 1])

            # slot in these values
            stan_data["N"].append(N)
            stan_data["f"][:, country_num] = h * s
            stan_data["y"].append(cases["cases"].values[0])
            stan_data["deaths"][:N, country_num] = cases["deaths"]
            stan_data["cases"][:N, country_num] = cases["cases"]
            covariates2 = np.zeros((N2, self.num_covariates))
            covariates2[:N, :] = cases[self.covariate_names].values
            covariates2[N:N2, :] = covariates2[N - 1, :]
            covariates2 = pd.DataFrame(covariates2, columns=self.covariate_names)

            for j, covariate in enumerate(self.covariate_names):
                stan_data["covariate{}".format(j+1)][:, country_num] = covariates2[
                    covariate
                ]
        # convert these arrays to integer dtype
        stan_data["cases"] = stan_data["cases"].astype(int) 
        stan_data["deaths"] = stan_data["deaths"].astype(int)
        return stan_data
