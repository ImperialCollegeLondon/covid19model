// model 3:
// Binomial using casesU as total
// and p specified as average in age band interval
// own implementation of binomial_lpmf to prepare vectorisation
// this should have exactly same log posterior as model 2

functions {
    real calculate_p_avg_for_obs(//pars
            vector p_predict,
            real study_rnde_local,
            // data
            int lower_age_idx_local,
            int upper_age_idx_local
        )
    {
        real ans;
        ans = mean( p_predict[ lower_age_idx_local:upper_age_idx_local ] );
        ans = inv_logit( logit( ans ) + study_rnde_local );
        return( ans );
    }

    real model_3_log_dens(
        int[] deaths_slice,
        int start,
        int end,
        //pars
        vector p_predict,
        vector study_rnde,
        // data
        int[] study_idx,
        int[] lower_age_idx,
        int[] upper_age_idx,
        real[,] log_choose_fct,
        int[] casesL,
        int[] casesU
        )
    {
        int n;
        real lpmf;
        int N_slice;
        real p_avg;
        real log_choose_local;
        
        lpmf = 0.0;
        N_slice = end - start + 1;
        for(n_slice in 1:N_slice)
        {
            n = n_slice + start - 1;
            
            p_avg =
                calculate_p_avg_for_obs(
                    p_predict,
                    study_rnde[study_idx[n]],
                    lower_age_idx[n],
                    upper_age_idx[n]
                    );
            
            log_choose_local = log_choose_fct[casesU[n]-casesL[n]+1, n];
            
            lpmf +=
                (
                    log_choose_local +
                    deaths_slice[n] * log(p_avg) +
                    (casesU[n] - deaths_slice[n] ) * log(1-p_avg)
                );
        }
        return( lpmf );
    }
}

data
{
    int<lower=0> N; // number of observations
    int<lower=0> N_predict; // number of predictions
    int<lower=0> M; // number of studies
    int deaths[N]; // number of deaths
    int casesU[N]; //  upper bound cases
    int casesL[N]; // lower bound cases
    vector<lower=0>[N_predict] age; //  age predictions
    vector<lower=0>[N] age_study; //  age midpoints
    int lower_age_idx[N]; // lower index of observed age band in age_predict
    int upper_age_idx[N]; // upper index of observed age band in age_predict
    int<lower=1,upper=M> study[N];
    int max_casesN;
    real log_choose_fct[max_casesN, N]; // precomputed log choose
}

parameters
{
    real alpha;
    real beta;
    real<lower=0> study_rnde_sd;
    vector[M] study_rnde;
}

transformed parameters
{
    vector<upper=0>[N_predict] log_p_predict;
    vector<lower=0,upper=1>[N_predict] p_predict;
    log_p_predict = alpha + beta * age;
    p_predict = exp( log_p_predict );
}

model
{
    target += normal_lpdf( alpha | 0, 10);
    target += normal_lpdf( beta | 0, 1);
    target += normal_lpdf( study_rnde | 0, study_rnde_sd);
    target += exponential_lpdf( study_rnde_sd | 10);

    //log lkl rstan version
    target += model_3_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    //target += reduce_sum(model_3_log_dens, deaths, 1,
        // pars
        p_predict,
        study_rnde,
        // data
        study,
        lower_age_idx,
        upper_age_idx,
        log_choose_fct,
        casesL,
        casesU
        );
}


