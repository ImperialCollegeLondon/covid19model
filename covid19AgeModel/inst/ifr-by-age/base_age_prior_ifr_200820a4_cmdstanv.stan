// model 4:
// marginalise Binomial between casesL and casesU as total
// and p specified as average in age band interval
// using implementation of binomial_lpmf to prepare vectorisation

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
    
    real calculate_marginal_binomial(//pars
        real p_avg,
        // data
        int total_N,
        vector log_choose_local,
        int suc,
        vector totals
        )
    {
        return(
            log_sum_exp(
                log_choose_local +
                rep_vector(suc * log(p_avg),total_N) +
                (totals - suc ) * log(1-p_avg)
                )
            );
    }

    real model_4_log_dens(
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
        int[] casesN,
        matrix log_choose_fct,
        matrix casesAux
        )
    {
        int n;
        real lpmf;
        int N_slice;
        real p_avg;
        
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
                        
            lpmf +=
                calculate_marginal_binomial(
                    p_avg,
                    casesN[n],
                    log_choose_fct[1:casesN[n], n],
                    deaths_slice[n],
                    casesAux[1:casesN[n], n]
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
    int max_casesN; // numnber of max auxiliary cases
    int casesN[N]; // numnber of auxiliary cases for each observation
    int deaths[N]; // number of deaths
    vector<lower=0>[N_predict] age; //  age predictions
    vector<lower=0>[N] age_study; //  age midpoints
    int lower_age_idx[N]; // lower index of observed age band in age_predict
    int upper_age_idx[N]; // upper index of observed age band in age_predict
    int<lower=1,upper=M> study[N];
    matrix[max_casesN, N] log_choose_fct; // precomputed log choose
    matrix[max_casesN, N] casesAux; // precomputed aux cases
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
    //target += model_4_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    target += reduce_sum(model_4_log_dens, deaths, 1,
        // pars
        p_predict,
        study_rnde,
        // data
        study,
        lower_age_idx,
        upper_age_idx,
        casesN,
        log_choose_fct,
        casesAux
        );
}


