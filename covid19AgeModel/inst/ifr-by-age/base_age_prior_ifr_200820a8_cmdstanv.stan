functions {
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

    real model_8_log_dens(
        int[] deaths_slice,
        int start,
        int end,
        //pars
        vector logit_p_avg,
        // data
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
            p_avg = inv_logit(logit_p_avg[n]);
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
    int deaths[N]; // number of deaths
    int casesN[N]; // numnber of auxiliary cases for each observation
    vector<lower=0>[N_predict] age; //  age predictions
    vector<lower=0>[N] age_study; //  age midpoints
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
    vector[N] logit_p_avg;
    logit_p_avg = alpha + beta * age_study + study_rnde[ study ];
}

model
{
    target += normal_lpdf( alpha | 0, 20);
    target += normal_lpdf( beta | 0, 1);
    target += normal_lpdf( study_rnde | 0, study_rnde_sd);
    target += exponential_lpdf( study_rnde_sd | 20);

    //log lkl rstan version
    target += model_8_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    //target += reduce_sum(model_8_log_dens, deaths, 1,
        // pars
        logit_p_avg,
        // data
        casesN,
        log_choose_fct,
        casesAux
        );
}

generated quantities
{
    vector<lower=0, upper=1>[N_predict] p_predict;
    int pred_deaths[N_predict];
    
    p_predict = inv_logit( alpha + beta * age );
    pred_deaths = binomial_rng(rep_array(10000000, N_predict), p_predict);
}
