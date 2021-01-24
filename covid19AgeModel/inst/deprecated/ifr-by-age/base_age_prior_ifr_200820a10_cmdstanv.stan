functions {
    real calculate_marginal_betabinomial(//pars
        real alpha_local,
        real beta_local,
        // data
        int total_N,
        int deaths_local,
        int[] totals
        )
    {
        real lpmfs[total_N];
        for(i in 1:total_N)
        {
            lpmfs[i] = beta_binomial_lpmf( deaths_local | totals[i], alpha_local, beta_local);
        }
        return( log_sum_exp( lpmfs ) );
    }

    real model_10_log_dens(
        int[] deaths_slice,
        int start,
        int end,
        //pars
        vector logit_p_obs,
        vector var_infl,
        // data
        int N,
        int[] casesN,
        int[,] casesAux
        )
    {
        int n;
        real lpmf;
        int N_slice;
        vector[N] p_obs;
        vector[N] overdisp;
        vector[N] alpha;
        vector[N] beta;
        
        // parameter transformations
        p_obs = inv_logit( logit_p_obs );
        overdisp = inv(var_infl);
        alpha = p_obs .* overdisp;
        beta = (1. - p_obs) .* overdisp;
        
        // likelihood
        lpmf = 0.0;
        N_slice = end - start + 1;
        for(n_slice in 1:N_slice)
        {
            n = n_slice + start - 1;
            lpmf +=
                calculate_marginal_betabinomial(
                    alpha[n],
                    beta[n],
                    casesN[n],
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
    int casesAux[max_casesN, N]; // precomputed aux cases
}

parameters
{
    real alpha0;
    real alpha1;
    real beta0;
    real beta1;
    real<lower=0> study_rnde_sd;
    vector[M] study_rnde;
    real<lower=0> study_rnde_sd2;
    vector[M] study_rnde2;
}

transformed parameters
{
    vector<lower=0>[N] var_infl;
    vector[N] logit_p_obs;
        
    var_infl = exp( alpha0 + alpha1 * age_study + study_rnde2[study] );
    logit_p_obs = beta0 + beta1 * age_study + study_rnde[study];
}

model
{
    target += normal_lpdf( alpha0 | 0, 20);
    target += normal_lpdf( alpha1 | 0, 1);
    target += normal_lpdf( beta0 | 0, 20);
    target += normal_lpdf( beta1 | 0, 1);
    target += normal_lpdf( study_rnde | 0, study_rnde_sd);
    target += exponential_lpdf( study_rnde_sd | 20);
    target += normal_lpdf( study_rnde2 | 0, study_rnde_sd2);
    target += exponential_lpdf( study_rnde_sd2 | 200);

    //log lkl rstan version
    target += model_10_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    //target += reduce_sum(model_10_log_dens, deaths, 1,
        // pars
        logit_p_obs,
        var_infl,
        // data
        N,
        casesN,
        casesAux
        );
}

generated quantities
{
    vector<lower=0, upper=1>[N_predict] p_predict;
    vector[N_predict] var_infl_predict;
    int pred_deaths[N_predict];
    
    p_predict = inv_logit( beta0 + beta1 * age );
    var_infl_predict = exp( alpha0 + alpha1 * age );
    
    {
        vector[N_predict] alpha;
        vector[N_predict] beta;
        alpha = p_predict .* inv( var_infl_predict );
        beta = (1-p_predict) .* inv( var_infl_predict );
    
        pred_deaths = beta_binomial_rng(rep_array(10000000, N_predict), alpha, beta);
    }
}
