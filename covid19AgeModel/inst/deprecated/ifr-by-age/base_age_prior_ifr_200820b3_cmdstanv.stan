functions {

    real model_log_dens(
        int[] deaths_slice,
        int start,
        int end,
        //pars
        vector logit_p_avg,
        // data
        int[] casesM
        )
    {
        int n;
        real lpmf;
        int N_slice;
        
        lpmf = 0.0;
        N_slice = end - start + 1;
        for(n_slice in 1:N_slice)
        {
            n = n_slice + start - 1;
            lpmf += binomial_logit_lpmf(deaths_slice[n] | casesM[n], logit_p_avg[n]);
        }
        return( lpmf );
    }
}

data
{
    int<lower=0> N; // number of observations
    int<lower=0> M; // number of studies
    int deaths[N]; // number of deaths
    int casesM[N]; // numnber of cases, midpoint estimate
    vector<lower=0>[N] age_study; //  age midpoints
}

transformed data
{
    real delta = 1e-9;
    int<lower=0> N_predict=21;
    vector<lower=0>[N_predict] age_predict;
    int age_predict_study_idx[N];
    
    for(i in 1:N_predict)
    {
        age_predict[i] = 100/(N_predict-1)*(i-1);
    }
    for(i in 1:N)
    {
        for(j in 2:N_predict)
        {
            if(age_predict[j-1]<=age_study[i] && age_study[i]<age_predict[j])
            {
                age_predict_study_idx[i] = j-1;
            }
        }
    }
    print(age_predict_study_idx);
}

parameters
{
    real alpha;
    real beta;
    real<lower=0> obs_rnde_sd;
    vector[N] obs_rnde;
    real<lower=0> gp_len_sc;
    real<lower=0> gp_msd;
    vector[N_predict] eta_f;
}

transformed parameters
{
    vector[N] logit_p_avg;
    vector[N_predict] gp_age_predict;
    {
        matrix[N_predict, N_predict] K = cov_exp_quad(to_array_1d(age_predict), gp_msd, gp_len_sc);

        // diagonal elements
        for (n in 1:N_predict)
            K[n, n] = K[n, n] + delta;

        gp_age_predict = cholesky_decompose(K) * eta_f;
        
        logit_p_avg = alpha + gp_age_predict[age_predict_study_idx] + beta * age_study + obs_rnde;
    }
    
}

model
{
    target += normal_lpdf( alpha | 0, 20);
    target += normal_lpdf( beta | 0, 1);
    target += cauchy_lpdf( obs_rnde_sd | 0, 1);
    target += normal_lpdf( obs_rnde | 0, obs_rnde_sd);
    target += normal_lpdf( gp_msd | 0, 2);
    //target += inv_gamma_lpdf( gp_len_sc | 16, 700); // qinvgamma(c(.01,.99), 16, 700) = 26.17519 85.56298
    target += inv_gamma_lpdf( gp_len_sc | 11, 400); // qinvgamma(c(.01,.99), 11, 400) =  [1]  19.85636 83.83554
    target += normal_lpdf( eta_f | 0, 1);
    
    //log lkl rstan version
    target += model_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    //target += reduce_sum(model_log_dens, deaths, 1,
        // pars
        logit_p_avg,
        // data
        casesM
        );
}

generated quantities
{
    vector<lower=0, upper=1>[N_predict] p_predict;
    int pred_deaths[N_predict];
    
    p_predict = inv_logit( alpha + gp_age_predict + beta * age_predict );
    pred_deaths = binomial_rng(rep_array(10000000, N_predict), p_predict);
}
