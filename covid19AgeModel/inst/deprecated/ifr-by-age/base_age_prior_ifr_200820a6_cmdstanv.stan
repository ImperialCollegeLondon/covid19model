functions {
    real calculate_logit_p_avg_for_obs(//pars
            vector p_predict,
            real study_rnde_local,
            // data
            int lower_age_idx_local,
            int upper_age_idx_local
        )
    {
        real ans;
        ans = mean( p_predict[ lower_age_idx_local:upper_age_idx_local ] );
        ans = logit( ans ) + study_rnde_local;
        return( ans );
    }

    real model_6_log_dens(
        int[] deaths_slice,
        int start,
        int end,
        //pars
        real[] logit_p_avg,
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
    int<lower=0> N_predict; // number of predictions
    int<lower=0> M; // number of studies
    int deaths[N]; // number of deaths
    int casesM[N]; // numnber of cases, midpoint estimate
    real<lower=0> age[N_predict]; //  age predictions
    vector<lower=0>[N] age_study; //  age midpoints
    int lower_age_idx[N]; // lower index of observed age band in age_predict
    int upper_age_idx[N]; // upper index of observed age band in age_predict
    int<lower=1,upper=M> study[N];
}

transformed data
{
    real delta = 1e-9;
}


parameters
{
    real beta;
    real<lower=0> study_rnde_sd;
    vector[M] study_rnde;
    real<lower=0> gp_len_sc;
    real<lower=0> gp_msd;
    vector[N_predict] eta_f;
}

transformed parameters
{
    vector<lower=0,upper=1>[N_predict] p_predict;
    real logit_p_avg[N];
    
    {
        matrix[N_predict, N_predict] L_K;
        matrix[N_predict, N_predict] K = cov_exp_quad(age, gp_msd, gp_len_sc);

        // diagonal elements
        for (n in 1:N_predict)
            K[n, n] = K[n, n] + delta;

        L_K = cholesky_decompose(K);
        
        p_predict = inv_logit( beta + L_K * eta_f );
        
        for(n in 1:N)
        {
            logit_p_avg[n] =
                calculate_logit_p_avg_for_obs(
                    p_predict,
                    study_rnde[study_idx[n]],
                    lower_age_idx[n],
                    upper_age_idx[n]
                    );
        }
    }
}

model
{
    target += normal_lpdf( beta | 0, 7); // x<- 2*c(-1,1)*7; exp(x)/(1+exp(x)) = 8.315280e-07 9.999992e-01
    target += normal_lpdf( gp_msd | 0, 2);
    target += inv_gamma_lpdf( gp_len_sc | 3.25, 45); // qinvgamma(c(.01,.99), 3.25, 45) =  [1]  5.099039 85.712677
    target += normal_lpdf( study_rnde | 0, study_rnde_sd);
    target += exponential_lpdf( study_rnde_sd | 10);
    target += normal_lpdf( eta_f | 0, 1);

    //log lkl rstan version
    target += model_6_log_dens(deaths, 1, N,
    //log lkl cmdstan version
    //target += reduce_sum(model_6_log_dens, deaths, 1,
        // pars
        logit_p_avg,
        // data
        casesM
        );
}

generated quantities
{
    int pred_deaths[N];
    pred_deaths = binomial_rng(casesM, inv_logit(logit_p_avg));
}
