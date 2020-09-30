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
    real<lower=0> age[N_predict]; //  age predictions
    vector<lower=0>[N] age_study; //  age midpoints
    int lower_age_idx[N]; // lower index of observed age band in age_predict
    int upper_age_idx[N]; // upper index of observed age band in age_predict
    int<lower=1,upper=M> study[N];
    matrix[max_casesN, N] log_choose_fct; // precomputed log choose
    matrix[max_casesN, N] casesAux; // precomputed aux cases
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
    
    {
        matrix[N_predict, N_predict] L_K;
        matrix[N_predict, N_predict] K = cov_exp_quad(age, gp_msd, gp_len_sc);

        // diagonal elements
        for (n in 1:N_predict)
            K[n, n] = K[n, n] + delta;

        L_K = cholesky_decompose(K);
        
        p_predict = inv_logit( beta + L_K * eta_f );
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


