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
    int<lower=1,upper=M> study[N];
}

transformed data
{
    int<lower=0> A=9; // number of age bands
    int<lower=0> N_predict=101;
    int age_band_obs[N];
    vector<lower=0>[N_predict] age_predict;
    int age_band_predict[N_predict];
    
    //A= 9;
    //N_predict= 101;
    for(i in 1:N)
    {
        if(age_study[i]<10)
            age_band_obs[i] = 1;
        else if(age_study[i]<20)
            age_band_obs[i] = 2;
        else if(age_study[i]<30)
            age_band_obs[i] = 3;
        else if(age_study[i]<40)
            age_band_obs[i] = 4;
        else if(age_study[i]<50)
            age_band_obs[i] = 5;
        else if(age_study[i]<60)
            age_band_obs[i] = 6;
        else if(age_study[i]<70)
            age_band_obs[i] = 7;
        else if(age_study[i]<80)
            age_band_obs[i] = 8;
        else
            age_band_obs[i] = 9;
    }
    print(age_band_obs);
    for(i in 1:N_predict)
    {
        age_predict[i] = i-1;
    }
    for(i in 1:N_predict)
    {
        if(age_predict[i]<10)
            age_band_predict[i] = 1;
        else if(age_predict[i]<20)
            age_band_predict[i] = 2;
        else if(age_predict[i]<30)
            age_band_predict[i] = 3;
        else if(age_predict[i]<40)
            age_band_predict[i] = 4;
        else if(age_predict[i]<50)
            age_band_predict[i] = 5;
        else if(age_predict[i]<60)
            age_band_predict[i] = 6;
        else if(age_predict[i]<70)
            age_band_predict[i] = 7;
        else if(age_predict[i]<80)
            age_band_predict[i] = 8;
        else
            age_band_predict[i] = 9;
    }
    print(age_band_predict);
}

parameters
{
    real alpha;
    real beta;
    real<lower=0> study_rnde_sd;
    vector[M] study_rnde;
    real<lower=0> age_rnde_sd;
    vector[A] age_rnde;
}

transformed parameters
{
    vector[N] logit_p_avg;
    logit_p_avg = alpha + age_rnde[age_band_obs] + beta * age_study + study_rnde[study];
}

model
{
    target += normal_lpdf( alpha | 0, 20);
    target += normal_lpdf( beta | 0, 1);
    target += exponential_lpdf( study_rnde_sd | 10);
    target += normal_lpdf( study_rnde | 0, study_rnde_sd);
    target += exponential_lpdf( age_rnde_sd | 10);
    target += normal_lpdf( age_rnde | 0, age_rnde_sd);
    
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
    
    p_predict = inv_logit( alpha + age_rnde[age_band_predict] + beta * age_predict );
    pred_deaths = binomial_rng(rep_array(10000000, N_predict), p_predict);
}
