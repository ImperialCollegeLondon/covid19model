functions {
    
    real binomial_logit_lpmf_marginalise(
      int[] deaths_slice,
      int start,
      int end,
      //pars
      vector p_log_avg,
      // data
      int[] casesL,
      int[] casesU,
      matrix log_choose_fct,
      matrix auxCases
      )
    {
      real lpmf = 0.0;
      int N_slice = end - start + 1;
      
    for(n_slice in 1:N_slice) {
      int n = n_slice + start - 1;
      int casesN_local = casesU[n]-casesL[n]+1;
      vector[casesN_local] log_choose_fct_local = log_choose_fct[1:(casesU[n]-casesL[n]+1), n];
      vector[casesN_local] auxCases_local = auxCases[1:(casesU[n]-casesL[n]+1), n];
      lpmf += log_sum_exp(  log_choose_fct_local + rep_vector(deaths_slice[n], casesN_local) .* log( exp(rep_vector(p_log_avg[n], casesN_local ))) + 
              (auxCases_local-rep_vector(deaths_slice[n], casesN_local)) .* log (1 - exp(rep_vector(p_log_avg[n], casesN_local)) ));
      
          }
  
     return( lpmf );

    }

}

data {
  int<lower=0> N; // number of observations
  int<lower=0> N_predict; // number of predictions
  int<lower=0> M; // number of studies
  int deaths[N]; // number of deaths
  int casesU[N]; //  upper bound cases
  int casesL[N]; // lower bound cases
  vector<lower=0>[N_predict] age; //  age
  int lower_age_idx[N]; // lower index 
  int upper_age_idx[N]; // lower index 
  int study[N]; 
  int max_casesN;
  matrix[max_casesN, N] log_choose_fct;
  matrix[max_casesN, N] auxCases;
}

parameters {
  real alpha;
  real beta;
  real gamma[M];
  real<lower=0> tau;
}

transformed parameters
{
    vector[N_predict] p;
    vector[N] p_avg;
    vector[N] p_log_avg;

   p = exp( alpha + beta*age );
   
    for (i in 1:N)
    {
      p_avg[i] = 0;
      for(j in lower_age_idx[i] : upper_age_idx[i])
      {
        p_avg[i] += p[j];
      }
      p_avg[i] /= (upper_age_idx[i]-lower_age_idx[i]+1 );
      p_log_avg[i] = log(p_avg[i]) + gamma[study[i]];
    }

}

model
{
  target += normal_lpdf( alpha | 0, 0.5);
  target += normal_lpdf( beta | 0, 0.5);
  target += normal_lpdf( gamma | 0, tau);
  target += cauchy_lpdf( tau | 0, 1);
    
  // rstan version
  target += binomial_logit_lpmf_marginalise(deaths, 1, N,
  // cmdstan version
  //target += reduce_sum(binomial_logit_lpmf_marginalise, deaths, 1,
       p_log_avg,
       casesL,
       casesU,
       log_choose_fct,
       auxCases
       );
}



