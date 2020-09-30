functions {
    
    real binomial_logit_lpmf_marginalise(
      int[] deaths_slice,
      int start,
      int end,
      //pars
      vector p_logit_avg,
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
      lpmf += log_sum_exp(  log_choose_fct_local + rep_vector(deaths_slice[n], casesN_local) .* log( inv_logit(rep_vector(p_logit_avg[n], casesN_local ))) + 
              (auxCases_local-rep_vector(deaths_slice[n], casesN_local)) .* log (1 - inv_logit(rep_vector(p_logit_avg[n], casesN_local)) ));
      
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
  real<lower=0> age[N_predict]; //  age
  int lower_age_idx[N]; // lower index 
  int upper_age_idx[N]; // lower index 
  int study[N]; 
  int max_casesN;
  matrix[max_casesN, N] log_choose_fct;
  matrix[max_casesN, N] auxCases;
}

transformed data {
  real delta = 1e-9;
}


parameters {
  real beta;
  real gamma[M];
  real<lower=0> rho;
  real<lower=0> alpha;
  vector[N_predict] eta_f;
}

transformed parameters
{
    vector[N_predict] p;
    vector[N] p_avg;
    vector[N] p_logit_avg;
    vector[N_predict] f;
  
    {
        matrix[N_predict, N_predict] L_K;
        matrix[N_predict, N_predict] K = cov_exp_quad(age, alpha, rho);

        // diagonal elements
        for (n in 1:N_predict)
            K[n, n] = K[n, n] + delta;

        L_K = cholesky_decompose(K);
        f = L_K * eta_f;
    }
  
   p = inv_logit( beta + f );
   
    for (i in 1:N)
    {
      p_avg[i] = 0;
      for(j in lower_age_idx[i] : upper_age_idx[i])
      {
        p_avg[i] += p[j];
      }
      p_avg[i] /= (upper_age_idx[i]-lower_age_idx[i]+1 );
      p_logit_avg[i] = logit(p_avg[i]) + gamma[study[i]];
    }
}

model
{
  target += normal_lpdf( beta | 0, 10);
  target += normal_lpdf( gamma | 0, 1);
  target += inv_gamma_lpdf( rho | 5, 5);
  target += normal_lpdf(alpha | 0, 2);
  target += normal_lpdf( eta_f | 0, 1);
    
  // rstan version
  //target += binomial_logit_lpmf_marginalise(deaths, 1, N,
  // cmdstan version
  target += reduce_sum(binomial_logit_lpmf_marginalise, deaths, 1,
       p_logit_avg,
       casesL,
       casesU,
       log_choose_fct,
       auxCases
       );
}


