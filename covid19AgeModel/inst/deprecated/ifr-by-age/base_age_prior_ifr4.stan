functions {

    vector binomial_logit_lpmf_array(
      //pars
      vector alpha,
      // data
      vector n,
      vector N,
      vector log_choose_fct_local,
      int casesN_local
    )
    {
      vector[casesN_local] loglik = log_choose_fct_local + n .* log( inv_logit( alpha )) + (N-n) .* log (1 - inv_logit(alpha));
      return(loglik);
    }
    
    real binomial_logit_lpmf_marginalise_totals(//pars
      real logit_p,
      // data
      int deaths_local,
      int casesL_local,
      int casesN_local,
      vector log_choose_fct_local,
      vector auxCases_local
      )
    {
     return( log_sum_exp(  log_choose_fct_local + rep_vector(deaths_local, casesN_local) .* log( inv_logit( rep_vector(logit_p, casesN_local) )) + (auxCases_local-rep_vector(deaths_local, casesN_local)) .* log (1 - inv_logit(rep_vector(logit_p, casesN_local))) ) );

    }
     


}

data {
  int<lower=0> N; // number of observations
  //int<lower=0> N2; // number of predictions
  int<lower=0> M; // number of studies
  int deaths[N]; // number of deaths
  int casesU[N]; //  upper bound cases
  int casesL[N]; // lower bound cases
  real<lower=0> age_study[N]; //  age
  //real<lower=0> age_predict[N2]; //  age
  int study[N]; 
  int max_casesN;
  matrix[max_casesN, N] log_choose_fct;
  matrix[max_casesN, N] auxCases;
}

transformed data {
  real delta = 1e-9;
  // int<lower=1> N_all = N + N2;
  // real age[N_all];
  // for (n1 in 1:N) age[n1] = age_study[n1];
  // for (n2 in 1:N2) age[N + n2] = age_predict[n2];
}


parameters {
  real beta;
  real gamma[M];
  real<lower=0> rho;
  real<lower=0> alpha;
  vector[N] eta_f;
}

transformed parameters
{
    vector[N] logit_p;
    vector[N] f;
  
    {
        matrix[N, N] L_K;
        matrix[N, N] K = cov_exp_quad(age_study, alpha, rho);

        // diagonal elements
        for (n in 1:N)
            K[n, n] = K[n, n] + delta;

        L_K = cholesky_decompose(K);
        f = L_K * eta_f;
    }
  
    for (i in 1:N)
    {
        logit_p[i] = beta + gamma[study[i]] + f[i];
    }
}

model
{
    target += normal_lpdf( beta | 0, 10);
    target += std_normal_lpdf( gamma );
    target += inv_gamma_lpdf( rho | 5, 5);
    target += cauchy_lpdf( alpha | 0, 1);
    target += std_normal_lpdf( eta_f );
      
    for(i in 1:N)
    {
        target +=
            binomial_logit_lpmf_marginalise_totals(
                logit_p[i],
                deaths[i],
                casesL[i],
                casesU[i]-casesL[i]+1,
                log_choose_fct[1:(casesU[i]-casesL[i]+1), i],
                auxCases[1:(casesU[i]-casesL[i]+1), i]
                );
    }
}

// generated quantities {
//     real deaths_hat[N];
//     real y_predict[N2];
//     

//     deaths_hat ~ binomial_rng(cases, p);
//     
//     for (n2 in 1:N2){
//         y_predict[n2] = normal_rng(beta[1] + beta_age * age_predict[n2] + f[N + n2], exp(beta[2] + g[N + n2])) ;
//     }
// 
// }



