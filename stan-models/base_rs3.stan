functions {
  // this is the partial sum function which calculates for
  // a subset of countries the log-lik contribution
  // so it computes for the countries m= start...end the
  // log lik
  // note: in the 2.23 release candidate the partial sum function has
  // this signature below, but that is changed to
  //real country_log_lik(real[] mu_slice,
  //                     int start, int end,
  // in the final 2.23... so adapt it next week
  real country_log_lik(int start, int end,
                       real[] mu_slice,
                       vector alpha,
                       real[] y,
                       real ophi,
                       real[] ifr_noise,
                       int[] N,
                       int N0,
                       int N2,
                       /*
                       matrix covariate1,
                       matrix covariate2,
                       matrix covariate3,
                       matrix covariate4,
                       matrix covariate5,
                       matrix covariate6,
                       */
                       matrix[] X,
                       vector SI_rev,
                       real[] pop,
                       vector[] f_rev,
                       int[,] deaths,
                       int[] EpidemicStart
                       ) {
    // log-lik of this subset
    real log_lik = 0.0;
    // number of countries
    int M_slice = end - start + 1;
    // terms of this country subset
    matrix[N2, M_slice] prediction = rep_matrix(0,N2,M_slice);
    matrix[N2, M_slice] E_deaths  = rep_matrix(0,N2,M_slice);
    matrix[N2, M_slice] Rt = rep_matrix(0,N2,M_slice);
    matrix[N2, M_slice] Rt_adj = Rt;
    matrix[N2,M_slice] cumm_sum = rep_matrix(0,N2,M_slice);

    // the index m runs with the actual index of each country in the
    // original data set while m_slice is offset such that it runs
    // from 1...M_slice (so a consecutive indexing of the subset
    // starting from 1).

    for (m in start:end) {
      int m_slice = m - start + 1;
      //for (i in 2:N0){
      //  cumm_sum[i,m_slice] = cumm_sum[i-1,m_slice] + y[m];
      //}
      prediction[1:N0,m_slice] = rep_vector(y[m], N0); // learn the
                                                       // number of
                                                       // cases in the
                                                       // first N0
                                                       // days
      cumm_sum[2:N0,m_slice] = cumulative_sum(prediction[2:N0,m_slice]);

      /*
      Rt[,m_slice] = mu_slice[m_slice] * exp( covariate1[,m] * (-alpha[1]) + covariate2[,m] * (-alpha[2]) +
                                              covariate3[,m] * (-alpha[3]) + covariate4[,m] * (-alpha[4]) + covariate5[,m] * (-alpha[5]) +
                                              covariate6[,m] * (-alpha[6]) );
      */

      Rt[,m_slice] = mu_slice[m_slice] * exp( - X[m] * alpha );
      Rt_adj[1:N0,m_slice] = Rt[1:N0,m_slice];

      for (i in (N0+1):N2) {
        /*
        real convolution=0;
        for(j in 1:(i-1)) {
          convolution += prediction[j, m_slice] * SI[i-j];
        }
        */
        real convolution = dot_product(sub_col(prediction, 1, m_slice, i-1), tail(SI_rev, i-1));

        cumm_sum[i,m_slice] = cumm_sum[i-1,m_slice] + prediction[i-1,m_slice];
        Rt_adj[i,m_slice] = ((pop[m]-cumm_sum[i,m_slice]) / pop[m]) * Rt[i,m_slice];
        prediction[i, m_slice] = Rt_adj[i,m_slice] * convolution;
      }

      /*
      E_deaths[1, m_slice]= 1e-15 * prediction[1,m_slice];
      for (i in 2:N2){
        for(j in 1:(i-1)){
          E_deaths[i,m_slice] += prediction[j,m_slice] * f[i-j,m] * ifr_noise[m];
        }
      }
      */
      E_deaths[1, m_slice]= 1e-15 * prediction[1,m_slice];
      for (i in 2:N2) {
        E_deaths[i,m_slice] = ifr_noise[m] * dot_product(sub_col(prediction, 1, m_slice, i), tail(f_rev[m], i));
        //for(j in 1:(i-1)){
        //  E_deaths[i,m_slice] += prediction[j,m_slice] * f[i-j,m] * ifr_noise[m];
        //}
      }
    }

    for(m in start:end) {
      int m_slice = m - start + 1;
      log_lik += neg_binomial_2_lpmf(deaths[EpidemicStart[m]:N[m], m]| E_deaths[EpidemicStart[m]:N[m], m_slice], ophi );
    }

    return(log_lik);
  }
}
data {
  int <lower=1> M; // number of countries
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int cases[N2,M]; // reported cases
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s
  matrix[N2, M] covariate1;
  matrix[N2, M] covariate2;
  matrix[N2, M] covariate3;
  matrix[N2, M] covariate4;
  matrix[N2, M] covariate5;
  matrix[N2, M] covariate6;
  int EpidemicStart[M];
  real pop[M];
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}
transformed data {
  vector[N2] f_rev[M]; // reversed order
  matrix[N2, 6] X[M];    // design matrix per country
  vector[N2] SI_rev;
  for(m in 1:M) {
    for(i in 1:N2) {
      f_rev[m,i] = f[N2-i+1,m];
    }

    X[m,:,1] = covariate1[:,m];
    X[m,:,2] = covariate2[:,m];
    X[m,:,3] = covariate3[:,m];
    X[m,:,4] = covariate4[:,m];
    X[m,:,5] = covariate5[:,m];
    X[m,:,6] = covariate6[:,m];
  }

  for(i in 1:N2)
    SI_rev[i] = SI[N2-i+1];
}

parameters {
  real<lower=0> mu[M]; // intercept for Rt
  real<lower=0> alpha_hier[6]; // sudo parameter for the hier term for alpha
  real<lower=0> kappa;
  real<lower=0> y[M];
  real<lower=0> ophi;
  real<lower=0> tau;
  real<lower=0> ifr_noise[M];
}

transformed parameters {
  vector[6] alpha;

  for(i in 1:6){
    alpha[i] = alpha_hier[i] - ( log(1.05) / 6.0 );
  }
}
model {
  tau ~ exponential(0.03);
  for (m in 1:M){
      y[m] ~ exponential(1/tau);
  }
  ophi ~ normal(0,5);
  kappa ~ normal(0,0.5);
  mu ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  alpha_hier ~ gamma(.1667,1);
  ifr_noise ~ normal(1,0.1);
  /*
  for(m in 1:M){
    deaths[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[EpidemicStart[m]:N[m], m], phi);
   }
  */
  target += reduce_sum(country_log_lik, mu, 1,
                       alpha,
                       y,
                       ophi,
                       ifr_noise,
                       N,
                       N0,
                       N2,
                       /*
                       covariate1,
                       covariate2,
                       covariate3,
                       covariate4,
                       covariate5,
                       covariate6,
                       */
                       X,
                       SI_rev,
                       pop,
                       f_rev,
                       deaths,
                       EpidemicStart
                       );
}
generated quantities {
    matrix[N2, M] prediction0 = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths0  = rep_matrix(0,N2,M);

    {
      matrix[N2,M] cumm_sum0 = rep_matrix(0,N2,M);
      for (m in 1:M){
         for (i in 2:N0){
          cumm_sum0[i,m] = cumm_sum0[i-1,m] + y[m];
        }
        prediction0[1:N0,m] = rep_vector(y[m],N0);
        for (i in (N0+1):N2) {
          real convolution0 = 0;
          for(j in 1:(i-1)) {
            convolution0 += prediction0[j, m] * SI[i-j];
          }
          cumm_sum0[i,m] = cumm_sum0[i-1,m] + prediction0[i-1,m];
          prediction0[i, m] =  ((pop[m]-cumm_sum0[i,m]) / pop[m]) * mu[m] * convolution0;
        }

        E_deaths0[1, m] = uniform_rng(1e-16, 1e-15);
        for (i in 2:N2){
          for(j in 1:(i-1)){
            E_deaths0[i,m] += prediction0[j,m] * f[i-j,m] * ifr_noise[m];
          }
        }
      }
    }
}
