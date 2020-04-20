functions {
  // calculates for a given country the model outputs as column
  // vectors. These are saved as an array with the outputs of (1)
  // E_deaths, (2) prediction, (3) Rt, (4) R_adj
  vector[] country_model(real mu_local,
                         vector alpha,
                         real y_local,
                         real ifr_noise_local,
                         int N0,
                         int N2,
                         matrix X_local,
                         vector SI_rev,
                         real pop_local,
                         vector f_rev_local
                         ) {
    vector[N2] prediction = rep_vector(0.0, N2);
    vector[N2] E_deaths = rep_vector(0.0, N2);
    vector[N2] Rt = rep_vector(0.0, N2);
    vector[N2] Rt_adj = rep_vector(0.0, N2);
    vector[N2] cumm_sum = rep_vector(0.0, N2);
    
    // learn the number of cases in the first N0 days
    prediction[1:N0] = rep_vector(y_local, N0); 
    cumm_sum[2:N0] = cumulative_sum(prediction[2:N0]);
    
    Rt = mu_local * exp( - X_local * alpha );
    Rt_adj[1:N0] = Rt[1:N0];
    
    for (i in (N0+1):N2) {
      real convolution = dot_product(head(prediction, i-1), tail(SI_rev, i-1));
      
      cumm_sum[i] = cumm_sum[i-1] + prediction[i-1];
      Rt_adj[i] = ((pop_local-cumm_sum[i]) / pop_local) * Rt[i];
      prediction[i] = Rt_adj[i] * convolution;
    }

    E_deaths[1]= 1e-15 * prediction[1];
    for (i in 2:N2) {
      E_deaths[i] = ifr_noise_local * dot_product(head(prediction, i-1), tail(f_rev_local, i-1));
    }

    return({ E_deaths, prediction, Rt, Rt_adj });
}
  
  // this is the partial sum function which calculates for
  // a subset of countries the log-lik contribution
  // so it computes for the countries m= start...end the
  // log lik
  real country_lpdf(real[] mu_slice,
                    int start, int end,
                    vector alpha,
                    real[] y,
                    real phi,
                    real[] ifr_noise,
                    int[] N,
                    int N0,
                    int N2,
                    matrix[] X,
                    vector SI_rev,
                    real[] pop,
                    vector[] f_rev,
                    int[,] deaths,
                    int[] EpidemicStart
                    ) {
    // log-lik of this subset
    real log_lik = 0.0;
    
    for (m in start:end) {
      int m_slice = m - start + 1;
      vector[N2] E_deaths = country_model(
          mu_slice[m_slice],
          alpha,
          y[m],
          ifr_noise[m],
          N0,
          N2,
          X[m],
          SI_rev,
          pop[m],
          f_rev[m])[1];
      
      log_lik += neg_binomial_2_lpmf(deaths[EpidemicStart[m]:N[m], m] |
                                     E_deaths[EpidemicStart[m]:N[m]], phi );
    }

    return(log_lik);
  }
}

data {
  int <lower=1> M; // number of countries
  int <lower=1> P; // number of covariates
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int cases[N2,M]; // reported cases
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s
  matrix[N2, P] X[M];
  int EpidemicStart[M];
  real pop[M];
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

transformed data {
  vector[N2] SI_rev; // SI in reverse order
  vector[N2] f_rev[M]; // f in reversed order
  
  for(i in 1:N2)
    SI_rev[i] = SI[N2-i+1];
    
  for(m in 1:M){
    for(i in 1:N2) {
     f_rev[m, i] = f[N2-i+1,m];
    }
  }
}


parameters {
  real<lower=0> mu[M]; // intercept for Rt
  real<lower=0> alpha_hier[P]; // sudo parameter for the hier term for alpha
  real<lower=0> kappa;
  real<lower=0> y[M];
  real<lower=0> phi;
  real<lower=0> tau;
  real <lower=0> ifr_noise[M];
}

transformed parameters {
    vector[P] alpha;
    for(i in 1:P){
      alpha[i] = alpha_hier[i] - ( log(1.05) / 6.0 );
    }
}

model {
  tau ~ exponential(0.03);
  for (m in 1:M){
      y[m] ~ exponential(1/tau);
  }
  phi ~ normal(0,5);
  kappa ~ normal(0,0.5);
  mu ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  alpha_hier ~ gamma(.1667,1);
  ifr_noise ~ normal(1,0.1);
  
  // reduce_sum requires CmdStan >= 2.23 and samples parallel when
  // STAN_THREADS=true is set in make/local
  //
  // the reduce_sum_static variant will always calculate the partial
  // sums in the same way resulting in run-to-run exatly the same
  // results whereas reduce_sum will dynamically according to system
  // load perform calculations such that there can be run-to-run
  // numerical differences.
  
  /*
  target += reduce_sum_static(
      country_lpdf, mu, 2,
      alpha,
      y,
      ophi,
      ifr_noise,
      N,
      N0,
      N2,
      X,
      SI_rev,
      pop,
      f_rev,
      deaths,
      EpidemicStart
                              );
  */
  
  // this version runs with RStan. 
  // Comment out if reduce_sum_static() is used above.
  target += country_lpdf(
      mu | 1, M,
      alpha,
      y,
      phi,
      ifr_noise,
      N,
      N0,
      N2,
      X,
      SI_rev,
      pop,
      f_rev,
      deaths,
      EpidemicStart);
}

generated quantities {
  matrix[N2, M] E_deaths;
  matrix[N2, M] prediction;
  matrix[N2, M] Rt;
  matrix[N2, M] Rt_adj;
  matrix[N2, M] E_deaths0;
  matrix[N2, M] prediction0;

  for(m in 1:M) {
    vector[N2] local[4]
        = country_model(
            mu[m],
            alpha,
            y[m],
            ifr_noise[m],
            N0,
            N2,
            X[m],
            SI_rev,
            pop[m],
            f_rev[m]);
    vector[N2] local0[4]
        = country_model(
            mu[m],
            alpha,
            y[m],
            ifr_noise[m],
            N0,
            N2,
            X[m],
            SI_rev,
            pop[m],
            f_rev[m]);

    E_deaths[:,m] = local[1];
    prediction[:,m] = local[2];
    Rt[:,m] = local[3];
    Rt_adj[:,m] = local[4];
    
    E_deaths0[:,m] = local0[1];
    prediction0[:,m] = local0[2];
  }
}

