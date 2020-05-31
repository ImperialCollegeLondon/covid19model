data {
  int <lower=1> M; // number of countries
  int <lower=1> P; // number of covariates for full pooling (global effects)
  int <lower=1> P_partial_regional; // number of covariates for partial pooling (region-level effects)
  int <lower=1> P_partial_state; // number of covariates for partial pooling (state-level effects)
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // ifr
  matrix[N2, P] X[M];
  matrix[N2, P_partial_regional] X_partial_regional[M];
  matrix[N2, P_partial_state] X_partial_state[M];
  int EpidemicStart[M];
  real pop[M];
  int W; // number of weeks for weekly effects
  int Q; // no.of regions
  int Region[M]; // Macro region index for each state
  int week_index[M,N2];
  real SI[N2]; // fixed SI using empirical data
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
  real<lower=0> mu[M]; 
  vector[P] alpha; 
  vector[P_partial_regional] alpha_region[Q];
  vector[P_partial_state] alpha_state[M];
  real<lower=0> gamma_region;
  real<lower=0> gamma_state;
  real<lower=0> kappa;
  real<lower=0> y[M];
  real<lower=0> phi;
  real<lower=0> tau;
  real <lower=0> ifr_noise[M];
  matrix[W+1,M] weekly_effect;
  real<lower=0, upper=1> weekly_rho;
  real<lower=0, upper=1> weekly_rho1;
  real<lower=0> weekly_sd;
}

transformed parameters {
    matrix[N2, M] prediction = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths  = rep_matrix(0,N2,M);
    matrix[N2, M] Rt = rep_matrix(0,N2,M);
    matrix[N2, M] Rt_adj = Rt;
    matrix[N2, M] infectiousness = rep_matrix(0,N2,M);
    
    {
      matrix[N2,M] cumm_sum = rep_matrix(0,N2,M);
      for (m in 1:M){
        prediction[1:N0,m] = rep_vector(y[m],N0); // learn the number of cases in the first N0 days
        cumm_sum[2:N0,m] = cumulative_sum(prediction[2:N0,m]);
        
        Rt[,m] = mu[m] * 2 * inv_logit(-X[m] * alpha 
                          -X_partial_regional[m] * alpha_region[Region[m]] 
                          -X_partial_state[m] * alpha_state[m] 
                          -weekly_effect[week_index[m],m]);
        Rt_adj[1:N0,m] = Rt[1:N0,m];
         for (i in 2:N0){
          real convolution = 0;
          for (j in 1:(i-1)){
             convolution += prediction[j,m] * SI[i-j] / max(SI);
          }
          infectiousness[i,m] = convolution;
        }
        for (i in (N0+1):N2) {
          real convolution = dot_product(sub_col(prediction, 1, m, i-1), tail(SI_rev, i-1));
          
          cumm_sum[i,m] = cumm_sum[i-1,m] + prediction[i-1,m];
          Rt_adj[i,m] = ((pop[m]-cumm_sum[i,m]) / pop[m]) * Rt[i,m];
          prediction[i, m] = prediction[i, m] + Rt_adj[i,m] * convolution;
          infectiousness[i,m] = convolution / max(SI);
        }
        E_deaths[1, m]= 1e-15 * prediction[1,m];
        for (i in 2:N2){
          E_deaths[i,m] = ifr_noise[m] * dot_product(sub_col(prediction, 1, m, i-1), tail(f_rev[m], i-1));
        }
      }
    }
}
model {
  tau ~ exponential(0.03);
  gamma_region ~ normal(0,.5);
  gamma_state ~ normal(0,.5);
  weekly_sd ~ normal(0,0.2);
  weekly_rho ~ normal(0.8, 0.05);
  weekly_rho1 ~ normal(0.1, 0.05);
  for (m in 1:M) {
      y[m] ~ exponential(1/tau);
       weekly_effect[3:(W+1), m] ~ normal( weekly_effect[2:W,m]* weekly_rho + weekly_effect[1:(W-1),m]* weekly_rho1, 
                                            weekly_sd *sqrt(1-pow(weekly_rho,2)-pow(weekly_rho1,2) - 2 * pow(weekly_rho,2) * weekly_rho1/(1-weekly_rho1)));
  }
  weekly_effect[2, ] ~ normal(0,weekly_sd *sqrt(1-pow(weekly_rho,2)-pow(weekly_rho1,2) - 2 * pow(weekly_rho,2) * weekly_rho1/(1-weekly_rho1)));
  weekly_effect[1, ] ~ normal(0, 0.01);
  for (q in 1:Q){
     alpha_region[q] ~ normal(0,gamma_region);
  }
  for (q in 1:M){
     alpha_state[q] ~ normal(0,gamma_state);
  }
  phi ~ normal(0,5);
  kappa ~ normal(0,0.5);
  mu ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  alpha ~ normal(0,0.5);
  ifr_noise ~ normal(1,0.1);
  for(m in 1:M){
    deaths[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[EpidemicStart[m]:N[m], m], phi);
   }
}
