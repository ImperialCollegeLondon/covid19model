data {
  int <lower=1> M; // number of countries
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  real<lower=0> x[N2]; // index of days (starting at 1)
  int cases[N2,M]; // reported cases
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s for deaths
  matrix[N2, M] covariate1;
  matrix[N2, M] covariate2;
  matrix[N2, M] covariate3;
  matrix[N2, M] covariate4;
  matrix[N2, M] covariate5;
  matrix[N2, M] covariate6;
  real SI[N2]; // fixed pre-calculated SI for cases
}

transformed data {
  real delta = 1e-5;
}

parameters {
  real<lower=0> mu[M]; // intercept for Rt
  real<lower=0> alpha[6]; // the hier term
  real<lower=0> kappa; // the variance in intercept
  real<lower=0> y[M]; // intial cases to be infered
  real<lower=0> phi; // dispression parameter for neg binomial
}

transformed parameters {
    real convolution;
    matrix[N2, M] prediction = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths  = rep_matrix(0,N2,M);
    matrix[N2, M] Rt = rep_matrix(0,N2,M);
    for (m in 1:M){
      prediction[1:N0,m] = rep_vector(y[m],N0); // learn the number of cases in the first N0 days
        Rt[,m] = mu[m] * exp(covariate1[,m] * (-alpha[1]) + covariate2[,m] * (-alpha[2]) +
        covariate3[,m] * (-alpha[3])+ covariate4[,m] * (-alpha[4]) + covariate5[,m] * (-alpha[5]) + 
        covariate6[,m] * (-alpha[6])); // Rt is intercept * covariates effects
      for (i in (N0+1):N2) {
        convolution=0;
        for(j in 1:(i-1)) {
          convolution += prediction[j, m]*SI[i-j]; // Renewal equation recursive sum
        }
        prediction[i, m] = Rt[i,m] * convolution; // renewal equation
      }
      
      E_deaths[1, m]= 1e-9;
      for (i in 2:N2){
        E_deaths[i,m]= 0;
        for(j in 1:(i-1)){
          E_deaths[i,m] += prediction[j,m]*f[i-j,m]; // relationship  that ties cases and deaths via hazard and survival function
        }
      }
    }

}
model {
  for (m in 1:M){
      y[m] ~ exponential(.2);
  }
  phi ~ normal(0,5);
  kappa ~ normal(0,1);
  mu ~ normal(2.34,kappa); // citation needed 
  alpha ~ gamma(.5,1);
  for(m in 1:M){
    for(i in 1:N[m]){
       deaths[i,m] ~ neg_binomial_2(E_deaths[i,m],phi); 
    }
   }
}

generated quantities {
  // using this block for counterfactual
    real convolution0;
    matrix[N2, M] prediction0 = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths0  = rep_matrix(0,N2,M);
    for (m in 1:M){
      prediction0[1:N0,m] = rep_vector(y[m],N0); 
      for (i in (N0+1):N2) {
        convolution0=0;
        for(j in 1:(i-1)) {
          convolution0 += prediction0[j, m]*SI[i-j];
        }
        prediction0[i, m] = mu[m] * convolution0;
      }
      E_deaths0[1, m]= 1e-9;
      for (i in 2:N2){
        E_deaths0[i,m]= 0;
        for(j in 1:(i-1)){
          E_deaths0[i,m] += prediction0[j,m]*f[i-j,m];
        }
      }
    }

}

