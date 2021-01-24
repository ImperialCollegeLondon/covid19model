data {
  int<lower=1> M; // number of countries
  int<lower=1> N0; // number of initial days for which to estimate infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
    int<lower=1> A; // number of age bands
  int<lower=1> SI_CUT; // number of days in serial interval to consider
  //	data
  real pop[M];
  matrix<lower=0, upper=1>[A,M] popByAge; // proportion of age bracket in population in location
  int epidemicStart[M];
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] covariate1;
  matrix[N2, M] covariate2;
  matrix[N2, M] covariate3;
  matrix[N2, M] covariate4;
  matrix[N2, M] covariate5;
  matrix[N2, M] covariate6;
  //	priors	
  matrix[A,A] cntct_mean[M]; // mean of prior contact rates between age groups
  real<lower=0> ifr_country_scale[M]; // relative probability of death for location, s days after infection, for age band a
  matrix<lower=0>[N2,A] ifr_age; // probability of death for age band a, stored N2 times
  row_vector[N2] rev_ifr_daysSinceInfection; // probability of death s days after infection in reverse order
  row_vector[SI_CUT] rev_serial_interval; // fixed pre-calculated serial interval using empirical data from Neil in reverse order
  int<lower=1, upper=A> init_A; // age band in which initial cases occur in the first N0 days
}

transformed data{
  vector<lower=0>[M] avg_cntct;
  vector[A] ones_vector_A;
  row_vector[A] ones_row_vector_A;
  ones_vector_A= rep_vector(1.,A);
  ones_row_vector_A= rep_row_vector(1.,A);
  
  for( m in 1:M )
  {
    avg_cntct[m] = popByAge[:,m]' * ( cntct_mean[m] * ones_vector_A );
  }
}

parameters {
  vector<lower=0>[M] R0; // R0
  //real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> alpha_hier[6]; // sudo parameter for the Gamma term in alpha
  real alpha_state[M];
  real<lower=0> ifr_noise[M];
  real<lower=0> kappa;
}

transformed parameters {
  // dummy variables
  // real<lower=0> saturation_adj;
  
  // regression coefficients for interventions
  real alpha[6];
  
  // probability of infection given contact in location m
  vector<lower=0>[M] rho0;
  
  // expected deaths by calendar day (1st dim) age (2nd dim) and country (3rd dim), under self-renewal model
  // and expected deaths by calendar day (rows) and country (cols), under self-renewal model
  matrix<lower=0>[N2,A] E_deathsByAge[M];
  matrix<lower=0>[N2,M] E_deaths= rep_matrix(0., N2, M);
  
  // expected new cases by calendar day, age, and location under self-renewal model
  // and a container to store the precomputed cases by age
  matrix<lower=0>[N2,A] E_casesByAge[M];
  row_vector<lower=0>[A] tmp_row_vector_A;
  
  // scaling of contacts after intervention effect on day t in location m
  matrix<lower=0>[N2, M] impact_intv;

  //  define hierarchical alpha
  for(i in 1:6)
  {
    alpha[i] = alpha_hier[i] - ( log(1.05) / 6.0 );
  }
  
  // define probability of infection given contact in location m
  rho0 = R0 ./  avg_cntct;
  
  // define contacts after intervention effect in each location
  // TODO we here keep the original assumption that all interventions act on all age contacts in the same way
  for (m in 1:M)
  {
    impact_intv[,m] = exp(covariate1[,m] * (-alpha[1]) + covariate2[,m] * (-alpha[2]) + covariate3[,m] * (-alpha[3]) + covariate4[,m] * (-alpha[4]) + covariate5[,m] * (-alpha[5]) + covariate6[,m] * (-alpha[6]) );
    impact_intv[,m] *= exp(alpha_state[m]);
  }
  
  // init expected cases by age and location
  // init expected cases by age and location in first N0 days
  for (m in 1:M)
  {
    E_casesByAge[m] = rep_matrix( 0., N2, A );
    E_casesByAge[m][1:N0,init_A] = rep_vector( e_cases_N0[m], N0 );
  }
  
  // calculate expected cases by age and country under self-renewal model after first N0 days
  // and adjusted for saturation
    for (m in 1:M)
  {
    for (t in (N0+1):N2)
    {
      tmp_row_vector_A = rev_serial_interval[max(1,(SI_CUT-t+2)):SI_CUT] * E_casesByAge[m][max(1,(t-SI_CUT)):(t-1),:];
      E_casesByAge[m][t,:] = tmp_row_vector_A * cntct_mean[m];
      E_casesByAge[m][t,:] .*= rep_row_vector( impact_intv[t,m] * rho0[m], A);
    }
  }
  
  // calculate expected deaths by age and country  
  for (m in 1:M)
  {
    E_deathsByAge[m] = rep_matrix( 0., N2, A );
    E_deathsByAge[m][1,:] = 1e-15 * E_casesByAge[m][1,:];
    for (t in 2:N2)
    {
      E_deathsByAge[m][t,:] = rev_ifr_daysSinceInfection[(N2-(t-1)+1):N2 ] * E_casesByAge[m][1:(t-1),:];
    }
    E_deathsByAge[m] .*= ifr_age;
    E_deathsByAge[m] *= (ifr_country_scale[m] * ifr_noise[m]);
  }
  
  // calculate expected deaths by country
  for (m in 1:M)
  {
    E_deaths[:,m] = E_deathsByAge[m] * ones_vector_A;
  }
}

model {
  // priors
  tau ~ exponential(0.03);
  e_cases_N0 ~ exponential(1/tau);
  phi ~ normal(0,5);
  alpha_hier ~ gamma(.1667,1);
  ifr_noise ~ normal(1,0.1);
  kappa ~ normal(0,0.5);
  R0 ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  for (m in 1:M){
    alpha_state[m] ~ normal(0,0.125);
  }
  // likelihood
  for(m in 1:M){
    deaths[epidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[epidemicStart[m]:N[m], m], phi);
  }
}

generated quantities {
  matrix<lower=0>[N2,M] Rt; // Rt for each location
  matrix<lower=0>[N2,A] RtByAge[M]; // Rt for each age band and each location
  for( m in 1:M )
  {
    RtByAge[m] = rep_matrix( ones_row_vector_A*cntct_mean[m] , N2);
    RtByAge[m] .*= rep_matrix( impact_intv[:,m], A);
    RtByAge[m] *= rho0[m];
    Rt[:,m] = RtByAge[m] * popByAge[:,m];
  }
}
