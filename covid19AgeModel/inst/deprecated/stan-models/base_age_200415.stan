data {
  int<lower=1> M; // number of countries
  int<lower=1> N0; // number of initial days for which to estimate infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
    int<lower=1> A; // number of age bands
  int<lower=1> SI_CUT; // number of days in serial interval to consider
  int<lower=1> IFR_CUT[M,A]; // number of days in ifr_daysSinceInfection interval to consider
  //	data
  real pop[M];
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
  real<lower=0> ifr_age[A]; // probability of death for age band a
  real<lower=0> ifr_daysSinceInfection[N2]; // probability of death s days after infection
  real serial_interval[SI_CUT]; // fixed pre-calculated serial interval using empirical data from Neil
  int<lower=1, upper=A> init_A; // age band in which initial cases occur in the first N0 days
}

transformed data{
  vector<lower=0>[M] avg_cntct;
  for( m in 1:M )
  {
    avg_cntct[m] = mean( cntct_mean[m] );
  }
}

parameters {
  vector<lower=0>[M] R0; // R0
  real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> alpha_hier[6]; // sudo parameter for the Gamma term in alpha
  real<lower=0> ifr_noise[M];
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
  matrix<lower=0>[N2,A] Prec_casesByAge;
  
  // scaling of contacts after intervention effect on day t in location m
  matrix<lower=0>[N2, M] cntct_intv;
  
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
    cntct_intv[,m] = exp( covariate1[,m] * (-alpha[1]) + covariate2[,m] * (-alpha[2]) + covariate3[,m] * (-alpha[3]) + covariate4[,m] * (-alpha[4]) + covariate5[,m] * (-alpha[5]) + covariate6[,m] * (-alpha[6]) );
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
    Prec_casesByAge = rep_matrix(0., N2, A);
    for ( t in 1:N0 )
    {
      for ( a in 1:A )
      {
        Prec_casesByAge[t,a] = sum( E_casesByAge[m][t,:] .* to_row_vector(cntct_mean[m][:,a]) );
      }
      Prec_casesByAge[t,:] *= cntct_intv[t,m];
    }
    
    for (t in (N0+1):N2)
    {
      for ( a in 1:A)
      {
        // saturation_adj= ( pop[m] - sum( E_casesByAge[m][1:(t-1),a] ) ) / pop[m]; // TODO we need age specific pop counts
        for (s in max(1,(t-SI_CUT)):(t-1))
        {
          E_casesByAge[m][t,a] += ( Prec_casesByAge[s,a] * serial_interval[t-s] );
        }
        // E_casesByAge[m][t,a] *= saturation_adj;
        Prec_casesByAge[t,a] = sum( E_casesByAge[m][t,:] .* to_row_vector(cntct_mean[m][:,a]) );
      }
      Prec_casesByAge[t,:] *= cntct_intv[t,m];
    }
    E_casesByAge[m] *= rho0[m];
  }
  
  
  // calculate expected deaths by age and country  
  for (m in 1:M)
  {
    E_deathsByAge[m] = rep_matrix( 0., N2, A );
    E_deathsByAge[m][1,:] = 1e-15 * E_casesByAge[m][1,:];
    for ( a in 1:A)
    {
      for (t in 2:N2)
      {
        for (s in max(1,(t-IFR_CUT[m,a])):(t-1))
        {
          E_deathsByAge[m][t,a] += ( E_casesByAge[m][s,a] * ifr_daysSinceInfection[t-s] );
        }
      }
      E_deathsByAge[m][:,a] *= ifr_age[a];
    }
    E_deathsByAge[m] *= (ifr_country_scale[m] * ifr_noise[m]);
  }
  
  // calculate expected deaths by country
  for (m in 1:M)
  {
    for (t in 1:N2)
    {
      E_deaths[t,m] = sum( E_deathsByAge[m][t,:] );
    }
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
  
  // likelihood
  for(m in 1:M){
    deaths[epidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[epidemicStart[m]:N[m], m], phi);
  }
}

generated quantities {
  // Rt for each age band and each location
  matrix<lower=0>[N2,A] Rt[M];
  for( m in 1:M )
  {
    for( a in 1:A )
    {
      Rt[m,:,a] = rep_vector( sum( cntct_mean[m][a,:] ), N2 );
    }
    for( t in 1:N2 )
    {
      Rt[m,t,:] *= cntct_intv[t,m];
    }
    Rt[m] *= rho0[m];
  }
}
