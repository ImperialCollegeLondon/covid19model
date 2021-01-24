functions {
  real country_lpmf(int[] deaths_local,
                    // parameters
                    real R0_local,
                    real e_cases_N0_local,
                    vector beta_res_wkend,
                    vector beta_res_wkday,
                    vector beta_nonres_wkday,
                    real phi,
                    real ifr_noise_local,
                    // data
                    int N0,
                    int N2,
                    int A,
                    int SI_CUT,
                    // wkend_idx[1:WKEND_IDX_N[m],m]
                    int[] wkend_idx_local,
                    real avg_cntct_local,
                    matrix covariates_res_local,
                    matrix covariates_nonres_local,
                    matrix cntct_weekends_mean_local,
                    matrix cntct_weekdays_mean_local,
                    row_vector rev_ifr_daysSinceInfection,
                    real ifr_country_scale_local,
                    matrix ifr_age,
                    row_vector rev_serial_interval,
                    int epidemicStart_local,
                    int N_local,
                    int init_A,
                    data vector ones_vector_A
                    ) {
    // dummy variables
    // real<lower=0> saturation_adj;

    int num_wkend_idx_local = size(wkend_idx_local);
    
    // probability of infection given contact in location m
    real rho0 = R0_local / avg_cntct_local;
    real log_rho0 = log(rho0);
    real zero = 0.0;
    
    // expected deaths by calendar day (1st dim) age (2nd dim) and country (3rd dim), under self-renewal model
    // and expected deaths by calendar day (rows) and country (cols), under self-renewal model
    //matrix<lower=0>[N2,A] E_deathsByAge[M];
    matrix[N2,A] E_deathsByAge = rep_matrix(zero, N2, A);
    vector[N2] E_deaths;
  
    // expected new cases by calendar day, age, and location under self-renewal model
    // and a container to store the precomputed cases by age
    matrix[N2,A] E_casesByAge = rep_matrix( zero, N2, A );
  
    // scaling of contacts after intervention effect on day t in location m
    vector[N2] impact_intv_nonres;
    vector[N2] impact_intv_res;

    // define multipliers for residential contacts in each location for both weekdays and weekends
    // define multipliers for non-residential contacts in each location for both weekdays and weekends
    // multiply the multipliers with rho0 in each location
    // TODO use
    // https://mc-stan.org/docs/2_18/stan-users-guide/QR-reparameterization-section.html
    /*
    impact_intv_res = exp( covariates_res_local * beta_res_wkday );
    impact_intv_nonres = exp( covariates_nonres_local * beta_nonres_wkday );
    impact_intv_res[ wkend_idx_local ] = exp( covariates_res_local[ wkend_idx_local, :] * beta_res_wkend );
    impact_intv_nonres[ wkend_idx_local ] = rep_vector(zero, num_wkend_idx_local );
    impact_intv_res *= rho0;
    impact_intv_nonres *= rho0;
    */

    impact_intv_res = exp( log_rho0 + covariates_res_local * beta_res_wkday );
    impact_intv_nonres = exp( log_rho0 + covariates_nonres_local * beta_nonres_wkday );
    impact_intv_res[ wkend_idx_local ] = exp( log_rho0 + covariates_res_local[ wkend_idx_local, :] * beta_res_wkend );
    impact_intv_nonres[ wkend_idx_local ] = rep_vector(zero, num_wkend_idx_local );

    // init expected cases by age and location in first N0 days
    E_casesByAge[1:N0,init_A] = rep_vector( e_cases_N0_local, N0 );
  
    // calculate expected cases by age and country under self-renewal model after first N0 days
    // and adjusted for saturation
    for (t in (N0+1):N2)
    {
      int start_idx_rev_serial = SI_CUT-t+2;
      int start_idx_E_casesByAge = t-SI_CUT;
      //tmp_row_vector_A = rev_serial_interval[max(1,(SI_CUT-t+2)):SI_CUT] * E_casesByAge[m][max(1,(t-SI_CUT)):(t-1),:];
      //E_casesByAge[m][t,:] = tmp_row_vector_A * (
      //impact_intv_res[t,m] * cntct_weekends_mean[m] +
      //impact_intv_nonres[t,m] * cntct_weekdays_mean[m] );
      if(start_idx_rev_serial < 1) {
        start_idx_rev_serial = 1;
      }
      if(start_idx_E_casesByAge < 1) {
        start_idx_E_casesByAge = 1;
      }
      {
        row_vector[A] tmp_row_vector_A = rev_serial_interval[start_idx_rev_serial:SI_CUT] * E_casesByAge[start_idx_E_casesByAge:(t-1)];
        //E_casesByAge[t] = tmp_row_vector_A * ( impact_intv_res[t] *
        //cntct_weekends_mean_local + impact_intv_nonres[t] *
        //cntct_weekdays_mean_local );
        // better version which does the reduction first (row-vector x
        //matrix) and then adds the scaled results
        E_casesByAge[t] = impact_intv_res[t]    * (tmp_row_vector_A * cntct_weekends_mean_local) +
                          impact_intv_nonres[t] * (tmp_row_vector_A * cntct_weekdays_mean_local);
      }
    }
  
    // calculate expected deaths by age and country  
    E_deathsByAge[1] = 1e-15 * E_casesByAge[1];
    for (t in 2:N2)
    {
      E_deathsByAge[t] = rev_ifr_daysSinceInfection[(N2-(t-1)+1):N2 ] * E_casesByAge[1:(t-1)];
    }
    E_deathsByAge .*= ifr_age;
    E_deathsByAge *= (ifr_country_scale_local * ifr_noise_local);
  
    E_deaths = E_deathsByAge * ones_vector_A;

    // likelihood
    return neg_binomial_2_lpmf(deaths_local[epidemicStart_local:N_local]| E_deaths[epidemicStart_local:N_local], phi );
  }

  real countries_log_dens(int[,] deaths_slice,
                      int start,
                      int end,
                      // parameters
                      vector R0,
                      real[] e_cases_N0,
                      vector beta_res_wkend,
                      vector beta_res_wkday,
                      vector beta_nonres_wkday,
                      real phi,
                      real[] ifr_noise,
                      // data
                      int N0,
                      int N2,
                      int A,
                      int SI_CUT,
                      // wkend_idx[1:WKEND_IDX_N[m],m]
                      int[] num_wkend_idx,
                      int[,] wkend_idx,
                      vector avg_cntct,
                      matrix[] covariates_res,
                      matrix[] covariates_nonres,
                      matrix[] cntct_weekends_mean,
                      matrix[] cntct_weekdays_mean,
                      row_vector rev_ifr_daysSinceInfection,
                      real[] ifr_country_scale,
                      matrix ifr_age,
                      row_vector rev_serial_interval,
                      int[] epidemicStart,
                      int[] N,
                      int init_A,
                      data vector ones_vector_A
                      ) {
    real lpmf = 0.0;
    int M_slice = end - start + 1;
    
    for(m_slice in 1:M_slice) {
      int m = m_slice + start - 1;
      
      lpmf += country_lpmf(
          deaths_slice[m_slice,:] |
          R0[m],
          e_cases_N0[m],
          beta_res_wkend,
          beta_res_wkday,
          beta_nonres_wkday,
          phi,
          ifr_noise[m],
          N0,
          N2,
          A,
          SI_CUT,
          wkend_idx[1:num_wkend_idx[m],m],
          avg_cntct[m],
          covariates_res[m],
          covariates_nonres[m],
          cntct_weekends_mean[m],
          cntct_weekdays_mean[m],
          rev_ifr_daysSinceInfection,
          ifr_country_scale[m],
          ifr_age,
          rev_serial_interval,
          epidemicStart[m],
          N[m],
          init_A,
          ones_vector_A
                           );
    }
    
    return(lpmf);
  }
}
data {
  int<lower=1> M; // number of countries
  int<lower=1> N0; // number of initial days for which to estimate infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int<lower=1> A; // number of age bands
  int<lower=1> SI_CUT; // number of days in serial interval to consider
  int<lower=1> P_RES; // number of predictors for residential contacts
  int<lower=1> P_NONRES; // number of predictors for non-residential contacts
  int WKEND_IDX_N[M]; // number of weekend indices in each location
  //	data
  real pop[M];
  matrix<lower=0, upper=1>[A,M] popByAge; // proportion of age bracket in population in location
  int epidemicStart[M];
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  int<lower=0> wkend_idx[N2,M]; //indices of 1:N2 that correspond to weekends in location m
  matrix[N2,P_RES] covariates_res[M]; // predictors for residential contacts
  matrix[N2,P_NONRES]  covariates_nonres[M]; // predictors for non-residential contacts
  //	priors
  matrix[A,A] cntct_weekdays_mean[M]; // mean of prior contact rates between age groups on weekdays
  matrix[A,A] cntct_weekends_mean[M]; // mean of prior contact rates between age groups on weekends
  real<lower=0> ifr_country_scale[M]; // relative probability of death for location, s days after infection, for age band a
  matrix<lower=0>[N2,A] ifr_age; // probability of death for age band a, stored N2 times
  row_vector[N2] rev_ifr_daysSinceInfection; // probability of death s days after infection in reverse order
  row_vector[SI_CUT] rev_serial_interval; // fixed pre-calculated serial interval using empirical data from Neil in reverse order
  int<lower=1, upper=A> init_A; // age band in which initial cases occur in the first N0 days
}

transformed data {
  vector<lower=0>[M] avg_cntct;
  vector[A] ones_vector_A = rep_vector(1.0, A);
  row_vector[A] ones_row_vector_A = rep_row_vector(1.0, A);
  int trans_deaths[M, N2]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  
  for( m in 1:M )
  {
    avg_cntct[m] = popByAge[:,m]' * ( cntct_weekdays_mean[m] * ones_vector_A ) * 5./7.;
    avg_cntct[m] += popByAge[:,m]' * ( cntct_weekends_mean[m] * ones_vector_A ) * 2./7.;

    trans_deaths[m,:] = deaths[:,m];
  }
}

parameters {
  vector<lower=0>[M] R0; // R0
  //real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  vector[P_RES] beta_res_wkend; // regression coefficients for time varying multipliers on residential contacts on weekends
  vector[P_RES] beta_res_wkday; // regression coefficients for time varying multipliers on residential contacts on weekdays
  vector[P_NONRES] beta_nonres_wkday; // regression coefficients for time varying multipliers on non-residential contacts on weekdays
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> ifr_noise[M];
  real<lower=0> kappa;
}

transformed parameters {
}

model {
  // priors
  tau ~ exponential(0.03);
  e_cases_N0 ~ exponential(1/tau);
  phi ~ normal(0,5);
  beta_res_wkend ~ normal(0,2);
  beta_res_wkday ~ normal(0,2);
  beta_nonres_wkday ~ normal(0,2);
  ifr_noise ~ normal(1,0.1);
  kappa ~ normal(0,0.5);
  R0 ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319

  // rstan version
  target += countries_log_dens(trans_deaths, 1, M,
  // cmdstan version
  //target += reduce_sum(countries_log_dens, trans_deaths, 1,
      R0,
      e_cases_N0,
      beta_res_wkend,
      beta_res_wkday,
      beta_nonres_wkday,
      phi,
      ifr_noise,
      N0,
      N2,
      A,
      SI_CUT,
      WKEND_IDX_N,
      wkend_idx,
      avg_cntct,
      covariates_res,
      covariates_nonres,
      cntct_weekends_mean,
      cntct_weekdays_mean,
      rev_ifr_daysSinceInfection,
      ifr_country_scale,
      ifr_age,
      rev_serial_interval,
      epidemicStart,
      N,
      init_A,
      ones_vector_A
                       );
  
}

generated quantities {
  /* disabled to speed up outputs

  matrix<lower=0>[N2,M] Rt; // Rt for each location
  matrix<lower=0>[N2,A] RtByAge[M]; // Rt for each age band and each location
  matrix<lower=0>[N2,A] tmp;
  for( m in 1:M )
  {
    RtByAge[m] = rep_matrix( ones_row_vector_A*cntct_weekends_mean[m] , N2);
    RtByAge[m] .*= rep_matrix( impact_intv_res[:,m], A);
    tmp = rep_matrix( ones_row_vector_A*cntct_weekdays_mean[m] , N2);
    tmp .*= rep_matrix( impact_intv_nonres[:,m], A);
    RtByAge[m] += tmp;
    Rt[:,m] = RtByAge[m] * popByAge[:,m];
  }
  */
}
