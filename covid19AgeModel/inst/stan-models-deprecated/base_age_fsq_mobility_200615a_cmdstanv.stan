functions {


   // function equivalent to %in% on R from https://discourse.mc-stan.org/t/stan-equivalent-of-rs-in-function/3849
  int r_in(int pos,int[] pos_var) {
   
    for (p in 1:(size(pos_var))) {
       if (pos_var[p]==pos) {
       // can return immediately, as soon as find a match
          return 1;
       } 
    }
    return 0;
  }

  /* 
   * returns multiplier on the rows of the contact matrix over time for one country
   */
  matrix country_impact(
      real R0_local,
      vector beta,
      int N2,
      int A,
      int COVARIATES_N,
      real avg_cntct_local,
      matrix[] covariates_local
      )
  {
    
    // probability of infection given contact in location m
    real rho0 = R0_local / avg_cntct_local;
    real log_rho0 = log(rho0);

    // scaling of contacts after intervention effect on day t in location m
    matrix[N2,A] impact_intv;
    
    // define multipliers for contacts in each location
    impact_intv = rep_matrix(log_rho0, N2, A);
    for(i in 1:COVARIATES_N)
    {
        impact_intv += beta[i] * covariates_local[i];
    }
    impact_intv = exp( impact_intv );
        
    return (impact_intv);
  }
  
    matrix country_EcasesByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              vector beta,
                              row_vector log_relsusceptibility_age,
                              // data
                              int N0,
                              int N2,
                              int A,
                              int COVARIATES_N,
                              int SI_CUT,
                              // wkend_idx[1:WKEND_IDX_N[m],m]
                              int[] wkend_idx_local,
                              real avg_cntct_local,
                              matrix[] covariates_local,
                              matrix cntct_weekends_mean_local,
                              matrix cntct_weekdays_mean_local,
                              row_vector rev_serial_interval,
                              row_vector popByAge_abs_local,
                              int init_A
                              )
  {
    // dummy variables
    // real<lower=0> saturation_adj;
    real zero = 0.0;
  
    // expected new cases by calendar day, age, and location under self-renewal model
    // and a container to store the precomputed cases by age
    matrix[N2,A] E_casesByAge = rep_matrix( zero, N2, A );

    matrix[N2,A] impact_intv = country_impact(R0_local,
                                               beta, 
                                               N2,
                                               A,
                                               COVARIATES_N,
                                               avg_cntct_local,
                                               covariates_local);

    // init expected cases by age and location in first N0 days
    E_casesByAge[1:N0,init_A] = rep_vector( e_cases_N0_local, N0 );
  
    // calculate expected cases by age and country under self-renewal model after first N0 days
    // and adjusted for saturation
    for (t in (N0+1):N2)
    {
      int start_idx_rev_serial = SI_CUT-t+2;
      int start_idx_E_casesByAge = t-SI_CUT;
      row_vector[A] prop_susceptibleByAge = rep_row_vector(1.0, A) - (rep_row_vector(1.0, t-1) * E_casesByAge[1:(t-1),:] ./ popByAge_abs_local);
      if(start_idx_rev_serial < 1) {
        start_idx_rev_serial = 1;
      }
      if(start_idx_E_casesByAge < 1) {
        start_idx_E_casesByAge = 1;
      }
      // TODO can t we vectorise this?
      for(a in 1:A){
        if(prop_susceptibleByAge[a] < 0) { // account for values of Ecases > pop at initalization
          prop_susceptibleByAge[a] = 0;
        }
      }
      {
        row_vector[A] tmp_row_vector_A = rev_serial_interval[start_idx_rev_serial:SI_CUT] * E_casesByAge[start_idx_E_casesByAge:(t-1)];
        tmp_row_vector_A .*= impact_intv[t,];
        if(r_in(t, wkend_idx_local) == 1){
            E_casesByAge[t] = tmp_row_vector_A * cntct_weekends_mean_local;
        }else{
            E_casesByAge[t] = tmp_row_vector_A * cntct_weekdays_mean_local;
        }
        E_casesByAge[t] .*= prop_susceptibleByAge;
        E_casesByAge[t] .*= append_col(exp(log_relsusceptibility_age), 1.);
      }
    }
  
    return(E_casesByAge);
  }
  
  matrix country_EdeathsByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              vector beta,
                              row_vector log_relsusceptibility_age,
                              real phi,
                              real ifr_noise_local,
                              // data
                              int N0,
                              int N2,
                              int A,
                              int COVARIATES_N,
                              int SI_CUT,
                              // wkend_idx[1:WKEND_IDX_N[m],m]
                              int[] wkend_idx_local,
                              real avg_cntct_local,
                              matrix[] covariates_local,
                              matrix cntct_weekends_mean_local,
                              matrix cntct_weekdays_mean_local,
                              row_vector rev_ifr_daysSinceInfection,
                              real ifr_country_scale_local,
                              row_vector ifr_age,
                              row_vector rev_serial_interval,
                              row_vector popByAge_abs_local,
                              int init_A
                              )
  {
    // dummy variables
    // real<lower=0> saturation_adj;

    real zero = 0.0;
    
    // expected deaths by calendar day (1st dim) age (2nd dim) and country (3rd dim), under self-renewal model
    // and expected deaths by calendar day (rows) and country (cols), under self-renewal model
    //matrix<lower=0>[N2,A] E_deathsByAge[M];
    matrix[N2,A] E_deathsByAge = rep_matrix( zero, N2, A );
  
    // expected new cases by calendar day, age, and location under self-renewal model
    // and a container to store the precomputed cases by age
    matrix[N2,A] E_casesByAge = country_EcasesByAge(R0_local,
                                    e_cases_N0_local,
                                    beta,
                                    log_relsusceptibility_age,
                                    N0,
                                    N2,
                                    A,
                                    COVARIATES_N,
                                    SI_CUT,
                                    wkend_idx_local,
                                    avg_cntct_local,
                                    covariates_local,
                                    cntct_weekends_mean_local,
                                    cntct_weekdays_mean_local,
                                    rev_serial_interval,
                                    popByAge_abs_local,
                                    init_A);

    // calculate expected deaths by age and country  
    E_deathsByAge[1] = 1e-15 * E_casesByAge[1];
    for (t in 2:N2)
    {
      E_deathsByAge[t] = rev_ifr_daysSinceInfection[(N2-(t-1)+1):N2 ] * E_casesByAge[1:(t-1)];
    }
    E_deathsByAge .*= rep_matrix(ifr_age, N2);
    E_deathsByAge *= (ifr_country_scale_local * ifr_noise_local);
  
    return(E_deathsByAge);
  }

  real countries_log_dens(int[,] deaths_slice,
                          int start,
                          int end,
                          // parameters
                          vector R0,
                          real[] e_cases_N0,
                          vector beta,
                          row_vector log_relsusceptibility_age,
                          real phi,
                          real[] ifr_noise,
                          // data
                          int N0,
                          int N2,
                          int A,
                          int COVARIATES_N,
                          int SI_CUT,
                          // wkend_idx[1:WKEND_IDX_N[m],m]
                          int[] num_wkend_idx,
                          int[,] wkend_idx,
                          vector avg_cntct,
                          matrix[,] covariates,
                          matrix[] cntct_weekends_mean,
                          matrix[] cntct_weekdays_mean,
                          row_vector rev_ifr_daysSinceInfection,
                          real[] ifr_country_scale,
                          row_vector ifr_age,
                          row_vector rev_serial_interval,
                          int[] epidemicStart,
                          int[] N,
                          int init_A,
                          int[] A_AD,
                          int[] dataByAgestart,
                          matrix[] map_age,
                          int[,,] deathsByAge,
                          int[,] map_country,
                          matrix popByAge_abs,
                          data vector ones_vector_A
                          )
  {
    real lpmf = 0.0;
    int M_slice = end - start + 1;
    int index_country_slice; 
    int min_age_slice;
    int max_age_slice;
    vector[N2] E_deaths;

    for(m_slice in 1:M_slice) {
      int m = m_slice + start - 1;
      matrix[N2, A] E_deathsByAge =
          country_EdeathsByAge(
              R0[m],
              e_cases_N0[m],
              beta,
              log_relsusceptibility_age,
              phi,
              ifr_noise[m],
              N0,
              N2,
              A,
              COVARIATES_N,
              SI_CUT,
              wkend_idx[1:num_wkend_idx[m],m],
              avg_cntct[m],
              covariates[m],
              cntct_weekends_mean[m],
              cntct_weekdays_mean[m],
              rev_ifr_daysSinceInfection,
              ifr_country_scale[m],
              ifr_age,
              rev_serial_interval,
              popByAge_abs[m,],
              init_A
                               );

      E_deaths = E_deathsByAge * ones_vector_A;
      
      // likelihood
      if(map_country[m,1] == 1){
        index_country_slice = map_country[m,2];
        lpmf += neg_binomial_2_lpmf(deaths_slice[m_slice, epidemicStart[m]:(dataByAgestart[index_country_slice]-1)] | E_deaths[epidemicStart[m]:(dataByAgestart[index_country_slice]-1)], phi );
        
        for(a in 1:A_AD[index_country_slice]){
          // first day of data is sumulated death
          lpmf += neg_binomial_2_lpmf(deathsByAge[dataByAgestart[index_country_slice], a, index_country_slice] | 
                                          rep_row_vector(1.0, (dataByAgestart[index_country_slice]-epidemicStart[m]+1)) * E_deathsByAge[epidemicStart[m]:dataByAgestart[index_country_slice], :] * map_age[index_country_slice][:, a], phi );
          // after daily death
          lpmf += neg_binomial_2_lpmf(deathsByAge[(dataByAgestart[index_country_slice]+1):N[m], a, index_country_slice] | 
                                          E_deathsByAge[(dataByAgestart[index_country_slice]+1):N[m], :] * map_age[index_country_slice][:, a], phi );
          }
      }
      if(map_country[m,1] == 0){
        lpmf += neg_binomial_2_lpmf(deaths_slice[m_slice, epidemicStart[m]:N[m]]| E_deaths[epidemicStart[m]:N[m]], phi );
      }
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
  int<lower=1> COVARIATES_N; // number of days in serial interval to consider
  int WKEND_IDX_N[M]; // number of weekend indices in each location
  //	data
  real pop[M];
  matrix<lower=0, upper=1>[A,M] popByAge; // proportion of age bracket in population in location
  int epidemicStart[M];
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  int<lower=0> wkend_idx[N2,M]; //indices of 1:N2 that correspond to weekends in location m
  matrix[N2,A] covariates[M, COVARIATES_N]; // predictors for fsq contacts by age
  // data by age
  int<lower=0> M_AD; // number of countries with deaths by age data
  int<lower=1> dataByAgestart[M_AD]; // start of death by age data
  int deathsByAge[N2, A, M_AD]; // reported deaths by age -- the rows with i < dataByAgestart[M_AD] contain -1 and should be ignored + the column with j > A2[M_AD] contain -1 and should be ignored 
  int<lower=2> A_AD[M_AD]; // number of age groups reported 
  matrix[A, A] map_age[M_AD]; // map the age groups reported with 5 y age group -- the column with j > A2[M_AD] contain -1 and should be ignored
  int map_country[M,2]; // first column indicates if country has death by age date (1 if yes), 2 column map the country to M_AD
  //	priors
  matrix[A,A] cntct_weekdays_mean[M]; // mean of prior contact rates between age groups on weekdays
  matrix[A,A] cntct_weekends_mean[M]; // mean of prior contact rates between age groups on weekends
  real<lower=0> ifr_country_scale[M]; // relative probability of death for location, s days after infection, for age band a
  real<lower=0> hyperpara_ifr_age[A,2]; // Beta hyper-parameters for probability of death in age band a
  row_vector[N2] rev_ifr_daysSinceInfection; // probability of death s days after infection in reverse order
  row_vector[SI_CUT] rev_serial_interval; // fixed pre-calculated serial interval using empirical data from Neil in reverse order
  int<lower=1, upper=A> init_A; // age band in which initial cases occur in the first N0 days
}

transformed data {
  vector<lower=0>[M] avg_cntct;
  vector[A] ones_vector_A = rep_vector(1.0, A);
  row_vector[A] ones_row_vector_A = rep_row_vector(1.0, A);
  int trans_deaths[M, N2]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[M,A] popByAge_abs;  
  
  for( m in 1:M )
  {
    avg_cntct[m] = popByAge[:,m]' * ( cntct_weekdays_mean[m] * ones_vector_A ) * 5./7.;
    avg_cntct[m] += popByAge[:,m]' * ( cntct_weekends_mean[m] * ones_vector_A ) * 2./7.;

    trans_deaths[m,:] = deaths[:,m];
    
    popByAge_abs[m,:] = popByAge[:,m]' * pop[m]; // pop by age is a proportion of pop by age and pop is the absolute number 
  }
}

parameters {
  vector<lower=0>[M] R0; // R0
  real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  vector[COVARIATES_N] beta; // regression coefficients for time varying multipliers on contacts
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> ifr_noise[M];
  row_vector<lower=0,upper=1>[A] ifr_age; // probability of death for age band a
  row_vector[A-1] log_relsusceptibility_age;
  real<lower=0> sd_log_relsusceptibility_age;
}

model {
  // priors
  tau ~ exponential(0.03);
  e_cases_N0 ~ exponential(1/tau);
  phi ~ normal(0,5);
  beta ~ normal(0,1);
  ifr_noise ~ normal(1,0.1);
  kappa ~ normal(0,0.5);
  R0 ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  ifr_age ~ beta(hyperpara_ifr_age[:,1], hyperpara_ifr_age[:,2]);
  log_relsusceptibility_age ~ normal(0, sd_log_relsusceptibility_age);
  sd_log_relsusceptibility_age ~ exponential(10);   // so the prior mean standard dev is 0.1
    
  // rstan version
  // target += countries_log_dens(trans_deaths, 1, M,
  // cmdstan version
  target += reduce_sum(countries_log_dens, trans_deaths, 1,
      R0,
      e_cases_N0,
      beta,
      log_relsusceptibility_age,
      phi,
      ifr_noise,
      N0,
      N2,
      A,
      COVARIATES_N,
      SI_CUT,
      WKEND_IDX_N,
      wkend_idx,
      avg_cntct,
      covariates,
      cntct_weekends_mean,
      cntct_weekdays_mean,
      rev_ifr_daysSinceInfection,
      ifr_country_scale,
      ifr_age,
      rev_serial_interval,
      epidemicStart,
      N,
      init_A,
      A_AD,
      dataByAgestart,
      map_age,
      deathsByAge,
      map_country,
      popByAge_abs,
      ones_vector_A
                       );
  
}
