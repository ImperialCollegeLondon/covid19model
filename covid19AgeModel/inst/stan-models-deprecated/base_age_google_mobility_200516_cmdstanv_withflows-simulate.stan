functions {

  /* 
   * returns impact_intv_res (first index) and impact_intv_nonres
   * (second index) for one country
   */
  vector[] country_impact(
      real R0_local,
      real beta_res_wkend,
      real beta_res_wkday,
      vector beta_nonres_wkday,
      int N2,
      // wkend_idx[1:WKEND_IDX_N[m],m]
      int[] wkend_idx_local,
      real avg_cntct_local,
      matrix covariates_res_local,
      matrix covariates_nonres_local
                          )
  {
    int num_wkend_idx_local = size(wkend_idx_local);
    
    // probability of infection given contact in location m
    real rho0 = R0_local / avg_cntct_local;
    real log_rho0 = log(rho0);
    
    real zero = 0.0;

    // scaling of contacts after intervention effect on day t in location m
    vector[N2] impact_intv_res;
    vector[N2] impact_intv_nonres;

    // define multipliers for residential contacts in each location for both weekdays and weekends
    // define multipliers for non-residential contacts in each location for both weekdays and weekends
    // multiply the multipliers with rho0 in each location
    // TODO use
    // https://mc-stan.org/docs/2_18/stan-users-guide/QR-reparameterization-section.html
    impact_intv_res = to_vector(exp( log_rho0 + covariates_res_local * beta_res_wkday ));
    impact_intv_nonres = exp( log_rho0 + covariates_nonres_local * beta_nonres_wkday );
    impact_intv_res[ wkend_idx_local ] = to_vector(exp( log_rho0 + covariates_res_local[ wkend_idx_local ] * beta_res_wkend ));
    impact_intv_nonres[ wkend_idx_local ] = rep_vector(zero, num_wkend_idx_local );

    return ({impact_intv_res, impact_intv_nonres});
  }
  
    matrix country_EcasesByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              real beta_res_wkend,
                              real beta_res_wkday,
                              vector beta_nonres_wkday,
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
                              row_vector rev_serial_interval,
                              int init_A
                              )
  {
    // dummy variables
    // real<lower=0> saturation_adj;
    real zero = 0.0;
  
    // expected new cases by calendar day, age, and location under self-renewal model
    // and a container to store the precomputed cases by age
    matrix[N2,A] E_casesByAge = rep_matrix( zero, N2, A );

    vector[N2] impact_intv[2] = country_impact(R0_local,
                                               beta_res_wkend, beta_res_wkday, beta_nonres_wkday,
                                               N2,
                                               wkend_idx_local,
                                               avg_cntct_local,
                                               covariates_res_local, covariates_nonres_local);
  
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
        //E_casesByAge[t] = impact_intv_res[t]    * (tmp_row_vector_A * cntct_weekends_mean_local) +
        //                  impact_intv_nonres[t] * (tmp_row_vector_A * cntct_weekdays_mean_local);
        E_casesByAge[t] = impact_intv[1,t] * (tmp_row_vector_A * cntct_weekends_mean_local) +
                          impact_intv[2,t] * (tmp_row_vector_A * cntct_weekdays_mean_local);
      }
    }
  
    return(E_casesByAge);
  }
  
  
    
  matrix[] country_EflowsByLowDimAge(// parameters
    real R0_local,
    real e_cases_N0_local,
    real beta_res_wkend,
    real beta_res_wkday,
    vector beta_nonres_wkday,
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
    row_vector rev_serial_interval,
    int init_A,
    int Nflow,
    int[] N_low_dim_age_band,
    int nrow_low_dim_age_band,
    int[,] low_dim_age_band,
    matrix E_casesByAge
    )
    {
      // dummy variables
      // real<lower=0> saturation_adj;
      real zero = 0.0;
  
      vector[N2] impact_intv[2] = country_impact(R0_local,
      beta_res_wkend, beta_res_wkday, beta_nonres_wkday,
      N2,
      wkend_idx_local,
      avg_cntct_local,
      covariates_res_local, covariates_nonres_local);
      
      row_vector[A] tmp_row_vector_A;                                          
      matrix[A,A] tmp_flow;
      matrix[nrow_low_dim_age_band,nrow_low_dim_age_band] flow[Nflow]; 
      
      for (i in 1:Nflow){
        flow[i]=rep_matrix( zero,nrow_low_dim_age_band,nrow_low_dim_age_band );
      }
      
      // calculate expected cases by age and country under self-renewal model after first N0 days
      // and adjusted for saturation
      for (t in (N0+1):N2)
      {
        int start_idx_rev_serial = SI_CUT-t+2;
        int start_idx_E_casesByAge = t-SI_CUT;
        int tflow = (t-N0-1)/7 + 1;
        
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
          
          tmp_row_vector_A = rev_serial_interval[start_idx_rev_serial:SI_CUT] * E_casesByAge[start_idx_E_casesByAge:(t-1)];
          
          
          tmp_flow = impact_intv[1,t] * rep_matrix(tmp_row_vector_A', A) .* cntct_weekends_mean_local +
          impact_intv[2,t] * rep_matrix(tmp_row_vector_A', A) .* cntct_weekdays_mean_local;
          
          
          for (a in 1:nrow_low_dim_age_band){
            for (aa in 1:nrow_low_dim_age_band){
              flow[tflow][a,aa] += sum( to_array_1d(tmp_flow[ low_dim_age_band[a,1:N_low_dim_age_band[a]],low_dim_age_band[aa,1:N_low_dim_age_band[aa]] ]));
            }
          }
      }
      return(flow);
    }
  
  
    matrix check_country_EflowsByLowDimAge(// parameters
                                int nrow_low_dim_age_band,
                                int Nflow,
                                int N0,
                                int N2,
                                matrix E_casesByAge,
                                matrix[] flow,
                                int[] N_low_dim_age_band,
                                int[,] low_dim_age_band
                              )
  {
    real zero = 0.0;
    matrix[Nflow,nrow_low_dim_age_band] E_casesByLowDimAge=rep_matrix(zero,Nflow,nrow_low_dim_age_band );
    matrix[Nflow,nrow_low_dim_age_band] flow_cases;
    matrix[Nflow,nrow_low_dim_age_band] differences;
    
    for (t in (N0+1):N2)
    {
      int tflow = (t-N0-1)/7 + 1;
      for (a in 1:nrow_low_dim_age_band){
        E_casesByLowDimAge[tflow,a] += sum(to_array_1d(E_casesByAge[t,low_dim_age_band[a,1:N_low_dim_age_band[a]]]));
      }
    }
    
    for (t in 1:Nflow){
          for (a in 1:nrow_low_dim_age_band){
            flow_cases[t,a] = sum(to_array_1d((flow[t][,a])));
          }
    }
    
    differences = E_casesByLowDimAge - flow_cases;
    return(differences);
  }
  
  
  matrix country_EdeathsByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              real beta_res_wkend,
                              real beta_res_wkday,
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
                              row_vector ifr_age,
                              row_vector rev_serial_interval,
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
    matrix[N2,A] E_casesByAge = country_EcasesByAge(R0_local, e_cases_N0_local, beta_res_wkend,
                              beta_res_wkday, beta_nonres_wkday, N0, N2, A, SI_CUT, wkend_idx_local,
                              avg_cntct_local, covariates_res_local, covariates_nonres_local,
                              cntct_weekends_mean_local, cntct_weekdays_mean_local, rev_serial_interval, init_A);
                              
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
  int<lower=0> wkend_idx[N2,M]; //indices of 1:N2 that correspond to weekends in location m
  matrix[N2,P_RES] covariates_res[M]; // predictors for residential contacts
  matrix[N2,P_NONRES]  covariates_nonres[M]; // predictors for non-residential contacts
  // data by age
  int<lower=0> M_AD; // number of countries with deaths by age data
  int<lower=1> dataByAgestart[M_AD]; // start of death by age data
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
  // low dimensional age band
  int nrow_low_dim_age_band;
  int ncol_low_dim_age_band;
  int N_low_dim_age_band[nrow_low_dim_age_band];
  int low_dim_age_band[nrow_low_dim_age_band,ncol_low_dim_age_band];
  int debug_flow;
}

transformed data {
  vector<lower=0>[M] avg_cntct;
  vector[A] ones_vector_A = rep_vector(1.0, A);
  row_vector[A] ones_row_vector_A = rep_row_vector(1.0, A);
  int Nflow;
  
  for( m in 1:M )
  {
    avg_cntct[m] = popByAge[:,m]' * ( cntct_weekdays_mean[m] * ones_vector_A ) * 5./7.;
    avg_cntct[m] += popByAge[:,m]' * ( cntct_weekends_mean[m] * ones_vector_A ) * 2./7.;

  }
  
  if((N2-N0-1)%7==0)
  {
    Nflow = (N2-N0-1)/7;
  }else{
    Nflow = (N2-N0-1)/7 + 1;
  }
}

parameters {
  vector<lower=0>[M] R0; // R0
  real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  real beta_res_wkday; // regression coefficients for time varying multipliers on residential contacts on weekdays
  real beta_res_wkend; // regression coefficients for time varying multipliers on residential contacts on weekends
  vector[P_NONRES] beta_nonres_wkday; // regression coefficients for time varying multipliers on non-residential contacts on weekdays
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> ifr_noise[M];
  row_vector<lower=0,upper=1>[A] ifr_age; // probability of death for age band a
}


generated quantities {
  // rho0 for each location
  vector[M] rho0;
  // eta for each location
  matrix<lower=0>[N2,M] eta_res;
  matrix<lower=0>[N2,M] eta_nonres;
  // Rt for each location
  matrix<lower=0>[N2,M] Rt;
  // Rt for each age band and each location
  matrix<lower=0>[N2,A] RtByAge[M];
  matrix<lower=0>[N2,M] E_deaths;
  matrix<lower=0>[N2,A] E_deathsByAge[M];
  matrix<lower=0>[N2,A] E_casesByAge[M];
  matrix<lower=0>[nrow_low_dim_age_band,nrow_low_dim_age_band] flow[M,Nflow];
  matrix[nrow_low_dim_age_band,nrow_low_dim_age_band] differences[M]; 



  for( m in 1:M )
  {
    matrix[N2,A] tmp;
    vector[N2] impact_intv[2] = country_impact(R0[m],
                                               beta_res_wkend, beta_res_wkday, beta_nonres_wkday,
                                               N2,
                                               wkend_idx[1:WKEND_IDX_N[m],m],
                                               avg_cntct[m],
                                               covariates_res[m], covariates_nonres[m]);
    E_casesByAge[m] = 
       country_EcasesByAge(R0[m], 
            e_cases_N0[m], 
            beta_res_wkend,
            beta_res_wkday, 
            beta_nonres_wkday, 
            N0, 
            N2, 
            A, 
            SI_CUT, 
            wkend_idx[1:WKEND_IDX_N[m],m],
            avg_cntct[m], 
            covariates_res[m], 
            covariates_nonres[m],
            cntct_weekends_mean[m], 
            cntct_weekdays_mean[m], 
            rev_serial_interval, 
            init_A);
            
    
    flow[m,] = country_EflowsByLowDimAge(
            R0[m], 
            e_cases_N0[m], 
            beta_res_wkend,
            beta_res_wkday, 
            beta_nonres_wkday, 
            N0, 
            N2, 
            A, 
            SI_CUT, 
            wkend_idx[1:WKEND_IDX_N[m],m],
            avg_cntct[m], 
            covariates_res[m], 
            covariates_nonres[m],
            cntct_weekends_mean[m], 
            cntct_weekdays_mean[m], 
            rev_serial_interval, 
            init_A,
            Nflow,
            N_low_dim_age_band,
            nrow_low_dim_age_band,
            low_dim_age_band,
            E_casesByAge[m]);
            
    if(debug_flow){ 
      differences[m] = check_country_EflowsByLowDimAge(nrow_low_dim_age_band,
                      Nflow,
                      N0,
                      N2,
                      E_casesByAge[m],
                      flow[m,],
                      N_low_dim_age_band,
                      low_dim_age_band);
    }
      
            
    E_deathsByAge[m] =
        country_EdeathsByAge(
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
            wkend_idx[1:WKEND_IDX_N[m],m],
            avg_cntct[m],
            covariates_res[m],
            covariates_nonres[m],
            cntct_weekends_mean[m],
            cntct_weekdays_mean[m],
            rev_ifr_daysSinceInfection,
            ifr_country_scale[m],
            ifr_age,
            rev_serial_interval,
            init_A
                             );
    E_deaths[:,m] = E_deathsByAge[m] * ones_vector_A;
    E_deaths[:,m] = E_deathsByAge[m] * ones_vector_A;
    RtByAge[m] = rep_matrix( (cntct_weekends_mean[m]*ones_vector_A )', N2);
    RtByAge[m] .*= rep_matrix( impact_intv[2], A);
    tmp = rep_matrix( (cntct_weekdays_mean[m] *ones_vector_A)', N2);
    tmp .*= rep_matrix( impact_intv[1], A);
    RtByAge[m] += tmp;
    Rt[:,m] = RtByAge[m] * popByAge[:,m];
    rho0[m] = R0[m] / avg_cntct[m];
    eta_res[:,m] = impact_intv[1]/rho0[m];
    eta_nonres[:,m] = impact_intv[2]/rho0[m];
  }
}
