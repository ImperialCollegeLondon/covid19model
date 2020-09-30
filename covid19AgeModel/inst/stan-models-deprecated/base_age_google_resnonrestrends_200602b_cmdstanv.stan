functions {
    
    /*
    * returns impact_intv_on_cntc_wday (first index) and impact_intv_on_cntc_wend
    * (second index) for one country
    */
    vector[] country_impact(// parameters
        real R0_local,
        real mixing_res_on_wday,
        real mixing_res_on_wend,
        // data
        int N2,
        int[] wkend_idx_local,
        real avg_cntct_local,
        matrix covariates_res_local,
        matrix covariates_nonres_local
        )
    {
        // probability of infection given contact in location m
        real rho0 = R0_local / avg_cntct_local;
    
        // scaling of contacts after intervention effect on day t in location m
        vector[N2] impact_intv_on_cntc_wday;
        vector[N2] impact_intv_on_cntc_wend;

        // define multipliers acting on the weekday contact matrix
        impact_intv_on_cntc_wday = (1-mixing_res_on_wday) * mixing_res_on_wend * covariates_nonres_local[,1];
        impact_intv_on_cntc_wday -= mixing_res_on_wday * (1-mixing_res_on_wend) * covariates_res_local[,1];
        impact_intv_on_cntc_wday[ wkend_idx_local ] = mixing_res_on_wend * (1-mixing_res_on_wend) * covariates_nonres_local[wkend_idx_local,1];
        impact_intv_on_cntc_wday[ wkend_idx_local ] -= mixing_res_on_wend * (1-mixing_res_on_wend) * covariates_res_local[wkend_idx_local,1];
        impact_intv_on_cntc_wday /= ( (1-mixing_res_on_wday) * mixing_res_on_wend - mixing_res_on_wday * (1-mixing_res_on_wend) );
    
        // define multipliers acting on the weekend contact matrix
        impact_intv_on_cntc_wend = mixing_res_on_wday * (1-mixing_res_on_wday) * covariates_res_local[,1];
        impact_intv_on_cntc_wend -= (1-mixing_res_on_wday) * mixing_res_on_wday * covariates_nonres_local[,1];
        impact_intv_on_cntc_wend[ wkend_idx_local ] = mixing_res_on_wend * (1-mixing_res_on_wday) * covariates_res_local[wkend_idx_local,1];
        impact_intv_on_cntc_wend[ wkend_idx_local ] -= mixing_res_on_wday * (1-mixing_res_on_wend) * covariates_nonres_local[wkend_idx_local,1];
        impact_intv_on_cntc_wday /= ( (1-mixing_res_on_wday) * mixing_res_on_wend - mixing_res_on_wday * (1-mixing_res_on_wend) );
    
        // multiply the multipliers with rho0 in each location
        impact_intv_on_cntc_wday *= rho0;
        impact_intv_on_cntc_wend *= rho0;
    
        return ({impact_intv_on_cntc_wday, impact_intv_on_cntc_wend});
    }
  
    matrix country_EcasesByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              real mixing_res_on_wday,
                              real mixing_res_on_wend,
                              // data
                              int N0,
                              int N2,
                              int A,
                              int SI_CUT,
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
                                                    mixing_res_on_wday, mixing_res_on_wend,
                                                    N2,
                                                    wkend_idx_local,
                                                    avg_cntct_local,
                                                    covariates_res_local,
                                                    covariates_nonres_local);
  
        // init expected cases by age and location in first N0 days
        E_casesByAge[1:N0,init_A] = rep_vector( e_cases_N0_local, N0 );
  
        // calculate expected cases by age and country under self-renewal model after first N0 days
        // and adjusted for saturation
        for (t in (N0+1):N2)
        {
            int start_idx_rev_serial = SI_CUT-t+2;
            int start_idx_E_casesByAge = t-SI_CUT;
            if(start_idx_rev_serial < 1)
            {
                start_idx_rev_serial = 1;
            }
            if(start_idx_E_casesByAge < 1)
            {
                start_idx_E_casesByAge = 1;
            }
            {
                row_vector[A] tmp_row_vector_A;
                print( "rev_serial_interval", rev_serial_interval[start_idx_rev_serial:SI_CUT] );
                print( "E_casesByAge", E_casesByAge[start_idx_E_casesByAge:(t-1)] );
                tmp_row_vector_A = rev_serial_interval[start_idx_rev_serial:SI_CUT] * E_casesByAge[start_idx_E_casesByAge:(t-1)];
                // do the reduction first (row-vector x matrix) and then add the scaled results to keep autodiff tape small
                // impact_intv[1,] is impact_intv_on_cntc_wday
                // impact_intv[2,] is impact_intv_on_cntc_wend
                E_casesByAge[t] = impact_intv[1,t] * (tmp_row_vector_A * cntct_weekdays_mean_local) + impact_intv[2,t] * (tmp_row_vector_A * cntct_weekends_mean_local);
            }
        }
        return(E_casesByAge);
    }
  
    matrix country_EdeathsByAge(// parameters
                              real R0_local,
                              real e_cases_N0_local,
                              real mixing_res_on_wday,
                              real mixing_res_on_wend,
                              real phi,
                              real ifr_noise_local,
                              // data
                              int N0,
                              int N2,
                              int A,
                              int SI_CUT,
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
    
        // expected deaths by calendar day , age  under self-renewal model
        matrix[N2,A] E_deathsByAge = rep_matrix( zero, N2, A );
  
        // expected new cases by calendar day, age, and location under self-renewal model
        // and a container to store the precomputed cases by age
        matrix[N2,A] E_casesByAge = country_EcasesByAge(R0_local,
                                                e_cases_N0_local,
                                                mixing_res_on_wday,
                                                mixing_res_on_wend,
                                                N0,
                                                N2,
                                                A,
                                                SI_CUT,
                                                wkend_idx_local,
                                                avg_cntct_local,
                                                covariates_res_local,
                                                covariates_nonres_local,
                                                cntct_weekends_mean_local,
                                                cntct_weekdays_mean_local,
                                                rev_serial_interval,
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
                          real mixing_res_on_wday,
                          real mixing_res_on_wend,
                          real phi,
                          real[] ifr_noise,
                          // data
                          int N0,
                          int N2,
                          int A,
                          int SI_CUT,
                          int[] num_wkend_idx,
                          int[,] wkend_idx,
                          vector avg_cntct,
                          matrix[] covariates_res,
                          matrix[] covariates_nonres,
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
                          data vector ones_vector_A
                          )
    {
        real lpmf = 0.0;
        int M_slice = end - start + 1;
        int index_country_slice;
        int min_age_slice;
        int max_age_slice;
        vector[N2] E_deaths;
    
        for(m_slice in 1:M_slice)
        {
            int m = m_slice + start - 1;
            matrix[N2, A] E_deathsByAge = country_EdeathsByAge(
                R0[m],
                e_cases_N0[m],
                mixing_res_on_wday,
                mixing_res_on_wend,
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
                init_A
                );

            E_deaths = E_deathsByAge * ones_vector_A;
      
            // likelihood for locations with age-specific death data
            if(map_country[m,1] == 1)
            {
                index_country_slice = map_country[m,2];
                lpmf += neg_binomial_2_lpmf(deaths_slice[m_slice, epidemicStart[m]:(dataByAgestart[index_country_slice]-1)] | E_deaths[epidemicStart[m]:(dataByAgestart[index_country_slice]-1)], phi );
        
                for(a in 1:A_AD[index_country_slice])
                {
                    // first day of data is cumulated death
                    lpmf += neg_binomial_2_lpmf(deathsByAge[dataByAgestart[index_country_slice], a, index_country_slice] |
                                          rep_row_vector(1.0, (dataByAgestart[index_country_slice]-epidemicStart[m]+1)) * E_deathsByAge[epidemicStart[m]:dataByAgestart[index_country_slice], :] * map_age[index_country_slice][:, a], phi );
                    // thereafter daily death
                    lpmf += neg_binomial_2_lpmf(deathsByAge[(dataByAgestart[index_country_slice]+1):N[m], a, index_country_slice] |
                                          E_deathsByAge[(dataByAgestart[index_country_slice]+1):N[m], :] * map_age[index_country_slice][:, a], phi );
                }
            }
            // likelihood for locations with no age-specific death data
            if(map_country[m,1] == 0)
            {
                lpmf += neg_binomial_2_lpmf(deaths_slice[m_slice, epidemicStart[m]:N[m]] | E_deaths[epidemicStart[m]:N[m]], phi );
            }
        }
        return(lpmf);
    }
}

data
{
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
  //    data by age
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
  real<lower=0> resnonres_constraints_on_alpha_beta[3]; // entry 1 is upper bound on alpha, entry 2 is constraint C1 in beta>alpha*C1, entry 3 is constraint C2 in beta>(C2-1+alpha)/C2
}

transformed data
{
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

parameters
{
  vector<lower=0>[M] R0; // R0
  real<lower=0> kappa; // variance parameter for country-specific R0  
  real<lower=0> tau; // prior rate of expected number of cases per day in the first N0 days, for each country
  real<lower=0> e_cases_N0[M]; // expected number of cases per day in the first N0 days, for each country
  real<lower=0,upper=resnonres_constraints_on_alpha_beta[1]> mixing_res_on_wday;
  real<lower=0,upper=1> mixing_res_on_wend_raw;
  real<lower=0> phi; // overdispersion parameter for likelihood model
  real<lower=0> ifr_noise[M];
  row_vector<lower=0,upper=1>[A] ifr_age; // probability of death for age band a
}

transformed parameters
{
  real<lower=0, upper=1> mixing_res_on_wend;
  {
    real tmp[2]= { mixing_res_on_wday * resnonres_constraints_on_alpha_beta[2], ( resnonres_constraints_on_alpha_beta[3] - 1 + mixing_res_on_wday ) / resnonres_constraints_on_alpha_beta[3] };
    if(tmp[1]>tmp[2])
    {
        mixing_res_on_wend = mixing_res_on_wend_raw * (1-tmp[1]) + tmp[1];
    }
    if(tmp[2]>tmp[1])
    {
        mixing_res_on_wend = mixing_res_on_wend_raw * (1-tmp[2]) + tmp[2];
    }
  }
}

model
{
  // priors
  tau ~ exponential(0.03);
  e_cases_N0 ~ exponential(1/tau);
  phi ~ exponential(1);
  mixing_res_on_wday ~ uniform(0, resnonres_constraints_on_alpha_beta[1]);
  mixing_res_on_wend_raw ~ uniform(0, 1);
  ifr_noise ~ normal(1,0.1);
  kappa ~ normal(0,0.5);
  R0 ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  ifr_age ~ beta(hyperpara_ifr_age[:,1], hyperpara_ifr_age[:,2]);

  // rstan version
  //target += countries_log_dens(trans_deaths, 1, M,
  // cmdstan version
   target += reduce_sum(countries_log_dens, trans_deaths, 1,
      R0,
      e_cases_N0,
      mixing_res_on_wday,
      mixing_res_on_wend,
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
      A_AD,
      dataByAgestart,
      map_age,
      deathsByAge,
      map_country,
      ones_vector_A
    );
}

generated quantities {
  // rho0 for each location
  vector[M] rho0;
  // eta for each location
  matrix<lower=0>[N2,M] eta_weekday;
  matrix<lower=0>[N2,M] eta_weekend;
  // Rt for each location
  matrix<lower=0>[N2,M] Rt;
  // Rt for each age band and each location
  matrix<lower=0>[N2,A] RtByAge[M];
  matrix<lower=0>[N2,M] E_deaths;
  matrix<lower=0>[N2,A] E_deathsByAge[M];
  matrix<lower=0>[N2,A] E_casesByAge[M];

  for( m in 1:M )
  {
    matrix[N2,A] tmp;
    // impact_intv[1,] is impact_intv_on_cntc_wday
    // impact_intv[2,] is impact_intv_on_cntc_wend
    vector[N2] impact_intv[2] = country_impact(R0[m],
                                               mixing_res_on_wday,
                                               mixing_res_on_wend,
                                               N2,
                                               wkend_idx[1:WKEND_IDX_N[m],m],
                                               avg_cntct[m],
                                               covariates_res[m],
                                               covariates_nonres[m]);
    E_casesByAge[m] = 
       country_EcasesByAge(R0[m], 
            e_cases_N0[m], 
            mixing_res_on_wday,
            mixing_res_on_wend,
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
            
    E_deathsByAge[m] =
        country_EdeathsByAge(
            R0[m],
            e_cases_N0[m],
            mixing_res_on_wday,
            mixing_res_on_wend,
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
    RtByAge[m] = rep_matrix( cntct_weekends_mean[m]*ones_vector_A , N2);
    RtByAge[m] .*= rep_matrix( impact_intv[2], A);
    tmp = rep_matrix( cntct_weekdays_mean[m] *ones_vector_A, N2);
    tmp .*= rep_matrix( impact_intv[1], A);
    RtByAge[m] += tmp;
    Rt[:,m] = RtByAge[m] * popByAge[:,m];
    rho0[m] = R0[m] / avg_cntct[m];
    eta_weekday[:,m] = impact_intv[1]/rho0[m];
    eta_weekend[:,m] = impact_intv[2]/rho0[m];
  }
}
