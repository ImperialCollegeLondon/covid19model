// functions {
//     matrix expected_deaths(matrix prediction, matrix f) {
//         int M = rows(prediction);
//         int N = cols(prediction);
//         matrix[M, N] y;
//         for (m in 1:M) {
//             y[1, m] = 1e-9;
// 	        for (i in 2:M) {
// 	            y[2, m] = 0;
// 	            for (j in 1:(i - 1)) {
// 	                y[i, m] += prediction[j, m] * f[i - j, m];
//                 }
//             }
//         }
//         return y;
//     }
// }

data {
    int<lower = 1> M; // number of countries
    int<lower = 1> N0; // number of days for which to impute infections
    int<lower = 1> N[M]; // days of observed data for country m. each entry must be <= N2
    int<lower = 1> N2; // # days of observed data (used in analysis) + # of days to forecast
    // real<lower=0> x[N2]; // index of days (starting at 1)
    int cases[N2, M]; // reported cases
    int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
    matrix[N2, M] f; // h * s
    matrix[N2, M] x1;
    matrix[N2, M] x2;
    matrix[N2, M] x3;
    matrix[N2, M] x4;
    matrix[N2, M] x5;
    matrix[N2, M] x6;
    int EpidemicStart[M];
    real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

transformed data {
    matrix[N2, M] matrix_1000 = rep_matrix(1000, N2, M);
    matrix[N2, M] matrix_0 = rep_matrix(0, N2, M);
}

parameters {
    vector<lower = 0>[M] y_raw;
    real<lower = 0> mu[M]; // intercept for Rt
    real<lower = 0> alpha[6]; // the hier term
    real<lower = 0> kappa_std;
    real<lower = 0> phi_std;
    real<lower = 0> tau_unit;
}

transformed parameters {
    real<lower = 0> tau = tau_unit / 0.03;
    real<lower = 0> phi = 5 * phi_std;
    real<lower = 0> kappa = 0.5 * kappa_std;
    vector<lower = 0>[M] y = tau * y_raw;

    matrix[N2, M] prediction = rep_matrix(0, N2, M);
    matrix[N2, M] E_deaths  = rep_matrix(0, N2, M);
    matrix[N2, M] Rt = exp(x1 * (-alpha[1]) 
                            + x2 * (-alpha[2]) 
                            + x3 * (-alpha[3]) 
                            + x4 * (-alpha[4]) 
                            + x5 * (-alpha[5]) 
                            + x6 * (-alpha[6])); // + GP[i]); // to_vector (x) * time_effect
    for (m in 1:M) {
        for (i in 1:N0) {
            prediction[i, m] = y[m];
        } // learn the number of cases in the first N0 days

        Rt[, m] *= mu[m];
        for (i in (N0 + 1):N2) {
            real convolution = y[m] * sum(SI[1:(i - 1)]);
            prediction[i, m] = Rt[i, m] * convolution;
        }
      
              E_deaths[1, m]= 1e-9;
      for (i in 2:N2){
        E_deaths[i,m]= 0;
        for(j in 1:(i-1)){
          E_deaths[i,m] += prediction[j,m]*f[i-j,m];
        }
      }
    }
    /* for(m in 1:M) {
        for(i in 1:N[m]) {
            LowerBound[i,m] = prediction[i,m] * 10 - cases[i,m];
        }
    }*/
}

model {
    tau_unit ~ exponential(1); // implies tau ~ exponential(0.03)
    target += -sum(y_raw); // exponential(1) prior on y_raw implies y ~ exponential(1 / tau)
    kappa_std ~ std_normal();
    phi_std ~ std_normal();
    mu ~ normal(2.4, kappa); // citation needed 
    alpha ~ gamma(0.5, 1);
    for(m in 1:M) {
        deaths[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(
            E_deaths[EpidemicStart[m]:N[m], m], 
            phi
        );
    }
}

generated quantities {
    matrix[N2, M] lp0 = matrix_1000; // log-probability for LOO for the counterfactual model
    matrix[N2, M] lp1 = matrix_1000; // log-probability for LOO for the main model
    matrix[N2, M] prediction0 = matrix_0;
    matrix[N2, M] E_deaths0  = matrix_0;
    for (m in 1:M) {
        prediction0[1:N0, m] = rep_vector(y[m], N0); // learn the number of cases in the first N0 days
        for (i in (N0+1):N2) {
            real convolution0 = 0;
            for(j in 1:(i-1)) {
                convolution0 += prediction0[j, m] * SI[i-j]; // Correctd 22nd March
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

        for(i in 1:N[m]) {
            lp0[i, m] = neg_binomial_2_lpmf(deaths[i, m] | E_deaths[i, m], phi); 
            lp1[i, m] = neg_binomial_2_lpmf(deaths[i, m] | E_deaths0[i, m], phi); 
        }
    }
}
