data {
  int<lower=1> N; 
  int<lower=1> P;
  int<lower=1> M;
  int<lower=1> Q;
  vector[N] y; 
  matrix[N,P] x; // x
  int group[N]; 
}
transformed data{
  int<lower=1> B=1;
}
parameters {
  matrix[M,B] alpha_nc; 
  vector[M] intercept_nc; 
  matrix[Q,B] basis_raw_nc; 
  vector<lower=0>[P] beta_nc;
  real<lower=0> lambda_nc;
  real<lower=0> sigma_nc;
  real<lower=0> phi_nc;
  real<lower=0> phi2_nc;
}
transformed parameters{
  vector[M] intercept=rep_vector(0,M); 
  matrix[M,B] alpha=rep_matrix(0,M,B); 
  vector[P] beta=rep_vector(0,P);
  real lambda=0;
  real sigma=0;
  real phi=0;
  real phi2=0;
  vector[N] random_effects=rep_vector(0,N);
  vector[N] fixed_effects=rep_vector(0,N);
  vector[N] lp=rep_vector(0,N);
  matrix[Q,B] basis_raw=rep_matrix(0,Q,B); 
  matrix[N,B] basis=rep_matrix(0,N,B); // x
  lambda  = lambda_nc*2.0;
  phi = phi_nc*0.5;
  phi2 = phi2_nc*0.5;
  sigma = sigma_nc*0.5;
  beta = beta_nc*lambda;
  alpha= alpha_nc*phi;
  intercept = intercept*phi2;
  basis_raw=basis_raw_nc*phi2;
  {  
    int ind=0;
    for (i in 1:M){
      for(j in 1:B){
        basis[(ind+1):(ind+Q),j]=basis_raw[1:Q,j];
      }
      ind=ind+Q;
    }
  }
  fixed_effects[1:N]=x*-beta;
  for (i in 1:N){
    for(j in 1:B){
      random_effects[i] += basis[i,j]*alpha[group[i],j]  + intercept[group[i]]; 
    }
  }
  lp[1:N] = fixed_effects[1:N] +  random_effects[1:N];
  
}

model {
  beta_nc ~ std_normal();
  for (i in 1:M){
    for(j in 1:B){
      alpha_nc[i,j] ~ std_normal();
    }
  }
  for (i in 1:Q){
    for(j in 1:B){
      basis_raw_nc[i,j] ~ std_normal();
    }
  }  
  lambda_nc ~ std_normal();
  intercept_nc ~ std_normal();
  phi_nc ~ std_normal();
  phi2_nc ~ std_normal();
  sigma_nc ~ std_normal();
  y ~ normal(lp , sigma);
}
