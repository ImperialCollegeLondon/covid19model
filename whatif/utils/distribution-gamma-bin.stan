data {
  int N;
  int N2;
  int y[N];
  real x[N];
  real xp[N2];
  real TOTAL;
}
parameters {
  real<lower = 0> shape;
  real<lower = 0> scale;
}
transformed parameters {
  vector[N] fit;
  fit[1] = exp(gamma_lcdf(x[1]+0.5 | shape,scale)) ;
  for(i in 2:N){
      fit[i] =exp(gamma_lcdf(x[i]+0.5 | shape,scale)) - exp(gamma_lcdf(x[i]-0.5 | shape,scale));
  }
}
model {
  // Priors
  shape ~ normal(0, 5);
  scale ~ normal(0, 5);
  y~poisson(TOTAL*fit);
} 
generated quantities{
  vector[N2] lp;
  for(i in 1:N2){
    lp[i] = poisson_rng(TOTAL*exp(gamma_lpdf(xp[i] | shape,scale)));
  }
}
