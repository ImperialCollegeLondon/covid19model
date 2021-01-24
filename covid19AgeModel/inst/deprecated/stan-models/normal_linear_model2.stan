data
{
    int<lower=0> P;
    int<lower=0> N;
    matrix[N,P] mvn_mean;
    matrix[P,P] mvn_cov[N];
}

transformed data
{
    matrix[P,P] mvn_L[N];
    for( n in 1:N )
    {
        mvn_L[n] = cholesky_decompose(mvn_cov[n]);
    }
}

parameters
{
    matrix[N,P] latent_data;
	real<lower=0> sigma;
    real w0;
    vector[P-1] w;
}

model
{
    // empirical prior on latent data
    for( n in 1:N )
    {
        latent_data[n,] ~ multi_normal_cholesky_lpdf( mvn_mean[n,], mvn_L[n] );
    }
    
    // priors on regression parameters
    w ~ normal(0, 1);
    w0 ~ normal(0, 5);
    sigma ~ cauchy(0, 1);
    
	// likelihood
    latent_data[,1] ~ normal(w0 + latent_data[,2:P] * w, sigma);
}
