data {
	int<lower=0> n; // number of observations
	int<lower=0> d; // number of predictors
	vector[n] y; // outputs
	matrix[n,d] X; // inputs
}

parameters {
	real<lower=0> sigma;
    real w0;
    vector[d] w;
}

model {
	// observation model
	y ~ normal(w0 + X*w, sigma);
	w ~ normal(0, 1);
    w0 ~ normal(0, 5);
    sigma ~ cauchy(0, 1);
}
