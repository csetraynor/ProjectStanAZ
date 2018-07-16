/*  Variable naming:
 // dimensions
 N          = total number of observations (length of data)
 S          = number of sample ids
 T          = max timepoint (number of timepoint ids)
 M          = number of covariates

 // data
 s          = sample id for each obs
 t          = timepoint id for each obs
 event      = integer indicating if there was an event at time t for sample s
 x          = matrix of real-valued covariates at time t for sample n [N, X]
 obs_t      = observed end time for interval for timepoint for that obs

*/

// Carlos Traynor <carlos.serratraynor@astrazeneca.com>
functions { 
} 
data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K;  // number of population-level effects 
  matrix[N, K] X;  // population-level design matrix 
  // data of smooth s(time)
  int nb_1;  // number of bases 
  int knots_1[nb_1]; 
  matrix[N, knots_1[1]] Zs_1_1; 
  vector[N] offset; 
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
  int Kc = K - 1; 
  matrix[N, K - 1] Xc;  // centered version of X 
  vector[K - 1] means_X;  // column means of X before centering 
  for (i in 2:K) { 
    means_X[i - 1] = mean(X[, i]); 
    Xc[, i - 1] = X[, i] - means_X[i - 1]; 
  } 
} 
parameters { 
  vector[Kc] b;  // population-level effects 
  real temp_Intercept;  // temporary intercept 
  // parameters of smooth s(time)
  vector[knots_1[1]] zs_1_1; 
  real<lower=0> sds_1_1; 
} 
transformed parameters { 
  vector[knots_1[1]] s_1_1 = sds_1_1 * zs_1_1; 
} 
model { 
  vector[N] mu = Xc * b + Zs_1_1 * s_1_1 + temp_Intercept + offset; 
  // priors including all constants 
  target += student_t_lpdf(temp_Intercept | 3, -2, 10); 
  target += normal_lpdf(zs_1_1 | 0, 1); 
  target += student_t_lpdf(sds_1_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  // likelihood including all constants 
  if (!prior_only) { 
    target += poisson_log_lpmf(Y | mu); 
  } 
} 
generated quantities { 
  // actual population-level intercept 
  real b_Intercept = temp_Intercept - dot_product(means_X, b); 
} 
