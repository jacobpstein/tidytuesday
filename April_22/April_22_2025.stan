data {
  int<lower=1> N;
  array[N] int<lower=0> fatalities;
  array[N] int<lower=1> day;
  array[N] int<lower=1> month;
  array[N] int<lower=1> year;
  int<lower=1> N_day;
  int<lower=1> N_month;
  int<lower=1> N_year;
}

parameters {
  real alpha;
  vector[N_day] z_day;
  vector[N_month] z_month;
  vector[N_year] z_year;
  real<lower=0> sigma_day;
  real<lower=0> sigma_month;
  real<lower=0> sigma_year;
}

transformed parameters {
  vector[N_day] day_effect = z_day * sigma_day;
  vector[N_month] month_effect = z_month * sigma_month;
  vector[N_year] year_effect = z_year * sigma_year;

  // Prior boost for Friday (4) and Saturday (5) 
  day_effect[4] += 0.5;
  day_effect[5] += 0.5;
}

model {
  // Priors
  alpha ~ normal(5, 1);
  sigma_day ~ exponential(1);
  sigma_month ~ exponential(1);
  sigma_year ~ exponential(1);
  z_day ~ normal(0, 1);
  z_month ~ normal(0, 1);
  z_year ~ normal(0, 1);

  // Likelihood
  for (n in 1:N)
    fatalities[n] ~ poisson_log(alpha + day_effect[day[n]] + month_effect[month[n]] + year_effect[year[n]]);
}

generated quantities {
  array[N] int fatalities_pred;
  array[N_day] real day_effect_exp;

  for (n in 1:N) {
    fatalities_pred[n] = poisson_log_rng(alpha + day_effect[day[n]] + month_effect[month[n]] + year_effect[year[n]]);
  }

  for (d in 1:N_day) {
    day_effect_exp[d] = exp(day_effect[d]);
  }
}
