library(bvars)

h = rep(1, 3)
u = rep(1, 3)
mixture = rbind(
  rep(0.1, 10),
  c(0, seq(5, 13)),
  rep(0.1, 10)
)
prior = list(sv_a = 1, sv_s = 9)
initial_sigma2_omega = 0.5
sigma2v = 0.2

set.seed(173205)
s_draws = replicate(10000, .Call(
  "_bvars_svar_ce1",
  h,
  0.5,
  sqrt(sigma2v),
  sigma2v,
  initial_sigma2_omega,
  0.1,
  rep(0L, length(h)),
  u,
  prior,
  mixture,
  TRUE,
  PACKAGE = "bvars"
)$aux_s_)

expected_s_mean = (prior$sv_s + 2 * initial_sigma2_omega) /
  (1 + 2 * prior$sv_a)
stopifnot(abs(mean(s_draws) - expected_s_mean) < 0.15)

fixed_s = 0.1
set.seed(223607)
sigma2_omega_draws = replicate(3000, .Call(
  "_bvars_svar_ce1",
  h,
  0.5,
  sqrt(sigma2v),
  sigma2v,
  initial_sigma2_omega,
  fixed_s,
  rep(0L, length(h)),
  u,
  prior,
  mixture,
  FALSE,
  PACKAGE = "bvars"
)$aux_sigma2_omega)

gamma_shape = 1 + 0.5 * prior$sv_a
gamma_scale = 1 / (1 / fixed_s + 1 / (2 * sigma2v))
stopifnot(abs(mean(sigma2_omega_draws) - gamma_shape * gamma_scale) < 0.015)
