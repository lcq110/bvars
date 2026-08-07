library(bvars)

df = 8
N = 3L
quadratic_form_per_dimension = rep(1, 20000)

set.seed(161803)
draws = .Call(
  "_bvars_sample_lambda",
  df,
  quadratic_form_per_dimension,
  N,
  PACKAGE = "bvars"
)

expected_mean = (N + df - 2) / (df + N - 2)
stopifnot(abs(mean(draws) - expected_mean) < 0.03)
