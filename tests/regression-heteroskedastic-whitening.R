library(bvars)

Y = matrix(c(0, 0, 0, 10), 1)
X = matrix(1, 1, 4)
Omega_inv = c(100, 100, 100, 0.01)
V_inv = matrix(0.01)
prior = list(A = matrix(0), S = matrix(1), nu = 4)

weights = Omega_inv
expected_mean = as.numeric(
  (Y %*% t(X * weights) + prior$A %*% V_inv) /
    (X %*% t(X * weights) + V_inv)
)

set.seed(271828)
draws = replicate(
  500,
  .Call(
    "_bvars_sample_ASigma",
    Y,
    X,
    V_inv,
    Omega_inv,
    prior,
    PACKAGE = "bvars"
  )[[1]][1, 1]
)

stopifnot(abs(mean(draws) - expected_mean) < 0.02)
