library(bvars)

probabilities = seq_len(10)
probabilities = probabilities / sum(probabilities)
means = seq(-2, 2.5, length.out = 10)
variances = seq(0.4, 2.2, length.out = 10)
mixture = rbind(probabilities, means, variances)
residuals = c(-1.25, 0.3, 2.1)

expected = unname(unlist(lapply(residuals, function(residual) {
  log_weights = log(probabilities) - 0.5 * log(variances) -
    0.5 * (residual - means)^2 / variances
  weights = exp(log_weights - max(log_weights))
  cumsum(weights / sum(weights))
})))

actual = .Call(
  "_bvars_find_mixture_indicator_cdf",
  residuals,
  mixture,
  PACKAGE = "bvars"
)

stopifnot(isTRUE(all.equal(as.numeric(actual), expected, tolerance = 1e-14)))
stopifnot(isTRUE(all.equal(actual[c(10, 20, 30)], rep(1, 3))))
