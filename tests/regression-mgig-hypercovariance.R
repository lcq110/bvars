library(bvars)

data("us_fiscal_lsuw", package = "bsvars")
set.seed(1)
specification = specify_bvar$new(us_fiscal_lsuw[1:250, ], p = 4)
posterior = estimate(specification, S = 50, show_progress = FALSE)$posterior

V = posterior$V
stopifnot(identical(dim(V), c(13L, 13L, 50L)))
stopifnot(all(is.finite(V)))
stopifnot(any(abs(V[, , 2] - V[, , 1]) > 1e-12))
stopifnot(all(vapply(
  seq_len(dim(V)[3]),
  function(s) min(eigen(V[, , s], symmetric = TRUE, only.values = TRUE)$values) > 0,
  logical(1)
)))
