library(bvars)

df = 2.05
proposal_sd = 1
lambda = rep(0.2, 8)

set.seed(152)
proposal = RcppTN::rtn(df, proposal_sd, 2, Inf)
forward_density = RcppTN::dtn(proposal, df, proposal_sd, 2, Inf)
reverse_density = RcppTN::dtn(df, proposal, proposal_sd, 2, Inf)
target_ratio = exp(
  .Call("_bvars_log_kernel_df", proposal, lambda, PACKAGE = "bvars") -
    .Call("_bvars_log_kernel_df", df, lambda, PACKAGE = "bvars")
)
acceptance_probability = min(
  1,
  target_ratio * reverse_density / forward_density
)
uniform_draw = runif(1)
expected_df = if (uniform_draw < acceptance_probability) proposal else df

set.seed(152)
actual = .Call(
  "_bvars_sample_df",
  df,
  proposal_sd,
  lambda,
  0L,
  c(0.44, 0.6),
  PACKAGE = "bvars"
)

stopifnot(isTRUE(all.equal(actual$aux_df, expected_df, tolerance = 1e-14)))
