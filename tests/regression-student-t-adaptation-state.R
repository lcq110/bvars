library(bvars)

set.seed(244949)
data = cbind(
  sin(seq_len(30) / 4) + rnorm(30, sd = 0.1),
  cos(seq_len(30) / 5) + rnorm(30, sd = 0.1)
)
specification = specify_bvar$new(data, p = 1, distribution = "t")

T = nrow(data) - 1
df_hessian = 0.25 * T * trigamma(15) - T * 29 / 28^2 - 2 / 29^2
expected_scale = sqrt(abs(1 / df_hessian))
initial_state = specification$starting_values$get_starting_values()

stopifnot(isTRUE(all.equal(
  initial_state$adaptive_scale,
  expected_scale,
  tolerance = 1e-14
)))
stopifnot(identical(initial_state$adaptation_iteration, 0L))

set.seed(264575)
first_part = estimate(specification, S = 4, show_progress = FALSE)
continuation_state = first_part$last_draw$starting_values$get_starting_values()

stopifnot(identical(continuation_state$adaptation_iteration, 4L))

set.seed(1)
expected = .Call(
  "_bvars_sample_df",
  continuation_state$df,
  continuation_state$adaptive_scale,
  continuation_state$lambda,
  continuation_state$adaptation_iteration,
  c(0.44, 0.6),
  PACKAGE = "bvars"
)

set.seed(1)
reset_state = .Call(
  "_bvars_sample_df",
  continuation_state$df,
  initial_state$adaptive_scale,
  continuation_state$lambda,
  0L,
  c(0.44, 0.6),
  PACKAGE = "bvars"
)
stopifnot(abs(expected$aux_df - reset_state$aux_df) > 1e-6)

set.seed(1)
continued = estimate(first_part, S = 2, show_progress = FALSE)
continued_state = continued$last_draw$starting_values$get_starting_values()

stopifnot(isTRUE(all.equal(
  continued$posterior$df[1],
  expected$aux_df,
  tolerance = 1e-14
)))
stopifnot(identical(continued_state$adaptation_iteration, 6L))
