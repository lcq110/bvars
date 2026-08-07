library(bvars)

h = c(1, 1)
u = c(1, 1)
omega = 2
mixture = rbind(
  rep(0.1, 10),
  c(0, -1, seq(5, 12)),
  rep(0.0001, 10)
)

set.seed(141421)
draw = .Call(
  "_bvars_svar_ce1",
  h,
  0.5,
  omega,
  0.2,
  0.5,
  0.2,
  rep(0L, length(h)),
  u,
  list(sv_a = 1, sv_s = 0.1),
  mixture,
  FALSE,
  PACKAGE = "bvars"
)

stopifnot(identical(as.integer(draw$aux_S), rep(0L, length(h))))
