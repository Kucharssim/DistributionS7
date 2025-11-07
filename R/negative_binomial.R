# negative_binomial <- S7::new_class(
#   "negative_binomial",
#   parent = distribution_discrete,
#   properties = parameter_properties(c("n", "p", "mu")),
#   constructor = function(n, p, mu) {
#     parametrization <- rlang::check_exclusive(p, mu)
#
#     parameters <- switch(
#       parametrization,
#       p = pars(
#         par (key = "n",  value = n, support = real(0)),
#         par (key = "p",  value = p, support = real(0, 1)),
#         tpar(key = "mu", value = expression(n / p - n), update = list(p = expression(n / (n + mu)))),
#         rargs=list(
#           size = expression(n),
#           prob = expression(p)
#         )
#       ),
#       mu = pars(
#         par (key = "n",  value = n,  support = real(0)),
#         par (key = "mu", value = mu, support = real(0)),
#         tpar(key = "p",  value = expression(n / (n + mu)), update = list(mu = expression(n / p - n))),
#         rargs=list(
#           size = expression(n),
#           mu = expression(mu)
#         )
#       )
#     )
#
#     S7::new_object(
#       S7::S7_object(),
#       name = "Negative binomial",
#       support = int(0),
#       parameters = parameters
#     )
#   }
# )
#
# S7::method(pdf_fn, negative_binomial) <- function(distribution) stats::dnbinom
#
# S7::method(rng_fn, negative_binomial) <- function(distribution) stats::rnbinom
