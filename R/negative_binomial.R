# NegativeBinomial <- S7::new_class(
#   "NegativeBinomial",
#   parent = DistributionDiscrete,
#   abstract = TRUE
# )
#
# NegativeBinomialMean <- S7::new_class(
#   "NegativeBinomialMean",
#   parent = NegativeBinomial,
#   properties = list(
#     mu = Parameter,
#     phi = Parameter
#   ),
#   constructor = function(mu, phi) {
#     S7::new_object(
#       S7::S7_object(),
#       name = "NegativeBinomial",
#       support = Int(min=0),
#       mu = Parameter("mu", "mean", "\\mu", mu, Real(min=0)),
#       phi = Parameter("phi", "dispersion", "\\phi", phi, Real(min=0))
#     )
#   }
# )
#
# NegativeBinomialProb <- S7::new_class(
#   "NegativeBinomialProb",
#   parent = NegativeBinomial,
#   properties = list(
#     p = Parameter,
#     k = Parameter
#   ),
#   constructor = function(p, k) {
#     S7::new_object(
#       S7::S7_object(),
#       name = "NegativeBinomial",
#       support = Int(min=0),
#       p = Parameter("p", "probability of success", "p", p, Real(min=0, max=1)),
#       k = Parameter("k", "number of successes", "k", k, Real(min=0))
#     )
#   }
# )
#
# negative_binomial <- function(mu, phi, p, k) {
#   parametrization <- rlang::check_exclusive(mu, p)
#   rlang::check_exclusive(mu, k)
#   rlang::check_exclusive(phi, k)
#
#   distribution <- switch (parametrization,
#     mu = NegativeBinomialMean(mu, phi),
#     p  = NegativeBinomialProb(p, k)
#   )
#
#   return(distribution)
# }
#
#
# S7::method(pdf_fn, NegativeBinomial) <- function(distribution) stats::dnbinom
#
# S7::method(cdf_fn, NegativeBinomial) <- function(distribution) stats::pnbinom
#
# S7::method(qf_fn, NegativeBinomial)  <- function(distribution) stats::qnbinom
#
# S7::method(rng_fn, NegativeBinomial) <- function(distribution) stats::rnbinom
#
#
#
# S7::method(rargs, NegativeBinomialMean) <- function(distribution, ...) {
#   return(list(
#     mu   = distribution@mu@value,
#     size = distribution@phi@value
#   ))
# }
#
# S7::method(rargs, NegativeBinomialProb) <- function(distribution, ...) {
#   return(list(
#     prob = distribution@p@value,
#     size = distribution@k@value
#   ))
# }
#
#
# S7::method(expectation, NegativeBinomialMean) <- function(distribution, ...) distribution@mu@value
#
# S7::method(expectation, NegativeBinomialProb) <- function(distribution, ...) {
#   pars <- parameter_values(distribution)
#   with(pars, k*(1-p)/p)
# }
#
# S7::method(variance, NegativeBinomialMean)  <- function(distribution, ...) {
#   pars <- parameter_values(distribution)
#   with(pars, mu + mu^2/phi)
# }
#
# S7::method(variance, NegativeBinomialProb) <- function(distribution, ...) {
#   pars <- parameter_values(distribution)
#   with(pars, k*(1-p)/p^2)
# }
#
# S7::method(skewness, NegativeBinomialProb) <- function(distribution, ...) {
#   pars <- parameter_values(distribution)
#   with(pars, 2-p / sqrt((1-p)*k) )
# }
#
# S7::method(excess_kurtosis, NegativeBinomialProb) <- function(distribution, ...) {
#   pars <- parameter_values(distribution)
#   with(pars, 6 / k + p^2 / ((1-p)*k) )
# }
#
#
#
# S7::method(point_estimates, NegativeBinomialMean) <- function(distribution, data, ...) {
#   fixed <- parameter_properties(distribution, "fixed") |> unlist()
#
#   if (all(!fixed)) {
#     # starting points using method of moments...
#     m <- mean(data)
#     v <- var(data)
#     distribution@mu@value <- m
#     distribution@phi@value <- m^2 / (v-m)
#   }
#
#   d <- S7::super(distribution, DistributionDiscrete)
#
#   point_estimates(distribution=d, data=data, ...)
# }
#
# S7::method(point_estimates, NegativeBinomialProb) <- function(distribution, data, ...) {
#   fixed <- parameter_properties(distribution, "fixed") |> unlist()
#
#   if (all(!fixed)) {
#     # starting points using method of moments...
#     m <- mean(data)
#     v <- var(data)
#     distribution@p@value <- m / v
#     distribution@k@value <- m^2 / (v - m)
#   }
#
#   d <- S7::super(distribution, DistributionDiscrete)
#
#   point_estimates(distribution=d, data=data, ...)
# }
