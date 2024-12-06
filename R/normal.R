# normal <- S7::new_class(
#   "normal",
#   parent = distribution_continuous,
#   properties = parameter_properties(c("mu", "sigma", "sigma2")),
#   constructor = function(mu, sigma, sigma2) {
#     parametrization <- rlang::check_exclusive(sigma, sigma2)
#
#     S7::new_object(
#       S7::S7_object(),
#       name = "Normal",
#       support = real(),
#       parameters = normal_definition[["pars"]],
#       transformed_parameters = normal_definition[["tpars"]],
#       mu=mu, sigma=sigma
#     )
#   })
#
# n <- normal(sigma=5)
#
# S7::method(pdf, normal) <- function(distribution, x, log = FALSE) {
#   stats::dnorm(x=x, mean=distribution@mu, sd=distribution@sigma, log=log)
# }
#
# S7::method(mle, normal) <- function(distribution, x) {
#   x <- na.omit(x)
#   x_bar <- mean(x)
#   sd <- sqrt(mean((x-x_bar)^2))
#   result <- normal(mu = x_bar, sigma = sd)
#
#   return(result)
# }
#
