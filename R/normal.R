normal_definition <- pars(
  list(
    par(key = "mu",    label = "\\mu",    value = 0, support = real()),
    par(key = "sigma", label = "\\sigma", value = 1, support = real(0))
  ),
  list(
    tpar(key = "sigma2", label = "\\sigma^2", transform = expression(sigma^2)),
    tpar(key = "tau",    label = "\\tau",     transform = expression(1 / sigma^2)),
    tpar(key = "kappa",  label = "\\kappa",   transform = expression(1/sigma))
  )
)

normal <- S7::new_class(
  "normal",
  parent = distribution_continuous,
  properties = normal_definition[["properties"]],
  constructor = function(mu=0, sigma=1) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      parameters = normal_definition[["pars"]],
      transformed_parameters = normal_definition[["tpars"]],
      mu=mu, sigma=sigma
    )
  })

n <- normal(sigma=5)

S7::method(pdf, normal) <- function(distribution, x, log = FALSE) {
  stats::dnorm(x=x, mean=distribution@mu, sd=distribution@sigma, log=log)
}

# S7::method(mle, normal) <- function(distribution, x) {
#   x <- na.omit(x)
#   x_bar <- mean(x)
#   sd <- sqrt(mean((x-x_bar)^2))
#   result <- normal(mu = x_bar, sigma = sd)
#
#   return(result)
# }

