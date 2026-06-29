#' @title Shash distribution
#' @description Create a Shash distribution object.
#' 
#' @param mu location parameter.
#' @param sigma scale parameter.
#' @param delta tailweight parameter.
#' @param epsilon skewness parameter.
#' @family distributions
#' @export
Shash <- S7::new_class(
  "Shash",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    sigma = Parameter,
    epsilon = Parameter,
    delta = Parameter
  ),
  constructor = function(mu, sigma, epsilon, delta) {
    S7::new_object(
      S7::S7_object(),
      name = "Shash",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0)),
      epsilon = Parameter("epsilon", "assymetry", "\\epsilon", epsilon, Real()),
      delta = Parameter("delta", "tailweight", "\\delta", delta, Real(min=0))
    )
  }
)

log_cosh <- function(x) {
  abs_x <- abs(x)
  abs_x + log1p(exp(-2 * abs_x)) - log(2)
}

S7::method(pdf_fn, Shash) <- function(distribution) function(x, mu, sigma, delta, epsilon, log=FALSE) {
  z <- (x-mu) / sigma
  lpdf <- log(delta) - (log(sigma) + 0.5 * log(2*pi))
  lpdf <- lpdf + log_cosh(delta * asinh(z) + epsilon) - 0.5 * log1p(z^2)
  lpdf <- lpdf - 0.5 * (sinh(delta * asinh(z) + epsilon))^2

  if (log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Shash) <- function(distribution) function(q, mu, sigma, delta, epsilon, lower.tail=TRUE, log.p=FALSE) {
  z <- (q-mu) / sigma
  p <- pnorm(sinh(delta * asinh(z) + epsilon), lower.tail=lower.tail, log.p=log.p)
  return(p)
}

S7::method(qf_fn, Shash) <- function(distribution) function(p, mu, sigma, delta, epsilon, lower.tail=TRUE, log.p=FALSE) {
  q <- mu + sigma * sinh((qnorm(p, log.p=log.p) - epsilon) / delta)
  return(q)
}

S7::method(rng_fn, Shash) <- function(distribution) function(n, mu, sigma, delta, epsilon) {
  y <- rnorm(n, 0, 1)
  x <- mu + sigma * sinh((asinh(y) - epsilon) / delta)
  return(x)
}

S7::method(rargs, Shash) <- function(distribution) {
  parameter_values(distribution)
}

S7::method(parameter_start, Shash) <- function(distribution, data) {
  parameters <- list()
  if (distribution@mu@free)      parameters[["mu"]]      <- mean(data)
  if (distribution@sigma@free)   parameters[["sigma"]]   <- sd(data)
  if (distribution@epsilon@free) parameters[["epsilon"]] <- 0
  if (distribution@delta@free)   parameters[["delta"]]   <- 1
  return(parameters)
}