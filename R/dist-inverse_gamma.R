#' @title Inverse gamma distribution
#' @description Create an inverse gamma distribution object.
#'
#' @param alpha shape parameter.
#' @param theta scale parameter.
#' @param lambda rate parameter.
#' @param mu mean parameter.
#'
#' @note Parameter meanings are on the inverse scale, e.g., `mu` is the mean of 1/x.
#' @family distributions
#' @export
InverseGamma <- function(alpha, theta, lambda, mu) {
  parametrization <- rlang::check_exclusive(theta, lambda, mu)
  distribution <- switch(
    parametrization,
    theta  = InverseGammaScale(alpha, theta),
    lambda = InverseGammaRate (alpha, lambda),
    mu     = InverseGammaMean (alpha, mu)
  )
  return(distribution)
}

InverseGammaClass <- S7::new_class(
  "InverseGammaClass",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter
  ),
  abstract=TRUE
)

InverseGammaScale <- S7::new_class(
  "InverseGammaScale",
  parent = InverseGammaClass,
  properties = list(
    theta = Parameter
  ),
  constructor = function(alpha, theta) {
    S7::new_object(
      S7::S7_object(),
      name = "Inverse gamma",
      support = Real(min=0),
      alpha = Parameter("alpha", "shape", "\\alpha", alpha, Real(min=0)),
      theta = Parameter("theta", "scale", "\\theta", theta, Real(min=0))
    )
  }
)

InverseGammaRate <- S7::new_class(
  "InverseGammaRate",
  parent = InverseGammaClass,
  properties = list(
    lambda = Parameter
  ),
  constructor = function(alpha, lambda) {
    S7::new_object(
      S7::S7_object(),
      name = "Inverse gamma",
      support = Real(min=0),
      alpha  = Parameter("alpha", "shape", "\\alpha",  alpha,  Real(min=0)),
      lambda = Parameter("lambda", "rate", "\\lambda", lambda, Real(min=0))
    )
  }
)

InverseGammaMean <- S7::new_class(
  "InverseGammaMean",
  parent = InverseGammaClass,
  properties = list(
    mu = Parameter
  ),
  constructor = function(alpha, mu) {
    S7::new_object(
      S7::S7_object(),
      name = "Inverse gamma",
      support = Real(min=0),
      alpha  = Parameter("alpha", "shape", "\\alpha", alpha,  Real(min=0)),
      mu     = Parameter("mu",    "inverse mean",  "\\mu",    mu,     Real(min=0))
    )
  }
)

S7::method(pdf_fn, InverseGammaClass) <- function(distribution) function(x, ..., log=FALSE) {
  xinv <- 1/x
  infinite <- is.infinite(xinv)
  lpdf <- numeric(length(x))
  lpdf[infinite] <- -Inf
  lpdf[!infinite] <- stats::dgamma(xinv[!infinite], ..., log=TRUE) - 2*log(x[!infinite])

  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, InverseGammaClass) <- function(distribution) function(q, ..., lower.tail=TRUE) {
  stats::pgamma(1/q, ..., lower.tail=!lower.tail)
}

S7::method(qf_fn, InverseGammaClass)  <- function(distribution) function(p, ...) {
  1/stats::qgamma(1-p, ...)
}

S7::method(rng_fn, InverseGammaClass) <- function(distribution) function(n, ...) {
  1/stats::rgamma(n, ...)
}

S7::method(rargs, InverseGammaScale) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, scale=distribution@theta@value))
}

S7::method(rargs, InverseGammaRate) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, rate=distribution@lambda@value))
}

S7::method(rargs, InverseGammaMean) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, scale=distribution@mu@value/distribution@alpha@value))
}

S7::method(parameter_estimates, list(InverseGammaClass, Estimator)) <- function(distribution, estimator, data) {
  parameters <- recreate_parameters(distribution)
  distribution <- do.call(gamma, parameters)
  parameter_estimates(distribution, estimator, 1/data)
}
