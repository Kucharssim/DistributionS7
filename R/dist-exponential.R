#' @title Exponential distribution
#' @description Create an exponential distribution object.
#'
#' @param lambda rate parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
Exponential <- function(lambda, beta) {
  parametrization <- rlang::check_exclusive(lambda, beta)
  distribution <- switch(
    parametrization,
    lambda = ExponentialRate(lambda),
    beta = ExponentialScale(beta)
  )
  return(distribution)
}

ExponentialClass <- S7::new_class(
  "ExponentialClass",
  parent = DistributionContinuous,
  abstract = TRUE
)

ExponentialRate <- S7::new_class(
  "ExponentialRate",
  parent = ExponentialClass,
  properties = list(
    lambda = Parameter
  ),
  constructor = function(lambda) {
    S7::new_object(
      S7::S7_object(),
      name = "Exponential",
      support = Real(min=0),
      lambda = Parameter("lambda", "rate", "\\lambda", lambda, Real(min=0))
    )
  }
)

ExponentialScale <- S7::new_class(
  "ExponentialScale",
  parent = ExponentialClass,
  properties = list(
    beta = Parameter
  ),
  constructor = function(beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Exponential",
      support = Real(min=0),
      beta = Parameter("beta", "scale", "\\beta", beta, Real(min=0))
    )
  }
)


S7::method(pdf_fn, ExponentialClass) <- function(distribution) stats::dexp

S7::method(cdf_fn, ExponentialClass) <- function(distribution) stats::pexp

S7::method(qf_fn, ExponentialClass)  <- function(distribution) stats::qexp

S7::method(rng_fn, ExponentialClass) <- function(distribution) stats::rexp

S7::method(rargs, ExponentialRate) <- function(distribution) {
  return(list(rate = distribution@lambda@value))
}

S7::method(rargs, ExponentialScale) <- function(distribution) {
  return(list(rate = 1/distribution@beta@value))
}

S7::method(parameter_estimates, list(ExponentialRate, Mom)) <- function(distribution, estimator, data) {
  return(list(lambda = 1 / mean(data)))
}

S7::method(parameter_estimates, list(ExponentialScale, Mom)) <- function(distribution, estimator, data) {
  return(list(beta = mean(data)))
}

S7::method(parameter_estimates, list(ExponentialClass, Mle)) <- function(distribution, estimator, data) {
  parameter_estimates(distribution, Mom(), data)
}

S7::method(parameter_estimates, list(ExponentialRate, BiasCorrected)) <- function(distribution, estimator, data) {
  n <- length(data)
  assertthat::assert_that(n > 2, msg = "Sample size must be greater than 2.")

  return(list(lambda = (n - 2)/sum(data)))
}

S7::method(parameter_estimates, list(ExponentialScale, BiasCorrected)) <- function(distribution, estimator, data) {
  parameter_estimates(distribution, Mom(), data)
}
