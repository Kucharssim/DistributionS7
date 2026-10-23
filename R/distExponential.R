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

S7::method(parameter_estimates, list(ExponentialRate, Mme)) <- function(distribution, estimator, data) {
  return(list(lambda = 1 / mean(data)))
}

S7::method(parameter_estimates, list(ExponentialScale, Mme)) <- function(distribution, estimator, data) {
  return(list(beta = mean(data)))
}

S7::method(parameter_estimates, list(ExponentialClass, Mle)) <- function(distribution, estimator, data) {
  parameter_estimates(distribution, Mme(), data)
}

S7::method(parameter_estimates, list(ExponentialRate, BiasCorrected)) <- function(distribution, estimator, data) {
  n <- length(data)
  assertthat::assert_that(n > 1, msg = "Sample size must be greater than 1.")

  return(list(lambda = (n - 1)/sum(data)))
}

S7::method(parameter_estimates, list(ExponentialScale, BiasCorrected)) <- function(distribution, estimator, data) {
  parameter_estimates(distribution, Mme(), data)
}

S7::method(parameter_inference, list(ExponentialClass, DefaultMethod)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  if (length(estimates) == 0) {
    distribution <- S7::super(distribution, Distribution)
    return(parameter_inference(distribution, inference_method, data))
  }

  key <- names(estimates)
  estimate <- estimates[[key]]
  n <- length(data)
  # 2 * rate * sum(data) ~ chi-squared(2n), which gives exact intervals regardless of the estimator
  total <- sum(data)
  q_lower <- stats::qchisq(inference_method@lower, df=2*n)
  q_upper <- stats::qchisq(inference_method@upper, df=2*n)

  if (key == "lambda") {
    # the unbiased rate estimate (n-1)/sum(data) has variance rate^2/(n-2)
    scale <- if (S7::S7_inherits(inference_method@estimator, BiasCorrected)) n-2 else n
    se <- estimate / sqrt(scale)
    lower <- q_lower / (2*total)
    upper <- q_upper / (2*total)
  } else {
    se <- estimate / sqrt(n)
    lower <- 2*total / q_upper
    upper <- 2*total / q_lower
  }

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}
