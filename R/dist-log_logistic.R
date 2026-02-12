#' @title Log-logistic distribution
#' @description Create a log-logistic distribution object.
#'
#' @param mu location parameter.
#' @param sigma scale parameter.
#' @param alpha scale parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
LogLogistic <- function(mu, sigma, alpha, beta) {
  parametrization <- rlang::check_exclusive(mu, alpha)
  rlang::check_exclusive(mu, beta)
  rlang::check_exclusive(sigma, alpha)
  rlang::check_exclusive(sigma, beta)

  distribution <- switch(
    parametrization,
    mu = LogLogisticLocation(mu, sigma),
    alpha = LogLogisticScale(alpha, beta)
  )
  return(distribution)
}

LogLogisticClass <- S7::new_class(
  "LogLogisticClass",
  parent = DistributionContinuous,
  abstract = TRUE
)

LogLogisticLocation <- S7::new_class(
  "LogLogisticLocation",
  parent = LogLogisticClass,
  properties = list(
    mu = Parameter,
    sigma = Parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Log-logistic",
      support = Real(min=0),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)

LogLogisticScale <- S7::new_class(
  "LogLogisticScale",
  parent = LogLogisticClass,
  properties = list(
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Log-logistic",
      support = Real(min=0),
      alpha = Parameter("alpha", "scale", "\\alpha", alpha,Real(min=0)),
      beta  = Parameter("beta", "shape",  "\\beta",  beta, Real(min=0))
    )
  }
)

S7::method(pdf_fn, LogLogisticClass) <- function(distribution) function(x, alpha, beta, log = FALSE) {
  lpdf <- log(beta) - log(alpha) + (beta-1) * (log(x) - log(alpha)) - 2 * log1p((x/alpha)^beta)

  if(log) return(lpdf) else exp(lpdf)
}

S7::method(cdf_fn, LogLogisticClass) <- function(distribution) function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  cdf <- 1 / (1+(q/alpha)^(-beta))

  if(!lower.tail) cdf <- 1-cdf

  if(log.p) return(log(cdf)) else return(cdf)
}

S7::method(qf_fn, LogLogisticClass) <- function(distribution) function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  x <- stats::qlogis(p = p, location = log(alpha), scale = 1/beta, lower.tail = lower.tail, log.p = log.p)
  x <- exp(x)

  return(x)
}

S7::method(rng_fn, LogLogisticClass) <- function(distribution) function(n, alpha, beta) {
  x <- stats::rlogis(n, location = log(alpha), scale = 1/beta)
  x <- exp(x)

  return(x)
}

S7::method(rargs, LogLogisticLocation) <- function(distribution) {
  list(alpha = exp(distribution@mu@value), beta = 1/distribution@sigma@value)
}

S7::method(rargs, LogLogisticScale) <- function(distribution) {
  parameter_values(distribution)
}

S7::method(parameter_estimates, list(LogLogisticLocation, Estimator)) <- function(distribution, estimator, data) {
  data <- log(data)
  parameters <- recreate_parameters(distribution)
  distribution <- do.call(Logistic, parameters)

  parameter_estimates(distribution, estimator, data)
}

S7::method(parameter_estimates, list(LogLogisticScale, Estimator)) <- function(distribution, estimator, data) {
  data <- log(data)
  parameters <- recreate_parameters(distribution)
  distribution <- Logistic(
    mu = log(parameters[["alpha"]]),
    sigma = 1/parameters[["beta"]]
  )
  est <- parameter_estimates(distribution, estimator, data)
  estimates <- list()
  if (!is.null(est[["mu"]]))    estimates[["alpha"]] <- exp(est[["mu"]])
  if (!is.null(est[["sigma"]])) estimates[["beta"]] <- 1/est[["sigma"]]

  return(estimates)
}
