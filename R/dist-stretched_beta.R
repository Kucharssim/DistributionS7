#' @title Stretched beta distribution
#' @description Create a stretched beta distribution object.
#'
#' @param alpha shape parameter.
#' @param beta shape parameter.
#' @param min minimum parameter.
#' @param max maximum parameter.
#' @family distributions
#' @export
StretchedBeta <- S7::new_class(
  "StretchedBeta",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter,
    beta = Parameter,
    min = Parameter,
    max = Parameter,
    range = S7::new_property(S7::class_numeric, getter = function(self) self@max@value - self@min@value)
  ),
  constructor = function(alpha, beta, min, max) {
    S7::new_object(
      S7::S7_object(),
      name = "Stretched beta",
      support = Real(min=expression(min), max=expression(max)),
      alpha = Parameter("alpha", "shape 1", "\\alpha", alpha, Real(min=0)),
      beta = Parameter("beta", "shape 2", "\\beta", beta, Real(min=0)),
      min = Parameter("min", "minimum", "\\text{min}", min, Real(), fixed=TRUE),
      max = Parameter("min", "maximum", "\\text{max}", max, Real(), fixed=TRUE)
    )
  },
  validator = function(self) {
    assertthat::assert_that(self@min@value < self@max@value)
    assertthat::assert_that(self@min@fixed)
    assertthat::assert_that(self@max@fixed)
    return(NULL)
  }
)

S7::method(pdf_fn, StretchedBeta) <- function(distribution) function(x, alpha, beta, min, max, log=FALSE) {
  x <- (x-min)/(max-min)
  stats::dbeta(x, alpha, beta, log=log)
}

S7::method(cdf_fn, StretchedBeta) <- function(distribution) function(q, alpha, beta, min, max, lower.tail=TRUE, log.p=FALSE) {
  q <- (q-min)/(max-min)
  stats::qbeta(q, alpha, beta, lower.tail = lower.tail, log.p = log.p)
}

S7::method(qf_fn, StretchedBeta) <- function(distribution) function(p, alpha, beta, min, max, lower.tail=TRUE, log.p=FALSE) {
  q <- stats::qbeta(p, alpha, beta, lower.tail = lower.tail, log.p = log.p)
  q * (max-min) + min
}

S7::method(rng_fn, StretchedBeta) <- function(distribution) function(n, alpha, beta, min, max) {
  x <- stats::rbeta(n, alpha, beta)
  x * (max-min) + min
}

S7::method(rargs, StretchedBeta) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(StretchedBeta, Mme)) <- function(distribution, estimator, data) {
  parameters <- recreate_parameters(distribution)
  min <- parameters[["min"]]
  max <- parameters[["max"]]
  parameters[["min"]] <- NULL
  parameters[["max"]] <- NULL
  distribution <- do.call(Beta, parameters)
  parameter_estimates(distribution, estimator, (data-min) / (max-min))
}
