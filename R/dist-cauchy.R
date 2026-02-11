#' @title Cauchy distribution
#' @description Create a Cauchy distribution object.
#'
#' @param mu location parameter.
#' @param sigma scale parameter.
#' @family distributions
#' @export
Cauchy <- S7::new_class(
  "Cauchy",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    sigma = Parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Cauchy",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)


S7::method(pdf_fn, Cauchy) <- function(distribution) stats::dcauchy

S7::method(cdf_fn, Cauchy) <- function(distribution) stats::pcauchy

S7::method(qf_fn, Cauchy)  <- function(distribution) stats::qcauchy

S7::method(rng_fn, Cauchy) <- function(distribution) stats::rcauchy

S7::method(rargs, Cauchy) <- function(distribution, ...) {
  return(list(location=distribution@mu@value, scale=distribution@sigma@value))
}

S7::method(parameter_estimates, list(Cauchy, Mme)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (!is.fixed(distribution@mu)) estimates[["mu"]] <- median(data)
  if (!is.fixed(distribution@sigma)) estimates[["sigma"]] <- stats::IQR(data, type = 8)/2

  return(estimates)
}
