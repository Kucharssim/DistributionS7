#' @title Chi-squared distribution
#' @description Create a chi-squared distribution object.
#'
#' @param nu degrees of freedom paramter.
#' @param kappa noncentrality parameter.
#' @family distributions
#' @export
ChiSquared <- S7::new_class(
  "ChiSquared",
  parent = DistributionContinuous,
  properties = list(nu = Parameter),
  constructor = function(nu) {
    S7::new_object(
      S7::S7_object(),
      name = "Chi-squared",
      support = Real(min=0),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0))
    )
  }
)

#' @rdname ChiSquared
#' @export
NoncentralChiSquared <- S7::new_class(
  "NoncentralChiSquared",
  parent = ChiSquared,
  properties = list(kappa = Parameter),
  constructor = function(nu, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral chi-squared",
      support = Real(min=0),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0)),
      kappa = Parameter("kappa", "noncentrality parameter", "\\kappa", kappa, Real(min=0))
    )
  }
)

S7::method(pdf_fn, ChiSquared) <- function(distribution) stats::dchisq
S7::method(cdf_fn, ChiSquared) <- function(distribution) stats::pchisq
S7::method(qf_fn,  ChiSquared) <- function(distribution) stats::qchisq
S7::method(rng_fn, ChiSquared) <- function(distribution) stats::rchisq
S7::method(rargs,  ChiSquared) <- function(distribution) {
  list(df = distribution@nu@value, ncp = 0)
}
S7::method(rargs, NoncentralChiSquared) <- function(distribution) {
  list(df = distribution@nu@value, ncp = distribution@kappa@value)
}

S7::method(parameter_estimates, list(ChiSquared, Mme)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@nu@fixed) estimates[["nu"]] <- mean(data)
  return(estimates)
}

S7::method(parameter_estimates, list(NoncentralChiSquared, Mme)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@nu@fixed && !distribution@kappa@fixed) {
    m <- mean(data)
    v <- var(data)

    nu <- 2*m - v/2
    kappa <- m - nu
    assertthat::assert_that(nu > 0)
    assertthat::assert_that(kappa > 0)
    estimates[["nu"]] <- nu
    estimates[["kappa"]] <- kappa
  } else if (!distribution@nu@fixed) {
    m <- mean(data)
    nu <- m - distribution@kappa@value
    assertthat::assert_that(nu > 0)
    estimates[["nu"]] <- nu
  } else if (!distribution@kappa@fixed) {
    m <- mean(data)
    kappa <- m - distribution@mu@value
    assertthat::assert_that(kappa > 0)
    estimates[["kappa"]] <- kappa
  }

  return(estimates)
}


