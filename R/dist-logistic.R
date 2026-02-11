#' @title Logistic distribution
#' @description Create a logistic distribution object.
#'
#' @param mu location parameter.
#' @param sigma scale parameter.
#' @family distributions
#' @export
Logistic <- S7::new_class(
  "Logistic",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    sigma = Parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Logistic",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)

S7::method(pdf_fn, Logistic) <- function(distribution) stats::dlogis
S7::method(cdf_fn, Logistic) <- function(distribution) stats::plogis
S7::method(qf_fn,  Logistic) <- function(distribution) stats::qlogis
S7::method(rng_fn, Logistic) <- function(distribution) stats::rlogis

S7::method(rargs, Logistic) <- function(distribution) {
  list(location = distribution@mu@value, scale = distribution@sigma@value)
}

S7::method(parameter_estimates, list(Logistic, Mom)) <- function(distribution, estimator, data) {
  is_fixed <- unlist(parameter_properties(distribution, property="fixed"))

  estimates <- list()

  if (all(!is_fixed)) {
    m <- mean(data)
    s <- sd(data)

    estimates[["mu"]] <- m
    estimates[["sigma"]] <- sqrt(3) * s / pi
  } else if(!is_fixed[["mu"]]) {
    estimates[["mu"]] <- mean(data)
  } else if(!is_fixed[["sigma"]]) {
    m <- distribution@mu@value
    v <- mean((data-m)^2)
    s <- sqrt(v)
    estimates[["sigma"]] <- sqrt(3) * s / pi
  }

  return(estimates)
}
