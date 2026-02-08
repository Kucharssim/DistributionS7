#' @title Symmetric generalized normal distribution (Subbotin)
#' @description Create a symmetric generalized normal distribution object.
#'
#' @param mu location parameter.
#' @param alpha scale parameter.
#' @param beta shape parameter.
#' @family distributions
#' @export
symmetric_generalized_normal <- function(mu, alpha, beta) SymmetricGeneralizedNormal(mu, alpha, beta)

#' @rdname symmetric_generalized_normal
#' @export
SymmetricGeneralizedNormal <- S7::new_class(
  "SymmetricGeneralizedNormal",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(mu, alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Symmetric generalized normal (Subbotin)",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      alpha = Parameter("alpha", "scale", "\\alpha", alpha, Real(min = 0)),
      beta = Parameter("beta", "shape", "\\beta", beta, Real(min = 0))
    )
  }
)

S7::method(pdf_fn, SymmetricGeneralizedNormal) <- function(distribution) gnorm::dgnorm
S7::method(cdf_fn, SymmetricGeneralizedNormal) <- function(distribution) gnorm::pgnorm
S7::method(qf_fn,  SymmetricGeneralizedNormal) <- function(distribution) gnorm::qgnorm
S7::method(rng_fn, SymmetricGeneralizedNormal) <- function(distribution) gnorm::rgnorm
S7::method(rargs,  SymmetricGeneralizedNormal) <- function(distribution) parameter_values(distribution)

S7::method(parameter_start, SymmetricGeneralizedNormal) <- function(distribution, data) {
  start <- list()
  if (!distribution@beta@fixed) {
    beta <- 2
    start[["beta"]] <- beta # start at normal
  } else {
    beta <- distribution@beta@value
  }

  if (!distribution@mu@fixed) {
    mu <- if(beta < 2) median(data) else mean(data)
    start[["mu"]] <- mu
  } else {
    mu <- distribution@mu@value
  }

  if (!distribution@alpha@fixed) {
    start[["alpha"]] <- (beta / length(data) * sum(abs(data-mu)^beta))^(1/beta)
  }

  parameter_values(distribution) <- start

  return(distribution)
}
