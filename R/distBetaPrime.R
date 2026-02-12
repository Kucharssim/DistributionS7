#' @title Beta prime distribution
#' @description Create a beta-prime distribution object.
#'
#' @param alpha shape parameter.
#' @param beta shape parameter
#' @family distributions
#' @export
BetaPrime <- S7::new_class(
  "BetaPrime",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Beta prime",
      support = Real(min = 0),
      alpha = Parameter("alpha", "shape 1", "\\alpha", alpha, Real(min=0)),
      beta = Parameter("beta", "shape 2", "\\beta", beta, Real(min=0))
    )
  }
)

S7::method(pdf_fn, BetaPrime) <- function(distribution) function(x, alpha, beta, log = FALSE) {
  lpdf <- (alpha-1) * log(x) - (alpha+beta) * log1p(x) - lbeta(alpha, beta)

  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, BetaPrime) <- function(distribution) function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  x <- q/(1+q)

  out <- pbeta(x, alpha, beta, lower.tail = lower.tail, log.p = log.p)

  return(out)
}

S7::method(qf_fn, BetaPrime) <- function(distribution) function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  x <- qbeta(p, alpha, beta, lower.tail = lower.tail, log.p = log.p)

  q <- x/(1-x)

  return(q)
}

S7::method(rng_fn, BetaPrime) <- function(distribution) function(n, alpha, beta) {
  x <- rbeta(n, alpha, beta)

  return(x/(1-x))
}

S7::method(rargs, BetaPrime) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(BetaPrime, Mme)) <- function(distribution, estimator, data) {
  parameters <- recreate_parameters(distribution)
  distribution <- do.call(Beta, parameters)
  parameter_estimates(distribution, estimator, data/(1+data))
}
