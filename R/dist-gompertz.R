#' @title Gompertz distribution
#' @description Create a Gompertz distribution object.
#'
#' @param eta shape parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
gompertz <- function(eta, beta) Gompertz(eta, beta)

#' @rdname gompertz
#' @export
Gompertz <- S7::new_class(
  "Gompertz",
  parent = DistributionContinuous,
  properties = list(
    eta = Parameter,
    beta = Parameter
  ),
  constructor = function(eta, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Gompertz",
      support = Real(min=0),
      eta = Parameter("eta", "shape", "\\eta", eta, Real(min=0)),
      beta = Parameter("beta", "scale", "\\beta", beta, Real(min=0))
    )
  }
)

S7::method(pdf_fn, Gompertz) <- function(distribution) function(x, eta, beta, log = FALSE) {
  lpdf <- log(beta) + log(eta) + (eta + beta*x - eta * exp(beta*x))

  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Gompertz) <- function(distribution) function(q, eta, beta, lower.tail = TRUE, log.p = FALSE) {
  out <- exp( -eta*(exp(beta*q) - 1) )

  if(lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

S7::method(qf_fn, Gompertz) <- function(distribution) function(p, eta, beta, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- 1/beta * log(1 - 1/eta * log1p(-p))

  return(out)
}

S7::method(rng_fn, Gompertz) <- function(distribution) function(n, eta, beta) {
  p <- runif(n, 0, 1)
  x <- qf(distribution, p)
  return(x)
}

S7::method(rargs, Gompertz) <- function(distribution) parameter_values(distribution)
