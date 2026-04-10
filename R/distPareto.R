#' @title Pareto distribution
#' @description Create a Pareto distribution object.
#'
#' @param alpha shape parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
Pareto <- S7::new_class(
  "Pareto",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Pareto",
      support = Real(min=expression(beta)),
      alpha = Parameter("alpha", "shape", "\\alpha", alpha, Real(min=0)),
      beta  = Parameter("beta",  "scale", "\\beta",  beta,  Real(min=0))
    )
  }
)

S7::method(pdf_fn, Pareto) <- function(distribution) function(x, alpha, beta, log=FALSE) {
  lpdf <- log(alpha) + alpha * log(beta) - (alpha+1) * log(x)
  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Pareto) <- function(distribution) function(q, alpha, beta, lower.tail=TRUE, log.p=FALSE) {
  # this is complementary cdf
  lcdf <- alpha * (log(beta) - log(q))

  if (lower.tail) lcdf <- log1p(-exp(lcdf))

  if (log.p) return(lcdf) else return(exp(lcdf))
}

S7::method(qf_fn, Pareto) <- function(distribution) function(p, alpha, beta, lower.tail=TRUE, log.p=FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1-p

  q <- beta/(1-p)^(1/alpha)

  return(q)
}

S7::method(rng_fn, Pareto) <- function(distribution) function(n, alpha, beta) {
  p <- runif(n, 0, 1)
  x <- qf(distribution, p)
  return(x)
}

S7::method(rargs, Pareto) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(Pareto, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@beta@fixed) {
    m <- min(data)
    estimates[["beta"]] <- m
  } else {
    m <- distribution@beta@value
  }

  if(!distribution@alpha@fixed) {
    estimates[["alpha"]] <- length(data) / sum(log(data) - log(m))
  }

  return(estimates)
}

