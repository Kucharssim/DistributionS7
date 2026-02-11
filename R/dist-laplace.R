#' @title Laplace distribution
#' @description Create a Laplace distribution object.
#'
#' @param mu location parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
Laplace <- S7::new_class(
  "Laplace",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    beta = Parameter
  ),
  constructor = function(mu, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Laplace",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      beta = Parameter("beta", "scale", "\\beta", beta, Real(min=0))
    )
  }
)

S7::method(pdf_fn, Laplace) <- function(distribution) function(x, mu, beta, log=FALSE) {
  lpdf <- - log(2*beta) - abs(x-mu)/beta

  if (log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Laplace) <- function(distribution) function(q, mu, beta, lower.tail=TRUE, log.p=FALSE) {
  lower <- q <= mu
  cdf <- numeric(length(q))

  x <- (q-mu)/beta
  cdf[lower]  <- exp(x[lower])/2
  cdf[!lower] <- 1-exp(-x[!lower])/2

  if (!lower.tail) cdf <- 1-cdf
  if (log.p) return(log(cdf)) else return(cdf)
}

S7::method(qf_fn, Laplace) <- function(distribution) function(p, mu, beta, lower.tail=TRUE, log.p=FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1-p

  q <- mu - beta * sign(p - 0.5) * log(1 - 2 * abs(p-0.5))
}

S7::method(rng_fn, Laplace) <- function(distribution) function(n, mu, beta) {
  p <- runif(n, 0, 1)
  x <- qf(distribution, p)
  return(x)
}

S7::method(rargs, Laplace) <- function(distribution) {
  parameter_values(distribution)
}

S7::method(parameter_estimates, list(Laplace, Mom)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (!distribution@mu@fixed) {
    m <- mean(data)
    s <- sd(data)
    estimates[["mu"]] <- m
  } else {
    m <- distribution@mu@value
    v <- mean((data-mu)^2)
    s <- sqrt(v)
  }

  if (!distribution@beta@fixed) estimates[["beta"]] <- s / sqrt(2)

  return(estimates)
}

S7::method(parameter_estimates, list(Laplace, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (!distribution@mu@fixed) {
    m <- median(data)
    estimates[["mu"]] <- m
  } else {
    m <- distribution@mu@value
  }

  if (!distribution@beta@fixed) estimates[["beta"]] <- mean(abs(data-m))

  return(estimates)
}
