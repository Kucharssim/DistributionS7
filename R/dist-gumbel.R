#' @title Gumbel distribution
#' @description Create a Gumbel distribution object.
#'
#' @param mu location parameter.
#' @param beta scale parameter.
#' @family distributions
#' @export
Gumbel <- S7::new_class(
  "Gumbel",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    beta = Parameter
  ),
  constructor = function(mu, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Gumbel",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      beta = Parameter("beta", "scale", "\\beta", beta, Real(min=0))
    )
  }
)

S7::method(pdf_fn, Gumbel) <- function(distribution) function(x, mu, beta, log=FALSE) {
  z <- (x - mu)/beta
  lpdf <- -log(beta) - (z + exp(-z))
  if (log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Gumbel) <- function(distribution) function(q, mu, beta, lower.tail = TRUE, log.p = FALSE) {
  z <- (q - mu)/beta
  out <- exp(-exp(-z))

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

S7::method(qf_fn, Gumbel) <- function(distribution) function(p, mu, beta, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- mu - beta * log(-log(p))

  return(out)
}

S7::method(rng_fn, Gumbel) <- function(distribution) function(n, mu, beta) {
  p <- runif(n, 0, 1)
  x <- qf(distribution, p)
  return(x)
}

S7::method(rargs, Gumbel) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(Gumbel, Mom)) <- function(distribution, estimator, data) {
  m <- mean(data)
  estimates <- list()

  if (!distribution@mu@fixed && !distribution@beta@fixed) {
    s <- sd(data)
    beta <- s * sqrt(6) / pi
    mu <- m - beta * (-digamma(1))
    estimates[["mu"]] <- mu
    estimates[["beta"]] <- beta
  } else if(!distribution@mu@fixed) {
    beta <- distribution@beta@value
    mu <- m - beta * (-digamma(1))
    estimates[["mu"]] <- mu
  } else if(!distribution@beta@fixed) {
    mu <- distribution@mu@value
    beta <- (m - mu) / -digamma(1)
    estimates[["beta"]] <- beta
  }

  return(estimates)
}

