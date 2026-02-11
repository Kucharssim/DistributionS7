#' @title Amoroso distribution
#' @description Create a normal distribution object.
#'
#' @param a location parameter.
#' @param theta scale parameter.
#' @param alpha shape parameter.
#' @param beta shape parameter.
#'
#' @family distributions
#' @export
Amoroso <- S7::new_class(
  "Amoroso",
  parent = DistributionContinuous,
  properties = list(
    a = Parameter,
    theta = Parameter,
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(a, theta, alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Amoroso",
      support = Real(
        min = expression(if(theta > 0) a else -Inf),
        max = expression(if(theta > 0) Inf else a)
      ),
      a = Parameter("a", "location", "a", a, Real(), fixed=TRUE),
      theta = Parameter("theta", "scale", "\\theta", theta, Real()),
      alpha = Parameter("alpha", "shape", "\\alpha", alpha, Real(min=0)),
      beta = Parameter("beta", "shape", "\\beta", beta, Real())
    )
  }
)


S7::method(pdf_fn, Amoroso) <- function(distribution) function(x, a, theta, alpha, beta, log = FALSE) {
  terms <- list()
  xi <- (x-a)/theta

  terms[[1]] <- -lgamma(alpha)
  terms[[2]] <- log(abs(beta/theta))
  terms[[3]] <- (alpha*beta - 1) * log(xi)
  terms[[4]] <- - xi^beta

  out <- Reduce("+", terms)
  if(theta > 0) {
    out[x<a] <- -Inf
  } else {
    out[x>a] <- -Inf
  }
  if(!log) out <- exp(out)

  return(out)
}

S7::method(cdf_fn, Amoroso) <- function(distribution) function(q, a, theta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  xi <- (q-a)/theta
  arg1 <- alpha
  arg2 <- xi^beta
  out <- stats::pgamma(q=arg2, scale = 1, shape = arg1, lower.tail = lower.tail, log.p = FALSE)

  if(theta > 0) {
    out[q<a] <- if(lower.tail) 0 else 1
  } else {
    out <- 1 - out
    out[q>a] <- if(lower.tail) 1 else 0
  }
  if(log.p) out <- log(out)

  return(out)
}

S7::method(qf_fn, Amoroso) <- function(distribution) function(p, a, theta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  if(theta < 0) p <- 1-p
  gamma <- stats::qgamma(p = p, scale = 1, shape = alpha, lower.tail = lower.tail, log.p = log.p)
  q <- a + theta*gamma^(1/beta)

  return(q)
}

S7::method(rng_fn, Amoroso) <- function(distribution) function(n, a, theta, alpha, beta) {
  gamma <- stats::rgamma(n = n, scale = 1, shape = alpha)
  q <- a + theta*gamma^(1/beta)

  return(q)
}

S7::method(rargs, Amoroso) <- function(distribution) parameter_values(distribution)
