Frechet <- S7::new_class(
  "Frechet",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter,
    sigma = Parameter,
    theta = Parameter
  ),
  constructor = function(alpha, sigma, theta) {
    S7::new_object(
      S7::S7_object(),
      name = "Frechet",
      support = Real(min=expression(theta)),
      alpha = Parameter("alpha", "shape",    "\\alpha", alpha, Real(min=0)),
      sigma = Parameter("sigma", "scale",    "\\sigma", sigma, Real(min=0)),
      theta = Parameter("theta", "location", "\\theta", theta, Real(), fixed=TRUE)
    )
  }
)

frechet <- function(alpha, sigma, theta) Frechet(alpha, sigma, theta)

S7::method(pdf_fn, Frechet) <- function(distribution) function(x, alpha, sigma, theta, log=FALSE) {
  xi <- (x-theta)/sigma

  lpdf <- log(alpha) - log(sigma) - (1+alpha) * log(xi) - xi^(-alpha)

  lpdf[xi == 0] <- -Inf

  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, Frechet) <- function(distribution) function(q, alpha, sigma, theta, lower.tail=TRUE, log.p=FALSE) {
  xi <- (q-theta)/sigma

  lcdf <- - xi^(-alpha)

  if (!lower.tail) lcdf <- log1p(-exp(lcdf))

  if (log.p) return(lcdf) else return(exp(lcdf))
}

S7::method(qf_fn, Frechet) <- function(distribution) function(p, alpha, sigma, theta, lower.tail=TRUE, log.p=FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  q <- theta + sigma * (-log(p))^(-1/alpha)

  return(q)
}

S7::method(rng_fn, Frechet) <- function(distribution) function(n, mu, beta) {
  p <- runif(n, 0, 1)
  x <- qf(distribution, p)
  return(x)
}

S7::method(rargs, Frechet) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(Frechet, Mle)) <- function(distribution, estimator, data) {
  if (!distribution@theta@fixed) {
    rlang::warn("Estimates for parameter `theta` are known to be unstable, consider fixing the parameter.")
    distribution@theta@value <- min(data)-1
  }

  distribution <- S7::super(distribution, Distribution)
  return(parameter_estimates(distribution, estimator, data))
}
