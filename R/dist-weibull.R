Weibull <- S7::new_class(
  "Weibull",
  parent = DistributionContinuous,
  properties = list(
    k = Parameter,
    lambda = Parameter,
    theta = Parameter
  ),
  constructor = function(k, lambda, theta) {
    S7::new_object(
      S7::S7_object(),
      name = "Weibull",
      support = Real(min=expression(theta)),
      k = Parameter("k", "shape", "\\text{k}", k, Real(min=0)),
      lambda = Parameter("lambda", "scale", "\\lambda", lambda, Real(min=0)),
      theta = Parameter("theta", "location", "\\theta", theta, Real())
    )
  }
)

weibull <- function(k, lambda, theta) Weibull(k, lambda, theta)

S7::method(pdf_fn, Weibull) <- function(distribution) function(x, k, lambda, theta, log=FALSE) {
  stats::dweibull(x-theta, shape = k, scale = lambda, log = log)
}

S7::method(cdf_fn, Weibull) <- function(distribution) function(q, k, lambda, theta, lower.tail=TRUE, log.p=FALSE) {
  stats::pweibull(q-theta, shape = k, scale = lambda, lower.tail = lower.tail, log.p = log.p)
}

S7::method(qf_fn, Weibull) <- function(distribution) function(p, k, lambda, theta, lower.tail=TRUE, log.p=FALSE) {
  stats::qweibull(p, shape = k, scale = lambda, lower.tail = lower.tail, log.p = log.p) + theta
}

S7::method(rng_fn, Weibull) <- function(distribution) function(n, k, lambda, theta) {
  stats::rweibull(n, shape = k, scale = lambda) + theta
}

S7::method(rargs, Weibull) <- function(distribution) parameter_values(distribution)

S7::method(parameter_start, Weibull) <- function(distribution, data) {
  if (!distribution@theta@fixed) {
    rlang::warn("Estimates of parameter `theta` is known to be unstable, consider fixing the parameter.")
    m <- min(data)
    distribution@theta@value <- m-1
    distribution@theta@support@max <- m
  }
  return(distribution)
}
