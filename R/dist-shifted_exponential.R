ShiftedExponential <- S7::new_class(
  "ShiftedExponential",
  parent = Exponential,
  abstract = TRUE
)

ShiftedExponentialRate <- S7::new_class(
  "ShiftedExponentialRate",
  parent = ShiftedExponential,
  properties = list(
    lambda = Parameter,
    shift = Parameter
  ),
  constructor = function(lambda, shift) {
    S7::new_object(
      S7::S7_object(),
      name = "Shifted Exponential",
      support = Real(min=expression(shift)),
      lambda = Parameter("lambda", "rate", "\\lambda", lambda, Real(min=0)),
      shift = Parameter("shift", "shift", "shift", shift, Real())
    )
  }
)

ShiftedExponentialScale <- S7::new_class(
  "ShiftedExponentialScale",
  parent = ShiftedExponential,
  properties = list(
    beta = Parameter,
    shift = Parameter
  ),
  constructor = function(beta, shift) {
    S7::new_object(
      S7::S7_object(),
      name = "Shifted Exponential",
      support = Real(min=expression(shift)),
      beta = Parameter("beta", "scale", "\\beta", beta, Real(min=0)),
      shift = Parameter("shift", "shift", "shift", shift, Real())
    )
  }
)

shifted_exponential <- function(lambda, beta, shift) {
  parametrization <- rlang::check_exclusive(lambda, beta)
  distribution <- switch(
    parametrization,
    lambda = ShiftedExponentialRate(lambda, shift),
    beta = ShifterExponentialScale(beta, shift)
  )
}

S7::method(pdf_fn, ShiftedExponential) <- function(distribution) function(x, rate, shift, log=FALSE) {
  stats::dexp(x - shift, rate, log)
}

S7::method(cdf_fn, ShiftedExponential) <- function(distribution) function(q, rate, shift, lower.tail = TRUE, log.p = FALSE) {
  stats::pexp(q - shift, rate, lower.tail, log.p)
}

S7::method(qf_fn, ShiftedExponential) <- function(distribution) function(p, rate, shift, lower.tail = TRUE, log.p = FALSE) {
  stats::qexp(p, rate, lower.tail, log.p) + shift
}

S7::method(rng_fn, ShiftedExponential) <- function(distribution) function(n, rate, shift) {
  stats::rexp(n, rate) + shift
}

S7::method(rargs, ShiftedExponentialRate) <- function(distribution) {
  return(list(rate = distribution@lambda@value, shift=distribution@shift@value))
}

S7::method(rargs, ShiftedExponentialScale) <- function(distribution) {
  return(list(rate = 1/distribution@beta@value, shift=distribution@shift@value))
}

S7::method(parameter_estimates, list(ShiftedExponentialRate, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@shift@fixed) {
    shift <- min(data)
    estimates[["shift"]] <- shift
  } else {
    shift <- distribution@shift@fixed
  }

  if (!distribution@lambda@fixed) {
    estimates[["lambda"]] <- 1 / mean(data-shift)
  }
  return(estimates)
}

S7::method(parameter_estimates, list(ShiftedExponentialScale, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@shift@fixed) {
    shift <- min(data)
    estimates[["shift"]] <- shift
  } else {
    shift <- distribution@shift@fixed
  }

  if (!distribution@lambda@fixed) {
    estimates[["beta"]] <- mean(data-shift)
  }
  return(estimates)
}
