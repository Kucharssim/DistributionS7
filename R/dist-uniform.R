Uniform <- S7::new_class(
  "Uniform",
  parent = DistributionContinuous,
  properties = list(
    min = Parameter,
    max = Parameter
  ),
  constructor = function(min, max) {
    S7::new_object(
      S7::S7_object(),
      name = "Uniform",
      support = Real(min=expression(min), max=expression(max)),
      min = Parameter("min", "lower bound", "a", min, Real()),
      max = Parameter("max", "upper bound", "b", max, Real())
    )
  }
)

uniform <- function(min, max) Uniform(min, max)

S7::method(pdf_fn, Uniform) <- function(distribution) stats::dunif

S7::method(cdf_fn, Uniform) <- function(distribution) stats::punif

S7::method(qf_fn, Uniform)  <- function(distribution) stats::qunif

S7::method(rng_fn, Uniform) <- function(distribution) stats::runif


S7::method(rargs, Uniform) <- function(distribution, ...) {
  return(parameter_values(distribution))
}

S7::method(expectation, Uniform) <- function(distribution, ...) (distribution@min@value + distribution@max@value) / 2

S7::method(variance, Uniform) <- function(distribution, ...) {
  pars <- parameter_values(distribution)
  with(pars, (max-min)^2 / 12)
}

S7::method(skewness, Uniform) <- function(distribution, ...) 0

S7::method(kurtosis, Uniform) <- function(distribution, ...) 3-6/5

S7::method(excess_kurtosis, Uniform) <- function(distribution, ...) -6/5

S7::method(parameter_estimates, list(Uniform, Mle)) <- function(distribution, estimator, data) {
  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimates <- numeric()
  if (!fixed[["min"]]) estimates[["min"]] <- min(data)
  if (!fixed[["max"]]) estimates[["max"]] <- max(data)

  return(estimates)
}

S7::method(parameter_estimates, list(Uniform, BiasCorrected)) <- function(distribution, estimator, data) {
  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  n <- length(data)
  m <- if (!fixed[["min"]]) min(data) else distribution@min@value
  M <- if (!fixed[["max"]]) max(data) else distribution@max@value
  r <- M - m

  estimates <- numeric()
  if (!fixed[["min"]]) estimates[["min"]] <- m-r/(n-1)
  if (!fixed[["max"]]) estimates[["max"]] <- M+r/(n-1)

  return(estimates)
}

S7::method(parameter_inference, list(Uniform, DefaultMethod)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  keys <- names(estimates)

  n <- length(data)
  mm <- range(data)
  r <- diff(mm)
  offset <- inference_method@alpha^(-1/(n-1)) - 1

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (!fixed[["min"]]) {
    lower[["min"]] <- mm[1] - offset * r
    upper[["min"]] <- mm[1]
  }
  if (!fixed[["max"]]) {
    lower[["max"]] <- mm[2]
    upper[["max"]] <- mm[2] + offset * r
  }

  return(estimates_table(distribution, estimates=estimates, lower=lower, upper=upper))
}
