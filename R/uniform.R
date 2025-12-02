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
      support = Real(min=min, max=max),
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


S7::method(point_estimates, Uniform) <- function(distribution, data, unbiased=TRUE, ...) {
  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimates <- numeric()
  n <- length(data)
  m <- if (!fixed[["min"]]) min(data) else distribution@min@value
  M <- if (!fixed[["max"]]) max(data) else distribution@max@value
  r <- M - m

  if (!fixed[["min"]]) {
    estimates[["min"]] <- if (unbiased) m-r/(n-1) else m
  }
  if (!fixed[["max"]]) {
    estimates[["max"]] <- if (unbiased) M+r/(n-1) else M
  }

  return(Estimates(values=estimates))
}

S7::method(parameter_inference_default, Uniform) <- function(distribution, data, ..., ci_level=0.95, unbiased=TRUE) {
  estimates <- point_estimates(distribution, data, unbiased=unbiased, ...)
  keys <- names(estimates@values)

  alpha <- 1-ci_level
  n <- length(data)

  mm <- range(data)
  r <- diff(mm)
  offset <- alpha^(-1/(n-1)) - 1

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimate <- estimates@values
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

  return(data.frame(key=keys, estimate=estimate, lower=lower, upper=upper))
}
