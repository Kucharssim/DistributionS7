Gamma <- S7::new_class(
  "Gamma",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter
  ),
  abstract=TRUE
)

GammaScale <- S7::new_class(
  "GammaScale",
  parent = Gamma,
  properties = list(
    theta = Parameter
  ),
  constructor = function(alpha, theta) {
    S7::new_object(
      S7::S7_object(),
      name = "Gamma",
      support = Real(min=0),
      alpha = Parameter("alpha", "shape", "\\alpha", alpha, Real(min=0)),
      theta = Parameter("theta", "scale", "\\theta", theta, Real(min=0))
    )
  }
)

GammaRate <- S7::new_class(
  "GammaRate",
  parent = Gamma,
  properties = list(
    lambda = Parameter
  ),
  constructor = function(alpha, lambda) {
    S7::new_object(
      S7::S7_object(),
      name = "Gamma",
      support = Real(min=0),
      alpha  = Parameter("alpha", "shape", "\\alpha",  alpha,  Real(min=0)),
      lambda = Parameter("lambda", "rate", "\\lambda", lambda, Real(min=0))
    )
  }
)

GammaMean <- S7::new_class(
  "GammaMean",
  parent = Gamma,
  properties = list(
    mu = Parameter
  ),
  constructor = function(alpha, mu) {
    S7::new_object(
      S7::S7_object(),
      name = "Gamma",
      support = Real(min=0),
      alpha  = Parameter("alpha", "shape", "\\alpha", alpha,  Real(min=0)),
      mu     = Parameter("mu",    "mean",  "\\mu",    mu,     Real(min=0))
    )
  }
)

gamma <- function(alpha, theta, lambda, mu) {
  parametrization <- rlang::check_exclusive(theta, lambda, mu)
  distribution <- switch(
    parametrization,
    theta  = GammaScale(alpha, theta),
    lambda = GammaRate (alpha, lambda),
    mu     = GammaMean (alpha, mu)
  )
  return(distribution)
}

cauchy <- function(mu, sigma) Cauchy(mu, sigma)

S7::method(pdf_fn, Gamma) <- function(distribution) stats::dgamma

S7::method(cdf_fn, Gamma) <- function(distribution) stats::pgamma

S7::method(qf_fn, Gamma)  <- function(distribution) stats::qgamma

S7::method(rng_fn, Gamma) <- function(distribution) stats::rgamma

S7::method(rargs, GammaScale) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, scale=distribution@theta@value))
}

S7::method(rargs, GammaRate) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, rate=distribution@lambda@value))
}

S7::method(rargs, GammaMean) <- function(distribution, ...) {
  return(list(shape=distribution@alpha@value, scale=distribution@mu@value/distribution@alpha@value))
}

S7::method(parameter_estimates, list(GammaScale, Mom)) <- function(distribution, estimator, data) {
  fixed <- unlist(parameter_properties(distribution, "fixed"))
  if (all(fixed)) return(list())

  m <- mean(data)

  estimates <- list()

  if (all(!fixed)) {
    v <- var(data)
    theta = v / m
    estimates[["alpha"]] <- m^2 / v
    estimates[["theta"]] <- v / m
  } else if (!fixed[["alpha"]]) {
    estimates[["alpha"]] <- m / distribution@theta@value
  } else if (!fixed[["theta"]]) {
    estimates[["theta"]] <- m / distribution@alpha@value
  }

  return(estimates)
}

S7::method(parameter_estimates, list(GammaRate, Mom)) <- function(distribution, estimator, data) {
  fixed <- unlist(parameter_properties(distribution, "fixed"))
  if (all(fixed)) return(list())

  m <- mean(data)

  estimates <- list()

  if (all(!fixed)) {
    v <- var(data)
    lambda = v / m
    estimates[["alpha"]] <- m^2 / v
    estimates[["lambda"]] <- m / v
  } else if (!fixed[["alpha"]]) {
    estimates[["alpha"]] <- m * distribution@lambda@value
  } else if (!fixed[["lambda"]]) {
    estimates[["lambda"]] <- distribution@alpha@value / m
  }

  return(estimates)
}

S7::method(parameter_estimates, list(GammaMean, Mom)) <- function(distribution, estimator, data) {
  fixed <- unlist(parameter_properties(distribution, property="fixed"))
  if (all(fixed)) return(list())

  m <- mean(data)

  estimates <- list()

  if (all(!fixed)) {
    v <- var(data)
    estimates[["alpha"]] <- m^2 / v
    estimates[["mu"]]    <- m
  } else if (!fixed[["alpha"]]) {
    m <- distribution@mu@value
    v <- mean((data-m)^2)
    estimates[["alpha"]] <- m / v
  } else if (!fixed[["mu"]]) {
    estimates[["mu"]] <- m
  }

  return(estimates)
}
