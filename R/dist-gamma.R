#' @title Gamma distribution
#' @description Create a gamma distribution object.
#'
#' @param alpha shape parameter.
#' @param theta scale parameter.
#' @param lambda rate parameter.
#' @param mu mean parameter.
#' @family distributions
#' @export
Gamma <- function(alpha, theta, lambda, mu) {
  parametrization <- rlang::check_exclusive(theta, lambda, mu)
  distribution <- switch(
    parametrization,
    theta  = GammaScale(alpha, theta),
    lambda = GammaRate (alpha, lambda),
    mu     = GammaMean (alpha, mu)
  )
  return(distribution)
}


GammaClass <- S7::new_class(
  "GammaClass",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter
  ),
  abstract=TRUE
)

GammaScale <- S7::new_class(
  "GammaScale",
  parent = GammaClass,
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
  parent = GammaClass,
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
  parent = GammaClass,
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

S7::method(pdf_fn, GammaClass) <- function(distribution) stats::dgamma

S7::method(cdf_fn, GammaClass) <- function(distribution) stats::pgamma

S7::method(qf_fn, GammaClass)  <- function(distribution) stats::qgamma

S7::method(rng_fn, GammaClass) <- function(distribution) stats::rgamma

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


S7::method(parameter_estimates, list(GammaScale, Mle)) <- function(distribution, estimator, data) {
  distribution <- parameter_start(distribution, data)
  estimates <- list()
  if (!distribution@alpha@fixed) {
    rhs <- log(mean(data)) - mean(log(data))
    fn <- function(par, d, rhs) {
      d@alpha@uvalue <- par[["ualpha"]]
      lhs <- log(d@alpha@value) - digamma(d@alpha@value)
      abs(lhs-rhs)
    }
    par <- c(ualpha = distribution@alpha@uvalue)
    par <- stats::optim(par, fn, d=distribution, rhs=rhs, method="BFGS")
    distribution@alpha@uvalue <- par[["par"]][["ualpha"]]
    alpha <- distribution@alpha@value
    estimates[["alpha"]] <- alpha
  } else {
    alpha <- distribution@alpha@value
  }

  if (!distribution@theta@fixed) {
    estimates[["theta"]] <- mean(data)/alpha
  }

  return(estimates)
}

S7::method(parameter_estimates, list(GammaRate, Mle)) <- function(distribution, estimator, data) {
  distribution <- parameter_start(distribution, data)
  estimates <- list()
  if (!distribution@alpha@fixed) {
    rhs <- log(mean(data)) - mean(log(data))
    fn <- function(par, d, rhs) {
      d@alpha@uvalue <- par[["ualpha"]]
      lhs <- log(d@alpha@value) - digamma(d@alpha@value)
      abs(lhs-rhs)
    }
    par <- c(ualpha = distribution@alpha@uvalue)
    par <- stats::optim(par, fn, d=distribution, rhs=rhs, method="BFGS")
    distribution@alpha@uvalue <- par[["par"]][["ualpha"]]
    alpha <- distribution@alpha@value
    estimates[["alpha"]] <- alpha
  } else {
    alpha <- distribution@alpha@value
  }

  if (!distribution@lambda@fixed) {
    estimates[["lambda"]] <- alpha/mean(data)
  }

  return(estimates)
}


S7::method(parameter_estimates, list(GammaMean, Mle)) <- function(distribution, estimator, data) {
  distribution <- parameter_start(distribution, data)
  estimates <- list()
  if (!distribution@alpha@fixed) {
    rhs <- log(mean(data)) - mean(log(data))
    fn <- function(par, d, rhs) {
      d@alpha@uvalue <- par[["ualpha"]]
      lhs <- log(d@alpha@value) - digamma(d@alpha@value)
      abs(lhs-rhs)
    }
    par <- c(ualpha = distribution@alpha@uvalue)
    par <- stats::optim(par, fn, d=distribution, rhs=rhs, method="BFGS")
    distribution@alpha@uvalue <- par[["par"]][["ualpha"]]
    alpha <- distribution@alpha@value
    estimates[["alpha"]] <- alpha
  } else {
    alpha <- distribution@alpha@value
  }

  if (!distribution@mu@fixed) {
    estimates[["mu"]] <- mean(data)
  }

  return(estimates)
}
