#' @title Wald (inverse gaussian) distribution
#' @description Create a Wald distribution object.
#'
#' @param mu mean parameter.
#' @param lambda shape parameter.
#' @param nu drift rate parameter.
#' @param alpha threshold parameter.
#' @param sigma noise parameter.
#' @family distributions
#' @export
Wald <- function(mu, lambda, nu, alpha, sigma=fixed(1)) {
  parametrization <- rlang::check_exclusive(mu, nu)

  distribution <- switch(
    parametrization,
    mu = WaldMean(mu, lambda),
    nu = WaldDrift(nu, alpha, sigma)
  )
  return(distribution)
}

WaldClass <- S7::new_class(
  "WaldClass",
  parent = DistributionContinuous,
  abstract = TRUE
)

WaldMean <- S7::new_class(
  "WaldMean",
  parent = WaldClass,
  properties = list(
    mu = Parameter,
    lambda = Parameter
  ),
  constructor = function(mu, lambda) {
    S7::new_object(
      S7::S7_object(),
      name = "Wald",
      support = Real(min=0),
      mu = Parameter("mu", "mean", "\\mu", mu, Real(min=0)),
      lambda = Parameter("lambda", "shape", "\\lambda", lambda, Real(min=0))
    )
  }
)

WaldDrift <- S7::new_class(
  "WaldDrift",
  parent = WaldClass,
  properties = list(
    nu = Parameter,
    alpha = Parameter,
    sigma = Parameter
  ),
  constructor = function(nu, alpha, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Wald",
      support = Real(min=0),
      nu = Parameter("nu", "drift", "\\nu", nu, Real(min=0)),
      alpha = Parameter("alpha", "threshold", "\\alpha", alpha, Real(min=0)),
      sigma = Parameter("sigma", "noise",     "\\sigma", sigma, Real(min=0))
    )
  },
  validator = function(self) {
    is_fixed <- unlist(parameter_properties(self, "fixed"))
    if (sum(is_fixed) == 0) return("At least one of the three parameters `nu`, `alpha`, and `sigma` must be fixed")
  }
)

S7::method(pdf_fn, WaldClass) <- function(distribution) function(x, mu, lambda, log = FALSE) {
  alpha <- sqrt(lambda)
  nu <- alpha / mu

  lpdf <- log(alpha) - 1.0/2.0 * log(2*pi) - 3.0/2.0*log(x) - (alpha - nu*x)^2/(2*x)
  lpdf[x<=0] <- -Inf

  if (log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, WaldClass) <- function(distribution) function(q, mu, lambda, lower.tail = TRUE, log.p = FALSE) {
  lx <- sqrt(lambda / q)
  xmu <- q / mu
  elmu <- exp(2*lambda / mu)

  out <- ifelse(q < 0, 0,
                pnorm(lx*(xmu - 1)) + elmu * pnorm(-lx * (xmu + 1)))

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(p)

  return(out)
}

S7::method(qf_fn, WaldClass) <- function(distribution) function(p, mu, lambda, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  zero <- p <= 0
  one <- p >= 1
  q <- numeric(length(p))
  q[zero] <- 0
  q[one] <- Inf


  q[!zero & !one] <- vapply(p[!zero & !one], function(pi) {
    fn <- function(log_q) cdf(distribution, exp(log_q)) - pi
    q <- try(uniroot(fn, lower = -100, upper = 100, extendInt = "yes"))
    if (inherits(q, "try-error")) return(NA)
    return(exp(q[["root"]]))
  }, FUN.VALUE = numeric(1))

  return(q)
}

S7::method(rng_fn, WaldClass) <- function(distribution) function(n, mu, lambda) {
  nu <- rnorm(n)
  y <- nu^2
  x <- mu + mu^2*y / (2*lambda) - mu / (2*lambda) * sqrt(4*mu*lambda*y + mu^2*y^2)
  z <- runif(n)

  out <- ifelse(z <= mu / (mu + x), x, mu^2 / x)

  return(out)
}

S7::method(rargs, WaldMean) <- function(distribution) parameter_values(distribution)

S7::method(rargs, WaldDrift) <- function(distribution) {
  list(
    mu = distribution@alpha@value / distribution@nu@value,
    lambda = (distribution@alpha@value / distribution@sigma@value)^2
  )
}

S7::method(parameter_estimates, list(WaldMean, Mme)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@mu@fixed) {
    mu <- mean(data)
    v <- var(data)
    estimates[["mu"]] <- mu
  } else {
    mu <- distribution@mu@value
    v <- mean((data-mu)^2)
  }

  if (!distribution@lambda@fixed) {
    estimates[["lambda"]] <- mu^3 / v
  }

  return(estimates)
}

S7::method(parameter_estimates, list(WaldDrift, Mme)) <- function(distribution, estimator, data) {
  is_fixed <- unlist(parameter_properties(distribution, property="fixed"))
  estimates <- list()
  if (is_fixed[["sigma"]]) {
    if (is_fixed[["nu"]] && !is_fixed[["alpha"]]) {
      estimates[["alpha"]] <- mean(data)
    } else if(!is_fixed[["nu"]] && is_fixed[["alpha"]]) {
      estimates[["nu"]] <- 1/mean(data)
    } else if(!is_fixed[["nu"]] && !is_fixed[["alpha"]]) {
      m <- mean(data)
      s <- sd(data)

      alpha <- sqrt(m^3) * distribution@sigma@value / s
      nu <- alpha / m

      estimates[["nu"]] <- nu
      estimates[["alpha"]] <- alpha
    }

  } else {
    m <- mean(data)
    s <- sd(data)
    if (is_fixed[["nu"]] && is_fixed[["alpha"]]) {
      nu <- distribution@nu@value
      alpha <- distribution@alpha@value
    } else if (!is_fixed[["nu"]]) {
      alpha <- distribution@alpha@value
      nu <- alpha / m
      estimates[["nu"]] <- nu
    } else if (!is_fixed[["alpha"]]) {
      nu <- distribution@nu@value
      alpha <- m * nu
      estimates[["alpha"]] <- alpha
    }

    estimates[["sigma"]] <- alpha * s / sqrt(m^3)

  }

  return(estimates)
}

S7::method(parameter_estimates, list(WaldMean, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@mu@fixed) {
    mu <- mean(data)
    estimates[["mu"]] <- mu
  } else {
    mu <- distribution@mu@value
  }

  if (!distribution@lambda@fixed) {
    estimates[["lambda"]] <- length(data) / sum(1/data - 1/mu)
  }

  return(estimates)
}


