distribution <- S7::new_class(
  name = "distribution",
  properties = list(
    name = S7::class_character,
    support = real | int,
    parameters = S7::class_list,
    transformed_parameters = S7::class_list,
    rargs = S7::class_list
  ),
  abstract = TRUE
)

distribution_discrete <- S7::new_class(
  name = "distribution_discrete",
  parent = distribution,
  properties = list(
    support = int
  ),
  abstract = TRUE
)


distribution_continuous <- S7::new_class(
  name = "distribution_continuous",
  parent = distribution,
  properties = list(
    support = real
  ),
  abstract = TRUE
)


# methods ----

pdf <- S7::new_generic("pdf", "distribution")

S7::method(pdf, distribution) <- function(distribution, x, log = FALSE, ...) {
  rlang::abort(message = "Probability density function not implemented for this distribution")
}

cdf <- S7::new_generic("pdf", "distribution")

S7::method(cdf, distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Cumulative distribution function not implemented for this distribution")

}

qf <- S7::new_generic("qf", "distribution")

S7::method(qf, distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Quantile function not implemented for this distribution")
}

rng <- S7::new_generic("rng", "distribution")

S7::method(rng, distribution) <- function(distribution, n) {
  rlang::abort(message = "Random number generation not implemented for this distribution")
}

S7::method(logLik, distribution) <- function(object, x, factor=0, ...) {
  loglik <- pdf(object, x, log=TRUE)

  return(factor*sum(loglik))
}

mle <- S7::new_generic("mle", "distribution")

S7::method(mle, distribution) <- function(distribution, x, ...) {
  rlang::inform(message = "Analytic MLE is not available/implemented, using numerical optimization...")

  x <- na.omit(x)

  start <- unconstrain(distribution@parameters)

  objective <- function(par) {
    unconstrain(distribution@parameters) <- par
    logLik(distribution, x, factor=-2)
  }

  result <- try(
    optim(par=start, objective, hessian=TRUE, ...),
    silent=TRUE
  )

  if(inherits(result, "try-error")) {
    rlang::abort("Optimization failed")
  }

  return(result)
}


rargs <- S7::new_generic("rargs", "distribution")

S7::method(rargs, distribution) <- function(distribution, ...) {
  env <- value(nb@parameters, simplify=FALSE) |> list2env()
  sapply(distribution@rargs, eval, envir=env, ...)
}
