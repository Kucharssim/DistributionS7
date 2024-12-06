distribution <- S7::new_class(
  name = "distribution",
  properties = list(
    name = S7::class_character,
    support = real | int,
    parameters = pars
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

pdf_fn <- S7::new_generic("pdf_fn", "distribution")

S7::method(pdf_fn, distribution) <- function(distribution) {
  rlang::abort(message = "Probability density function not implemented for this distribution")
}

pdf <- S7::new_generic("pdf", "distribution")

S7::method(pdf, distribution) <- function(distribution, x, log = FALSE, ...) {
  supported <- inside(distribution@support, x)
  missing <- is.na(x)

  out <- vector(mode = "numeric", length = length(x))
  out[!supported] <- if(log) -Inf else 0
  out[missing] <- NA_real_

  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["log"]] <- log
  args[["x"]] <- x[supported & !missing]

  out[supported & !missing] <- do.call(pdf_fn(distribution), args)

  return(out)
}

cdf <- S7::new_generic("pdf", "distribution")

S7::method(cdf, distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Cumulative distribution function not implemented for this distribution")

}

qf <- S7::new_generic("qf", "distribution")

S7::method(qf, distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Quantile function not implemented for this distribution")
}

rng_fn <- S7::new_generic("rng_fn", "distribution")

S7::method(rng_fn, distribution) <- function(distribution) {
  rlang::abort(message = "Random number generation not implemented for this distribution")
}

rng <- S7::new_generic("rng", "distribution")

S7::method(rng, distribution) <- function(distribution, n, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["n"]] <- n

  x <- do.call(rng_fn(distribution), args)

  return(x)
}

S7::method(logLik, distribution) <- function(object, x, factor=1, ...) {
  loglik <- pdf(object, x, log=TRUE)

  return(factor*sum(loglik))
}

mle <- S7::new_generic("mle", "distribution")

S7::method(mle, distribution) <- function(distribution, x, ...) {
  rlang::inform(message = "Analytic MLE is not available/implemented, using numerical optimization...")

  x <- na.omit(x)

  start <- uvalue(distribution)

  objective <- function(par, d, x) {
    uvalue(d) <- par
    logLik(d, x, factor=-2)
  }

  result <- try(
    optim(par=start, fn=objective, d=distribution, x=x, hessian=TRUE, ...),
    silent=TRUE
  )

  if (inherits(result, "try-error")) {
    rlang::warn("Optimization failed")
    return(result)
  }

  vcov <- try(solve(result[["hessian"]]))

  if (inherits(vcov, "try-error")) {
    rlang::warn("Variance covariance matrix could not be computed")
    vcov <- NULL
  }


  uvalue(distribution) <- result$par
  distribution@parameters@estimates <- estimates(mean = result$par, vcov = vcov)

  return(distribution)
}


rargs <- S7::new_generic("rargs", "x")

S7::method(rargs, distribution) <- function(x, ...) rargs(x@parameters, ...)

S7::method(rargs, pars) <- function(x, ...) {
  env <- parameters(x, transformed=TRUE, as_env=TRUE)

  sapply(x@rargs, eval, envir=env, ...)
}

parameters <- S7::new_generic("parameters", "x")

S7::method(parameters, distribution) <- function(x, ...) parameters(x@parameters, ...)

S7::method(parameters, pars) <- function(x, transformed=TRUE, as_env=FALSE, ...) {
  env <- rlang::env()

  for (p in names(x@main)) env[[p]] <- x@main[[p]]@value

  if (transformed)
    for (p in names(x@transformed)) env[[p]] <- eval(x@transformed[[p]]@value, envir=env)

  if (!as_env) env <- as.list(env)
  return(env)
}


free_parameters <- S7::new_generic("free_parameters", "x")

S7::method(free_parameters, distribution) <- function(x) free_parameters(x@parameters)

S7::method(free_parameters, pars) <- function(x) {
  f = vapply(x@main, \(p) !p@fixed, logical(1))
  return(names(f)[f])
}

uvalue <- S7::new_generic("uvalue", "x")

S7::method(uvalue, distribution) <- function(x, ...) uvalue(x@parameters)

S7::method(uvalue, pars) <- function(x, fixed=FALSE,...) {
  par_names <- if(fixed) names(x@main) else free_parameters(x)
  pars <- x@main[par_names]

  vapply(pars, \(p) p@uvalue, numeric(1), ...)
}

`uvalue<-` <- S7::new_generic("uvalue<-", "x")

S7::method(`uvalue<-`, distribution) <- function(x, values) {
  uvalue(x@parameters) <- values
  x
}

S7::method(`uvalue<-`, pars) <- function(x, values) {
  for (p in free_parameters(x)) {
    if (is.null(x@main[[p]])) rlang::warn(sprintf("Parameter %s was not found", p))
    v <- values[[p]]
    if (is.null(v)) next
    x@main[[p]]@uvalue <- v
  }
  x
}

# summary after fitting ---

S7::method(summary, distribution) <- function(distribution, ciLevel=0.95) {
  if (is.null(distribution@parameters@estimates)) {
    rlang::inform("The distribution has not been fitted to data")
    return(invisible())
  }
  npar <- length(distribution@parameters@estimates@mean)

  estimate <- unlist(parameters(distribution))
  key <- names(estimate)
  sd <- setNames(vector(length=length(key)), key)

  est_names <- names(distribution@parameters@estimates@mean)
  dxdy <- setNames(vector(length=length(est_names)), est_names)
  for (k in names(distribution@parameters@estimates@mean)) {
    par <- distribution@parameters@main[[k]]
    dxdy[k] <- derivative(par)
  }
  jac <- if (npar == 1) matrix(dxdy) else diag(dxdy)
  vcov <- jac %*% distribution@parameters@estimates@vcov %*% jac

  sd[est_names] <- sqrt(diag(vcov))

  for (k in names(distribution@parameters@transformed)) {
    par = distribution@parameters@transformed[[k]]
    dxdy <- derivative(par, est_names, estimate)
    var <- dxdy %*% vcov %*% t(dxdy)
    sd[k] <- sqrt(var)
  }

  alpha = 1-ciLevel
  lower <- qnorm(  alpha/2, estimate, sd)
  upper <- qnorm(1-alpha/2, estimate, sd)


  return(data.frame(parameter = key, estimate = estimate, sd = sd, lower=lower, upper=upper))
}
