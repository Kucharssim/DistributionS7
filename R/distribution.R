distribution <- S7::new_class(
  name = "distribution",
  properties = list(
    name = S7::class_character,
    support = real | int,
    parameters = S7::class_list,
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

cdf_fn <- S7::new_generic("cdf_fn", "distribution")

S7::method(cdf_fn, distribution) <- function(distribution) {
  rlang::abort(message = "Cumulative distribution function not implemented for this distribution")
}

cdf <- S7::new_generic("cdf", "distribution")

S7::method(cdf, distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["q"]] <- q
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]] <- log.p

  out <- do.call(cdf_fn(distribution), args)

  return(out)
}

qf_fn <- S7::new_generic("qf_fn", "distribution")

S7::method(qf_fn, distribution) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Quantile function not implemented for this distribution")
}

qf <- S7::new_generic("qf", "distribution")

S7::method(qf, distribution) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["p"]] <- p
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]] <- log.p

  out <- do.call(qf_fn(distribution), args)

  return(out)
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

likelihood <- S7::new_generic("likelihood", "distribution")

S7::method(likelihood, distribution) <- function(distribution, x, log=TRUE, factor=1, ...) {
  loglik <- factor*sum(pdf(distribution, x, log=TRUE))
  if (log) return(loglik)

  return(exp(loglik))
}


rargs <- S7::new_generic("rargs", "x")

S7::method(rargs, distribution) <- function(x, ...) {
  env <- parameter_values(x, as_env=TRUE)

  lapply(x@rargs, eval, envir=env, ...)
}

# parameter properties ----
parameter_properties <- S7::new_generic("parameter_properties", "x")

S7::method(parameter_properties, distribution) <- function(
    x, property = c("key", "label", "value", "uvalue", "support", "fixed"),
    as_env=FALSE, which = c("all", "free", "fixed"), ...) {
  property <- match.arg(property)
  which <- match.arg(which)

  output <- switch(
    property,
    key     = lapply(x@parameters, \(p) p@key    ),
    label   = lapply(x@parameters, \(p) p@label  ),
    value   = lapply(x@parameters, \(p) p@value  ),
    uvalue  = lapply(x@parameters, \(p) p@uvalue ),
    support = lapply(x@parameters, \(p) p@support),
    fixed   = lapply(x@parameters, \(p) p@fixed  )
  )

  if (which != "all") {
    fixed <- sapply(x@parameters, slot, "fixed")

    output <- if (which == "fixed") output[fixed] else output[!fixed]
  }

  if (as_env) output <- rlang::new_environment(data = output, parent = rlang::current_env())

  return(output)
}

parameter_values <- S7::new_generic("parameter_values", "x")

S7::method(parameter_values, distribution) <- function(x, as_env=FALSE, which = c("all", "free", "fixed"), ...) {
  parameter_properties(x, property="value", as_env=as_env, which=which, ...)
}

`parameter_values<-` <- S7::new_generic("parameter_values<-", "x")

S7::method(`parameter_values<-`, distribution) <- function(x, values) {
  for (key in names(values)) {
    if (is.null(x@parameters[[key]])) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    x@parameters[[key]]@value <- values[[key]]
  }

  return(x)
}

parameter_uvalues <- S7::new_generic("parameter_uvalues", "x")

S7::method(parameter_uvalues, distribution) <- function(x, as_env=FALSE, which = c("all", "free", "fixed"), ...) {
  parameter_properties(x, property="uvalue", as_env=as_env, which=which, ...)
}

`parameter_uvalues<-` <- S7::new_generic("parameter_uvalues<-", "x")

S7::method(`parameter_uvalues<-`, distribution) <- function(x, values) {
  for (key in names(values)) {
    if (is.null(x@parameters[[key]])) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    x@parameters[[key]]@uvalue <- values[[key]]
  }

  return(x)
}
