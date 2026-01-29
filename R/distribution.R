Distribution <- S7::new_class(
  name = "Distribution",
  properties = list(
    name = S7::class_character,
    support = Real | Int
    #parameters = S7::class_list
  ),
  abstract = TRUE
)

DistributionDiscrete <- S7::new_class(
  name = "DistributionDiscrete",
  parent = Distribution,
  properties = list(
    support = Int
  ),
  abstract = TRUE
)


DistributionContinuous <- S7::new_class(
  name = "DistributionContinuous",
  parent = Distribution,
  properties = list(
    support = Real
  ),
  abstract = TRUE
)

# methods ----

pdf_fn <- S7::new_generic("pdf_fn", "distribution")

S7::method(pdf_fn, Distribution) <- function(distribution) {
  rlang::abort(message = "Probability density function not implemented for this distribution")
}

pdf <- S7::new_generic("pdf", "distribution")

S7::method(pdf, Distribution) <- function(distribution, x, log = FALSE, ...) {
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

S7::method(cdf_fn, Distribution) <- function(distribution) {
  rlang::abort(message = "Cumulative distribution function not implemented for this distribution")
}

cdf <- S7::new_generic("cdf", "distribution")

S7::method(cdf, Distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["q"]] <- q
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]] <- log.p

  out <- do.call(cdf_fn(distribution), args)

  return(out)
}

qf_fn <- S7::new_generic("qf_fn", "distribution")

S7::method(qf_fn, Distribution) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  rlang::abort(message = "Quantile function not implemented for this distribution")
}

qf <- S7::new_generic("qf", "distribution")

S7::method(qf, Distribution) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["p"]] <- p
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]] <- log.p

  out <- do.call(qf_fn(distribution), args)

  return(out)
}
rng_fn <- S7::new_generic("rng_fn", "distribution")

S7::method(rng_fn, Distribution) <- function(distribution) {
  rlang::abort(message = "Random number generation not implemented for this distribution")
}

rng <- S7::new_generic("rng", "distribution")

S7::method(rng, Distribution) <- function(distribution, n, ...) {
  args <- rargs(distribution)
  args <- c(args, rlang::dots_list(...))
  args[["n"]] <- n

  x <- do.call(rng_fn(distribution), args)

  return(x)
}

likelihood <- S7::new_generic("likelihood", "distribution")

S7::method(likelihood, Distribution) <- function(distribution, x, log=TRUE, factor=1, ...) {
  loglik <- factor*sum(pdf(distribution, x, log=TRUE))
  if (log) return(loglik)

  return(exp(loglik))
}


rargs <- S7::new_generic("rargs", "distribution")



# parameter properties ----

parameters <- S7::new_generic("parameters", "distribution")

S7::method(parameters, Distribution) <- function(distribution, which = c("all", "free", "fixed"), ...) {
  which <- match.arg(which)
  props <- S7::props(distribution)
  is_par <- vapply(props, is.parameter, logical(1))

  all_pars <- props[is_par]

  if (which == "all") return(all_pars)

  fixed <- vapply(all_pars, S7::prop, logical(1), "fixed")

  pars <- if (which == "fixed") all_pars[fixed] else all_pars[!fixed]

  return(pars)
}

parameter_properties <- S7::new_generic("parameter_properties", "distribution")

S7::method(parameter_properties, Distribution) <- function(
    distribution,
    property = c("key", "label", "value", "uvalue", "support", "fixed"),
    which = "all", ...) {
  property <- match.arg(property)

  pars <- parameters(distribution, which=which)

  output <- switch(
    property,
    key     = lapply(pars, \(p) p@key    ),
    label   = lapply(pars, \(p) p@label  ),
    value   = lapply(pars, \(p) p@value  ),
    uvalue  = lapply(pars, \(p) p@uvalue ),
    support = lapply(pars, \(p) p@support),
    fixed   = lapply(pars, \(p) p@fixed  )
  )

  return(output)
}

parameter_values <- S7::new_generic("parameter_values", "distribution")

S7::method(parameter_values, Distribution) <- function(distribution, which = "all", ...) {
  parameter_properties(distribution, property="value", which=which, ...)
}

`parameter_values<-` <- S7::new_generic("parameter_values<-", "distribution")

S7::method(`parameter_values<-`, Distribution) <- function(distribution, values) {
  for (key in names(values)) {
    if(!S7::prop_exists(distribution, key) || !(S7::prop(distribution, key) |> is.parameter())) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    par <- S7::prop(distribution, key)
    par@value <- values[[key]]
    S7::prop(distribution, key) <- par
  }

  return(distribution)
}

parameter_uvalues <- S7::new_generic("parameter_uvalues", "distribution")

S7::method(parameter_uvalues, Distribution) <- function(distribution, which = "all", ...) {
  parameter_properties(distribution, property="uvalue", which=which, ...)
}

`parameter_uvalues<-` <- S7::new_generic("parameter_uvalues<-", "distribution")

S7::method(`parameter_uvalues<-`, Distribution) <- function(distribution, values) {
  for (key in names(values)) {
    if(!S7::prop_exists(distribution, key) || !(S7::prop(distribution, key) |> is.parameter())) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    par <- S7::prop(distribution, key)
    par@uvalue <- values[[key]]
    S7::prop(distribution, key) <- par
  }

  return(distribution)
}

recreate_parameters <- S7::new_generic("recreate_parameters", "distribution")

S7::method(recreate_parameters, Distribution) <- function(distribution) {
  parameters <- parameter_values(distribution)
  is_fixed <- parameter_properties(distribution, property="fixed")

  for(key in names(parameters))
    if (is_fixed[[key]]) parameters[[key]] <- fixed(parameters[[key]])

  return(parameters)
}

# from support.R
S7::method(unconstrain, Distribution) <- function(x, ..., which="all") {
  support <- parameter_properties(x, "support", which=which)
  unconstrain_fn <- lapply(support, unconstrain)
  return(unconstrain_fn)
}

S7::method(constrain, Distribution) <- function(x, ..., which="all") {
  support <- parameter_properties(x, "support", which=which)
  constrain_fn <- lapply(support, constrain)
  return(constrain_fn)
}

# S7::method(derivative, Distribution) <- function(x, ..., which="all") {
#   support <- parameter_properties(x, "support", which=which)
#   derivative_fn <- lapply(support, derivative)
#   return(derivative_fn)
# }
