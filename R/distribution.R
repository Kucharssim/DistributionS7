#' @title Distribution classes
#' @description
#' These classes are abstract and provide the base classes for all individual distributions in the package.
#' These classes are not intended for direct use, unless you want to implement a custom distribution class.
#'
#' @name distribution
#' @export
Distribution <- S7::new_class(
  name = "Distribution",
  properties = list(
    name = S7::class_character,
    support = Real | Int
  ),
  abstract = TRUE
)

#' @rdname distribution
#' @export
DistributionDiscrete <- S7::new_class(
  name = "DistributionDiscrete",
  parent = Distribution,
  properties = list(
    support = Int
  ),
  abstract = TRUE
)

#' @rdname distribution
#' @export
DistributionContinuous <- S7::new_class(
  name = "DistributionContinuous",
  parent = Distribution,
  properties = list(
    support = Real
  ),
  abstract = TRUE
)

# methods ----

#' @title Distribution functions
#' @description
#' Methods for distribution objects, providing the density (mass), cumulative probability, quantiles, and random number
#' generation.
#' @param distribution an object of \code{S7} class \code{Distribution}.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param log logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' @param log.p logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE}, probabilities to the left of the quantile are given, otherwise to the right.
#' @param factor numeric; factor by which to weight the total likelihood.
#' @name distribution-functions
NULL

pdf_fn <- S7::new_generic("pdf_fn", "distribution")

#' @rdname distribution-functions
#' @export
pdf <- S7::new_generic("pdf", "distribution", function(distribution, x, log=FALSE) {
  S7::S7_dispatch()
})

S7::method(pdf, Distribution) <- function(distribution, x, log = FALSE) {
  supported <- inside(distribution, x)
  missing <- is.na(x)

  out <- vector(mode = "numeric", length = length(x))
  out[!supported] <- if(log) -Inf else 0
  out[missing] <- NA_real_

  args <- rargs(distribution)
  args[["log"]] <- log
  args[["x"]] <- x[supported & !missing]

  out[supported & !missing] <- do.call(pdf_fn(distribution), args)

  return(out)
}

cdf_fn <- S7::new_generic("cdf_fn", "distribution")

#' @rdname distribution-functions
#' @export
cdf <- S7::new_generic("cdf", "distribution", function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  S7::S7_dispatch()
})

S7::method(cdf, Distribution) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  support <- support(distribution)
  lower <- q < support@min
  upper <- q > support@max
  missing <- is.na(q)
  valid <- !lower & !upper & !missing

  out <- vector(mode = "numeric", length = length(q))
  if (lower.tail) {
    out[lower] <- if(log.p) -Inf else 0
    out[upper] <- if(log.p) 0 else 1
  } else {
    out[lower] <- if(log.p) 0 else 1
    out[upper] <- if(log.p) -Inf else 0
  }
  out[missing] <- NA_real_

  args <- rargs(distribution)
  args[["q"]] <- q[valid]
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]] <- log.p

  out[valid] <- do.call(cdf_fn(distribution), args)

  return(out)
}

qf_fn <- S7::new_generic("qf_fn", "distribution")

#' @rdname distribution-functions
#' @export
qf <- S7::new_generic("qf", "distribution", function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  S7::S7_dispatch()
})

S7::method(qf, Distribution) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  args <- rargs(distribution)
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

#' @rdname distribution-functions
#' @export
rng <- S7::new_generic("rng", "distribution", function(distribution, n) {
  S7::S7_dispatch()
})

S7::method(rng, Distribution) <- function(distribution, n) {
  args <- rargs(distribution)
  args[["n"]] <- n

  x <- do.call(rng_fn(distribution), args)

  return(x)
}

#' @rdname distribution-functions
#' @export
likelihood <- S7::new_generic("likelihood", "distribution", function(distribution, x, log=TRUE, factor=1, ...) {
  S7::S7_dispatch()
})

S7::method(likelihood, Distribution) <- function(distribution, x, log=TRUE, factor=1, ...) {
  loglik <- factor*sum(pdf(distribution, x, log=TRUE))
  if (log) return(loglik)

  return(exp(loglik))
}


rargs <- S7::new_generic("rargs", "distribution")



# parameter properties ----

#' @title Parameter properties
#' @description
#' Get or set parameter properties. These methods are mostly intended for internal use, but can make some tasks easier elsewhere.
#'
#' @param distribution Object of class [Distribution()].
#' @param which character; should all, free, or fixed parameters be extracted?
#' @param property character; which parameter property to extract.
#' @param value list of parameter values (constrained or uncosntrained) to which to set the parameters.
#'
#' @name parameter-properties
#' @export
parameters <- S7::new_generic("parameters", "distribution", function(distribution, which = c("all", "free", "fixed")) {
  S7::S7_dispatch()
})

S7::method(parameters, Distribution) <- function(distribution, which = c("all", "free", "fixed")) {
  which <- match.arg(which)
  props <- S7::props(distribution)
  is_par <- vapply(props, is.parameter, logical(1))

  all_pars <- props[is_par]

  if (which == "all") return(all_pars)

  fixed <- vapply(all_pars, S7::prop, logical(1), "fixed")

  pars <- if (which == "fixed") all_pars[fixed] else all_pars[!fixed]

  return(pars)
}

#' @rdname parameter-properties
#' @export
parameter_properties <- S7::new_generic(
  "parameter_properties", "distribution",
  function(
    distribution,
    property = c("key", "name", "label", "value", "uvalue", "derivative", "support", "fixed"),
    which = "all") {
    S7::S7_dispatch()
  })

S7::method(parameter_properties, Distribution) <- function(
    distribution,
    property = c("key", "name", "label", "value", "uvalue", "derivative", "support", "fixed"),
    which = "all") {
  property <- match.arg(property)

  pars <- parameters(distribution, which=which)

  output <- switch(
    property,
    key        = lapply(pars, \(p) p@key    ),
    name       = lapply(pars, \(p) p@name   ),
    label      = lapply(pars, \(p) p@label  ),
    value      = lapply(pars, \(p) p@value  ),
    uvalue     = lapply(pars, \(p) p@uvalue ),
    derivative = lapply(pars, \(p) p@derivative),
    support    = lapply(pars, \(p) p@support),
    fixed      = lapply(pars, \(p) p@fixed  )
  )

  return(output)
}

#' @rdname parameter-properties
#' @export
parameter_values <- S7::new_generic("parameter_values", "distribution", function(distribution, which = "all") {
  S7::S7_dispatch()
})

S7::method(parameter_values, Distribution) <- function(distribution, which = "all") {
  parameter_properties(distribution, property="value", which=which, ...)
}

#' @rdname parameter-properties
#' @export
`parameter_values<-` <- S7::new_generic("parameter_values<-", "distribution", function(distribution, value) {
  S7::S7_dispatch()
})

S7::method(`parameter_values<-`, Distribution) <- function(distribution, value) {
  for (key in names(value)) {
    if(!S7::prop_exists(distribution, key) || !(S7::prop(distribution, key) |> is.parameter())) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    par <- S7::prop(distribution, key)
    par@value <- value[[key]]
    S7::prop(distribution, key) <- par
  }

  return(distribution)
}

#' @rdname parameter-properties
#' @export
parameter_uvalues <- S7::new_generic("parameter_uvalues", "distribution", function(distribution, which = "all") {
  S7::S7_dispatch()
})

S7::method(parameter_uvalues, Distribution) <- function(distribution, which = "all") {
  parameter_properties(distribution, property="uvalue", which=which, ...)
}

#' @rdname parameter-properties
#' @export
`parameter_uvalues<-` <- S7::new_generic("parameter_uvalues<-", "distribution", function(distribution, value) {
  S7::S7_dispatch()
})

S7::method(`parameter_uvalues<-`, Distribution) <- function(distribution, value) {
  for (key in names(value)) {
    if(!S7::prop_exists(distribution, key) || !(S7::prop(distribution, key) |> is.parameter())) {
      rlang::warn(sprintf("Parameter `%s` was not found", key))
      next
    }
    par <- S7::prop(distribution, key)
    par@uvalue <- value[[key]]
    S7::prop(distribution, key) <- par
  }

  return(distribution)
}

#' @rdname parameter-properties
#' @export
recreate_parameters <- S7::new_generic("recreate_parameters", "distribution")

S7::method(recreate_parameters, Distribution) <- function(distribution) {
  parameters <- parameter_values(distribution)
  is_fixed <- parameter_properties(distribution, property="fixed")

  for(key in names(parameters))
    if (is_fixed[[key]]) parameters[[key]] <- fixed(parameters[[key]])

  return(parameters)
}

# from support.R

S7::method(support, Distribution) <- function(object) {
  pars <- parameter_values(object)
  if (is.expression(object@support@min))
    object@support@min <- eval(object@support@min, pars)
  if (is.expression(object@support@max))
    object@support@max <- eval(object@support@max, pars)
  return(object@support)
}

S7::method(inside, Distribution) <- function(object, x, ...) {
  inside(support(object), x, ...)
}


as_latex <- S7::new_generic("as_latex", "distribution", function(distribution, digits=3) {
  S7::S7_dispatch()
})

S7::method(as_latex, Distribution) <- function(distribution, digits=3) {
  keys   <- parameter_properties(distribution, property="key")
  labels <- parameter_properties(distribution, property="label")
  fixed  <- parameter_properties(distribution, property="fixed")
  values <- parameter_values(distribution)

  pars <- list()
  for (key in keys) {
    lab <- labels[[key]]
    if (!fixed[[key]]) lab <- sprintf("\\hat{%s}", lab)
    pars[[key]] <- sprintf("%1$s = %2$s", lab, format(values[[key]], digits=digits))
  }
  pars <- paste(pars, collapse=", ")

  text <- sprintf("\\text{%1$s}(%2$s)", distribution@name, pars)

  return(text)
}
