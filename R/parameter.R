par <- S7::new_class(
  "par",
  properties = list(
    key = S7::class_character,
    label = S7::class_character,
    value = S7::new_property(
      class = S7::class_numeric,
      setter = function(self, value) {
        if (length(value) != 1) rlang::abort("value must be of value 1")
        if (outside(self@support, value) && !is.na(value)) rlang::abort("value is outside of the parameter support")
        self@value <- value
        self
      }
    ),
    uvalue = S7::new_property(
      class = S7::class_numeric,
      getter = function(self) {
        fn <- unconstrain(self@support)
        fn(self@value)
      },
      setter = function(self, value) {
        fn <- constrain(self@support)
        self@value <- fn(value)
        return(self)
      }
    ),
    support = support,
    fixed = S7::class_logical
  ),
  constructor = function(key="", label=key, value, support, fixed) {
    if(missing(fixed)) fixed <- is.fixed(value)
    attr(value, "fixed") <- NULL

    S7::new_object(S7::S7_object(), key=key, label=label, value=value, support=support, fixed=fixed)
  }
)

tpar <- S7::new_class(
  "tpar",
  properties = list(
    key = S7::class_character,
    label = S7::class_character,
    value = S7::class_expression,
    update = S7::class_list
  ),
  constructor = function(key="", label=key, value, update = list()) {
    S7::new_object(S7::S7_object(), key=key, label=label, value=value, update=update)
  }
)

estimates <- S7::new_class(
  "estimates",
  properties = list(
    mean = S7::class_numeric,
    vcov = S7::class_numeric
  )
)


pars <- S7::new_class(
  "pars",
  properties = list(
    main = S7::class_list,
    transformed = S7::class_list,
    rargs = S7::class_list,
    estimates = NULL | estimates
  ),
  constructor = function(..., rargs = list()) {
    pars <- rlang::dots_list(...)
    par_keys <- vapply(pars, \(par) par@key, character(1))
    names(pars) <- par_keys

    is_par <- vapply(pars, is.par, logical(1))

    S7::new_object(S7::S7_object(), main = pars[is_par], transformed = pars[!is_par], rargs = rargs)
  }
)



# methods -----
## make properties out of parameters ----

parameter_property <- function(key) {
  S7::new_property(
    class = S7::class_numeric,
    getter = function(self) {
      par <- self@parameters@main[[key]]

      if (!is.null(par))
        return(par@value)

      return(parameters(self)[[key]])
    },
    setter = function(self, value) {
      par <- self@parameters@main[[key]]

      if (!is.null(par)) {
        self@parameters@main[[key]]@value <- value
        return(self)
      }

      par <- self@parameters@transformed[[key]]

      if (is.null(par)) rlang::abort(sprintf("Parameter %s not found", key))

      env <- parameters(self, as_env=TRUE)
      env[[key]] <- value
      for (p in names(par@update)) {
        self@parameters@main[[p]]@value <- eval(par@update[[p]], env)
      }

      return(self)
    }
  )
}

parameter_properties <- function(keys) {
  out <- lapply(keys, parameter_property)
  names(out) <- keys
  return(out)
}

## free/fixed parameters convenience ----

fixed <- function(x) {
  attr(x, "fixed") <- TRUE
  return(x)
}

is.fixed <- function(x) {
  isTRUE(attr(x, "fixed"))
}


is.par <- function(x) {
  inherits(x, "par") || inherits(x, "distributions7::par")
}

is.tpar <- function(x) {
  inherits(x, "tpar") || inherits(x, "distributions7::tpar")
}


### derivatives of support transformations ----

derivative <- S7::new_generic("derivative", "object")

S7::method(derivative, par) <- function(object, ...) {
  d_fn <- derivative(object@support)
  d_fn(object@uvalue)
}

S7::method(derivative, tpar) <- function(object, names, values, ...) {
  d_fn <- deriv(object@value, names)

  res = eval(d_fn, list2env(as.list(values)))

  return(attr(res, "gradient"))
}

S7::method(derivative, real) <- function(object, ...) {
  if (is.infinite(object@min) && is.infinite(object@max))
    return(identity)

  if (is.infinite(object@max))
    return(exp)

  if (is.infinite(object@min))
    return(function(x) -exp(x))

  return(function(x) {
    p <- 1 / (1 + exp(-x))
    (object@max-object@min) * p * (1-p)
  }
  )
}

S7::method(derivative, int) <- function(object, ...) return(identity)
