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


pars <- function(...) {
  pars <- rlang::dots_list(...)
  par_keys <- vapply(pars, \(par) par@key, character(1))
  names(pars) <- par_keys

  return(pars)
}

# methods -----
## make properties out of parameters ----

value <- S7::new_generic("value", "x")

S7::method(value, par) <- function(x, ...) {
  x@value
}

S7::method(value, tpar) <- function(x, ...) {
  NA
}

S7::method(value, S7::new_S3_class("list")) <- function(x, ...) {
  sapply(x, value, ...)
}

parameter_property <- function(key) {
  S7::new_property(
    class = S7::class_numeric,
    getter = function(self) {
      par <- self@parameters[[key]]

      if (inherits(par, "par"))
        return(par@value)

      env <- value(self@parameters, simplify=FALSE) |> list2env()
      return(eval(par@value, env))
    },
    setter = function(self, value) {
      par <- self@parameters[[key]]

      if (inherits(par, "par")) {
        self@parameters[[key]]@value <- value
        return(self)
      }

      env <- value(self@parameters, simplify=FALSE) |> list2env()
      env[[key]] <- value
      for (p in names(par@update)) {
        self@parameters[[p]]@value <- eval(par@update[[p]], env)
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
