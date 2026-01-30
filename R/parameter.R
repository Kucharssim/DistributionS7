Parameter <- S7::new_class(
  "Parameter",
  properties = list(
    key = S7::class_character,
    name = S7::class_character,
    label = S7::class_character,
    value = S7::class_numeric,
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
    support = Support,
    fixed = S7::class_logical
  ),
  constructor = function(key="", name=key, label=name, value, support, fixed) {
    if(missing(fixed)) {
      fixed <- is.fixed(value)
    } else if(fixed && !is.fixed(value)) {
      rlang::inform(rlang::englue("Parameter `{key}` was fixed by default. Set `distribution@{key}@fixed <- FALSE` if you are sure you want to estimate it, and you know what you are doing."))
    }
    attr(value, "fixed") <- NULL

    S7::new_object(S7::S7_object(), key=key, name=name, label=label, value=value, support=support, fixed=fixed)
  },
  validator = function(self) {
    if (length(self@value) != 1) rlang::abort("Parameter value must be of length 1")
    if (self@support@min > self@value) rlang::abort("Parameter value is smaller than the minimum of the parameter support")
    if (self@support@max < self@value) rlang::abort("Parameter value is larger than the maximum of the parameter support")
  },
)



# methods -----



## from support.R
S7::method(unconstrain, Parameter) <- function(x, ...) {
  unconstrain(x@support)
}

S7::method(constrain, Parameter) <- function(x, ...) {
  constrain(x@support)
}

## free/fixed parameters convenience ----

fixed <- function(x) {
  attr(x, "fixed") <- TRUE
  return(x)
}

is.fixed <- function(x) {
  isTRUE(attr(x, "fixed"))
}


is.parameter <- function(x) {
  S7::S7_inherits(x, Parameter)
}

### derivatives of support transformations ----

derivative <- S7::new_generic("derivative", "object")

S7::method(derivative, Parameter) <- function(object, ...) {
  derivative(object@support)
}

S7::method(derivative, Real) <- function(object, ...) {
  if (is.infinite(object@min) && is.infinite(object@max))
    return(\(x, ..) return(1))

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

S7::method(derivative, Int) <- function(object, ...) return(identity)
