parameter <- S7::new_class(
  "parameter",
  properties = list(
    key = S7::class_character,
    name = S7::class_character,
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
  constructor = function(key="", name=key, label=name, value, support, fixed) {
    if(missing(fixed)) fixed <- is.fixed(value)
    attr(value, "fixed") <- NULL

    S7::new_object(S7::S7_object(), key=key, name=name, label=label, value=value, support=support, fixed=fixed)
  }
)



# methods -----



## from support.R
S7::method(unconstrain, parameter) <- function(x, ...) {
  unconstrain(x@support)
}

S7::method(constrain, parameter) <- function(x, ...) {
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
  inherits(x, "parameter") || inherits(x, "DistributionS7::parameter")
}

### derivatives of support transformations ----

derivative <- S7::new_generic("derivative", "object")

S7::method(derivative, parameter) <- function(object, ...) {
  d_fn <- derivative(object@support)
  d_fn(object@uvalue)
}

S7::method(derivative, real) <- function(object, ...) {
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

S7::method(derivative, int) <- function(object, ...) return(identity)
