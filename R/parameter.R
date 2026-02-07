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
        unconstrain(self, self@value)
      },
      setter = function(self, value) {
        self@value <- constrain(self, value)
        return(self)
      }
    ),
    derivative = S7::new_property(
      class = S7::class_numeric,
      getter = function(self) {
        derivative(self, self@uvalue)
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
    if (length(self@value) != 1) return("Parameter value must be of length 1")
    if (self@support@min > self@value) return("Parameter value is smaller than the minimum of the parameter support")
    if (self@support@max < self@value) return("Parameter value is larger than the maximum of the parameter support")
  },
)



# methods -----



## from support.R
S7::method(unconstrain, list(Parameter, S7::class_numeric)) <- function(object, x) {
  unconstrain(object@support, x)
}

S7::method(constrain, list(Parameter, S7::class_numeric)) <- function(object, x) {
  constrain(object@support, x)
}

S7::method(derivative, list(Parameter, S7::class_numeric)) <- function(object, x) {
  derivative(object@support, x)
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


