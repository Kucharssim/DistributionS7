#' @title Parameter class
#' @description
#' Parameter classes encapsulating important information about distribution parameters.
#' This class is used internally and is not suppored to by used directly, unless one wants to implement a custom distribution.
#'
#' @param key character; Parameter key.
#' @param name character; Optional name of the parameter.
#' @param label character; Optional label of the parameter. Typically a LaTeX expression (for convenience to render in software like in JASP).
#' @param value character; Value of the parameter.
#' @param support Object of class [Support()].
#' @param fixed logical; Is the parameter fixed? If true, the parameter will not be estimated with parameter estimation methods.
#' @param x object to check.
#' @details
#' Fixed parameter can be either specified by setting the `fixed` property to `TRUE`, but one can also fix the parameter by
#' using the `fixed()` function on the parameter value. For example `normal(mu=0, sigma=fixed(1))` specifies a normal distribution
#' with the standard deviation parameter fixed to 1.
#'
#'
#' @name parameter
#' @export
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

#' @name parameter
#' @export
fixed <- function(value) {
  attr(value, "fixed") <- TRUE
  return(value)
}

is.fixed <- function(x) {
  isTRUE(attr(x, "fixed"))
}

#' @name parameter
#' @export
is.parameter <- function(x) {
  S7::S7_inherits(x, Parameter)
}


