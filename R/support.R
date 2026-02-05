Support <- S7::new_class(
  name = "Support",
  properties = list(
    min = S7::new_property(
      class = S7::class_numeric | S7::class_expression,
      validator = function(value) if (length(value) != 1) "must be of length 1",
      default = -Inf
    ),
    max = S7::new_property(
      class = S7::class_numeric | S7::class_expression,
      validator = function(value) if (length(value) != 1) "must be of length 1",
      default = Inf
    ),
    numeric = S7::new_property(
      class = S7::class_logical,
      getter = function(self) { !is.expression(self@min) && !is.expression(self@max) }
    )
  ),
  validator = function(self) {
    if (self@numeric) stopifnot(self@min <= self@max)
  }
)

Real <- S7::new_class(
  name = "Real",
  parent = Support
)

Int <- S7::new_class(
  name = "Int",
  parent = Support
)

# Methods -----

support <- S7::new_generic("support", "object")

S7::method(support, Support) <- function(object) object

inside <- S7::new_generic("inside", "object")

S7::method(inside, Real) <- function(object, x, ...) object@min <= x & x <= object@max

S7::method(inside, Int) <- function(object, x, ...) {
  if (rlang::is_integerish(x))
    return(object@min <= x & x <= object@max)

  return(FALSE)
}

## unconstrain ----
unconstrain <- S7::new_generic("unconstrain", c("object", "x"))

S7::method(unconstrain, list(Real, S7::class_numeric)) <- function(object, x) {
  if (is.infinite(object@min) && is.infinite(object@max)) return(x)

  if (is.finite(object@min) && is.finite(object@max)) {
    p <- (x - object@min) / (object@max - object@min)
    return(log(p) - log1p(-p))
  }

  if (is.finite(object@min)) return(log(x - object@min))
  if (is.finite(object@max)) return(log(object@max - x))
}

S7::method(unconstrain, list(Int, S7::class_numeric)) <- function(object, x) return(x)


## constrain ----
constrain <- S7::new_generic("constrain", c("object", "x"))

S7::method(constrain, list(Real, S7::class_numeric)) <- function(object, x) {
  if (is.infinite(object@min) && is.infinite(object@max)) return(x)

  if (is.finite(object@min) && is.finite(object@max)) {
    p <- 1 / (1 + exp(-x))
    return(object@min + (object@max - object@min) * p)
  }

  if (is.finite(object@min)) return(exp(x) + object@min)
  if (is.finite(object@max)) return(object@max - exp(x))
}

S7::method(constrain, list(Int, S7::class_numeric)) <- function(object, x) return(x)


## derivatives of the constrain transform---
derivative <- S7::new_generic("derivative", c("object", "x"))

S7::method(derivative, list(Real, S7::class_numeric)) <- function(object, x) {
  if (is.infinite(object@min) && is.infinite(object@max)) return(1)

  if (is.finite(object@min) && is.finite(object@max)) {
    p <- 1 / (1 + exp(-x))
    return( (object@max-object@min) * p * (1-p))
  }

  if (is.finite(object@min)) return(exp(x))
  if (is.finite(object@max)) return(exp(x))
}

S7::method(derivative, list(Int, S7::class_numeric)) <- function(object, x) return(NaN)

