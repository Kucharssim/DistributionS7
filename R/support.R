#' @title Support classes
#' @description
#' Classes the encapsulate support of distributions or parameters.
#'
#' @param min Minimum of the support. Can be either a numeric, or an expression.
#' @param max Maximum of the support. Can be either a numeric, or an expression.
#' @param object Object of class [Support()], [Parameter()], or [Distribution()].
#' @param x Vector or values.
#'
#' @returns
#' [support()] returns the [Support()] of the object (either the object itself or the `support` property). For [Distribution()],
#' the returned support object will always be `support@numeric == TRUE`, evaluating `min` and `max` properties if they are
#' specified as expressions. This allows you to inpect support of a distribution for distributions whose support depends on its
#' parameter values.
#'
#' [inside()] returns a logical vector indicating whether `x` is inside of the support. Note: support limits are always
#' treated as inclusive; one has to treat boundary conditions manually.
#'
#' [constrain()] returns a numeric vector transforming `x` from unconstrained space into a constrained space.
#' [unconstrain()] returns a numeric vector transforming `x` from a constrained space into an unconstrained space.
#' [derivative()] returns a derivative of the constrain transform at the unconstrained value `x`.
#' @export
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
    if (self@numeric && self@min > self@max) return("min cannot be larger than max")
  }
)

#' @rdname Support
#' @export
Real <- S7::new_class(
  name = "Real",
  parent = Support
)

#' @rdname Support
#' @export
Int <- S7::new_class(
  name = "Int",
  parent = Support
)

# Methods -----

#' @rdname Support
#' @export
support <- S7::new_generic("support", "object", function(object) S7::S7_dispatch() )

S7::method(support, Support) <- function(object) object

#' @rdname Support
#' @export
inside <- S7::new_generic("inside", "object", function(object, x) {
  S7::S7_dispatch()
})

S7::method(inside, Real) <- function(object, x) object@min <= x & x <= object@max

S7::method(inside, Int) <- function(object, x) {
  if (rlang::is_integerish(x))
    return(object@min <= x & x <= object@max)

  return(FALSE)
}


## unconstrain ----
#' @rdname Support
#' @export
unconstrain <- S7::new_generic("unconstrain", c("object", "x"), function(object, x) S7::S7_dispatch())

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
#' @rdname Support
#' @export
constrain <- S7::new_generic("constrain", c("object", "x"), function(object, x) S7::S7_dispatch())

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
#' @rdname Support
#' @export
derivative <- S7::new_generic("derivative", c("object", "x"), function(object, x) S7::S7_dispatch())

S7::method(derivative, list(Real, S7::class_numeric)) <- function(object, x) {
  if (is.infinite(object@min) && is.infinite(object@max)) return(1)

  if (is.finite(object@min) && is.finite(object@max)) {
    p <- 1 / (1 + exp(-x))
    return( (object@max-object@min) * p * (1-p))
  }

  if (is.finite(object@min)) return(exp(x))
  if (is.finite(object@max)) return(-exp(x))
}

S7::method(derivative, list(Int, S7::class_numeric)) <- function(object, x) return(NaN)
