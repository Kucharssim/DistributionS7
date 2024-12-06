support <- S7::new_class(
  name = "support",
  properties = list(
    min = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) if (length(value) != 1) "must be of length 1",
      default = -Inf
    ),
    max = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) if (length(value) != 1) "must be of length 1",
      default = Inf
    )
  ),
  validator = function(self) {
    stopifnot(self@min <= self@max)
  }
)

real <- S7::new_class(
  name = "real",
  parent = support
)

int <- S7::new_class(
  name = "int",
  parent = support
)


# Methods -----

inside <- S7::new_generic("inside", "support")

S7::method(inside, support) <- function(support, x, ...) support@min <= x & x <= support@max

S7::method(inside, int) <- function(support, x, ...) {
  if (rlang::is_integerish(x))
    return(support@min <= x & x <= support@max)

  return(FALSE)
}


outside <- S7::new_generic("outside", "support")

S7::method(outside, support) <- function(support, x, ...) !inside(support, x, ...)


## unconstrain ----
unconstrain <- S7::new_generic("unconstrain", "x")

S7::method(unconstrain, real) <- function(x, ...) {
  if (is.infinite(x@min) && is.infinite(x@max))
    return(identity)

  if (is.infinite(x@max))
    return(\(value) log(value - x@min))

  if (is.infinite(x@min))
    return(\(value) log(x@max - value))

  return(\(value) {
    p <- (value - x@min) / (x@max - x@min)

    log(p) - log1p(-p)
  })
}

S7::method(unconstrain, int) <- function(x, ...) identity


## constrain ----
constrain <- S7::new_generic("constrain", "x")

S7::method(constrain, real) <- function(x, ...) {
  if (is.infinite(x@min) && is.infinite(x@max))
    return(identity)

  if (is.infinite(x@max))
    return(\(value) exp(value) + x@min)

  if (is.infinite(x@min))
    return(\(value) x@max - exp(value))

  return(\(value) {
    p <- 1 / (1 + exp(-value))
    return(x@min + (x@max - x@min) * p)
  })
}

