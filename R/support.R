scalar <- S7::new_property(
  class = S7::class_numeric,
  validator = function(value) if (length(value) != 1) "must be of length 1",
  default = 0
)

support <- S7::new_class(
  name = "support",
  properties = list(
    min = scalar,
    max = scalar
  ),
  validator = function(self) {
    stopifnot(self@min <= self@max)
  }
)

real <- S7::new_class(
  name = "real",
  parent = support,
  constructor = function(min=-Inf, max=Inf) {
    S7::new_object(S7::S7_object(), min=min, max=max)
  }
)

int <- S7::new_class(
  name = "int",
  parent = support,
  constructor = function(min=-Inf, max=Inf) {
    S7::new_object(S7::S7_object(), min=min, max=max)
  }
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
