parameter <- S7::new_class(
  "parameter",
  properties = list(
    label = S7::class_character,
    value = S7::class_numeric,
    support = support,
    fixed = S7::class_logical
  ),
  constructor = function(label="", value, support, fixed) {
    S7::new_object(S7::S7_object(), label=label, value=value, support=support, fixed=fixed)
  }
)

transformed_parameter <- S7::new_class(
  "transformed_parameter",
  properties = list(
    label = S7::class_character,
    transform = S7::class_expression
  )
)

parameters <- S7::new_class(
  "parameters",
  properties = list(
    parameters = S7::class_list,
    transformed_parameters = S7::class_list
  )
)

# methods -----

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

S7::method(unconstrain, parameter) <- function(x, ...) {
  fn <- unconstrain(x@support)
  return(fn(x@value))
}

S7::method(unconstrain, parameters) <- function(x, ...) {
  return(sapply(x@parameters, unconstrain))
}

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
    p <- (value - x@min) / (x@max - x@min)

    return(x@min + (x@max - x@min) * p)
  })
}

S7::method(constrain, parameter) <- function(x, ...) {
  return(x@value)
}

S7::method(constrain, parameters) <- function(x, ...) {
  return(sapply(x@parameters, constrain))
}

## assign to unconstrained ----
`unconstrain<-` <- S7::new_generic("`unconstrain<-`", "x")

S7::method(`unconstrain<-`, parameter) <- function(x, value, ...) {
  fn <- constrain(x@support)
  x@value <- fn(value)
  return(x)
}

S7::method(`unconstrain<-`, parameters) <- function(x, value, ...) {
  par_names <- sort(names(x@parameters))
  val_names <- sort(names(value))

  stopifnot(all(val_names %in% par_names))

  for (par in val_names) {
    unconstrain(x@parameters[[par]]) <- value[[par]]
  }

  return(x)
}



## make properties out of parameters ----

as.property <- S7::new_generic("as.property", "x")

S7::method(as.property, parameter) <- function(x, ...) {
  S7::new_property(
    class = S7::class_numeric,
    validator = function(value) {
      if (length(value) != 1) "must be of length 1"
      if (outside(x@support, value)) "value is outside of parameter support"
    },
    default=0
  )
}

S7::method(as.property, transformed_parameter) <- function(x, ...) {
  S7::new_property(
    class = S7::class_numeric,
    getter = function(self) {
      env <- constrain(self@parameters) |> as.list() |> list2env()
      eval(x@transform, env)
    },
    setter = function(self, value) {
      return(self)
    },
    default=0
  )
}

S7::method(as.property, parameters) <- function(x, ...) {
  pars <- lapply(x@parameters, as.property)
  tpars <- lapply(x@transformed_parameters, as.property)
  return(c(pars, tpars))
}
