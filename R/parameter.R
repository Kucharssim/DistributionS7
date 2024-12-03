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
  constructor = function(key="", label=key, value=NA_real_, support, fixed=FALSE) {
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


pars <- function(pars=list(), tpars=list(), rargs=list()) {
  par_keys <- vapply(pars, \(par) par@key, character(1))
  names(pars) <- par_keys

  properties <- lapply(pars, as.property)

  par_keys <- vapply(tpars, \(par) par@key, character(1))
  names(tpars) <- par_keys

  properties <- c(properties, lapply(tpars, as.property))

  return(list(pars = pars, tpars = tpars, properties = properties, rargs=rargs))
}

# methods -----
## make properties out of parameters ----

value <- S7::new_generic("value", "x")

S7::method(value, par) <- function(x, ...) {
  x@value
}

S7::method(value, S7::new_S3_class("list")) <- function(x, ...) {
  sapply(x, value, ...)
}

as.property <- S7::new_generic("as.property", "x")

S7::method(as.property, par) <- function(x, ...) {
  S7::new_property(
    class = S7::class_numeric,
    getter = function(self) {
      self@parameters[[x@key]]@value
    },
    setter = function(self, value) {
      self@parameters[[x@key]]@value <- value
      return(self)
    }
  )
}

S7::method(as.property, tpar) <- function(x, ...) {
  S7::new_property(
    class = S7::class_numeric,
    getter = function(self) {
      env <- value(self@parameters, simplify=FALSE) |> list2env()
      eval(x@value, env)
    },
    setter = function(self, value) {
      env <- value(self@parameters, simplify=FALSE) |> list2env()
      env[[x@key]] <- value
      for (par in names(x@update)) {
        self@parameters[[par]]@value <- eval(x@update[[par]], env)
      }
      return(self)
    }
  )
}
