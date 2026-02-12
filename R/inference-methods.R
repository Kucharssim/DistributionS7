#' @title Parameter inference methods
#' @description
#' These classes encapsulate methods for parameter inference. This is yet experimental; methods for [Bootstrap()] and [ProfileLikelihood()] are not implemented at all.
#' [DefaultMethod()] will currently default to [NormalTheory()], *which is not a good default for some distributions!*
#' @name parameter-inference
#'
#' @param estimator Object of class [Estimator()].
#' @param ci_level numeric; confidence level.
#' @param args list; optional arguments for the method.
#' @param constrained logical; should the CIs be computed on the unconstrained space (and then transformed),
#' or should be computed directly on the constrained space.
#' @param control list; passed to [stats::optimHess].
#' @param samples integer; How many samples to take.
#' @param callback function; Optional function to exectute on every iteration.
#'
#' @seealso [parameter_inference()]
NULL

#' @rdname parameter-inference
#' @export
InferenceMethod <- S7::new_class(
  name = "InferenceMethod",
  properties = list(
    estimator = Estimator,
    ci_level = S7::new_property(
      class = S7::class_double,
      validator = function(value) {
        if (value > 1) return("`ci_level` must be smaller than 1.")
        if (value < 0) return("`ci_level` must be larger than 0.")
      },
      default = 0.95
    ),
    alpha = S7::new_property(getter = function(self) 1-self@ci_level),
    lower = S7::new_property(getter = function(self) self@alpha/2),
    upper = S7::new_property(getter = function(self) 1-self@alpha/2)
  ),
  constructor = function(estimator = Mle(), ci_level = 0.95) {
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level
    )
  }
)

#' @rdname parameter-inference
#' @export
DefaultMethod <- S7::new_class(
  name = "DefaultMethod",
  parent = InferenceMethod,
  properties = list(
    args = S7::class_list
  ),
  constructor = function(estimator = Mle(), ci_level = 0.95, args = list()) {
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level,
      args = args
    )
  }
)

#' @rdname parameter-inference
#' @export
NormalTheory <- S7::new_class(
  name = "NormalTheory",
  parent = InferenceMethod,
  properties = list(
    constrained = S7::class_logical,
    control = S7::class_list
  ),
  constructor = function(estimator = Mle(), ci_level = 0.95, constrained = FALSE, control = list()) {
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level,
      constrained = constrained,
      control = control
    )
  }
)

#' @rdname parameter-inference
#' @usage ProfileLikelihood(estimator=Mle(), ci_level=0.95)
#' @export
ProfileLikelihood <- S7::new_class(
  name = "Profile",
  parent = InferenceMethod,
  constructor = function(estimator = Mle(), ci_level = 0.95) {
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level
    )
  }
)

#' @rdname parameter-inference
#' @usage Bootstrap(estimator=Mle(), ci_level=0.95, samples=1000L, callback=function() {})
#' @export
Bootstrap <- S7::new_class(
  name = "Bootstrap",
  parent = InferenceMethod,
  properties = list(
    samples = S7::class_integer,
    callback = S7::class_function
  ),
  constructor = function(estimator = Mle(), ci_level = 0.95, samples = 1000L, callback = function() {}) {
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level,
      samples = samples,
      callback = callback
    )
  }
)
