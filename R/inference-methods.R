#' @title Parameter inference methods
#' @description
#' These classes encapsulate methods for parameter inference. This is yet experimental; methods for [ProfileLikelihood()] are not implemented at all.
#' [DefaultMethod()] will currently default to [NormalTheory()], *which is not a good default for some distributions!*
#'
#' [NormalTheory()] uses the observed Fisher information evaluated at the estimates, and is therefore valid only for
#' maximum likelihood estimates ([Mle()]); using other estimators triggers a warning. With `constrained = TRUE`,
#' the confidence intervals are computed directly on the parameter space and may extend outside of the parameter support.
#'
#' [Bootstrap()] re-estimates the parameters on `samples` bootstrap data sets, drawn either from the distribution
#' fitted to the data (`type = "parametric"`), or by resampling the data with replacement (`type = "nonparametric"`).
#' The standard errors are the standard deviations of the bootstrap estimates. Confidence intervals (`ci_method`) are either
#' `"percentile"` intervals (quantiles of the bootstrap estimates), or `"basic"` intervals
#' (\eqn{2\hat{\theta} - q_{1-\alpha/2}, 2\hat{\theta} - q_{\alpha/2}}). Percentile intervals are invariant to monotone
#' transformations and therefore always respect the parameter support; basic intervals are computed on the unconstrained
#' parameter space and transformed back, so that they also respect the parameter support.
#' Bootstrap samples on which the estimation fails are dropped with a warning. The bootstrap estimates are returned in
#' the `"bootstrap"` attribute of the output of [parameter_inference()].
#' Note that the bootstrap is unreliable for parameters that determine the bounds of the support of the distribution
#' (e.g., the minimum and maximum of [Uniform()]).
#' @name parameter-inference
#'
#' @param estimator Object of class [Estimator()].
#' @param ci_level numeric; confidence level.
#' @param args list; optional arguments for the method.
#' @param constrained logical; should the CIs be computed on the unconstrained space (and then transformed),
#' or should be computed directly on the constrained space.
#' @param control list; passed to [stats::optimHess].
#' @param samples integer; How many samples to take.
#' @param type character; type of bootstrap, either `"parametric"` (simulate from the fitted distribution) or
#' `"nonparametric"` (resample the data with replacement).
#' @param ci_method character; type of bootstrap confidence intervals, either `"percentile"` or `"basic"`.
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
        if (value >= 1) return("`ci_level` must be smaller than 1.")
        if (value <= 0) return("`ci_level` must be larger than 0.")
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
  name = "ProfileLikelihood",
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
#' @usage Bootstrap(estimator=Mle(), ci_level=0.95, samples=1000L,
#'   type=c("parametric", "nonparametric"), ci_method=c("percentile", "basic"), callback=function() {})
#' @export
Bootstrap <- S7::new_class(
  name = "Bootstrap",
  parent = InferenceMethod,
  properties = list(
    samples = S7::class_integer,
    type = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || !value %in% c("parametric", "nonparametric"))
          return("`type` must be either \"parametric\" or \"nonparametric\".")
      },
      default = "parametric"
    ),
    ci_method = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || !value %in% c("percentile", "basic"))
          return("`ci_method` must be either \"percentile\" or \"basic\".")
      },
      default = "percentile"
    ),
    callback = S7::class_function
  ),
  constructor = function(estimator = Mle(), ci_level = 0.95, samples = 1000L,
                         type = c("parametric", "nonparametric"), ci_method = c("percentile", "basic"),
                         callback = function() {}) {
    type <- rlang::arg_match(type)
    ci_method <- rlang::arg_match(ci_method)
    S7::new_object(
      S7::S7_object(),
      estimator = estimator,
      ci_level = ci_level,
      samples = samples,
      type = type,
      ci_method = ci_method,
      callback = callback
    )
  }
)

# draw one bootstrap data set
bootstrap_data <- function(method, distribution, data) {
  switch(
    method@type,
    parametric    = rng(distribution, length(data)),
    nonparametric = data[sample.int(length(data), replace = TRUE)]
  )
}
