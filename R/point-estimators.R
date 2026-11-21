#' @title Estimators of distribution parameters.
#' @description
#' These classes encapsulate methods for parameter estimation.
#'
#' @name parameter-estimators
#'
#' @param silent logical; Should we suppress info messages during fitting.
#' @param optim logical; Should we force generic numerical optimization, even if there is a specific method to find the estimates for the particular distribution?
#' @param constrained logical; Should numerical optimization be done on the constrained or on the unconstrained parameter space?
#' @param method character; passed to [stats::optim()].
#' @param start method/function, list, or object of \code{S7} class \code{Estimator}. See details.
#' @param control list; passed to [stats::optim()].
#'
#' @details
#'
#' These classes are used for dispatching methods such as [fit()], [parameter_estimates()], or as a subroutine for [InferenceMethod()].
#'
#' [Mme()] specifies method of moments estimator, [Mle()]
#' specifies maximum likelihood estimator, and [BiasCorrected()] specifies some version of bias corrected estimators
#' (this is highly dependent on the type of distribution). For example, for [Normal()] the estimate of the variance is
#' unbiased, but the estimates of the standard deviation and of the precision are its square root and reciprocal, and are
#' therefore not unbiased. For [Exponential()], the estimates of both the rate and the scale are unbiased.
#'
#' [Mse()] specifies the maximum spacing estimator (also known as maximum product of spacings estimator, Cheng & Amin,
#' 1983; Ranneby, 1984), which maximizes the geometric mean of the spacings between the cumulative distribution function
#' evaluated at the ordered data. It is available for continuous distributions and always uses numerical optimization.
#' The estimator is consistent and asymptotically equivalent to the MLE where the MLE exists, and remains well
#' defined where the likelihood is unbounded, e.g., for the shift parameter of [ShiftedGamma()] or [ShiftedWeibull()]
#' when the shape parameter is smaller than 1. Zero spacings due to ties in the data are replaced by the density at
#' the tied value. Normal theory standard errors assume MLE; use [Bootstrap()] for inference with [Mse()].
#'
#' [Mle()] is the default method that is implemented for any distribution. If there are no closed-form solutions for the MLE,
#' then numerical optimization of the log likelihood is used.
#'
#' \code{start} is used for initialization of the distribution parameters for numerical optimization.
#' By default, the routine calls [parameter_start()], which for selected distributions attempts to use some heuristics to find
#' reasonable starting values, otherwise call \code{parameteter_estimates(distribution, Mme(), data)} to start the parameters
#' with method of moments estimation. In the absence of an implementation of either of the methods for a particular distribution,
#' the starting parameter values default to the values set by the distribution.
#' One can pass an arbitrary method/function with arguments \code{distribution}, \code{data}, to implement a custom parameter value initialization.
#' Otherwise, one can also pass a named list to overwrite the parameter values before starting the optimization.
#'
NULL

#' @rdname parameter-estimators
#' @export
Estimator <- S7::new_class(
  name = "Estimator",
  abstract = TRUE,
  properties = list(
    silent = S7::new_property(class = S7::class_logical, default=FALSE)
  )
)

#' @rdname parameter-estimators
#' @export
Mme <- S7::new_class(
  name = "Mme",
  parent = Estimator
)

#' @rdname parameter-estimators
#' @export
Mle <- S7::new_class(
  name = "Mle",
  parent = Estimator,
  properties = list(
    optim = S7::class_logical,
    constrained = S7::class_logical,
    method = S7::class_character,
    start = Estimator | S7::class_function | S7::class_list,
    control = S7::class_list
  ),
  constructor = function(optim=FALSE, constrained=FALSE, method=if(constrained) "L-BFGS-B" else "BFGS", start=parameter_start, control=list(), silent=FALSE) {
    S7::new_object(
      S7::S7_object(),
      optim=optim,
      constrained=constrained,
      method=method,
      start=start,
      control=control,
      silent=silent
    )
  }
)

#' @rdname parameter-estimators
#' @export
Mse <- S7::new_class(
  name = "Mse",
  parent = Estimator,
  properties = list(
    constrained = S7::class_logical,
    method = S7::class_character,
    start = Estimator | S7::class_function | S7::class_list,
    control = S7::class_list
  ),
  constructor = function(constrained=FALSE, method=if(constrained) "L-BFGS-B" else "BFGS", start=parameter_start, control=list(), silent=FALSE) {
    S7::new_object(
      S7::S7_object(),
      constrained=constrained,
      method=method,
      start=start,
      control=control,
      silent=silent
    )
  }
)

#' @rdname parameter-estimators
#' @export
BiasCorrected <- S7::new_class(
  name = "BiasCorrected",
  parent = Estimator
)
