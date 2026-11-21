#' @title Shift a distribution by a scalar.
#' @description Create a distribution that has an additional shift parameter.
#'
#' @param distribution An object of class [Distribution()].
#' @param shift A shift parameter.
#'
#' @inheritParams LogNormal
#' @inheritParams Gamma
#' @inheritParams Wald
#' @inheritParams Weibull
#' @inheritParams Exponential
#'
#' @note
#' Estimating the shift parameter might not be a good idea, depending on the distribution. Distributions with their
#' own location parameter (e.g., normal), do not need a shift parameter in the first place.
#'
#' For convenience, additional distribution factories are added for distributions that are typical to parametrise with
#' an additional shift parameter. For some, custom fitting methods are used to estimate the shift parameter.
#'
#' @seealso [LogNormal()], [Gamma()], [InverseGamma()], [LogLogistic()], [Wald()], [Weibull()], [Exponential()].
#' @family distributions
#' @export
Shifted <- S7::new_class(
  "Shifted",
  parent = DistributionContinuous,
  properties = list(
    distribution = DistributionContinuous,
    shift = Parameter
  ),
  constructor = function(distribution, shift=0) {
    if (!distribution@support@numeric) rlang::abort("Distribution cannot be shifted (shift can be already accomodated by its intrinsic parameters)")

    S7::new_object(
      S7::S7_object(),
      name = paste("Shifted", distribution@name),
      support = Real(
        min = as.expression(call("+", distribution@support@min, quote(shift))),
        max = as.expression(call("+", distribution@support@max, quote(shift)))
      ),
      distribution = distribution,
      shift = Parameter(key = "shift", name="shift", label="\\text{shift}", value = shift, support = Real())
    )
  }
)

#' @rdname Shifted
#' @export
ShiftedLogNormal <- function(mu, sigma, shift=0) Shifted(LogNormal(mu, sigma), shift)

#' @rdname Shifted
#' @export
ShiftedGamma <- function(alpha, theta, lambda, mu, shift=0) Shifted(Gamma(alpha, theta, lambda, mu), shift)

#' @rdname Shifted
#' @export
ShiftedInverseGamma <- function(alpha, theta, lambda, mu, shift=0) Shifted(InverseGamma(alpha, theta, lambda, mu), shift)

#' @rdname Shifted
#' @export
ShiftedLogLogistic <- function(mu, sigma, alpha, beta, shift=0) Shifted(LogLogistic(mu, sigma, alpha, beta), shift)

#' @rdname Shifted
#' @export
ShiftedWald <- function(mu, lambda, nu, alpha, sigma=fixed(1), shift=0) Shifted(Wald(mu, lambda, nu, alpha, sigma), shift)

#' @rdname Shifted
#' @export
ShiftedWeibull <- function(shape, scale, shift=0) Shifted(Weibull(shape, scale), shift)


S7::method(pdf, Shifted) <- function(distribution, x, log = FALSE) {
  pdf(distribution@distribution, x-distribution@shift@value, log=log)
}

S7::method(cdf, Shifted) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  cdf(distribution@distribution, q-distribution@shift@value, lower.tail = lower.tail, log.p = log.p)
}

S7::method(qf, Shifted) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  q <- qf(distribution@distribution, p, lower.tail = lower.tail, log.p = log.p)
  q + distribution@shift@value
}

S7::method(rng, Shifted) <- function(distribution, n) {
  x <- rng(distribution@distribution, n = n)
  x + distribution@shift@value
}

S7::method(parameter_estimates, list(Shifted, Mle)) <- function(distribution, estimator, data) {
  if (distribution@shift@fixed)
    return(parameter_estimates(distribution@distribution, estimator, data-distribution@shift@value))

  base <- distribution@distribution

  # profile likelihood of the shift: the parameters of the unshifted distribution are estimated given the shift;
  # trial shifts at which the estimation fails are rejected rather than aborting the search
  objective <- function(shift) {
    tryCatch({
      x <- data - shift
      parameter_values(base) <- suppressWarnings(suppressMessages(parameter_estimates(base, estimator, x)))
      value <- suppressWarnings(likelihood(base, x, log=TRUE, factor=-2))
      if (is.finite(value)) value else Inf
    }, error = function(e) Inf)
  }

  interval <- shift_interval(base, data)
  scale <- interval[["scale"]]
  tol <- 1e-8 * scale

  # one-dimensional bounded search directly on the shift; the open ends of the interval are widened if the optimum
  # runs into them
  for (i in 0:3) {
    lower <- interval[["lower"]] - is.infinite(interval[["min"]]) * 10^(1+i) * scale
    upper <- interval[["upper"]] + is.infinite(interval[["max"]]) * 10^(1+i) * scale
    lower <- max(lower, interval[["min"]] + tol)
    upper <- min(upper, interval[["max"]] - tol)

    result <- stats::optimize(objective, lower=lower, upper=upper, tol=tol)
    if (!is.finite(result[["objective"]])) rlang::abort("Could not estimate shift parameter")

    at_open_end <- (is.infinite(interval[["min"]]) && result[["minimum"]] - lower < 1e3 * tol) ||
                   (is.infinite(interval[["max"]]) && upper - result[["minimum"]] < 1e3 * tol)
    if (!at_open_end) break
  }

  if (at_open_end)
    rlang::warn("The estimate of the shift parameter drifted to the limit of the search interval; the distribution may fit better without a finite shift.")

  shift <- result[["minimum"]]

  # the likelihood can increase without bounds as the support of the distribution approaches the data (e.g., gamma
  # with shape below 1): the MLE does not exist, and the shift is placed one data spacing away from the data
  if (is.finite(interval[["max"]]) && interval[["max"]] - shift < 0.01 * interval[["gap_max"]]) {
    shift <- interval[["max"]] - interval[["gap_max"]]
    warn_unbounded_likelihood()
  } else if (is.finite(interval[["min"]]) && shift - interval[["min"]] < 0.01 * interval[["gap_min"]]) {
    shift <- interval[["min"]] + interval[["gap_min"]]
    warn_unbounded_likelihood()
  }

  estimates <- parameter_estimates(base, estimator, data - shift)
  estimates[["shift"]] <- shift

  return(estimates)
}

# limits of the shift such that the support of the shifted distribution covers the data, and a search interval within
shift_interval <- function(distribution, data) {
  support_min <- distribution@support@min
  support_max <- distribution@support@max

  m <- min(data)
  M <- max(data)
  x <- sort(unique(data))
  if (length(x) < 2) rlang::abort("Shift parameter cannot be estimated from data with fewer than two distinct values")

  # the shifted support [support_min + shift, support_max + shift] must contain [m, M]
  min <- M - support_max
  max <- m - support_min
  if (min > max)
    rlang::abort("data has a larger range than the distribution support allows, distribution cannot be fitted to data")

  scale <- stats::sd(data)

  list(
    min = min, max = max,
    lower = if (is.finite(min)) min else if (is.finite(max)) max else m,
    upper = if (is.finite(max)) max else if (is.finite(min)) min else M,
    scale = scale,
    # distance between the data and the shifted support in case of a boundary estimate: the spacing of the data at
    # the respective end
    gap_max = x[2] - x[1],
    gap_min = x[length(x)] - x[length(x)-1]
  )
}

warn_unbounded_likelihood <- function() {
  rlang::warn(c(
    "The likelihood increases without bounds as the shift approaches the data; the MLE of the shift does not exist.",
    i = "The shift was set one data spacing away from the data.",
    i = "Consider fixing the shift parameter, e.g., `shift = fixed(...)`."
  ))
}

S7::method(parameter_start, Shifted) <- function(distribution, data) {
  max <- distribution@distribution@support@max
  min <- distribution@distribution@support@min
  len <- max-min

  M <- max(data)
  m <- min(data)

  if (!distribution@shift@fixed) {
    # the shifted support [min + shift, max + shift] must contain [m, M]
    if ((M-m) > len) {
      rlang::abort("data has a larger range than the distribution support allows, distribution cannot be fitted to data")
    } else if ((M-m) == len) {
      distribution@shift@value <- m - min
    } else if (is.finite(max) & is.finite(min)){
      distribution@shift@value <- (M - max + m - min)/2
      distribution@shift@support@min <- M - max
      distribution@shift@support@max <- m - min
    } else if (is.finite(min)) {
      distribution@shift@value <- m - min - stats::sd(data)
      distribution@shift@support@max <- m - min
    } else if (is.finite(max)) {
      distribution@shift@value <- M - max + stats::sd(data)
      distribution@shift@support@min <- M - max
    }
  }

  # start the parameters of the unshifted distribution at the shifted data
  distribution@distribution <- parameter_start(distribution@distribution, data - distribution@shift@value)

  return(distribution)
}

# the shift is a property of the shifted distribution, the remaining parameters belong to the unshifted distribution
S7::method(`parameter_values<-`, Shifted) <- function(distribution, value) {
  value <- as.list(value)
  if (!is.null(value[["shift"]])) distribution@shift@value <- value[["shift"]]
  value[["shift"]] <- NULL
  if (length(value) > 0) parameter_values(distribution@distribution) <- value

  return(distribution)
}

S7::method(`parameter_uvalues<-`, Shifted) <- function(distribution, value) {
  value <- as.list(value)
  if (!is.null(value[["shift"]])) distribution@shift@uvalue <- value[["shift"]]
  value[["shift"]] <- NULL
  if (length(value) > 0) parameter_uvalues(distribution@distribution) <- value

  return(distribution)
}

S7::method(parameters, Shifted) <- function(distribution, which = c("all", "free", "fixed")) {
  pars <- parameters(distribution@distribution, which=which)

  distribution <- S7::super(distribution, DistributionContinuous)
  shift <- parameters(distribution, which=which)

  return(c(pars, shift))
}

S7::method(fit_distribution, list(Shifted, Estimator)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, estimator, data)
  if(!is.null(estimates[["shift"]])) {
    shift <- estimates["shift"]
    estimates[["shift"]] <- NULL
    parameter_values(distribution) <- shift
  }
  parameter_values(distribution@distribution) <- estimates


  # make a new dist object - this ensures all properties are valid
  dist_class <- S7::S7_class(distribution@distribution)
  parameters <- recreate_parameters(distribution@distribution)
  distribution@distribution <- do.call(dist_class, parameters)

  shift <- distribution@shift@value
  if (distribution@shift@fixed) shift <- fixed(shift)

  distribution <- Shifted(distribution@distribution, shift=shift)
  return(distribution)
}

