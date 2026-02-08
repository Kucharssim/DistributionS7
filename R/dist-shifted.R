#' @title Shift a distribution by a scalar.
#' @description Create a distribution that has an additional shift parameter.
#'
#' @param distribution An object of class [Distribution()].
#' @param shift A shift parameter.
#'
#' @note
#' Estimating the shift parameter might not be a good idea, depending on the distribution. Distributions with their
#' own location parameter (e.g., normal), do not need a shift parameter in the first place.
#'
#' For convenience, additional distribution factories are added for distributions that are typical to parametrise with
#' an additional shift parameter. For some, custom fitting methods are used to estimate the shift parameter.
#'
#' @seealso [log_normal()], [gamma()], [inverse_gamma()], [log_logistic()], [wald()], [weibull()], [exponential()].
#' @family distributions
#' @export
shifted <- function(distribution, shift = 0) Shifted(distribution, shift)

#' @rdname shifted
#' @export
Shifted <- S7::new_class(
  "Shifted",
  parent = DistributionContinuous,
  properties = list(
    distribution = DistributionContinuous,
    shift = Parameter
  ),
  constructor = function(distribution, shift) {
    if (!distribution@support@numeric) rlang::abort("Distribution cannot be shifted (shift can be already accomodated by its intrinsic parameters)")

    S7::new_object(
      S7::S7_object(),
      name = paste("Shifted", distribution@name),
      support = Real(
        min = as.expression(call("+", distribution@support@min, quote(shift))),
        max = as.expression(call("+", distribution@support@max, quote(shift)))
      ),
      distribution = distribution,
      shift = Parameter(key = "shift", name="shift", label="shift", value = shift, support = Real())
    )
  }
)

#' @rdname shifted
#' @export
shifted_log_normal <- function(mu, sigma, shift=0) shifted(log_normal(mu, sigma), shift)

#' @rdname shifted
#' @export
shifted_gamma <- function(alpha, theta, lambda, mu, shift=0) shifted(gamma(alpha, theta, lambda, mu), shift)

#' @rdname shifted
#' @export
shifted_inverse_gamma <- function(alpha, theta, lambda, mu, shift=0) shifted(inverse_gamma(alpha, theta, lambda, mu), shift)

#' @rdname shifted
#' @export
shifted_log_logistic <- function(mu, sigma, alpha, beta, shift=0) shifted(log_logistic(mu, sigma, alpha, beta), shift)

#' @rdname shifted
#' @export
shifted_wald <- function(mu, lambda, nu, alpha, sigma=fixed(1), shift=0) shifted(wald(mu, lambda, nu, alpha, sigma), shift)

#' @rdname shifted
#' @export
shifted_weibull <- function(shape, scale, shift=0) shifted(weibull(shape, scale), shift)


S7::method(pdf, Shifted) <- function(distribution, x, log = FALSE) {
  pdf(distribution@distribution, x-distribution@shift@value, log=log)
}

S7::method(cdf, Shifted) <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  cdf(distribution@distribution, q-distribution@shift@value, lower.tail = lower.tail, log.p = log.p, ...)
}

S7::method(qf, Shifted) <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  q <- qf(distribution@distribution, p, lower.tail = lower.tail, log.p = log.p, ...)
  q + distribution@shift@value
}

S7::method(rng, Shifted) <- function(distribution, n) {
  x <- rng(distribution@distribution, n = n, ...)
  x + distribution@shift@value
}

S7::method(parameter_estimates, list(Shifted, Mle)) <- function(distribution, estimator, data) {
  if (distribution@shift@fixed)
    return(parameter_estimates(distribution@distribution, estimator, data-distribution@shift@value))

  distribution <- parameter_start(distribution, data)
  start <- c(ushift = distribution@shift@uvalue)

  objective <- function(par, d, data) {
    d@shift@uvalue <- par[["ushift"]]
    # estimate parameters of the unshifted distribution
    parameter_values(d@distribution) <- suppressMessages(
      parameter_estimates(distribution@distribution, estimator, data - d@shift@value)
    )
    likelihood(d, data, log=TRUE, factor=-2)
  }

  result <- try(
    stats::optim(
      par=start, fn=objective, d=distribution, data=data,
      method="BFGS",
      control=estimator@control, hessian=FALSE),
    silent=TRUE
  )

  if (inherits(result, "try-error")) rlang::abort("Could not estimate shift parameter")

  distribution@shift@uvalue <- result[["par"]][["ushift"]]

  estimates <- parameter_estimates(distribution@distribution, estimator, data-distribution@shift@value)

  estimates[["shift"]] <- distribution@shift@value

  return(estimates)
}

S7::method(parameter_start, Shifted) <- function(distribution, data) {
  max <- distribution@distribution@support@max
  min <- distribution@distribution@support@min
  len <- max-min

  M <- max(data)
  m <- min(data)

  if ((M-m) > len) {
    rlang::abort("data has a larger range than the distribution support allows, distribution cannot be fitted to data")
  } else if ((M-m) == len) {
    distribution@shift@value <- m
  } else if (is.finite(max) & is.finite(min)){
    distribution@shift@value <- (M+m-l)/2
    distribution@shift@support@min <- M - l
    distribution@shift@support@max <- m
  } else if (is.finite(min)) {
    distribution@shift@value <- m - 1
    distribution@shift@support@max <- m
  } else if (is.finite(max)) {
    distribution@shift@value <- M + 1
    distribution@shift@support@min <- M
  }

  return(distribution)
}

S7::method(parameters, Shifted) <- function(distribution, which = c("all", "free", "fixed")) {
  pars <- parameters(distribution@distribution, which=which, ...)

  distribution <- S7::super(distribution, DistributionContinuous)
  shift <- parameters(distribution, which=which, ...)

  return(c(pars, shift))
}

S7::method(fit_distribution, list(Shifted, Mle)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, estimator, data)
  if(!is.null(estimates[["shift"]])) {
    shift <- estimates["shift"]
    estimates[["shift"]] <- NULL
  }
  parameter_values(distribution@distribution) <- estimates
  parameter_values(distribution) <- shift


  # make a new dist object - this ensures all properties are valid
  dist_class <- S7::S7_class(distribution@distribution)
  parameters <- recreate_parameters(distribution@distribution)
  distribution@distribution <- do.call(dist_class, parameters)

  shift <- distribution@shift@value
  if (distribution@shift@fixed) shift <- fixed(shift)

  distribution <- shifted(distribution@distribution, shift=shift)
  return(distribution)
}

