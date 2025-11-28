# general moments ----
moment <- S7::new_generic("moment", "distribution")

S7::method(moment, Distribution) <- function(distribution, ...) {
  rlang::abort(message = "Analytic expression for moment is not available/implemented")
}

S7::method(moment, DistributionContinuous) <- function(distribution, moment, type, value_only=TRUE, ...) {
  type <- match.arg(type, choices=c("raw", "central", "standardized"))
  stopifnot(moment > 0)

  if (moment == 1 && type %in% c("central", "standardized")) return(0)
  if (moment == 2 && type == "standardized") return(1)

  if (type == "raw") {
    integrand <- \(x) x^moment * pdf(distribution, x)
  } else if (type == "central") {
    mean <- expectation(distribution, value_only=TRUE, ...)
    integrand <- \(x) (x-mean)^moment * pdf(distribution, x)
  } else {
    mean <- expectation(distribution, value_only=TRUE, ...)
    sd <- std_dev(distribution, value_only=TRUE, ...)
    integrand <- \(x) ((x-mean)/sd)^moment * pdf(distribution, x)
  }

  lower <- distribution@support@min
  upper <- distribution@support@max

  result <- integrate(integrand, lower, upper, ...)

  if(value_only) result <- result[["value"]]

  return(result)
}

# expectation ----
expectation <- S7::new_generic("expectation", "distribution")

S7::method(expectation, DistributionContinuous) <- function(distribution,...) {
  rlang::inform(message = "Analytic expression for expectation is not available/implemented, using numerical integration...")
  return(
    moment(distribution, moment=1, type="raw", value_only=TRUE, ...)
  )
}

# variance ----
variance <- S7::new_generic("variance", "distribution")

S7::method(variance, DistributionContinuous) <- function(distribution, ...) {
  rlang::inform(message = "Analytic expression for variance is not available/implemented, using numerical integration...")
  return(
    moment(distribution, moment=2, type="central", value_only=TRUE, ...)
  )
}

# skewness ----
skewness <- S7::new_generic("skewness", "distribution")

S7::method(skewness, DistributionContinuous) <- function(distribution,...) {
  rlang::inform(message = "Analytic expression for skewness is not available/implemented, using numerical integration...")
  return(
    moment(distribution, moment=3, type="standardized", value_only=TRUE, ...)
  )
}

# kurtosis ----
kurtosis <- S7::new_generic("kurtosis", "distribution")

S7::method(kurtosis, DistributionContinuous) <- function(distribution,...) {
  rlang::inform(message = "Analytic expression for kurtosis is not available/implemented, using numerical integration...")
  return(
    moment(distribution, moment=4, type="standardized", value_only=TRUE, ...)
  )
}

# supporting methods ----

## excess kurtosis ----
excess_kurtosis <- S7::new_generic("excess_kurtosis", "distribution")

S7::method(excess_kurtosis, DistributionContinuous) <- function(distribution,...) {
  return(
    kurtosis(distribution, ...) - 3
  )
}

## sd ----
std_dev <- S7::new_generic("std_dev", "distribution")

S7::method(std_dev, Distribution) <- function(distribution, ...) {
  sqrt(variance(distribution, ...))
}

## coefficient of variation ----

coef_variation <- S7::new_generic("coef_variation", "distribution")

S7::method(coef_variation, Distribution) <- function(distribution, ...) {
  return(
    std_dev(distribution, ...) / expectation(distribution, ...)
  )
}

coef_dispersion <- S7::new_generic("coef_dispersion", "distribution")

S7::method(coef_dispersion, Distribution) <- function(distribution, ...) {
  return(
    variance(distribution, ...) / expectation(distribution, ...)
  )
}
