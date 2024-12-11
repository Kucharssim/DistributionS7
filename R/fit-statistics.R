S7::method(logLik, distribution) <- function(object, x, ...) {
  supported <- inside(object@support, x)
  missing <- is.na(x)
  x <- x[supported & !missing]
  result <- likelihood(object, x=x, log=TRUE)

  attr(result, "df") <- length(free_parameters(object))
  attr(result, "nobs") <- length(x)
  class(result) <- "logLik"

  return(result)
}

S7::method(AIC, distribution) <- function(object, x, ...) AIC(logLik(object, x=x), ...)

S7::method(BIC, distribution) <- function(object, x, ...) BIC(logLik(object, x=x), ...)


ks_test <- S7::new_generic("ks_test", "distribution")

S7::method(ks_test, distribution_continuous) <- function(distribution, x, ...) {
  result <- ks.test(x, cdf, distribution=distribution)

  result[["test"]] <- "ks_test"

  return(result)
}

S7::method(ks_test, distribution_discrete) <- function(distribution, ...)
  rlang::inform("Kolmogorov-Smirnov test is available only for continuous distributions")


cvm_test <- S7::new_generic("cvm_test", "distribution")

S7::method(cvm_test, distribution_continuous) <- function(distribution, x, estimated=FALSE) {
  if(!estimated && !is.null(distribution@parameters@estimates))
    rlang::warn("`estimated` was set to FALSE but the distribution appears to be fitted to data.")

  fn <- function(x) cdf(distribution, x)

  result <- goftest::cvm.test(x, fn, estimated=estimated, nullname=distribution@name)
  result[["test"]] <- "cvm_test"

  return(result)
}

S7::method(cvm_test, distribution_discrete) <- function(distribution, ...)
  rlang::inform("Cramer-von Mises test is available only for continuous distributions")

ad_test <- S7::new_generic("ad_test", "distribution")

S7::method(ad_test, distribution_continuous) <- function(distribution, x, estimated=FALSE) {
  if(!estimated && !is.null(distribution@parameters@estimates))
    rlang::warn("ad_test: `estimated` was set to FALSE but the distribution appears to be fitted to data.")

  fn <- function(x) cdf(distribution, x)

  result <- goftest::ad.test(x, fn, estimated=estimated, nullname=distribution@name)
  result[["test"]] <- "ad_test"

  return(result)
}

S7::method(ad_test, distribution_discrete) <- function(distribution, ...)
  rlang::inform("Anderson-Darling test is available only for continuous distributions")



fit_statistics <- S7::new_generic("fit_statistics", "distribution")

S7::method(fit_statistics, distribution_continuous) <- function(distribution, x, estimated) {
  if (missing(estimated)) {
    estimated <- !is.null(distribution@parameters@estimates)
    if (estimated)
      rlang::inform("Assuming that the distribution was fitted to the current data...")
  }

  absolute = list(
    ks_test=ks_test(distribution, x),
    cvm_test=cvm_test(distribution, x, estimated=estimated),
    ad_test=ad_test(distribution, x, estimated=estimated)
  )
  absolute = lapply(absolute, \(x) {
    data.frame(test=x[["test"]], statistic=x[["statistic"]], p_value=x[["p.value"]], method=x[["method"]][1])
  })
  absolute = do.call(rbind, absolute)


  log_lik <- logLik(distribution, x)
  relative = data.frame(
    n_par = attr(log_lik, "df"),
    n_obs = attr(log_lik, "nobs"),
    log_lik = (\(ll) {attributes(ll) <- NULL; return(ll)})(log_lik),
    aic = AIC(log_lik),
    bic = BIC(log_lik)
  )

  return(list(absolute=absolute, relative=relative))
}
