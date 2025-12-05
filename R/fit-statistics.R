S7::method(logLik, Distribution) <- function(object, x, ...) {
  x <- na.omit(x)
  result <- likelihood(object, x=x, log=TRUE)

  fixed <- parameter_properties(object, "fixed") |> unlist()
  attr(result, "df") <- sum(!fixed)
  attr(result, "nobs") <- length(x)
  class(result) <- "logLik"

  return(result)
}

S7::method(AIC, Distribution) <- function(object, x, ...) AIC(logLik(object, x=x), ...)

S7::method(BIC, Distribution) <- function(object, x, ...) BIC(logLik(object, x=x), ...)


ks_test <- S7::new_generic("ks_test", "distribution")

S7::method(ks_test, DistributionContinuous) <- function(distribution, data) {
  result <- ks.test(data, cdf, distribution=distribution)

  result <- data.frame(
    test      = "ks_test",
    statistic = result[["statistic"]],
    p_value   = result[["p.value"]]
  )

  return(result)
}

S7::method(ks_test, DistributionDiscrete) <- function(distribution, data)
  rlang::inform("Kolmogorov-Smirnov test is available only for continuous distributions")


cvm_test <- S7::new_generic("cvm_test", "distribution")

S7::method(cvm_test, DistributionContinuous) <- function(distribution, data) {
  fn <- function(x) cdf(distribution, x)

  result <- goftest::cvm.test(data, fn, nullname=distribution@name)

  result <- data.frame(
    test = "cvm_test",
    statistic = result[["statistic"]],
    p_value = result[["p.value"]]
  )

  return(result)
}

S7::method(cvm_test, DistributionDiscrete) <- function(distribution, data)
  rlang::inform("Cramer-von Mises test is available only for continuous distributions")

ad_test <- S7::new_generic("ad_test", "distribution")

S7::method(ad_test, DistributionContinuous) <- function(distribution, data) {
  fn <- function(x) cdf(distribution, x)

  result <- goftest::ad.test(data, fn, nullname=distribution@name)

  result <- data.frame(
    test = "ad_test",
    statistic = result[["statistic"]],
    p_value = result[["p.value"]]
  )

  return(result)
}

S7::method(ad_test, DistributionDiscrete) <- function(distribution, data)
  rlang::inform("Anderson-Darling test is available only for continuous distributions")



fit_statistics <- S7::new_generic("fit_statistics", "distribution", function(distribution, data, ...) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(fit_statistics, Distribution) <- function(distribution, data, ..., estimated=FALSE, bootstrap=0L) {
  results <- list(
    absolute = fit_statistics_absolute(distribution, data, estimated=estimated, bootstrap=bootstrap, ...),
    relative = fit_statistics_relative(distribution, data)
  )
  return(results)
}

fit_statistics_absolute <- S7::new_generic("fit_statistics_absolute", "distribution", function(distribution, data, ...) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(fit_statistics_absolute, DistributionContinuous) <- function(distribution, data, ..., estimated=FALSE, bootstrap=0L) {
  results = list(
    ks_test  = ks_test (distribution, data),
    cvm_test = cvm_test(distribution, data),
    ad_test  = ad_test (distribution, data)
  )
  results <- do.call(rbind, results)

  if (bootstrap > 0L) {
    boot_fn <- function(distribution, data, estimated, ...) {
      n <- length(data)
      data_boot <- rng(distribution, n)
      if (estimated) dist <- fit_distribution(distribution, data_boot, ...) else dist <- distribution
      res <- fit_statistics_absolute(distribution, data_boot, estimated=FALSE, bootstrap=0)
      return(res[["statistic"]])
    }

    statistics <- replicate(bootstrap, boot_fn(distribution=distribution, data=data, estimated=estimated, ...))
    results[["p_value"]] <- mean(statistics >= results[["statistic"]])
  } else if (estimated) {
    rlang::warn("Absolute fit tests are invalid if the distribution is fitted to the data. Use bootstrap to estimate corrected p-values")
  }

  return(results)
}

S7::method(fit_statistics_absolute, DistributionDiscrete) <- function(distribution, data, ..., estimated=FALSE, bootstrap=0L) {
  rlang::inform("Absolute fit statistics not yet implemented")

  results <- data.frame(test=character(), statistic=character(), p_value=numeric(), method=numeric())
  return(results)
}

fit_statistics_absolute_bootstrap <- function(distribution, data, estimated, bootstrap) {
  if (boostrap > 0L) {
    boot_fn <- function(estimated, ...) {
      n <- length(data)
      data_boot <- rng(distribution, n)
      if (estimated) dist <- fit_distribution(distribution, data_boot, ...) else dist <- distribution
      ks <- fit_statistics_absolute(distribution, data, estimated=FALSE, bootstrap=0)
      return(ks[["statistic"]])
    }
  } else {
    rlang::warn("KS test is invalid if the distribution is fitted to the data. Use bootstrap to estimate corrected p-values")
  }
}

fit_statistics_relative <- S7::new_generic("fit_statistics_relative", "distribution", function(distribution, data) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(fit_statistics_relative, Distribution) <- function(distribution, data) {
  log_lik <- logLik(distribution, data)
  ll <- log_lik
  attributes(ll) <- NULL
  results = data.frame(
    n_par = attr(log_lik, "df"),
    n_obs = attr(log_lik, "nobs"),
    log_lik = ll,
    aic = AIC(log_lik),
    bic = BIC(log_lik)
  )
  return(results)
}
