log_lik <- S7::new_generic("log_lik", "distribution", function(distribution, data) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(log_lik, Distribution) <- function(distribution, data) {
  result <- likelihood(distribution, x=data, log=TRUE, )

  fixed <- parameter_properties(distribution, "fixed") |> unlist()
  attr(result, "df") <- sum(!fixed)
  attr(result, "nobs") <- length(data)
  class(result) <- c("log_lik", "logLik")

  return(result)
}

aic <- S7::new_generic("aic", "distribution", function(distribution, data) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(aic, Distribution) <- function(distribution, data) {
  ll <- log_lik(distribution, data)
  AIC(ll)
}

bic <- S7::new_generic("bic", "distribution", function(distribution, data) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(bic, Distribution) <- function(distribution, data) {
  ll <- log_lik(distribution, data)
  BIC(ll)
}

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

S7::method(fit_statistics, Distribution) <- function(distribution, data, estimated=FALSE, estimator=Mle(), bootstrap=0L) {
  results <- list(
    absolute = gof_test(distribution, data, estimated=estimated, estimator=estimator, bootstrap=bootstrap),
    relative = information_criteria(distribution, data, estimated=estimated)
  )
  return(results)
}

gof_test <- S7::new_generic("gof_test", "distribution", function(distribution, data, estimated=FALSE, estimator=Mle(), bootstrap=0L) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(gof_test, DistributionContinuous) <- function(distribution, data, estimated=FALSE, estimator=Mle(), bootstrap=0L) {
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
      if (estimated) dist <- fit_distribution(distribution, estimator, data_boot) else dist <- distribution
      res <- gof_test(dist, data_boot, estimated=FALSE, bootstrap=0)
      return(res[["statistic"]])
    }

    statistics <- replicate(bootstrap, boot_fn(distribution=distribution, data=data, estimated=estimated, ...))
    # compare to observed to get boostrapped p-vals
    results[["p_value"]] <- sweep(statistics, 1, results[["statistic"]], ">") |> rowMeans()
  } else if (estimated & any(!(parameter_properties(distribution, "fixed") |> unlist()))) {
    rlang::warn("Absolute fit tests are invalid if the distribution is fitted to the data. Use bootstrap to estimate corrected p-values")
  }

  return(results)
}

information_criteria <- S7::new_generic("information_criteria", "distribution", function(distribution, data, estimated=FALSE) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(information_criteria, Distribution) <- function(distribution, data, estimated=FALSE) {
  if (estimated) fitted <- fit_distribution(distribution, Mle(), data)

  log_lik <- log_lik(distribution, data)
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
