log_lik <- S7::new_generic("log_lik", "distribution", function(distribution, data) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(log_lik, Distribution) <- function(distribution, data) {
  result <- likelihood(distribution, x=data, log=TRUE)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()
  attr(result, "df") <- sum(!fixed)
  attr(result, "nobs") <- length(data)
  class(result) <- c("log_lik", "logLik")

  return(result)
}

aic <- S7::new_generic("aic", "distribution", function(distribution, data) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(aic, Distribution) <- function(distribution, data) {
  ll <- log_lik(distribution, data)
  AIC(ll)
}

bic <- S7::new_generic("bic", "distribution", function(distribution, data) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(bic, Distribution) <- function(distribution, data) {
  ll <- log_lik(distribution, data)
  BIC(ll)
}

information_criteria <- S7::new_generic("information_criteria", "distribution", function(distribution, data) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(information_criteria, Distribution) <- function(distribution, data) {
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


cvm_test <- S7::new_generic("cvm_test", "distribution", function(distribution, data, estimated=FALSE) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(cvm_test, DistributionContinuous) <- function(distribution, data, estimated=FALSE) {
  fn <- function(x) cdf(distribution, x)

  result <- goftest::cvm.test(data, fn, nullname=distribution@name, estimated=estimated)

  result <- data.frame(
    test = "cvm_test",
    statistic = result[["statistic"]],
    p_value = result[["p.value"]]
  )

  return(result)
}

ad_test <- S7::new_generic("ad_test", "distribution", function(distribution, data, estimated=FALSE) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(ad_test, DistributionContinuous) <- function(distribution, data, estimated=FALSE) {
  fn <- function(x) cdf(distribution, x)

  result <- goftest::ad.test(data, fn, nullname=distribution@name, estimated=estimated)

  result <- data.frame(
    test = "ad_test",
    statistic = result[["statistic"]],
    p_value = result[["p.value"]]
  )

  return(result)
}

gof_test <- S7::new_generic("gof_test", "distribution", function(distribution, data, estimated=FALSE, bootstrap=Bootstrap(samples=0L)) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(gof_test, DistributionContinuous) <- function(distribution, data, estimated=FALSE, bootstrap=Bootstrap(samples=0L)) {
  results <- list()
  # for bootstrapping, we only need simple hypothesis tests...
  estimated <- estimated && bootstrap@samples == 0

  if (!estimated) results[["ks_test"]] <- ks_test(distribution, data)
  results[["cvm_test"]] <- cvm_test(distribution, data, estimated)
  results[["ad_test"]]  <- ad_test (distribution, data, estimated)
  results <- do.call(rbind, results)

  if (bootstrap@samples > 0L) {
    bootstrap@estimator@silent <- TRUE
    boot_fn <- function(distribution, data, estimated, bootstrap) {
      n <- length(data)
      data_boot <- rng(distribution, n)
      if (estimated) distribution <- try(fit_distribution(distribution, bootstrap@estimator, data_boot), silent=TRUE)
      if (inherits(distribution, "try-error")) return(rep(NA, 3))
      res <- gof_test(distribution, data_boot, estimated=FALSE, bootstrap=Bootstrap(samples=0L))
      bootstrap@callback()
      return(res[["statistic"]])
    }

    statistics <- replicate(
      n = bootstrap@samples,
      expr = boot_fn(distribution=distribution, data=data, estimated=estimated, bootstrap=bootstrap)
      )
    # compare to observed to get boostrapped p-vals
    results[["p_value"]] <- sweep(statistics, 1, results[["statistic"]], ">") |> rowMeans()
  }

  return(results)
}
