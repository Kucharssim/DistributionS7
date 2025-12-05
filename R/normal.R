Normal <- S7::new_class(
  "Normal",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter
  ),
  abstract = TRUE
)

NormalSigma <- S7::new_class(
  "NormalSigma",
  parent = Normal,
  properties = list(
    sigma = Parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = Real(),
      mu = Parameter("mu", "mean", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "std.deviation", "\\sigma", sigma, Real(0))
    )
  }
)

NormalSigma2 <- S7::new_class(
  "NormalSigma2",
  parent = Normal,
  properties = list(
    sigma2 = Parameter
  ),
  constructor = function(mu, sigma2) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = Real(),
      mu = Parameter("mu", "mean", "\\mu", mu, Real()),
      sigma2 = Parameter("sigma2", "variance", "\\sigma^2", sigma2, Real(0))
    )
  }
)

NormalTau <- S7::new_class(
  "NormalTau",
  parent = Normal,
  properties = list(
    mu = Parameter,
    tau = Parameter
  ),
  constructor = function(mu, tau) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = Real(),
      mu = Parameter("mu", "mean", "\\mu", mu, Real()),
      tau = Parameter("tau", "precision", "\\tau", tau, Real(0))
    )
  }
)

normal <- function(mu, sigma, sigma2, tau) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau)
  distribution <- switch(
    parametrization,
    sigma = NormalSigma(mu=mu, sigma=sigma),
    sigma2 = NormalSigma2(mu=mu, sigma2=sigma2),
    tau = NormalTau(mu=mu, tau=tau)
    )

  return(distribution)
}


S7::method(pdf_fn, Normal) <- function(distribution) stats::dnorm

S7::method(cdf_fn, Normal) <- function(distribution) stats::pnorm

S7::method(qf_fn, Normal)  <- function(distribution) stats::qnorm

S7::method(rng_fn, Normal) <- function(distribution) stats::rnorm


S7::method(expectation, Normal) <- function(distribution, ...) distribution@mu@value


S7::method(variance, NormalSigma)  <- function(distribution, ...) distribution@sigma@value^2

S7::method(variance, NormalSigma2) <- function(distribution, ...) distribution@sigma2@value

S7::method(variance, NormalTau)    <- function(distribution, ...) 1 / distribution@tau@value


S7::method(std_dev, NormalSigma)  <- function(distribution, ...) distribution@sigma@value

S7::method(std_dev, NormalSigma2) <- function(distribution, ...) sqrt(distribution@sigma2@value)

S7::method(std_dev, NormalTau)    <- function(distribution, ...) sqrt(1/distribution@tau@value)


S7::method(skewness, Normal) <- function(distribution, ...) 0

S7::method(kurtosis, Normal) <- function(distribution, ...) 3

S7::method(excess_kurtosis, Normal) <- function(distribution, ...) 0


S7::method(rargs, Normal) <- function(distribution, ...) {
  return(
    list(
      mean = expectation(distribution),
      sd = std_dev(distribution)
    )
  )
}


S7::method(point_estimates, NormalSigma) <- function(distribution, data, bessels_correction=TRUE, ...) {
  estimates <- numeric()
  df <- length(data)

  if (!is.fixed(distribution@mu)) {
    if (bessels_correction) df <- length(data) - 1
    mu <- mean(data)
    estimates[["mu"]] <- mu
  } else{
    mu <- distribution@mu@value
  }

  if (!is.fixed(distribution@sigma)) {
    estimates[["sigma"]] <- sqrt(sum((data-mu)^2) / df)
  }

  return(Estimates(values=estimates))
}

S7::method(point_estimates, NormalSigma2) <- function(distribution, data, bessels_correction=TRUE, ...) {
  estimates <- numeric()
  df <- length(data)

  if (!is.fixed(distribution@mu)) {
    if (bessels_correction) df <- length(data) - 1
    mu <- mean(data)
    estimates[["mu"]] <- mu
  } else{
    mu <- distribution@mu@value
  }

  if (!is.fixed(distribution@sigma2)) {
    estimates[["sigma2"]] <- sum((data-mu)^2) / df
  }

  return(Estimates(values=estimates))
}

S7::method(point_estimates, NormalTau) <- function(distribution, data, bessels_correction=TRUE, ...) {
  estimates <- numeric()
  df <- length(data)

  if (!is.fixed(distribution@mu)) {
    if (bessels_correction) df <- length(data) - 1
    mu <- mean(data)
    estimates[["mu"]] <- mu
  } else{
    mu <- distribution@mu@value
  }

  if (!is.fixed(distribution@tau)) {
    estimates[["tau"]] <- df / sum((data-mu)^2)
  }

  return(Estimates(values=estimates))
}

S7::method(parameter_inference_default, NormalSigma) <- function(distribution, data, ..., ci_level=0.95, bessels_correction=TRUE) {
  estimates <- point_estimates(distribution, data, bessels_correction=bessels_correction)
  keys <- names(estimates@values)
  alpha <- 1-ci_level
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimate <- estimates@values
  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(x) / sqrt(n)
    lower[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(  alpha/2, df=df)
    upper[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(1-alpha/2, df=df)

    scale <- if (bessels_correction) df else n

    se[["sigma"]] <- estimate[["sigma"]] / sqrt(2*scale)
    lower[["sigma"]] <- sqrt(scale * estimate[["sigma"]]^2 / qchisq(1-alpha/2, df=df))
    upper[["sigma"]] <- sqrt(scale * estimate[["sigma"]]^2 / qchisq(  alpha/2, df=df))

  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(  alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(1-alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
  } else {
    d <- S7::super(distribution, Distribution)
    return(parameter_inference_default(d, data, ..., ci_level=ci_level, bessels_correction=bessels_correction))
  }

  return(data.frame(key=keys, estimate=estimate, se=se, lower=lower, upper=upper))
}

S7::method(parameter_inference_default, NormalSigma2) <- function(distribution, data, ..., ci_level=0.95, bessels_correction=TRUE) {
  estimates <- point_estimates(distribution, data, bessels_correction=bessels_correction)
  keys <- names(estimates@values)
  alpha <- 1-ci_level
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimate <- estimates@values
  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(x) / sqrt(n)
    lower[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(  alpha/2, df=df)
    upper[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(1-alpha/2, df=df)

    scale <- if (bessels_correction) df else n

    se[["sigma2"]] <- estimate[["sigma2"]] * sqrt(2/scale)
    lower[["sigma2"]] <- scale * estimate[["sigma2"]] / qchisq(1-alpha/2, df=df)
    upper[["sigma2"]] <- scale * estimate[["sigma2"]] / qchisq(  alpha/2, df=df)

  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(  alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(1-alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
  } else {
    d <- S7::super(distribution, Distribution)
    return(parameter_inference_default(d, data, ..., ci_level=ci_level, bessels_correction=bessels_correction))
  }

  return(data.frame(key=keys, estimate=estimate, se=se, lower=lower, upper=upper))
}

S7::method(parameter_inference_default, NormalTau) <- function(distribution, data, ..., ci_level=0.95, bessels_correction=TRUE) {
  estimates <- point_estimates(distribution, data, bessels_correction=bessels_correction)
  keys <- names(estimates@values)
  alpha <- 1-ci_level
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  estimate <- estimates@values
  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(x) / sqrt(n)
    lower[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(  alpha/2, df=df)
    upper[["mu"]] <- estimate[["mu"]] + se[["mu"]] * qt(1-alpha/2, df=df)

    scale <- if (bessels_correction) df else n

    se[["tau"]] <- estimate[["tau"]] * sqrt(2/scale)
    lower[["tau"]] <- qchisq(  alpha/2, df=df) * estimate[["tau"]] / scale
    upper[["tau"]] <- qchisq(1-alpha/2, df=df) * estimate[["tau"]] / scale

  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(  alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(1-alpha/2, mean=estimate[["mu"]], sd=se[["mu"]])
  } else {
    d <- S7::super(distribution, Distribution)
    return(parameter_inference_default(d, data, ..., ci_level=ci_level, bessels_correction=bessels_correction))
  }

  return(data.frame(key=keys, estimate=estimate, se=se, lower=lower, upper=upper))
}


S7::method(fit_statistics_absolute, Normal) <- function(distribution, data, ..., estimated=FALSE, bootstrap=0L) {
  if(!estimated && bootstrap==0L) {
    results = list(
      ks_test  = ks_test (distribution, data),
      cvm_test = cvm_test(distribution, data),
      ad_test  = ad_test (distribution, data)
    )
    results <- do.call(rbind, results)
  } else if (estimated && bootstrap==0L) { # analytic normality tests
    results <- try(list(
      lillie_test          = nortest::lillie.test(data),
      cvm_test             = nortest::cvm.test(data),
      ad_test              = nortest::ad.test(data),
      shapiro_wilk_test    = shapiro.test(data),
      shapiro_francia_test = nortest::sf.test(data)
    ))
    if (inherits(results, "try-error")) rlang::abort("Could not compute exact p-values for absolute fit statistics. Try bootstrap.")

    statistic <- vapply(results, "[[", numeric(1), "statistic")
    p_value <- vapply(results, "[[", numeric(1), "p.value")

    results <- data.frame(test = names(results), statistic = statistic, p_value = p_value)
  } else {
    results <- fit_statistics_absolute(distribution, data, estimated=FALSE, bootstrap=0L)
    boot_fn <- function(distribution, data, estimated, ...) {
      n <- length(data)
      data_boot <- rng(distribution, n)
      if (estimated) dist <- fit_distribution(distribution, data_boot, ...) else dist <- distribution
      res <- fit_statistics_absolute(dist, data_boot, estimated=FALSE, bootstrap=0L)
      return(res[["statistic"]])
    }

    statistics <- replicate(bootstrap, boot_fn(distribution=distribution, data=data, estimated=estimated, ...))
    # compare to observed to get boostrapped p-vals
    results[["p_value"]] <- sweep(statistics, 1, results[["statistic"]], ">") |> rowMeans()
  }

  return(results)
}
