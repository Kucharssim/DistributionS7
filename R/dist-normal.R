#' @title Normal distribution
#' @description Create a normal distribution object.
#'
#' @param mu Mean.
#' @param sigma Standard deviation.
#' @param sigma2 Variance.
#' @param tau Precision.
#'
#' @importFrom nortest lillie.test
#' @importFrom nortest sf.test
#' @family distributions
#' @export
Normal <- function(mu, sigma, sigma2, tau) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau)
  distribution <- switch(
    parametrization,
    sigma = NormalSigma(mu=mu, sigma=sigma),
    sigma2 = NormalSigma2(mu=mu, sigma2=sigma2),
    tau = NormalTau(mu=mu, tau=tau)
  )

  return(distribution)
}

NormalClass <- S7::new_class(
  "NormalClass",
  parent = DistributionContinuous,
  abstract = TRUE
)

#' @rdname Normal
#' @export
StandardNormal <- S7::new_class(
  "StandardNormal",
  parent = NormalClass,
  constructor = function() {
    S7::new_object(
      S7::S7_object(),
      name = "Standard normal",
      support = Real()
    )
  }
)

NormalSigma <- S7::new_class(
  "NormalSigma",
  parent = NormalClass,
  properties = list(
    mu = Parameter,
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
  parent = NormalClass,
  properties = list(
    mu = Parameter,
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
  parent = NormalClass,
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


S7::method(pdf_fn, NormalClass) <- function(distribution) stats::dnorm

S7::method(cdf_fn, NormalClass) <- function(distribution) stats::pnorm

S7::method(qf_fn, NormalClass)  <- function(distribution) stats::qnorm

S7::method(rng_fn, NormalClass) <- function(distribution) stats::rnorm

S7::method(rargs, StandardNormal) <- function(distribution) return(list(mean=0, sd=1))

S7::method(rargs, NormalSigma) <- function(distribution) {
  return(list(mean = distribution@mu@value, sd = distribution@sigma@value))
}

S7::method(rargs, NormalSigma2) <- function(distribution) {
  return(list(mean = distribution@mu@value, sd = sqrt(distribution@sigma2@value)))
}

S7::method(rargs, NormalTau) <- function(distribution) {
  return(list(mean = distribution@mu@value, sd = 1/sqrt(distribution@sigma2@value)))
}

S7::method(parameter_estimates, list(NormalSigma, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (is.fixed(distribution@mu)) {
    mu <- distribution@mu@value
  } else {
    mu <- mean(data)
    estimates[["mu"]] <- mu
  }

  if (is.fixed(distribution@sigma)) return(estimates)

  estimates[["sigma"]] <- sqrt(sum((data-mu)^2) / length(data))

  return(estimates)
}

S7::method(parameter_estimates, list(NormalSigma2, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (is.fixed(distribution@mu)) {
    mu <- distribution@mu@value
  } else {
    mu <- mean(data)
    estimates[["mu"]] <- mu
  }

  if (is.fixed(distribution@sigma2)) return(estimates)

  estimates[["sigma2"]] <- sum((data-mu)^2) / length(data)

  return(estimates)
}

S7::method(parameter_estimates, list(NormalTau, Mle)) <- function(distribution, estimator, data) {
  estimates <- list()

  if (is.fixed(distribution@mu)) {
    mu <- distribution@mu@value
  } else {
    mu <- mean(data)
    estimates[["mu"]] <- mu
  }

  if (is.fixed(distribution@tau)) return(estimates)

  estimates[["tau"]] <- 1/(sum((data-mu)^2) / length(data))

  return(estimates)
}

S7::method(parameter_estimates, list(NormalSigma, BiasCorrected)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, Mle(), data)
  if (!is.null(estimates[["sigma"]])) {
    n <- length(data)
    df <- n-1
    sigma2 <- estimates[["sigma"]]^2
    estimates[["sigma"]] <- sqrt(sigma2 * n / df)
  }

  return(estimates)
}

S7::method(parameter_estimates, list(NormalSigma2, BiasCorrected)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, Mle(), data)
  if (!is.null(estimates[["sigma2"]])) {
    n <- length(data)
    df <- n-1
    sigma2 <- estimates[["sigma2"]]
    estimates[["sigma2"]] <- sigma2 * n / df
  }

  return(estimates)
}

S7::method(parameter_estimates, list(NormalTau, BiasCorrected)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, Mle(), data)
  if (!is.null(estimates[["tau"]])) {
    n <- length(data)
    df <- n-1
    sigma2 <- 1/estimates[["tau"]]
    estimates[["tau"]] <- 1 / (sigma2 * n / df)
  }

  return(estimates)
}

S7::method(parameter_inference, list(NormalSigma, DefaultMethod)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  keys <- names(estimates)
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(data) / sqrt(n)
    lower[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@lower, df=df)
    upper[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@upper, df=df)

    scale <- if (S7::S7_inherits(inference_method@estimator, BiasCorrected)) df else n

    se[["sigma"]] <- estimates[["sigma"]] / sqrt(2*scale)
    lower[["sigma"]] <- sqrt(scale * estimates[["sigma"]]^2 / qchisq(inference_method@upper, df=df))
    upper[["sigma"]] <- sqrt(scale * estimates[["sigma"]]^2 / qchisq(inference_method@lower, df=df))

  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(inference_method@lower, mean=estimates[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(inference_method@upper, mean=estimates[["mu"]], sd=se[["mu"]])
  } else {
    distribution <- S7::super(distribution, Distribution)
    return(parameter_inference(distribution, inference_method, data))
  }

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}

S7::method(parameter_inference, list(NormalSigma2, DefaultMethod)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  keys <- names(estimates)
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(data) / sqrt(n)
    lower[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@lower, df=df)
    upper[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@upper, df=df)

    scale <- if (S7::S7_inherits(inference_method@estimator, BiasCorrected)) df else n

    se[["sigma2"]] <- estimates[["sigma2"]] * sqrt(2/scale)
    lower[["sigma2"]] <- scale * estimates[["sigma2"]] / qchisq(inference_method@upper, df=df)
    upper[["sigma2"]] <- scale * estimates[["sigma2"]] / qchisq(inference_method@lower, df=df)
  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(inference_method@lower, mean=estimates[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(inference_method@upper, mean=estimates[["mu"]], sd=se[["mu"]])
  } else {
    distribution <- S7::super(distribution, Distribution)
    return(parameter_inference(distribution, inference_method, data))
  }

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}

S7::method(parameter_inference, list(NormalTau, DefaultMethod)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  keys <- names(estimates)
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  se       <- numeric(length = sum(!fixed)) |> setNames(keys)
  lower    <- numeric(length = sum(!fixed)) |> setNames(keys)
  upper    <- numeric(length = sum(!fixed)) |> setNames(keys)

  if (all(!fixed)) {
    df <- n-1
    se[["mu"]] <- sd(data) / sqrt(n)
    lower[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@lower, df=df)
    upper[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(inference_method@upper, df=df)

    scale <- if (S7::S7_inherits(inference_method@estimator, BiasCorrected)) df else n

    se[["tau"]] <- estimates[["tau"]] * sqrt(2/scale)
    lower[["tau"]] <- qchisq(inference_method@lower, df=df) * estimates[["tau"]] / scale
    upper[["tau"]] <- qchisq(inference_method@upper, df=df) * estimates[["tau"]] / scale
  } else if (!fixed["mu"]) {
    se[["mu"]] <- std_dev(distribution) / sqrt(n)
    lower[["mu"]] <- qnorm(inference_method@lower, mean=estimates[["mu"]], sd=se[["mu"]])
    upper[["mu"]] <- qnorm(inference_method@upper, mean=estimates[["mu"]], sd=se[["mu"]])
  } else {
    distribution <- S7::super(distribution, Distribution)
    return(parameter_inference(distribution, inference_method, data))
  }

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}


S7::method(gof_test, NormalClass) <- function(distribution, data, estimated=FALSE, bootstrap=Bootstrap(samples=0L)) {
  if(estimated && bootstrap@samples == 0) { # analytic normality tests
    results <- try(list(
      lillie_test          = nortest::lillie.test(data),
      cvm_test             = nortest::cvm.test(data),
      ad_test              = nortest::ad.test(data),
      shapiro_wilk_test    = stats::shapiro.test(data),
      shapiro_francia_test = nortest::sf.test(data)
    ))
    if (inherits(results, "try-error")) rlang::abort("Could not compute exact p-values for absolute fit statistics. Try bootstrap.")
    statistic <- vapply(results, "[[", numeric(1), "statistic")
    p_value <- vapply(results, "[[", numeric(1), "p.value")

    results <- data.frame(test = names(results), statistic = statistic, p_value = p_value)
    return(results)
  }

  distribution <- S7::super(distribution, DistributionContinuous)
  gof_test(distribution = distribution, data = data, estimated = estimated, bootstrap = bootstrap)
}

S7::method(gof_test, StandardNormal) <- function(distribution, data, estimated=FALSE, bootstrap=Bootstrap(samples=0L)) {
  if (estimated) rlang::inform("Ignoring `estimated=TRUE`; Standard normal is always fixed")
  estimated <- FALSE
  distribution <- S7::super(distribution, DistributionContinuous)
  gof_test(distribution = distribution, data = data, estimated = estimated, bootstrap = bootstrap)
}
