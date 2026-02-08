#' @title Normal distribution
#' @description Create a normal distribution object.
#' @name normal
#'
#' @param mu Mean.
#' @param sigma Standard deviation.
#' @param sigma2 Variance.
#' @param tau Precision.
#'
#' @importFrom nortest lillie.test
#' @importFrom nortest sf.test
#' @returns Object of class [NormalSigma()], [NormalSigma2()], or [NormalTau()].
#' @family distributions
#' @export
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

Normal <- S7::new_class(
  "Normal",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter
  ),
  abstract = TRUE
)

#' @rdname normal
#' @export
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

#' @rdname normal
#' @export
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

#' @rdname normal
#' @export
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


S7::method(gof_test, Normal) <- function(distribution, data, estimated=FALSE, bootstrap=Bootstrap(samples=0L)) {
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
