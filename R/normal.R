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

NormalKappa <- S7::new_class(
  "NormalKappa",
  parent = Normal,
  properties = list(
    mu = Parameter,
    kappa = Parameter
  ),
  constructor = function(mu, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = Real(),
      mu = Parameter("mu", "mean", "\\mu", mu, Real()),
      kappa = Parameter("kappa", "inverse of std.deviation", "\\kappa", kappa, Real(0))
    )
  }
)

normal <- function(mu, sigma, sigma2, tau, kappa) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau, kappa)
  distribution <- switch(
    parametrization,
    sigma = NormalSigma(mu=mu, sigma=sigma),
    sigma2 = NormalSigma2(mu=mu, sigma2=sigma2),
    tau = NormalTau(mu=mu, tau=tau),
    kappa = NormalKappa(mu=mu, kappa=kappa)
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

S7::method(variance, NormalKappa)  <- function(distribution, ...) 1 / distribution@kappa@value^2


S7::method(std_dev, NormalSigma)  <- function(distribution, ...) distribution@sigma@value

S7::method(std_dev, NormalSigma2) <- function(distribution, ...) sqrt(distribution@sigma2@value)

S7::method(std_dev, NormalTau)    <- function(distribution, ...) sqrt(1/distribution@tau@value)

S7::method(std_dev, NormalKappa)  <- function(distribution, ...) distribution@kappa@value


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
    estimates[["sigma"]] <- sqrt(sum(data-mu)^2 / df)
  }

  return(Estimates(values=estimates))
}

S7::method(parameter_inference_default, NormalSigma) <- function(distribution, data, ..., ci_level=0.95, bessels_correction=TRUE) {
  estimates <- point_estimates(distribution, data, bessels_correction=bessels_correction)
  keys <- names(estimates@values)
  alpha <- 1-ci_level
  n <- length(data)

  fixed <- parameter_properties(distribution, "fixed") |> unlist()

  if (all(fixed)) {
    return(NULL)
  } else if (any(fixed)) {
    d <- S7::super(distribution, Distribution)
    return(parameter_inference_default(d, data, ..., ci_level=ci_level, bessels_correction=bessels_correction))
  } else {
    estimate <- numeric()
    se       <- numeric()
    lower    <- numeric()
    upper    <- numeric()

  }

  return(data.frame(key=character(), estimate=estimate, se=se, lower=lower, upper=upper))
}

# S7::method(parameter_inference_default, list(distribution, estimates)) <- function(distribution, estimates, data=NULL, ..., ci_level=0.95, bessels_correction=TRUE) {
#   estimates <- estimates@values
#   keys <- names(estimates)
#   alpha <- 1-ci_level
#   p_ci <- c(lower=alpha/2, upper=1-alpha/2)
#   n <- length(data)
#
#   df <- n - 1
#   m <- mean(x)
#   s2 <- var(x)
#   chiSq <- qchisq(p_ci, df)
#
#   se <- numeric()
#   lower <- numeric()
#   upper <- numeric()
#
#   if (is.fixed(distribution@mu) && is.fixed(distribution@sigma)) {
#     output <- NULL
#   } else if (is.fixed(distribution@sigma)) {
#
#
#   } else if (is.fixed(distribution@mu)) {
#
#   } else {
#     df <- n - 1
#     se[["mu"]] <- sqrt(s2) / sqrt(n)
#     lower[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(p_ci[["lower"]], df=df)
#     upper[["mu"]] <- estimates[["mu"]] + se[["mu"]] * qt(p_ci[["upper"]], df=df)
#
#     if(bessels_correction) {
#       se[["sigma"]] <-
#
#     }
#
#   }
#
# }



# S7::method(mle, normal) <- function(distribution, x, bessels_correction=FALSE, ci_level=0.95, ...) {
#   alpha <- 1-ci_level
#   p_ci <- c(alpha/2, 1-alpha/2)
#
#   x <- na.omit(x)
#   n <- length(x)
#
#   fixed <- parameter_properties(distribution, "fixed") |> unlist()
#
#   if (all(!fixed)) {
#     df <- n - 1
#     mu <- mean(x)
#     sigma2 <- var(x)
#     chiSq <- qchisq(p_ci, df)
#
#     if (bessels_correction) {
#       sigma2_ci <- sigma2 * df / rev(chiSq)
#       se_scale <- df
#     } else {
#       sigma2 <- sigma2 * df / n
#       sigma2_ci <- sigma2 * n / rev(chiSq)
#       se_scale <- n
#     }
#
#     t <- qt(p_ci, df=df)
#     mu_se <- sqrt(sigma2) / sqrt(n)
#     mu_ci <- mu + t * mu_se
#
#     loc <- data.frame(
#       key = "mu", estimate = mu, se = mu_se, lower = mu_ci[1], upper = mu_ci[2]
#     )
#
#     if (!is.null(distribution@parameters[["sigma2"]])) {
#       scale <- data.frame(
#         key="sigma2",
#         estimate=sigma2,
#         se=sigma2 * sqrt(2) / se_scale,
#         lower=sigma2_ci[1],
#         upper=sigma2_ci[2])
#     } else if (!is.null(distribution@parameters[["sigma"]])) {
#       scale <- data.frame(
#         key="sigma",
#         estimate=sqrt(sigma2),
#         se=sqrt(sigma2 / (2*se_scale)),
#         lower=sqrt(sigma2_ci[1]),
#         upper=sqrt(sigma2_ci[2]))
#     } else if (!is.null(distribution@parameters[["tau"]])) {
#       scale <- data.frame(
#         key="tau",
#         estimate=1/sigma2,
#         se=sqrt(2)/(sigma2 * se_scale),
#         lower=1/sigma2_ci[1],
#         upper=1/sigma2_ci[2])
#     } else if (!is.null(distribution@parameters[["tau"]])) {
#       scale <- data.frame(
#         key="kappa",
#         estimate=1/sqrt(sigma2),
#         se=1/sqrt(2*sigma2*se_scale),
#         lower=1/sqrt(sigma2_ci[1]),
#         upper=1/sqrt(sigma2_ci[2]))
#     }
#
#     return(rbind(loc, scale))
#
#   } else if (fixed[["mu"]]) {
#     mu <- parameter_values(distribution)[["mu"]]
#     sigma2 <- mean((x-mu)^2)
#
#   } else if (!all(fixed)) {
#     mu <- mean(x)
#
#     return(data.frame(estimate=mu))
#   } else {
#     rlang::abort("All parameters are fixed, nothing to estimate!")
#   }
#
#
# }
