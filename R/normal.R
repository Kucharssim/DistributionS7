normal <- S7::new_class(
  "normal",
  parent = distribution_continuous,
  properties = list(
    mu = parameter
  ),
  abstract = TRUE
)

normal_sigma <- S7::new_class(
  "normal_sigma",
  parent = normal,
  properties = list(
    mu = parameter,
    sigma = parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      mu = parameter("mu", "mean", "\\mu", mu, real()),
      sigma = parameter("sigma", "std.deviation", "\\sigma", sigma, real(0))
    )
  }
)

normal_sigma2 <- S7::new_class(
  "normal_sigma2",
  parent = normal,
  properties = list(
    mu = parameter,
    sigma2 = parameter
  ),
  constructor = function(mu, sigma2) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      mu = parameter("mu", "mean", "\\mu", mu, real()),
      sigma2 = parameter("sigma2", "variance", "\\sigma^2", sigma2, real(0))
    )
  }
)

normal_tau <- S7::new_class(
  "normal_tau",
  parent = normal,
  properties = list(
    mu = parameter,
    tau = parameter
  ),
  constructor = function(mu, tau) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      mu = parameter("mu", "mean", "\\mu", mu, real()),
      tau = parameter("tau", "precision", "\\tau", tau, real(0))
    )
  }
)

normal_kappa <- S7::new_class(
  "normal_kappa",
  parent = normal,
  properties = list(
    mu = parameter,
    kappa = parameter
  ),
  constructor = function(mu, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      mu = parameter("mu", "mean", "\\mu", mu, real()),
      kappa = parameter("kappa", "inverse of std.deviation", "\\kappa", kappa, real(0))
    )
  }
)

Normal <- function(mu, sigma, sigma2, tau, kappa) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau, kappa)
  distribution <- switch(
    parametrization,
    sigma = normal_sigma(mu=mu, sigma=sigma),
    sigma2 = normal_sigma2(mu=mu, sigma2=sigma2),
    tau = normal_tau(mu=mu, tau=tau),
    kappa = normal_kappa(mu=mu, kappa=kappa)
    )

  return(distribution)
}


S7::method(pdf_fn, normal) <- function(distribution) stats::dnorm

S7::method(cdf_fn, normal) <- function(distribution) stats::pnorm

S7::method(qf_fn, normal)  <- function(distribution) stats::qnorm

S7::method(rng_fn, normal) <- function(distribution) stats::rnorm


S7::method(expectation, normal) <- function(distribution, ...) distribution@mu@value


S7::method(variance, normal_sigma)  <- function(distribution, ...) distribution@sigma@value^2

S7::method(variance, normal_sigma2) <- function(distribution, ...) distribution@sigma2@value

S7::method(variance, normal_tau)    <- function(distribution, ...) 1 / distribution@tau@value

S7::method(variance, normal_kappa)  <- function(distribution, ...) 1 / distribution@kappa@value^2


S7::method(std_dev, normal_sigma)  <- function(distribution, ...) distribution@sigma@value

S7::method(std_dev, normal_sigma2) <- function(distribution, ...) sqrt(distribution@sigma2@value)

S7::method(std_dev, normal_tau)    <- function(distribution, ...) sqrt(1/distribution@tau@value)

S7::method(std_dev, normal_kappa)  <- function(distribution, ...) distribution@kappa@value


S7::method(skewness, normal) <- function(distribution, ...) 0

S7::method(kurtosis, normal) <- function(distribution, ...) 3

S7::method(excess_kurtosis, normal) <- function(distribution, ...) 0


S7::method(rargs, normal) <- function(distribution, ...) {
  return(
    list(
      mean = expectation(distribution),
      sd = std_dev(distribution)
    )
  )
}

S7::method(sufficient_statistics, normal) <- function(distribution, x, ...) {
  x <- na.omit(x)
  n <- length(x)
  mean <- mean(x)
  ss <- sum((x - mean)^2) # sum of squares
  return(list(n=n, mean=mean, ss=ss))
}


S7::method(mle, normal_sigma) <- function(distribution, x, ci_level=0.95, ...) {

}


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
