#' @title Skew distribution family
#' @description Create an object of Skew family.
#'
#' @param xi location parameter.
#' @param omega scale parameter.
#' @param alpha slant (skew) parameter.
#' @param nu degrees of freedom parameter.
#' @family distributions
#' @import sn
#' @name skew
#' @export
skew_normal <- function(xi, omega, alpha) SkewNormal(xi, omega, alpha)

#' @rdname skew
#' @export
skew_cauchy <- function(xi, omega, alpha) SkewCauchy(xi, omega, alpha)
#' @rdname skew
#' @export
skew_t      <- function(xi, omega, alpha, nu) SkewT (xi, omega, alpha, nu)


SkewDistribution <- S7::new_class(
  "SkewDistribution",
  parent = DistributionContinuous,
  properties = list(
    xi = Parameter,
    omega = Parameter,
    alpha = Parameter
  ),
  abstract = TRUE
)

#' @rdname skew
#' @export
SkewNormal <- S7::new_class(
  "SkewNormal",
  parent = SkewDistribution,
  constructor = function(xi, omega, alpha) {
    S7::new_object(
      S7::S7_object(),
      name = "Skew Normal",
      support = Real(),
      xi = Parameter("xi", "location", "\\xi", xi, Real()),
      omega = Parameter("omega", "scale", "\\omega", omega, Real(min=0)),
      alpha = Parameter("alpha", "slant", "\\alpha", alpha, Real())
    )
  }
)

#' @rdname skew
#' @export
SkewCauchy <- S7::new_class(
  "SkewCauchy",
  parent = SkewDistribution,
  constructor = function(xi, omega, alpha) {
    S7::new_object(
      S7::S7_object(),
      name = "Skew Cauchy",
      support = Real(),
      xi = Parameter("xi", "location", "\\xi", xi, Real()),
      omega = Parameter("omega", "scale", "\\omega", omega, Real(min=0)),
      alpha = Parameter("alpha", "slant", "\\alpha", alpha, Real())
    )
  }
)

#' @rdname skew
#' @export
SkewT <- S7::new_class(
  "SkewT",
  parent = SkewDistribution,
  properties = list(nu = Parameter),
  constructor = function(xi, omega, alpha, nu) {
    S7::new_object(
      S7::S7_object(),
      name = "Skew T",
      support = Real(),
      xi = Parameter("xi", "location", "\\xi", xi, Real()),
      omega = Parameter("omega", "scale", "\\omega", omega, Real(min=0)),
      alpha = Parameter("alpha", "slant", "\\alpha", alpha, Real()),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0))
    )
  }
)

S7::method(pdf_fn, SkewNormal) <- function(distribution) sn::dsn
S7::method(cdf_fn, SkewNormal) <- function(distribution) function(q, xi, omega, alpha, lower.tail=TRUE, log.p=FALSE) {
  p <- sn::psn(q, xi, omega, alpha)
  if (!lower.tail) p <- 1-p
  if (log.p) return(log(p)) else return(p)
}
S7::method(qf_fn,  SkewNormal) <- function(distribution) function(p, xi, omega, alpha, lower.tail=TRUE, log.p=FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1-p
  sn::qsn(p, xi, omega, alpha)
}

S7::method(rng_fn, SkewNormal) <- function(distribution) sn::rsn

S7::method(pdf_fn, SkewCauchy) <- function(distribution) sn::dsc
S7::method(cdf_fn, SkewCauchy) <- function(distribution) function(q, xi, omega, alpha, lower.tail=TRUE, log.p=FALSE) {
  p <- sn::psc(q, xi, omega, alpha)
  if (!lower.tail) p <- 1-p
  if (log.p) return(log(p)) else return(p)
}
S7::method(qf_fn,  SkewCauchy) <- function(distribution) function(p, xi, omega, alpha, lower.tail=TRUE, log.p=FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1-p
  sn::qsc(p, xi, omega, alpha)
}
S7::method(rng_fn, SkewCauchy) <- function(distribution) sn::rsc

S7::method(pdf_fn, SkewT) <- function(distribution) sn::dst
S7::method(cdf_fn, SkewT) <- function(distribution) function(q, xi, omega, alpha, nu, lower.tail=TRUE, log.p=FALSE) {
  p <- sn::pst(q, xi, omega, alpha, nu)
  if (!lower.tail) p <- 1-p
  if (log.p) return(log(p)) else return(p)
}
S7::method(qf_fn,  SkewT) <- function(distribution) function(p, xi, omega, alpha, nu, lower.tail=TRUE, log.p=FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1-p
  sn::qst(p, xi, omega, alpha, nu)
}
S7::method(rng_fn, SkewT) <- function(distribution) sn::rst

S7::method(rargs, SkewDistribution) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(SkewNormal, Mom)) <- function(distribution, estimator, data) {
  estimates <- list()
  m <- mean(data)
  v <- mean((data-m)^2)

  if (distribution@alpha@fixed) {
    alpha <- distribution@alpha@value
    delta <- alpha / sqrt(1 + alpha^2)
  } else {
    s <- mean(((data-m)/sqrt(v))^3)
    if(abs(s) > 0.995) {
      rlang::warn("Sample skewness is larger than the theoretical maximum")
      s <- sign(s) * 0.995
    }

    g <- abs(s)^(2/3)
    nom <- pi * g
    den <- 2*(g + ((4-pi)/2)^(2/3))
    delta <- sign(s) * sqrt(nom/den)

    estimates[["alpha"]] <- delta / sqrt(1-delta^2)
  }

  if (!distribution@omega@fixed) {
    omega <- sqrt(v) / sqrt(1 - 2 * delta^2/pi)
    estimates[["omega"]] <- omega
  } else {
    omega <- distribution@omega@value
  }

  if (!distribution@xi@fixed) estimates[["xi"]] <- m - omega * delta * sqrt(2/pi)

  return(estimates)
}

S7::method(parameter_start, SkewCauchy) <- function(distribution, data) {
  pars <- recreate_parameters(distribution)
  d <- do.call(skew_normal, pars)
  pars <- parameter_start(d, data) |> recreate_parameters()
  distribution <- do.call(skew_cauchy, pars)
  return(distribution)
}

S7::method(parameter_start, SkewT) <- function(distribution, data) {
  pars <- recreate_parameters(distribution)
  pars[["nu"]] <- NULL
  d <- do.call(skew_normal, pars)
  pars <- parameter_start(d, data) |> recreate_parameters()
  pars[["nu"]] <- distribution@nu@value
  distribution <- do.call(skew_t, pars)
  return(distribution)
}
