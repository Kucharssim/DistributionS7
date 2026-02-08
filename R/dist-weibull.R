Weibull <- S7::new_class(
  "Weibull",
  parent = DistributionContinuous,
  properties = list(
    shape = Parameter,
    scale = Parameter
  ),
  constructor = function(shape, scale) {
    S7::new_object(
      S7::S7_object(),
      name = "Weibull",
      support = Real(min=0),
      shape = Parameter("shape", "shape", "\\text{k}", shape, Real(min=0)),
      scale = Parameter("scale", "scale", "\\lambda", scale, Real(min=0))
    )
  }
)

weibull <- function(shape, scale) Weibull(shape, scale)

S7::method(pdf_fn, Weibull) <- function(distribution) stats::dweibull
S7::method(cdf_fn, Weibull) <- function(distribution) stats::pweibull
S7::method(qf_fn,  Weibull) <- function(distribution) stats::qweibull
S7::method(rng_fn, Weibull) <- function(distribution) stats::rweibull
S7::method(rargs,  Weibull) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(Weibull, Mom)) <- function(distribution, estimator, data) {
  estimates <- list()
  if (!distribution@shape@fixed) {
    lhs <- var(data)/mean(data)^2

    fn <- function(par, d, lhs) {
      d@shape@uvalue <- par[["ushape"]]
      shape <- d@shape@value
      g <- lgamma(1 + 2/shape)
      g2 <- 2 * lgamma(1 + 1/shape)
      rhs <- exp(g - g2) - 1
      abs(rhs - lhs)
    }

    shape <- try(stats::optim(
      par=c(ushape=distribution@shape@uvalue), fn, d=distribution, lhs=lhs, method="BFGS")
      )

    if (inherits(shape, "try-error")) rlang::abort("Could not determine `shape` parameter")

    distribution@shape@uvalue <- shape[["par"]][["ushape"]]
    estimates[["shape"]] <- distribution@shape@value
    shape <- estimates[["shape"]]
  } else {
    shape <- distribution@shape@value
  }

  if (!distribution@shape@fixed) {
    lm <- log(mean(data))
    estimates[["scale"]] <- exp(lm - lgamma(1 + 1/shape))
  }

  return(estimates)
}
