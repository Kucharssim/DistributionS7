Beta <- S7::new_class(
  "Beta",
  parent = DistributionContinuous,
  properties = list(
    alpha = Parameter,
    beta = Parameter
  ),
  constructor = function(alpha, beta) {
    S7::new_object(
      S7::S7_object(),
      name = "Beta",
      support = Real(min=0, max=1),
      alpha = Parameter("alpha", "shape 1", "\\alpha", alpha, Real(min=0)),
      beta = Parameter("beta", "shape 2", "\\beta", beta, Real(min=0))
    )
  }
)

beta <- function(alpha, beta) Beta(alpha, beta)

S7::method(pdf_fn, Beta) <- function(distribution) stats::dbeta

S7::method(cdf_fn, Beta) <- function(distribution) stats::pbeta

S7::method(qf_fn, Beta)  <- function(distribution) stats::qbeta

S7::method(rng_fn, Beta) <- function(distribution) stats::rbeta

S7::method(rargs, Beta) <- function(distribution, ...) {
  return(list(shape1=distribution@alpha@value, shape2=distribution@beta@value))
}

S7::method(parameter_estimates, list(Beta, Mom)) <- function(distribution, estimator, data) {
  fixed <- unlist(parameter_properties(distribution, property="fixed"))
  estimates <- list()

  if (all(!fixed)) {
    x_bar <- mean(x)
    xm_bar <- 1-x_bar
    v <- var(x)

    assertthat::assert_that(
      v < x_bar * xm_bar,
      msg="Sample variance is larger than sample mean times complementary sample mean,
      estimates of parameters based on moments are undefined")

    estimates[["alpha"]] <- x_bar * (x_bar * xm_bar / v - 1)
    estimates[["beta"]] <- xm_bar * (x_bar * xm_bar / v - 1)
  } else if (!fixed[["alpha"]]) {
    x_bar <- mean(x)
    estimates[["alpha"]] <- x_bar * distribution@beta@value / (1-x_bar)
  } else if (!fixed[["beta"]]) {
    x_bar <- 1-mean(x)
    estimates[["beta"]] <- x_bar * distribution@alpha@value / (1-x_bar)
  }

  return(estimates)
}

S7::method(parameter_estimates, list(Beta, Mle)) <- function(distribution, estimator, data) {
  estimates <- try(parameter_estimates(distribution, Mom(), data))
  if (!inherits(estimates, "try-error")) parameter_values(distribution) <- estimates

  distribution <- S7::super(distribution, Distribution)
  return(parameter_estimates(distribution, estimator, data))
}
