CentralF <- S7::new_class(
  "CentralF",
  parent = DistributionContinuous,
  properties = list(
    nu1 = Parameter,
    nu2 = Parameter
  ),
  constructor = function(nu1, nu2, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral F",
      support = Real(min=0),
      nu1 = Parameter("nu1", "degrees of freedom", "\\nu_1", nu1, Real(min=0)),
      nu2 = Parameter("nu2", "degrees of freedom", "\\nu_2", nu2, Real(min=0))
    )
  }
)

NoncentralF <- S7::new_class(
  "NoncentralF",
  parent = CentralF,
  properties = list(
    kappa = Parameter
  ),
  constructor = function(nu1, nu2, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral F",
      support = Real(min=0),
      nu1 = Parameter("nu1", "degrees of freedom", "\\nu_1", nu1, Real(min=0)),
      nu2 = Parameter("nu2", "degrees of freedom", "\\nu_2", nu2, Real(min=0)),
      kappa = Parameter("kappa", "noncentrality parameter", "\\kappa", kappa, Real(min=0))
    )
  }
)

central_f <- function(nu1, nu2) CentralF(nu1, nu2)
noncentral_f <- function(nu1, nu2, kappa) NoncentralF(nu1, nu2, kappa)

S7::method(pdf_fn, CentralF) <- function(distribution) stats::df
S7::method(cdf_fn, CentralF) <- function(distribution) stats::pf
S7::method(qf_fn,  CentralF) <- function(distribution) stats::qf
S7::method(rng_fn, CentralF) <- function(distribution) stats::rf

S7::method(rargs, CentralF) <- function(distribution) {
  list(df1 = distribution@nu1@value, df2 = distribution@nu2@value)
}

S7::method(rargs, NoncentralF) <- function(distribution) {
  list(df1 = distribution@nu1@value, df2 = distribution@nu2@value, ncp = distribution@kappa@value)
}

S7::method(parameter_estimates, list(CentralF, Mom)) <- function(distribution, estimator, data) {
  rlang::inform("Moments matching for degrees of freedom of an F-distribution is extremely crude.")

  estimates <- list()
  m <- mean(data)
  if (!distribution@nu2@fixed) {
    assertthat::assert_that(m > 1, msg = "Mean of the data must be larger than 1.")
    nu2 <- 2 * m / (m - 1)
    estimates[["nu2"]] <- nu2
  } else {
    nu2 <- distribution@nu@value
  }

  if (!distribution@nu1@fixed) {
    assertthat::assert_that(nu2 > 4, msg = "degrees of freedom `nu2` must be larger than 4 to estimate 'nu1'.")
    v <- var(data)
    nu1 <- 2 * m^2 / (m^2 - m^3 + (2 - m)*v)
    assertthat::assert_that(nu1 > 0, msg = "degrees of freedom `nu1` estimates were negative.")
    estimates[["nu1"]] <- nu1
  }
  return(estimates[sort(names(estimates))])
}

S7::method(parameter_estimates, list(NoncentralF, Mom)) <- function(distribution, estimator, data) {
  d <- S7::super(distribution, CentralF)
  estimates <- parameter_estimates(d, estimator, data)

  if (!distribution@kappa@fixed){
    rlang::inform("Parameter `kappa` is not estimated with moments matching.")
    estimates[["kappa"]] <- distribution@kappa@value
  }

  return(estimates)
}
