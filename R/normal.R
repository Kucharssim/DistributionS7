normal <- S7::new_class(
  "normal",
  parent = distribution_continuous,
  constructor = function(mu, sigma, sigma2, tau, kappa) {
    parametrization <- rlang::check_exclusive(sigma, sigma2, tau, kappa)

    parameters <- list(
      mu = parameter(key="mu", label="\\mu", value=mu, support=real())
    )

    if (parametrization == "sigma") {
      parameters[["sigma"]] <- parameter(key="sigma", label="\\sigma", value=sigma, support=real(0))

      rargs <- list(mean=expression(mu), sd=expression(sigma))
    } else if (parametrization == "sigma2") {
      parameters[["sigma2"]] <- parameter(key="sigma2", label="\\sigma^2", value=sigma2, support=real(0))

      rargs=list(mean=expression(mu), sd=expression(sqrt(sigma2)))
    } else if (parametrization == "tau") {
      parameters[["tau"]] <- parameter(key="tau", label="\\tau^2", value=tau, support=real(0))

      rargs=list(mean=expression(mu), sd=expression(1/tau))
    } else {
      parameters[["kappa"]] <- parameter(key="kappa", label="\\kappa^2", value=kappa, support=real(0))

      rargs=list(mean=expression(mu), sd=expression(1/sqrt(kappa)))
    }

    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      parameters = parameters,
      rargs = rargs
    )
  })

S7::method(pdf_fn, normal) <- function(distribution) stats::dnorm

S7::method(cdf_fn, normal) <- function(distribution) stats::pnorm

S7::method(qf_fn, normal)  <- function(distribution) stats::qnorm

S7::method(rng_fn, normal) <- function(distribution) stats::rnorm

