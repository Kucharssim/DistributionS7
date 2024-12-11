normal <- S7::new_class(
  "normal",
  parent = distribution_continuous,
  properties = parameter_properties(c("mu", "sigma", "sigma2")),
  constructor = function(mu, sigma, sigma2) {
    parametrization <- rlang::check_exclusive(sigma, sigma2)

    parameters <- switch(
      parametrization,
      sigma = pars(
        par(key="mu", label="\\mu", value=mu, support=real()),
        par(key="sigma", label="\\sigma", value=sigma, support=real(0)),
        tpar(key="sigma2", label="\\sigma^2", value=expression(sigma^2), update=list(sigma=expression(sqrt(sigma2)))),
        rargs=list(mean=expression(mu), sd=expression(sigma))
      ),
      sigma2 = pars(
        par(key="mu", label="\\mu", value=mu, support=real()),
        par(key="sigma2", label="\\sigma^2", value=sigma2, support=real(0)),
        tpar(key="sigma", label="\\sigma", value=expression(sqrt(sigma2)), update=list(sigma2=expression(sigma^2))),
        rargs=list(mean=expression(mu), sd=expression(sigma))
      )
    )

    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      parameters = parameters
    )
  })

S7::method(pdf_fn, normal) <- function(distribution) stats::dnorm

S7::method(cdf_fn, normal) <- function(distribution) stats::pnorm

S7::method(qf_fn, normal)  <- function(distribution) stats::qnorm

S7::method(rng_fn, normal) <- function(distribution) stats::rnorm

