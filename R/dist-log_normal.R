LogNormal <- S7::new_class(
  "LogNormal",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    sigma = Parameter
  ),
  constructor = function(mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "LogNormal",
      support = Real(min=0),
      mu = Parameter("mu", "log mean", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "log std.deviation", "\\sigma", sigma, Real(0))
    )
  }
)

log_normal <- function(mu, sigma) LogNormal(mu, sigma)

S7::method(pdf_fn, LogNormal) <- function(distribution) stats::dlnorm
S7::method(cdf_fn, LogNormal) <- function(distribution) stats::plnorm
S7::method(qf_fn, LogNormal)  <- function(distribution) stats::qlnorm
S7::method(rng_fn, LogNormal) <- function(distribution) stats::rlnorm

S7::method(rargs, LogNormal) <- function(distribution) {
  return(list(meanlog=distribution@mu@value, sdlog=distribution@sigma@value))
}

S7::method(parameter_estimates, list(LogNormal, Estimator)) <- function(distribution, estimator, data) {
  data <- log(data)
  parameters <- recreate_parameters(distribution)
  distribution <- do.call(normal, parameters)
  parameter_estimates(distribution, estimator, data)
}
