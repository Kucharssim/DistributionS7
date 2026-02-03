SkewedGeneralizedT <- S7::new_class(
  "SkewedGeneralizedT",
  parent = DistributionContinuous,
  properties = list(
    mu = Parameter,
    sigma = Parameter,
    lambda = Parameter,
    pp = Parameter,
    qq = Parameter
  ),
  constructor = function(mu, sigma, lambda, p, q) {
    S7::new_object(
      S7::S7_object(),
      name = "Skewed generalized t",
      support = Real(),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0)),
      lambda = Parameter("lambda", "skew", "\\lambda", lambda, Real(min=-1, max=1)),
      pp = Parameter("pp", "kurtosis", "p", p, Real(min=0)),
      qq = Parameter("qq", "kurtosis", "q", q, Real(min=0))
    )
  }
)

skewed_generalized_t <- function(mu, sigma, lambda, p, q) SkewedGeneralizedT(mu, sigma, lambda, p, q)

S7::method(pdf_fn, SkewedGeneralizedT) <- function(distribution) function(x, mu, sigma, lambda, pp, qq, log=FALSE){
  sgt::dsgt(x, mu, sigma, lambda, pp, qq, mean.cent = FALSE, var.adj = FALSE, log=log)
}

S7::method(cdf_fn, SkewedGeneralizedT) <- function(distribution) function(q, mu, sigma, lambda, pp, qq, lower.tail=TRUE, log.p=FALSE){
  sgt::psgt(q, mu, sigma, lambda, pp, qq, mean.cent = FALSE, var.adj = FALSE, lower.tail=lower.tail, log.p=log.p)
}

S7::method(qf_fn, SkewedGeneralizedT) <- function(distribution) function(p, mu, sigma, lambda, pp, qq, lower.tail=TRUE, log.p=FALSE){
  sgt::qsgt(p, mu, sigma, lambda, pp, qq, mean.cent = FALSE, var.adj = FALSE, lower.tail=lower.tail, log.p=log.p)
}

S7::method(rng_fn, SkewedGeneralizedT) <- function(distribution) function(n, mu, sigma, lambda, pp, qq, lower.tail=TRUE, log.p=FALSE){
  sgt::rsgt(n, mu, sigma, lambda, pp, qq, mean.cent = FALSE, var.adj = FALSE)
}

S7::method(rargs, SkewedGeneralizedT) <- function(distribution) parameter_values(distribution)

S7::method(parameter_start, SkewedGeneralizedT) <- function(distribution, data) {
  start = list()
  if (!distribution@mu@fixed)     start[["mu"]] <- median(data)
  if (!distribution@sigma@fixed)  start[["sigma"]] <- stats::IQR(data, type = 8)/2
  if (!distribution@lambda@fixed) start[["lambda"]] <- 0
  if (!distribution@pp@fixed)     start[["pp"]] <- 2
  if (!distribution@qq@fixed)     start[["qq"]] <- 2

  parameter_values(distribution) <- start
  return(distribution)
}

parameter_estimates(
  skewed_generalized_t(0, 1, 0.5, fixed(5), fixed(5)),
  Mle(),
  rng(
    skewed_generalized_t(4, 2, -0.5, 5, 5),
    1000
  )
)
