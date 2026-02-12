#' @title T-distribution
#' @description Create a t-distribution object.
#'
#' @param nu degrees of freedom parameter.
#' @param kappa noncentrality parameter.
#' @param mu location parameter.
#' @param sigma scale parameter.
#' @family distributions
#' @name t-distribution
#' @export
StandardT <- S7::new_class(
  "StandardT",
  parent = DistributionContinuous,
  properties = list(nu = Parameter),
  constructor = function(nu) {
    S7::new_object(
      S7::S7_object(),
      name = "Standard t",
      support = Real(),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0))
    )
  }
)

#' @rdname t-distribution
#' @export
StudentT <- S7::new_class(
  "StudentT",
  parent = StandardT,
  properties = list(mu = Parameter, sigma = Parameter),
  constructor = function(nu, mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Student t",
      support = Real(),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0)),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)

#' @rdname t-distribution
#' @export
NoncentralT <- S7::new_class(
  "NoncentralT",
  parent = StandardT,
  properties = list(kappa = Parameter),
  constructor = function(nu, kappa) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral t",
      support = Real(),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0)),
      kappa = Parameter("kappa", "noncentrality", "\\kappa", kappa, Real())
    )
  }
)


#' @rdname t-distribution
#' @export
NoncentralStudentT <- S7::new_class(
  "NoncentralStudentT",
  parent = StudentT,
  properties = list(kappa = Parameter),
  constructor = function(nu, kappa, mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral Student t",
      support = Real(),
      nu = Parameter("nu", "degrees of freedom", "\\nu", nu, Real(min=0)),
      kappa = Parameter("kappa", "noncentrality", "\\kappa", kappa, Real()),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)

S7::method(pdf_fn, StandardT) <- function(distribution) function(x, nu, kappa=0, mu=0, sigma=1, log=FALSE) {
  xi <- (x-mu)/sigma
  lpdf <- dt(xi, df=nu, ncp=kappa, log=TRUE) - log(sigma)
  if(log) return(lpdf) else return(exp(lpdf))
}

S7::method(cdf_fn, StandardT) <- function(distribution) function(q, nu, kappa=0, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) {
  qi <- (q-mu)/sigma
  return(pt(qi, df=nu, ncp=kappa, lower.tail = lower.tail, log.p = log.p))
}

S7::method(qf_fn, StandardT) <- function(distribution) function(p, nu, kappa=0, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) {
  qi <- qt(p, df=nu, ncp=kappa, lower.tail = lower.tail, log.p = log.p)
  return(qi * sigma + mu)
}

S7::method(rng_fn, StandardT) <- function(distribution) function(n, nu, kappa=0, mu=0, sigma=1) {
  xi <- rt(n, df=nu, ncp=kappa)
  return(xi * sigma + mu)
}

S7::method(rargs, StandardT) <- function(distribution) parameter_values(distribution)

S7::method(parameter_start, StandardT) <- function(distribution, data) {
  if (S7::prop_exists(distribution, "mu") && !distribution@mu@fixed)
    distribution@mu@value    <- median(data)
  if (S7::prop_exists(distribution, "sigma") && !distribution@sigma@fixed)
    distribution@sigma@value <- stats::IQR(data, type = 8)/2

  return(distribution)
}


