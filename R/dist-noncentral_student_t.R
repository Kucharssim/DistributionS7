NoncentralStudentT <- S7::new_class(
  "NoncentralStudentT",
  parent = DistributionContinuous,
  properties = list(
    nu = Parameter,
    kappa = Parameter,
    mu = Parameter,
    sigma = Parameter
  ),
  constructor = function(nu, kappa, mu, sigma) {
    S7::new_object(
      S7::S7_object(),
      name = "Noncentral t",
      support = Real(),
      nu = Parameter("nu", "degrees of freedom", "nu", nu, Real(min=0)),
      kappa = Parameter("kappa", "noncentrality", "\\kappa", kappa, Real()),
      mu = Parameter("mu", "location", "\\mu", mu, Real()),
      sigma = Parameter("sigma", "scale", "\\sigma", sigma, Real(min=0))
    )
  }
)

noncentral_student_t <- function(nu, kappa, mu, sigma) NoncentralStudentT(nu=nu, kappa=kappa, mu=mu, sigma=sigma)

noncentral_t <- function(nu, kappa) NoncentralStudentT(nu=nu, kappa=kappa, mu=fixed(0), sigma=fixed(1))

student_t <- function(nu, mu, sigma) NoncentralStudentT(nu=nu, kappa=fixed(0), mu=mu, sigma=sigma)

S7::method(pdf_fn, NoncentralStudentT) <- function(distribution) function(x, nu, kappa, mu, sigma, log=FALSE) {
  xi <- (x-mu)/sigma
  return(dt(xi, df=nu, ncp=kappa, log=log))
}

S7::method(cdf_fn, NoncentralStudentT) <- function(distribution) function(q, nu, kappa, mu, sigma, lower.tail=TRUE, log.p=FALSE) {
  qi <- (q-mu)/sigma
  return(pt(qi, df=nu, ncp=kappa, lower.tail = lower.tail, log.p = log.p))
}

S7::method(qf_fn, NoncentralStudentT) <- function(distribution) function(p, nu, kappa, mu, sigma, lower.tail=TRUE, log.p=FALSE) {
  qi <- qt(p, df=nu, ncp=kappa, lower.tail = lower.tail, log.p = log.p)
  return(qi * sigma + mu)
}

S7::method(rng_fn, NoncentralStudentT) <- function(distribution) function(n, nu, kappa, mu, sigma) {
  xi <- rt(n, df=nu, ncp=kappa)
  return(xi * sigma + mu)
}

S7::method(rargs, NoncentralStudentT) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(NoncentralStudentT, Mom)) <- function(distribution, estimator, data) {
  rlang::inform("Moments matching for t-distribution is extremely crude; further, parameters `nu` and `kappa` are not estimated.")

  estimates <- list()
  if(!distribution@nu@fixed)    estimates[["nu"]]    <- distribution@nu@value
  if(!distribution@kappa@fixed) estimates[["kappa"]] <- distribution@kappa@value
  if(!distribution@mu@fixed)    estimates[["mu"]]    <- median(data)
  if(!distribution@sigma@fixed) estimates[["sigma"]] <- stats::IQR(data, type = 8)/2

  return(estimates)
}
