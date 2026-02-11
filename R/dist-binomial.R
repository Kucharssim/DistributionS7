#' @title Binomial distribution
#' @description Create a binomial distribution object.
#'
#' @param prob Probability of success parameter.
#' @param size Sample size parameter.
#' @family distributions
#' @export
Binomial <- S7::new_class(
  "Binomial",
  parent = DistributionDiscrete,
  properties = list(
    prob = Parameter,
    size = Parameter
  ),
  constructor = function(prob, size) {
    S7::new_object(
      S7::S7_object(),
      name = "Binomial",
      support = Int(min=0, max=expression(size)),
      prob = Parameter("prob", "probability of success", "p", prob, Real(min=0, max=1)),
      size = Parameter("size", "sample size", "n", size, Int(min=0), fixed=TRUE)
    )
  },
  validator = function(self) {
    assertthat::assert_that(self@size@fixed)
    return(NULL)
  }
)

S7::method(pdf_fn, Binomial) <- function(distribution) stats::dbinom
S7::method(cdf_fn, Binomial) <- function(distribution) stats::pbinom
S7::method(qf_fn,  Binomial) <- function(distribution) stats::qbinom
S7::method(rng_fn, Binomial) <- function(distribution) stats::rbinom
S7::method(rargs,  Binomial) <- function(distribution) parameter_values(distribution)

S7::method(parameter_estimates, list(Binomial, Mom)) <- function(distribution, estimator, data) {
  return(list(prob = mean(data) / distribution@size@value))
}

S7::method(parameter_estimates, list(Binomial, Mle)) <- function(distribution, estimator, data) {
  return(list(prob = mean(data) / distribution@size@value))
}


Bernoulli <- S7::new_class(
  "Bernoulli",
  parent = Binomial,
  constructor = function(prob) {
    S7::new_object(
      S7::S7_object(),
      name = "Bernoulli",
      support = Int(min=0, max=1),
      prob = Parameter("prob", "probability of success", "p", prob, Real(min=0, max=1)),
      size = Parameter("size", "sample size", "n", fixed(1L), Int(min=1, max=1), fixed=TRUE)
    )
  }
)

bernoulli <- function(prob) Bernoulli(prob)

