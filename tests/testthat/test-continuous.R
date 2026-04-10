testthat::test_that("`Normal` distribution functions work", {
  dist <- Normal(mu=1, sigma=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dnorm(x, mean=dist@mu@value, sd=dist@sigma@value))
  testthat::expect_equal(cdf(dist, x), pnorm(x, mean=dist@mu@value, sd=dist@sigma@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Beta` distribution functions work", {
  dist <- Beta(alpha=2, beta=3)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dbeta(x, shape1=dist@alpha@value, shape2=dist@beta@value))
  testthat::expect_equal(cdf(dist, x), pbeta(x, shape1=dist@alpha@value, shape2=dist@beta@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`BetaPrime` distribution functions work", {
  dist <- BetaPrime(alpha=2, beta=3)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Cauchy` distribution functions work", {
  dist <- Cauchy(mu=1, sigma=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dcauchy(x, location=dist@mu@value, scale=dist@sigma@value))
  testthat::expect_equal(cdf(dist, x), pcauchy(x, location=dist@mu@value, scale=dist@sigma@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`ChiSquared` distribution functions work", {
  dist <- ChiSquared(nu=5)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dchisq(x, df=dist@nu@value))
  testthat::expect_equal(cdf(dist, x), pchisq(x, df=dist@nu@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`NoncentralChiSquared` distribution functions work", {
  dist <- NoncentralChiSquared(nu=5, kappa=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dchisq(x, df=dist@nu@value, ncp=dist@kappa@value))
  testthat::expect_equal(cdf(dist, x), pchisq(x, df=dist@nu@value, ncp=dist@kappa@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Exponential` distribution (rate parametrization) functions work", {
  dist <- Exponential(lambda=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dexp(x, rate=dist@lambda@value))
  testthat::expect_equal(cdf(dist, x), pexp(x, rate=dist@lambda@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Exponential` distribution (scale parametrization) functions work", {
  dist <- Exponential(beta=0.5)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dexp(x, rate=1/dist@beta@value))
  testthat::expect_equal(cdf(dist, x), pexp(x, rate=1/dist@beta@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Gamma` distribution (scale parametrization) functions work", {
  dist <- Gamma(alpha=2, theta=3)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dgamma(x, shape=dist@alpha@value, scale=dist@theta@value))
  testthat::expect_equal(cdf(dist, x), pgamma(x, shape=dist@alpha@value, scale=dist@theta@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Gamma` distribution (rate parametrization) functions work", {
  dist <- Gamma(alpha=2, lambda=0.5)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dgamma(x, shape=dist@alpha@value, rate=dist@lambda@value))
  testthat::expect_equal(cdf(dist, x), pgamma(x, shape=dist@alpha@value, rate=dist@lambda@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Gamma` distribution (mean parametrization) functions work", {
  dist <- Gamma(alpha=2, mu=6)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dgamma(x, shape=dist@alpha@value, scale=dist@mu@value/dist@alpha@value))
  testthat::expect_equal(cdf(dist, x), pgamma(x, shape=dist@alpha@value, scale=dist@mu@value/dist@alpha@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`InverseGamma` distribution functions work", {
  dist <- InverseGamma(alpha=2, theta=3)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Logistic` distribution functions work", {
  dist <- Logistic(mu=1, sigma=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dlogis(x, location=dist@mu@value, scale=dist@sigma@value))
  testthat::expect_equal(cdf(dist, x), plogis(x, location=dist@mu@value, scale=dist@sigma@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`LogLogistic` distribution (scale parametrization) functions work", {
  dist <- LogLogistic(alpha=2, beta=3)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`LogLogistic` distribution (location parametrization) functions work", {
  dist <- LogLogistic(mu=1, sigma=0.5)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`LogNormal` distribution functions work", {
  dist <- LogNormal(mu=1, sigma=0.5)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dlnorm(x, meanlog=dist@mu@value, sdlog=dist@sigma@value))
  testthat::expect_equal(cdf(dist, x), plnorm(x, meanlog=dist@mu@value, sdlog=dist@sigma@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Laplace` distribution functions work", {
  dist <- Laplace(mu=1, beta=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Gumbel` distribution functions work", {
  dist <- Gumbel(mu=1, beta=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Pareto` distribution functions work", {
  dist <- Pareto(alpha=2, beta=1)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Triangular` distribution functions work", {
  dist <- Triangular(a=0, b=5, c=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Uniform` distribution functions work", {
  dist <- Uniform(min=0, max=3)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dunif(x, min=dist@min@value, max=dist@max@value))
  testthat::expect_equal(cdf(dist, x), punif(x, min=dist@min@value, max=dist@max@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Weibull` distribution functions work", {
  dist <- Weibull(shape=2, scale=3)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dweibull(x, shape=dist@shape@value, scale=dist@scale@value))
  testthat::expect_equal(cdf(dist, x), pweibull(x, shape=dist@shape@value, scale=dist@scale@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Frechet` distribution functions work", {
  dist <- Frechet(alpha=2, sigma=1, theta=0)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Amoroso` distribution functions work", {
  dist <- Amoroso(a=fixed(0), theta=1, alpha=2, beta=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`StretchedBeta` distribution functions work", {
  dist <- StretchedBeta(alpha=2, beta=3, min=fixed(1), max=fixed(5))
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`SymmetricGeneralizedNormal` distribution functions work", {
  dist <- SymmetricGeneralizedNormal(mu=1, alpha=2, beta=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), gnorm::dgnorm(x, mu=dist@mu@value, alpha=dist@alpha@value, beta=dist@beta@value))
  testthat::expect_equal(cdf(dist, x), gnorm::pgnorm(x, mu=dist@mu@value, alpha=dist@alpha@value, beta=dist@beta@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Wald` distribution (mean parametrization) functions work", {
  dist <- Wald(mu=2, lambda=3)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x, tolerance=1e-3)
})

testthat::test_that("`Wald` distribution (drift parametrization) functions work", {
  dist <- Wald(nu=2, alpha=3, sigma=fixed(1))
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x, tolerance=1e-3)
})

testthat::test_that("`CentralF` distribution functions work", {
  dist <- CentralF(nu1=5, nu2=10)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), df(x, df1=dist@nu1@value, df2=dist@nu2@value))
  testthat::expect_equal(cdf(dist, x), pf(x, df1=dist@nu1@value, df2=dist@nu2@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`NoncentralF` distribution functions work", {
  dist <- NoncentralF(nu1=5, nu2=10, kappa=3)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), df(x, df1=dist@nu1@value, df2=dist@nu2@value, ncp=dist@kappa@value))
  testthat::expect_equal(cdf(dist, x), pf(x, df1=dist@nu1@value, df2=dist@nu2@value, ncp=dist@kappa@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`StandardT` distribution functions work", {
  dist <- StandardT(nu=5)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dt(x, df=dist@nu@value))
  testthat::expect_equal(cdf(dist, x), pt(x, df=dist@nu@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`StudentT` distribution functions work", {
  dist <- StudentT(nu=5, mu=1, sigma=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`NoncentralT` distribution functions work", {
  dist <- NoncentralT(nu=5, kappa=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`NoncentralStudentT` distribution functions work", {
  dist <- NoncentralStudentT(nu=5, kappa=2, mu=1, sigma=2)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`SkewNormal` distribution functions work", {
  dist <- SkewNormal(xi=1, omega=2, alpha=3)
  x <- rng(dist, 10)
  attributes(x) <- NULL

  testthat::expect_equal(pdf(dist, x), sn::dsn(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value))
  testthat::expect_equal(cdf(dist, x), sn::psn(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`SkewCauchy` distribution functions work", {
  dist <- SkewCauchy(xi=1, omega=2, alpha=3)
  x <- rng(dist, 10)
  attributes(x) <- NULL

  testthat::expect_equal(pdf(dist, x), sn::dsc(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value))
  testthat::expect_equal(cdf(dist, x), sn::psc(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`SkewT` distribution functions work", {
  dist <- SkewT(xi=1, omega=2, alpha=3, nu=5)
  x <- withr::with_seed(2345, rng(dist, 10))
  attributes(x) <- NULL

  testthat::expect_equal(pdf(dist, x), sn::dst(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value, nu=dist@nu@value))
  testthat::expect_equal(cdf(dist, x), sn::pst(x, xi=dist@xi@value, omega=dist@omega@value, alpha=dist@alpha@value, nu=dist@nu@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`SkewedGeneralizedT` distribution functions work", {
  dist <- SkewedGeneralizedT(mu=1, sigma=2, lambda=0.3, p=2, q=3)
  x <- rng(dist, 10)

  testthat::expect_equal(
    pdf(dist, x),
    sgt::dsgt(x, mu=dist@mu@value, sigma=dist@sigma@value, lambda=dist@lambda@value,
              p=dist@p@value, q=dist@q@value, mean.cent=FALSE, var.adj=FALSE)
  )
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`Gompertz` distribution functions work", {
  dist <- Gompertz(eta=2, beta=1)
  x <- rng(dist, 10)

  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`ShiftedLogNormal` distribution functions work", {
  dist <- ShiftedLogNormal(mu=1, sigma=0.5, shift=2)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dlnorm(x - dist@shift@value, meanlog=dist@distribution@mu@value, sdlog=dist@distribution@sigma@value))
  testthat::expect_equal(cdf(dist, x), plnorm(x - dist@shift@value, meanlog=dist@distribution@mu@value, sdlog=dist@distribution@sigma@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`ShiftedExponential` distribution (rate parametrization) functions work", {
  dist <- ShiftedExponential(lambda=2, shift=1)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dexp(x - dist@shift@value, rate=dist@lambda@value))
  testthat::expect_equal(cdf(dist, x), pexp(x - dist@shift@value, rate=dist@lambda@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})

testthat::test_that("`ShiftedExponential` distribution (scale parametrization) functions work", {
  dist <- ShiftedExponential(beta=0.5, shift=1)
  x <- rng(dist, 10)

  testthat::expect_equal(pdf(dist, x), dexp(x - dist@shift@value, rate=1/dist@beta@value))
  testthat::expect_equal(cdf(dist, x), pexp(x - dist@shift@value, rate=1/dist@beta@value))
  testthat::expect_equal(qf(dist, cdf(dist, x)), x)
})
