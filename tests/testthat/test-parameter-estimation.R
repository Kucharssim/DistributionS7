testthat::test_that("parameter_estimates works", {
  dist <- Normal(mu=0, sigma2=2)
  x <- withr::with_seed(93235, rng(dist, 100))
  x_bar <- mean(x)

  # analytic
  pars <- parameter_estimates(dist, data=x, estimator=BiasCorrected())
  testthat::expect_equal(
    pars,
    list(mu = x_bar, sigma2 = var(x))
  )

  # analytic mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle())
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2))
  )

  # optimizing mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle(optim=TRUE, start=list(mu=-1)))
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2)),
    tolerance=1e-3
  )
})