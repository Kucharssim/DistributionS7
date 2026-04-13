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

  # constrained optimizing mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle(optim=TRUE, constrained=TRUE))
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2)),
    tolerance=1e-3
  )

})

testthat::test_that("parameter_inference works", {
  dist <- Wald(mu=1, lambda=2)
  x <- withr::with_seed(34698, rng(dist, 20))

  # DefaultMethod switches to NormalTheory
  testthat::expect_message(
    parameter_inference(dist, DefaultMethod(args=list(constrained=TRUE)), data=x),
    "Computing normal theory SE and CIs..."
  )
  
  # and passess args forward
  result_default <- parameter_inference(dist, DefaultMethod(args=list(constrained=TRUE)), data=x)
  result_normal <- parameter_inference(dist, NormalTheory(constrained=TRUE), data=x)

  testthat::expect_equal(result_default, result_normal)

  
  dist <- Normal(mu=0, sigma=1)
  
  # test analytic estimates + analytic ci
  result <- parameter_inference(dist, DefaultMethod(), data=x)
  testthat::expect_snapshot(result)

  # analytic estimates + normal theory cis
  result <- parameter_inference(dist, NormalTheory(), data=x)
  testthat::expect_snapshot(result)

  # analytic estimates + normal theory cis on constrained space
  result <- parameter_inference(dist, NormalTheory(constrained=TRUE), data=x)
  testthat::expect_snapshot(result)

  # optim estimates + normal theory cis
  result <- parameter_inference(dist, NormalTheory(estimator=Mle(optim=TRUE)), data=x)
  testthat::expect_snapshot(result)
})