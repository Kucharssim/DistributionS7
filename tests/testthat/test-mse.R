testthat::test_that("maximum spacing estimates are close to MLE for large samples", {
  x <- withr::with_seed(1, stats::rnorm(2000, 3, 2))

  mle <- parameter_estimates(Normal(0, 1), Mle(), data=x)
  mse <- parameter_estimates(Normal(0, 1), Mse(), data=x)

  testthat::expect_equal(mse, mle, tolerance=1e-2)
})

testthat::test_that("maximum spacing estimator gives an interior shift where the MLE does not exist", {
  x <- withr::with_seed(1, stats::rgamma(100, shape=0.6) + 5)

  testthat::expect_no_warning(fitted <- fit(ShiftedGamma(2, 1), Mse(), data=x))
  testthat::expect_lt(fitted@shift@value, min(x))
  testthat::expect_lt(fitted@distribution@alpha@value, 1)

  x <- withr::with_seed(2, stats::rweibull(200, shape=0.7) + 5)

  fitted <- suppressWarnings(fit(ShiftedWeibull(2, 1), Mse(), data=x))
  testthat::expect_lt(fitted@shift@value, min(x))
  testthat::expect_lt(fitted@distribution@shape@value, 1)
})

testthat::test_that("maximum spacing estimator handles ties", {
  x <- round(withr::with_seed(3, stats::rgamma(100, shape=3)), 1)
  testthat::expect_true(anyDuplicated(x) > 0)

  dist <- Gamma(2, 1)
  testthat::expect_true(is.finite(spacings_objective(dist, x)))

  fitted <- fit(dist, Mse(), data=x)
  testthat::expect_equal(fitted@alpha@value, 3, tolerance=0.3)
})

testthat::test_that("maximum spacing estimator is not available for discrete distributions", {
  testthat::expect_error(
    fit(Binomial(size=fixed(10), prob=0.5), Mse(), data=c(1, 2, 3)),
    "Can't find method"
  )
})

testthat::test_that("fixed shift is not estimated by the maximum spacing estimator", {
  x <- withr::with_seed(1, stats::rgamma(100, shape=3) + 5)
  fitted <- fit(ShiftedGamma(2, 1, shift=fixed(4)), Mse(), data=x)

  testthat::expect_equal(fitted@shift@value, 4)
  testthat::expect_true(fitted@shift@fixed)
})

testthat::test_that("parameters of shifted distributions can be set", {
  dist <- ShiftedGamma(2, 1, shift=5)

  testthat::expect_no_warning(parameter_values(dist) <- list(alpha=3, theta=4, shift=6))
  testthat::expect_equal(parameter_values(dist), list(alpha=3, theta=4, shift=6))

  # generic numerical optimization updates all parameters
  x <- withr::with_seed(1, stats::rgamma(100, shape=3) + 5)
  fitted <- suppressWarnings(fit(ShiftedGamma(2, 1), Mle(optim=TRUE), data=x))
  testthat::expect_false(isTRUE(all.equal(
    c(fitted@distribution@alpha@value, fitted@distribution@theta@value), c(2, 1)
  )))
})

testthat::test_that("bootstrap inference works with the maximum spacing estimator", {
  x <- withr::with_seed(1, stats::rgamma(100, shape=1.5) + 5)
  testthat::expect_no_warning(result <- withr::with_seed(2,
    parameter_inference(ShiftedGamma(2, 1), Bootstrap(estimator=Mse(), samples=20L), data=x)
  ))

  testthat::expect_equal(nrow(result), 3)
})
