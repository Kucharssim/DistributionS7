# -2 log-likelihood of a shifted distribution at a given shift, with the remaining parameters estimated by MLE
profile_deviance <- function(dist, data, shift) {
  base <- dist@distribution
  parameter_values(base) <- suppressMessages(parameter_estimates(base, Mle(), data - shift))
  likelihood(base, data - shift, log=TRUE, factor=-2)
}

testthat::test_that("shift MLE finds the maximum of the profile likelihood", {
  x <- withr::with_seed(5729, stats::rgamma(200, shape=3, scale=2) + 5)
  dist <- ShiftedGamma(2, 1)

  testthat::expect_no_warning(fitted <- fit(dist, data=x))
  shift <- fitted@shift@value

  testthat::expect_lt(shift, min(x))
  testthat::expect_gt(fitted@distribution@alpha@value, 1)

  # neighbouring shifts fit worse
  d <- 0.01 * sd(x)
  testthat::expect_lt(profile_deviance(dist, x, shift), profile_deviance(dist, x, shift - d))
  testthat::expect_lt(profile_deviance(dist, x, shift), profile_deviance(dist, x, min(shift + d, min(x) - 1e-6)))
})

testthat::test_that("shift MLE is equivariant to the location and scale of the data", {
  x <- withr::with_seed(5729, stats::rgamma(200, shape=3, scale=2) + 5)
  dist <- ShiftedGamma(2, 1)
  reference <- fit(dist, data=x)

  for (scale in c(0.01, 1000)) {
    fitted <- fit(dist, data=scale * x + 3)
    testthat::expect_equal(fitted@shift@value, scale * reference@shift@value + 3, tolerance=1e-4)
    testthat::expect_equal(fitted@distribution@alpha@value, reference@distribution@alpha@value, tolerance=1e-4)
  }
})

testthat::test_that("unbounded likelihood gives a boundary estimate of the shift with a warning", {
  x <- withr::with_seed(1, stats::rgamma(100, shape=0.6) + 5)
  s <- sort(x)

  testthat::expect_warning(fitted <- fit(ShiftedGamma(2, 1), data=x), "without bounds")
  testthat::expect_equal(fitted@shift@value, s[1] - (s[2] - s[1]))

  # not specific to gamma
  x <- withr::with_seed(2, stats::rweibull(200, shape=0.7) + 5)
  s <- sort(x)

  testthat::expect_warning(fitted <- fit(ShiftedWeibull(2, 1), data=x), "without bounds")
  testthat::expect_equal(fitted@shift@value, s[1] - (s[2] - s[1]))
})

testthat::test_that("fixed shift is not estimated", {
  x <- withr::with_seed(5729, stats::rgamma(200, shape=3, scale=2) + 5)
  fitted <- fit(ShiftedGamma(2, 1, shift=fixed(4)), data=x)

  testthat::expect_equal(fitted@shift@value, 4)
  testthat::expect_true(fitted@shift@fixed)
})

testthat::test_that("normal theory inference works for shifted distributions", {
  x <- withr::with_seed(5729, stats::rgamma(200, shape=3, scale=2) + 5)
  dist <- ShiftedGamma(2, 1)

  result <- suppressMessages(parameter_inference(dist, DefaultMethod(), data=x))
  fitted <- fit(dist, data=x)

  testthat::expect_equal(result[["key"]], c("alpha", "theta", "shift"))
  testthat::expect_equal(result[["estimate"]][3], fitted@shift@value)
  testthat::expect_true(all(is.finite(result[["se"]])))
  testthat::expect_true(all(result[["lower"]] < result[["estimate"]] & result[["estimate"]] < result[["upper"]]))
  # the interval of the shift respects its support
  testthat::expect_lte(result[["upper"]][3], min(x))
})
