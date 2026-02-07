testthat::test_that("Distribution functions work", {
  n <- normal(0, 1)

  testthat::expect_no_error(x <- rng(n, 100))
  testthat::expect_no_error(p <- rng(uniform(0, 1), 100))

  testthat::expect_equal(pdf(n, x), dnorm(x))
  testthat::expect_equal(cdf(n, x), pnorm(x))
  testthat::expect_equal(qf (n, p), qnorm(p))
  testthat::expect_equal(likelihood(n, x, log=TRUE, factor=-2), -2*sum(dnorm(x, log=TRUE)))
})

testthat::test_that("Dynamic support works", {
  d <- triangular(0, 1, 0.5)

  testthat::expect_false(d@support@numeric)
  testthat::expect_true(support(d)@numeric)
})

testthat::test_that("Parameter methods work", {
  d <- normal(0, fixed(1))

  fixed <- parameter_properties(d, "fixed")
  testthat::expect_equal(fixed, list(mu=FALSE, sigma=TRUE))

  testthat::expect_equal(
    parameter_values(d),
    list(mu=0, sigma=1)
  )

  testthat::expect_equal(
    parameter_uvalues(d),
    list(mu=0, sigma=0)
  )

  parameter_values(d) <- list(mu=1)
  testthat::expect_equal(d@mu@value, 1)

})
