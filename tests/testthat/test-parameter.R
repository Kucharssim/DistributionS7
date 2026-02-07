testthat::test_that("Parameters work", {
  mu = Parameter("mu", "mean", "\\mu", fixed(0.5), support = Real())

  testthat::expect_true(is.parameter(mu))
  testthat::expect_true(is.fixed(mu))

  mu@support@min <- 0
  mu@support@max <- 1
  testthat::expect_error(mu@value <- c(0.1, 0.2))
  testthat::expect_error(mu@value <- -1)
  testthat::expect_error(mu@value <-  2)

  testthat::expect_equal(unconstrain(mu, 0.5), 0)
  testthat::expect_equal(constrain(mu, 0), 0.5)
})
