testthat::test_that("Basic support functionality works", {
  support <- Real(min=0, max=1)

  testthat::expect_equal(
    inside(support, c(-1, 0, 0.5, 1, 2)),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )

  testthat::expect_error(Real(min=1, max=0))
})

testthat::test_that("Constrain/unconstrain transforms work", {
  min <- -1
  support <- Real(min=min)
  v <- c(0, 1, 2)
  uv <- unconstrain(support, v)

  testthat::expect_equal(uv, log(v - min))
  testthat::expect_equal(constrain(support, uv), v)
  testthat::expect_equal(derivative(support, uv), exp(uv))

  max <- 5
  support <- Real(max=max)
  uv <- unconstrain(support, v)

  testthat::expect_equal(uv, log(max - v))
  testthat::expect_equal(constrain(support, uv), v)
  testthat::expect_equal(derivative(support, uv), -exp(uv))

  support <- Real(min=min, max=max)
  uv <- unconstrain(support, v)

  p <- (v - min) / (max - min)
  testthat::expect_equal(uv, log(p) - log1p(-p))
  testthat::expect_equal(constrain(support, uv), v)
  testthat::expect_equal(derivative(support, uv), (max - min) * p * (1-p))

})
