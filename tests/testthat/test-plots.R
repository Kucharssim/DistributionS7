testthat::test_that("probability and density inputs are validated", {
  d <- Normal(mu = 0, sigma = 1)
  testthat::expect_error(plot_pdf(d, probability = data.frame(lower = 0, upper = 1)), "xmin")
  testthat::expect_error(plot_pdf(d, probability = data.frame(xmin = 1, xmax = 0)), "smaller")
  testthat::expect_error(plot_cdf(d, density = data.frame(y = 0)), "column")
  testthat::expect_no_error(plot_pdf(d, density = c(0, 1)))
})

testthat::test_that("missing colors are filled from a palette", {
  out <- prepare_probability(data.frame(xmin = c(0, 1), xmax = c(1, 2), color = c("red", NA)))
  testthat::expect_equal(out$.color, c("red", scales::hue_pal()(2)[2]))
  testthat::expect_equal(out$.group, 1:2)
})

testthat::test_that("interval positions are truncated to xlim, but probabilities are not", {
  d <- Normal(mu = 0, sigma = 1)
  probability <- data.frame(xmin = c(-Inf, 1, 3), xmax = c(-1, 5, 4))

  p <- plot_pdf(d, xlim = c(-2, 2), probability = probability)
  ribbon <- ggplot2::layer_data(p, 1)
  testthat::expect_equal(range(ribbon$x), c(-2, 2))
  testthat::expect_equal(sort(unique(ribbon$group)), 1:2) # interval fully outside xlim is dropped

  label <- ggplot2::layer_data(p, 2)
  testthat::expect_equal(label$label, sprintf("%.3f", c(pnorm(-1), pnorm(1, lower.tail = FALSE))))

  p <- plot_cdf(d, xlim = c(-2, 2), probability = probability)
  area <- ggplot2::layer_data(p, 1)
  testthat::expect_true(all(area$xmin == -2))
  testthat::expect_equal(range(area$xmax), c(-2, 2))
})

testthat::test_that("cdf tangents are listed in a legend by their slope", {
  d <- Normal(mu = 0, sigma = 1)
  p <- plot_cdf(d, density = data.frame(x = c(0, 1), color = c("blue", NA)))
  legend <- ggplot2::get_guide_data(p, "colour")
  testthat::expect_equal(legend$.label, sprintf("%.3f", dnorm(c(0, 1))))
  testthat::expect_equal(legend$colour[1], "blue")

  p <- plot_qf(d, density = data.frame(x = 0))
  legend <- ggplot2::get_guide_data(p, "colour")
  testthat::expect_equal(legend$.label, sprintf("%.3f", 1 / dnorm(0)))
})
