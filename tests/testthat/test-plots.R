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

# layer data of all layers with the given stat and geom
find_layers <- function(plot, stat, geom) {
  which(vapply(plot$layers, function(l) inherits(l$stat, stat) && inherits(l$geom, geom), logical(1)))
}
layers_data <- function(plot, stat, geom) {
  lapply(find_layers(plot, stat, geom), function(i) ggplot2::layer_data(plot, i))
}
x_limits <- function(plot) ggplot2::layer_scales(plot)$x$get_limits()

testthat::test_that("plots do not set position scales", {
  d <- Normal(mu = 0, sigma = 1)
  x <- withr::with_seed(1, rnorm(30))
  plots <- list(
    plot_pdf(d, density = 0, probability = data.frame(xmin = -1, xmax = 1)),
    plot_cdf(d, density = 0, probability = data.frame(xmin = -1, xmax = 1)),
    plot_qf(d, density = 0, probability = data.frame(xmin = -1, xmax = 1)),
    plot_hist(d, x), plot_qq(d, x), plot_ecdf(d, x), plot_pp(d, x)
  )
  for (p in plots) {
    testthat::expect_false(p$scales$has_scale("x"))
    testthat::expect_false(p$scales$has_scale("y"))
    testthat::expect_no_message(p + ggplot2::scale_x_continuous() + ggplot2::scale_y_continuous())
  }
})

testthat::test_that("curves span the x range, and follow the limits of a downstream x scale", {
  d <- Normal(mu = 0, sigma = 1)

  for (plot_fn in list(plot_pdf, plot_cdf, plot_qf)) {
    p <- plot_fn(d, xlim = c(-2, 2))
    stat <- if (identical(plot_fn, plot_pdf)) "StatPdf" else "StatCdf"
    testthat::expect_equal(x_limits(p), c(-2, 2))
    testthat::expect_equal(range(layers_data(p, stat, "GeomLine")[[1]]$x), c(-2, 2))

    wider <- p + ggplot2::scale_x_continuous(limits = c(-5, 5))
    testthat::expect_equal(range(layers_data(wider, stat, "GeomLine")[[1]]$x), c(-5, 5))

    narrower <- p + ggplot2::scale_x_continuous(limits = c(-1, 1))
    testthat::expect_no_warning(curve <- layers_data(narrower, stat, "GeomLine")[[1]])
    testthat::expect_equal(range(curve$x), c(-1, 1))
  }

  testthat::expect_equal(x_limits(plot_pdf(d)), x_range(d))
})

testthat::test_that("intervals are truncated to the plot, but their probabilities are not", {
  d <- Normal(mu = 0, sigma = 1)
  probability <- data.frame(xmin = c(-Inf, 3), xmax = c(-1.96, 4))

  p <- plot_pdf(d, xlim = c(-2, 2), probability = probability)
  for (plot in list(p, p + ggplot2::scale_x_continuous())) {
    testthat::expect_equal(x_limits(plot), c(-2, 2))

    ribbons <- layers_data(plot, "StatPdf", "GeomRibbon")
    testthat::expect_equal(range(ribbons[[1]]$x), c(-2, -1.96))
    testthat::expect_equal(nrow(ribbons[[2]]), 0) # interval entirely outside of the plot is not drawn

    labels <- layers_data(plot, "StatCdfInterval", "GeomLabel")
    testthat::expect_equal(labels[[1]]$label, "0.025")
    testthat::expect_equal(nrow(labels[[2]]), 0)
  }

  for (plot_fn in list(plot_cdf, plot_qf)) {
    p <- plot_fn(d, xlim = c(-2, 2), probability = probability)
    testthat::expect_equal(x_limits(p), c(-2, 2))

    area <- layers_data(p, "StatCdf", "GeomRibbon")
    testthat::expect_true(all(area[[1]]$xmin == -2))
    testthat::expect_equal(range(area[[1]]$xmax), c(-2, -1.96))
    testthat::expect_equal(nrow(area[[2]]), 0)

    labels <- layers_data(p, "StatCdfInterval", "GeomLabel")
    testthat::expect_equal(labels[[1]]$label, "0.025")
  }
})

testthat::test_that("density points outside of the plot do not widen the x range", {
  d <- Normal(mu = 0, sigma = 1)
  for (plot_fn in list(plot_pdf, plot_cdf, plot_qf)) {
    p <- plot_fn(d, xlim = c(-2, 2), density = c(0, 10))
    testthat::expect_equal(x_limits(p), c(-2, 2))
  }
})

testthat::test_that("pdf plot reserves room for density labels in the data", {
  d <- Normal(mu = 0, sigma = 1)
  p <- plot_pdf(d, density = 0)
  testthat::expect_gte(ggplot2::layer_scales(p)$y$get_limits()[2], 1.15 * dnorm(0))
})

testthat::test_that("histogram plot covers all bins and the density", {
  d <- Normal(mu = 0, sigma = 1)
  x <- withr::with_seed(1, rnorm(30))
  breaks <- seq(-4, 4, by = 0.5)
  p <- plot_hist(d, x, breaks = breaks)

  limits <- x_limits(p)
  testthat::expect_true(limits[1] <= min(breaks) && max(breaks) <= limits[2])
  testthat::expect_gte(ggplot2::layer_scales(p)$y$get_limits()[2], dnorm(0))
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
