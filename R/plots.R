#' @title Theoretical plots
#' @description
#' Plot the probability density, cumulative distribution, and quantile function of a distribution.
#'
#' @param distribution Object of class [Distribution()].
#' @param type Character; which plots to plot.
#' @param xlim Numeric vector of length 2; limits of the x axis. If missing, the limits are derived from the support
#'   of the distribution:
#'   - bounded from both sides: the entire support;
#'   - bounded from one side: the bound, and the quantile such that the range covers 99% of the distribution;
#'   - unbounded: the central 99% of the distribution.
#'
#'   The derived limits are then expanded by 5% of their range on each side. Limits supplied by the user are used as is.
#' @param density Data frame with a numeric column `x` and an optional column `color`; points at which to show the
#'   density. In `plot_pdf()`, the density is shown as a label at the curve. In `plot_cdf()` and `plot_qf()`, the
#'   density is shown as the slope of a tangent, listed in the legend. A numeric vector is treated as column `x`.
#'   Points outside of `xlim` are not drawn.
#' @param probability Data frame with numeric columns `xmin` and `xmax` and an optional column `color`; intervals
#'   whose probability to show. Intervals are shown as shaded areas truncated to `xlim`, labelled with their
#'   (untruncated) probability.
#' @param ... Not used.
#'
#' @details
#' Colors in the `color` columns are used as is. Missing colors are filled from the default ggplot2 hue palette.
#'
#' The plots do not set position scales: `xlim` is added as data (via [ggplot2::expand_limits()]), and the curves are
#' computed over the final limits of the x scale. Any position scale (e.g., [ggplot2::scale_x_continuous()]) can
#' therefore be added to the plots; widening the limits of the x scale extends the curves.
#'
#' @return A [ggplot2::ggplot()] object; for `plot_distribution(type = "all")` a [patchwork::wrap_plots()] object.
#'
#' @name plot-distribution
#' @export
plot_distribution <- S7::new_generic("plot_distribution", "distribution", function(distribution, type=c("all", "pdf", "cdf", "qf"), ..., xlim, density=NULL, probability=NULL) {
  S7::S7_dispatch()
})

#' @rdname plot-distribution
#' @export
plot_pdf <- S7::new_generic("plot_pdf", "distribution", function(distribution, ..., xlim, density=NULL, probability=NULL) {
  S7::S7_dispatch()
})

#' @rdname plot-distribution
#' @export
plot_cdf <- S7::new_generic("plot_cdf", "distribution", function(distribution, ..., xlim, density=NULL, probability=NULL) {
  S7::S7_dispatch()
})

#' @rdname plot-distribution
#' @export
plot_qf  <- S7::new_generic("plot_qf",  "distribution", function(distribution, ..., xlim, density=NULL, probability=NULL) {
  S7::S7_dispatch()
})

S7::method(plot_distribution, Distribution) <- function(distribution, type=c("all", "pdf", "cdf", "qf"), ..., xlim, density=NULL, probability=NULL) {
  type <- rlang::arg_match(type, multiple=FALSE)
  # a missing xlim stays missing when passed on
  plot <- switch(
    type,
    pdf = plot_pdf(distribution, xlim=xlim, density=density, probability=probability),
    cdf = plot_cdf(distribution, xlim=xlim, density=density, probability=probability),
    qf  = plot_qf (distribution, xlim=xlim, density=density, probability=probability),
    all = list(
      plot_pdf(distribution, xlim=xlim, density=density, probability=probability),
      plot_cdf(distribution, xlim=xlim, density=density, probability=probability),
      plot_qf (distribution, xlim=xlim, density=density, probability=probability)
    )
  )

  if(type == "all") {
    plot[[4]] <- plot[[3]]
    plot[[3]] <- patchwork::plot_spacer()
    plot[[1]] <- plot[[1]] + ggplot2::ggtitle(gettext("Probability Density Plot"))
    plot[[2]] <- plot[[2]] + ggplot2::ggtitle(gettext("Cumulative Distribution Plot"))
    plot[[4]] <- plot[[4]] + ggplot2::ggtitle(gettext("Quantile Plot"))
    plot <- patchwork::wrap_plots(plot, ncol=2, nrow=2, byrow = FALSE)
  }

  return(plot)
}

S7::method(plot_pdf, DistributionContinuous) <- function(distribution, ..., xlim, density=NULL, probability=NULL) {
  xlim <- x_range(distribution, xlim)
  probability <- prepare_probability(probability)
  density <- prepare_density(density, xlim)

  # the x range is data that trains the x scale; the curves follow the (final) limits of the x scale
  plot <- ggplot2::ggplot() +
    ggplot2::expand_limits(x = xlim) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity()

  for (i in seq_len(NROW(probability))) {
    interval <- c(probability[["xmin"]][i], probability[["xmax"]][i])
    color <- probability[[".color"]][i]
    plot <- plot +
      stat_pdf(distribution, xlim=interval, geom="ribbon", fill=color, alpha=0.8) +
      stat_cdf_interval(distribution, xlim=interval, fill=color,
                        mapping=ggplot2::aes(label=ggplot2::after_stat(sprintf("%.3f", probability))))
  }

  plot <- plot +
    stat_pdf(distribution, geom="line", linewidth=1.5)

  if (!is.null(density)) {
    # place the label on the side where the density decreases away from the point, so that it does not cross the curve:
    # upper left if the density is increasing, upper right if decreasing, above if at a mode
    h <- 1e-6 * pmax(1, abs(density[["x"]]))
    slope <- pdf(distribution, density[["x"]] + h) - pdf(distribution, density[["x"]] - h)
    density[[".hjust"]] <- ifelse(slope > 0, 1.1, ifelse(slope < 0, -0.1, 0.5))

    plot <- plot +
      stat_pdf(distribution, data=density,
               mapping = ggplot2::aes(x=x, y=0, yend=ggplot2::after_stat(density), group=.group, colour=.color),
               geom = "segment", linetype=2) +
      stat_pdf(distribution, data=density,
               mapping = ggplot2::aes(x=x, group=.group, fill=.color),
               size = 3, shape = 21, colour = "black", alpha = NA, stroke = 0.5) +
      stat_pdf(distribution, data=density,
               mapping = ggplot2::aes(x=x, group=.group, colour=.color, hjust=.hjust,
                                      label=ggplot2::after_stat(sprintf("%.3f", density))),
               geom = "label", vjust = -0.4, fill = "white"
               ) +
      # leave room for labels placed above the curve
      ggplot2::expand_limits(y = c(0, 1.15 * max(pdf(distribution, density[["x"]]))))
  }

  plot <- plot + ggplot2::labs(x = gettext("X"), y = gettext("Density"))

  return(plot)
}

S7::method(plot_cdf, DistributionContinuous) <- function(distribution, ..., xlim, density=NULL, probability=NULL, .inverse_slope=FALSE) {
  xlim <- x_range(distribution, xlim)
  probability <- prepare_probability(probability)
  density <- prepare_density(density, xlim)

  # the x range is data that trains the x scale; the curves follow the (final) limits of the x scale
  plot <- ggplot2::ggplot() +
    ggplot2::expand_limits(x = xlim) +
    ggplot2::scale_fill_identity() +
    slope_scale(distribution, density, .inverse_slope)

  for (i in seq_len(NROW(probability))) {
    # probability of an interval is the rise of the cdf between xmin and xmax:
    # shade the area between the left edge and the cdf, and mark the rise on the left edge
    interval <- c(probability[["xmin"]][i], probability[["xmax"]][i])
    color <- probability[[".color"]][i]
    plot <- plot +
      stat_cdf(distribution, xlim=interval, fill=color, alpha=0.4, geom="ribbon", orientation="y",
               mapping = ggplot2::aes(y=ggplot2::after_stat(probability),
                                      xmin=ggplot2::after_stat(xleft), xmax=ggplot2::after_stat(x))) +
      stat_cdf_interval(distribution, xlim=interval, colour=color, geom="segment", linetype=2, na.rm=TRUE,
                        mapping = ggplot2::aes(x=ggplot2::after_stat(xleft), xend=ggplot2::after_stat(xminplot),
                                               y=ggplot2::after_stat(pmin_visible), yend=ggplot2::after_stat(pmin_visible))) +
      stat_cdf_interval(distribution, xlim=interval, colour=color, geom="segment", linetype=2, na.rm=TRUE,
                        mapping = ggplot2::aes(x=ggplot2::after_stat(xleft), xend=ggplot2::after_stat(xmaxplot),
                                               y=ggplot2::after_stat(pmax_visible), yend=ggplot2::after_stat(pmax_visible))) +
      stat_cdf_interval(distribution, xlim=interval, colour=color, geom="segment", linewidth=2,
                        mapping = ggplot2::aes(x=ggplot2::after_stat(xleft), xend=ggplot2::after_stat(xleft),
                                               y=ggplot2::after_stat(pmin), yend=ggplot2::after_stat(pmax)))
  }

  if (!is.null(density)) {
    plot <- plot +
      stat_pdf(distribution, data = density,
               mapping = ggplot2::aes(x=x, group=.group, colour=.color), geom="abline", show.legend = TRUE)
  }

  plot <- plot +
    stat_cdf(distribution, geom="line", linewidth=1.5)

  for (i in seq_len(NROW(probability))) {
    interval <- c(probability[["xmin"]][i], probability[["xmax"]][i])
    plot <- plot +
      stat_cdf_interval(distribution, xlim=interval, fill=probability[[".color"]][i],
                        mapping = ggplot2::aes(x=ggplot2::after_stat(x_center_cdf), y=ggplot2::after_stat(y_center_cdf),
                                               label=ggplot2::after_stat(sprintf("%.3f", probability))))
  }

  if (!is.null(density)) {
    plot <- plot +
      stat_cdf(distribution, data=density,
               mapping = ggplot2::aes(x=x, group=.group, fill=.color),
               size = 3, shape = 21, colour = "black", alpha = NA, stroke = 0.5) +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification.inside = c(1, 0),
        legend.margin = ggplot2::margin(5, 5, 5, 5)
      )
  }

  plot <- plot + ggplot2::labs(x = gettext("X"), y = gettext("Cumulative probability"))

  return(plot)
}

S7::method(plot_qf, DistributionContinuous) <- function(distribution, ..., xlim, density=NULL, probability=NULL) {
  plot <- plot_cdf(distribution, xlim=xlim, density=density, probability=probability, .inverse_slope=TRUE)
  plot <- plot + ggplot2::coord_flip()

  return(plot)
}

# colours are used as is; the legend lists only the tangents, labelled by their slope
slope_scale <- function(distribution, density, inverse=FALSE) {
  if (is.null(density)) return(ggplot2::scale_colour_identity())

  slope <- pdf(distribution, density[["x"]])
  if (inverse) slope <- 1/slope

  breaks <- unique(density[[".color"]])
  labels <- vapply(breaks, function(color) {
    paste(sprintf("%.3f", slope[density[[".color"]] == color]), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)

  ggplot2::scale_colour_identity(name = gettext("Slope"), guide = "legend", breaks = breaks, labels = labels)
}

prepare_probability <- function(probability) {
  if (is.null(probability)) return(NULL)

  if (!is.data.frame(probability) || !all(c("xmin", "xmax") %in% colnames(probability)))
    cli::cli_abort("{.arg probability} must be a data frame with columns {.field xmin} and {.field xmax}")
  if (!is.numeric(probability[["xmin"]]) || !is.numeric(probability[["xmax"]]))
    cli::cli_abort("Columns {.field xmin} and {.field xmax} of {.arg probability} must be numeric")
  if (anyNA(probability[["xmin"]]) || anyNA(probability[["xmax"]]))
    cli::cli_abort("Columns {.field xmin} and {.field xmax} of {.arg probability} must not contain missing values")
  if (any(probability[["xmin"]] > probability[["xmax"]]))
    cli::cli_abort("Minimum of an interval must be smaller than the maximum of an interval")

  prepare_color(probability)
}

prepare_density <- function(density, xlim) {
  if (is.null(density)) return(NULL)

  if (is.numeric(density)) density <- data.frame(x = density)

  if (!is.data.frame(density) || !"x" %in% colnames(density))
    cli::cli_abort("{.arg density} must be a data frame with column {.field x}")
  if (!is.numeric(density[["x"]]) || anyNA(density[["x"]]))
    cli::cli_abort("Column {.field x} of {.arg density} must be numeric without missing values")

  density <- prepare_color(density)

  # points are data and would widen the x range; points outside of the plot are not drawn
  density <- density[xlim[1] <= density[["x"]] & density[["x"]] <= xlim[2], , drop=FALSE]
  if (nrow(density) == 0) return(NULL)

  density
}

prepare_color <- function(data) {
  n <- nrow(data)
  color <- if ("color" %in% colnames(data)) as.character(data[["color"]]) else rep(NA_character_, n)
  palette <- scales::hue_pal()(n)
  color[is.na(color)] <- palette[is.na(color)]

  data[[".group"]] <- seq_len(n)
  data[[".color"]] <- color
  data
}

x_range <- S7::new_generic("x_range", "distribution")

S7::method(x_range, DistributionContinuous) <- function(distribution, xlim, coverage=0.99) {
  if(missing(xlim)) {
    # bounded sides span up to the bound, unbounded sides cover the specified coverage
    s <- support(distribution)
    lower_bounded <- is.finite(s@min)
    upper_bounded <- is.finite(s@max)

    if (lower_bounded && upper_bounded) {
      xlim <- c(s@min, s@max)
    } else if (lower_bounded) {
      xlim <- c(s@min, qf(distribution, coverage))
    } else if (upper_bounded) {
      xlim <- c(qf(distribution, 1-coverage), s@max)
    } else {
      tail <- (1-coverage) / 2
      xlim <- qf(distribution, c(tail, 1-tail))
    }

    # expand slightly, e.g., to show that the density drops to zero beyond the bounds of the support
    xlim <- xlim + c(-1, 1) * 0.05 * diff(xlim)
  }

  return(xlim)
}
