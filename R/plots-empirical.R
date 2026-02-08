#' @title Empirical plots
#' @description
#' Plot a distribution against data.
#'
#' @param distribution Object of class [Distribution()].
#' @param data Numeric containing the data.
#' @param type Character; which plots to plot.
#' @param name Character; Name of the variable.
#' @param ci Logical; Should confidence bands be plotted?
#' @param ci_level Numeric; Confidence level of confidence bands.
#' @param bin_width_type Character; Passed to [jaspGraphs::jaspHistogramBinWidth()].
#' @param number_of_bins Integer; Passed to [jaspGraphs::jaspHistogramBinWidth()].
#'
#' @name plot-empirical
#' @export
plot_empirical <- S7::new_generic("plot_empirical", "distribution", function(distribution, data, type=c("all", "hist", "qq", "ecdf", "pp"), ..., name, ci=FALSE, ci_level=0.95) {
  S7::S7_dispatch()
})

#' @rdname plot-empirical
#' @export
plot_hist  <- S7::new_generic("plot_hist",  "distribution", function(distribution, data, ..., name, bin_width_type="doane", number_of_bins=NA) {
  S7::S7_dispatch()
})

#' @rdname plot-empirical
#' @export
plot_qq    <- S7::new_generic("plot_qq",    "distribution", function(distribution, data, ..., ci=FALSE, ci_level=0.95) {
  S7::S7_dispatch()
})

#' @rdname plot-empirical
#' @export
plot_ecdf  <- S7::new_generic("plot_ecdf",  "distribution", function(distribution, data, ..., name) {
  S7::S7_dispatch()
})

#' @rdname plot-empirical
#' @export
plot_pp    <- S7::new_generic("plot_pp",    "distribution", function(distribution, data, ..., ci=FALSE, ci_level=0.95) {
  S7::S7_dispatch()
})

S7::method(plot_empirical, Distribution) <- function(distribution, data, type=c("all", "hist", "qq", "ecdf", "pp"), ..., name, ci=FALSE, ci_level=0.95) {
  if (missing(name)) name <- deparse1(substitute(data))

  type <- rlang::arg_match(type, multiple=FALSE)
  plot <- switch(
    type,
    hist = plot_hist(distribution, data, name=name),
    qq   = plot_qq  (distribution, data, ci=ci, ci_level=ci_level),
    ecdf = plot_ecdf(distribution, data, name=name),
    pp   = plot_pp  (distribution, data, ci=ci, ci_level=ci_level),
    all = list(
      plot_hist(distribution, data, name=name),
      plot_qq  (distribution, data, ci=ci, ci_level=ci_level),
      plot_ecdf(distribution, data, name=name),
      plot_pp  (distribution, data, ci=ci, ci_level=ci_level)
      )
  )

  plot <- lapply(plot, \(p) p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe())
  plot[[1]] <- plot[[1]] + ggplot2::ggtitle(gettext("Histogram vs. Theoretical Density"))
  plot[[2]] <- plot[[2]] + ggplot2::ggtitle(gettext("Q-Q plot"))
  plot[[3]] <- plot[[3]] + ggplot2::ggtitle(gettext("Empirical vs. Theoretical Cumulative Probability"))
  plot[[4]] <- plot[[4]] + ggplot2::ggtitle(gettext("P-P plot"))
  plot <- patchwork::wrap_plots(plot, ncol=2, nrow=2, byrow = TRUE) +
    ggplot2::theme(plot.margin = ggplot2::margin(t=5, l=5))

  return(plot)
}


S7::method(plot_hist, DistributionContinuous) <- function(distribution, data, ..., name, bin_width_type="doane", number_of_bins=NA) {
  if (missing(name)) name <- deparse1(substitute(data))

  bin_width_type <- jaspGraphs::jaspHistogramBinWidth(data, binWidthType = bin_width_type, numberOfBins = number_of_bins)
  h <- graphics::hist(data, plot=FALSE, breaks=bin_width_type)

  y_breaks <- jaspGraphs::getPrettyAxisBreaks(c(0, h[["density"]]*1.05))
  y_labs   <- jaspGraphs::axesLabeller(y_breaks)
  x_breaks <- jaspGraphs::getPrettyAxisBreaks(data)
  x_labs   <- jaspGraphs::axesLabeller(x_breaks)

  plot <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(mapping = ggplot2::aes(x=x, y=ggplot2::after_stat(density)), breaks=h[["breaks"]], fill="grey", col="black", linewidth=0.7) +
    ggplot2::geom_rug() +
    stat_pdf(distribution, xlim = range(data), geom="line", linewidth = 1.5, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(name = name,               breaks = x_breaks, labels = x_labs) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = y_breaks, labels = y_labs) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()
}

S7::method(plot_qq, DistributionContinuous) <- function(distribution, data, ..., ci=FALSE, ci_level=0.95) {

  sample <- sort(data)
  n <- length(sample)
  p <- stats::ppoints(n)

  theoretical <- qf(distribution, p)

  df <- data.frame(sample, theoretical)

  if (ci) {
    # Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
    # Chapter 3.1.3
    # We do not scale the results to make it clear if the mean/variance of the data doesn't match
    alpha       <- 1-ci_level
    pdf         <- pdf(distribution, theoretical)
    se <- sqrt(p * (1 - p) / n) / pdf


    df[["upper"]] <- theoretical + se * qnorm(alpha/2, lower.tail = FALSE)
    df[["lower"]] <- theoretical + se * qnorm(alpha/2, lower.tail = TRUE)

    ci_layer <-
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(x = theoretical, ymin = lower, ymax = upper),
        fill = "steelblue", color = "black", alpha = 0.5
      )
  } else {
    ci_layer <- NULL
  }

  y_breaks <- jaspGraphs::getPrettyAxisBreaks(as.vector(as.matrix(df)))
  y_labs   <- jaspGraphs::axesLabeller(y_breaks)
  x_breaks <- jaspGraphs::getPrettyAxisBreaks(theoretical)
  x_labs   <- jaspGraphs::axesLabeller(x_breaks)

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(sample = sample)) +
    ci_layer +
    ggplot2::geom_line(mapping = ggplot2::aes(x = theoretical, y = theoretical), size = 1) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = theoretical, y = sample), shape=21, fill = "grey", size=3) +
    ggplot2::scale_x_continuous(name = gettext("Theoretical"), breaks = x_breaks, labels = x_labs) +
    ggplot2::scale_y_continuous(name = gettext("Sample"),      breaks = y_breaks, labels = y_labs) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  return(plot)
}

S7::method(plot_ecdf, DistributionContinuous) <- function(distribution, data, ..., name) {
  if (missing(name)) name <- deparse1(substitute(data))

  x_breaks <- jaspGraphs::getPrettyAxisBreaks(data)
  x_labs   <- jaspGraphs::axesLabeller(x_breaks)

  plot <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes(x = x)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    stat_cdf(distribution, xlim = range(data), geom="line", linewidth = 1.5, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(breaks=x_breaks, labels=x_labs) +
    ggplot2::scale_y_continuous(limits = 0:1) +
    ggplot2::ylab(gettext("Cumulative Probability")) +
    ggplot2::xlab(name) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  return(plot)
}

S7::method(plot_pp, DistributionContinuous) <- function(distribution, data, ..., ci=FALSE, ci_level=0.95) {
  data <- sort(data)
  n <- length(data)
  theoretical <- stats::ppoints(n)

  sample <- cdf(distribution, data)
  df <- data.frame(sample, theoretical)

  if (ci) {
    # Stirling, W. D. (1982). Enhancements to aid interpretation of probability plots. Journal of the Royal Statistical Society: Series D (The Statistician), 31(3), 211-220.
    # Quesenberry, C. P., & Hales, C. (1980). Concentration bands for uniformity plots. Journal of Statistical Computation and Simulation, 11(1), 41-53.
    i     <- seq_along(sample)
    alpha <- 1-ci_level

    df[["lower"]] <- qbeta(  alpha/2, i, n-i+1)
    df[["upper"]] <- qbeta(1-alpha/2, i, n-i+1)

    ci_layer <- ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = theoretical, ymin = lower, ymax = upper),
      fill = "steelblue", color = "black", alpha = 0.5
    )
  } else {
    ci_layer <- NULL
  }

  plot <- ggplot2::ggplot(data = df) +
    ci_layer +
    jaspGraphs::geom_abline2(slope = 1, intercept = 0, linewidth = 1) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = theoretical, y = sample), shape=21, fill = "grey", size=3) +
    ggplot2::scale_x_continuous(name = gettext("Theoretical"), limits = 0:1, expand = ggplot2::expand_scale(mult = 0, add = c(0.05, 0.1))) +
    ggplot2::scale_y_continuous(name = gettext("Sample"),      limits = 0:1, expand = ggplot2::expand_scale(mult = 0, add = c(0.05, 0.1))) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  return(plot)
}
