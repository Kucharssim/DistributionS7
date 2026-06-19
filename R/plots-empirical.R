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
#' @param breaks Passed to [graphics::hist()].
#' @param ... Not used.
#'
#' @import patchwork
#' @name plot-empirical
#' @export
plot_empirical <- S7::new_generic("plot_empirical", "distribution", function(distribution, data, type=c("all", "hist", "qq", "ecdf", "pp"), ..., name, ci=FALSE, ci_level=0.95) {
  S7::S7_dispatch()
})

#' @rdname plot-empirical
#' @export
plot_hist  <- S7::new_generic("plot_hist",  "distribution", function(distribution, data, ..., name, breaks = "Sturges") {
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

  plot[[1]] <- plot[[1]] + ggplot2::ggtitle(gettext("Histogram vs. Theoretical Density"))
  plot[[2]] <- plot[[2]] + ggplot2::ggtitle(gettext("Q-Q plot"))
  plot[[3]] <- plot[[3]] + ggplot2::ggtitle(gettext("Empirical vs. Theoretical Cumulative Probability"))
  plot[[4]] <- plot[[4]] + ggplot2::ggtitle(gettext("P-P plot"))
  plot <- patchwork::wrap_plots(plot, ncol=2, nrow=2, byrow = TRUE) +
    ggplot2::theme(plot.margin = ggplot2::margin(t=5, l=5))

  return(plot)
}


S7::method(plot_hist, DistributionContinuous) <- function(distribution, data, ..., name, breaks = "Sturges") {
  if (missing(name)) name <- deparse1(substitute(data))

  h <- graphics::hist(data, plot=FALSE, breaks=breaks)
  d <- pdf(distribution, data)

  y_breaks <- pretty(c(0, h[["density"]], d))
  y_range  <- range(y_breaks)
  x_breaks <- pretty(data)
  x_range  <- range(x_breaks)

  plot <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(mapping = ggplot2::aes(x=x, y=ggplot2::after_stat(density)), breaks=h[["breaks"]], fill="grey", col="black", linewidth=0.7) +
    ggplot2::geom_rug() +
    stat_pdf(distribution, xlim = range(x_range), geom="line", linewidth = 1.5, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(name = name,               limits = x_range, breaks = x_breaks) +
    ggplot2::scale_y_continuous(name = gettext("Density"), limits = y_range, breaks = y_breaks)

  return(plot)
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

  y_breaks <- pretty(as.vector(as.matrix(df)))
  y_range  <- range(y_breaks)
  x_breaks <- pretty(theoretical)
  x_range  <- range(x_breaks)

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(sample = sample)) +
    ci_layer +
    ggplot2::geom_line(mapping = ggplot2::aes(x = theoretical, y = theoretical), linewidth = 1) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = theoretical, y = sample), shape=21, fill = "grey", size=3) +
    ggplot2::scale_x_continuous(
      name = gettext("Theoretical"), limits = x_range, breaks = x_breaks) +
    ggplot2::scale_y_continuous(
      name = gettext("Sample"),      limits = y_range, breaks = y_breaks)

  return(plot)
}

S7::method(plot_ecdf, DistributionContinuous) <- function(distribution, data, ..., name) {
  if (missing(name)) name <- deparse1(substitute(data))

  x_breaks <- pretty(data)
  x_range  <- range(x_breaks)

  plot <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes(x = x)) +
    ggplot2::geom_rug() +
    stat_cdf(distribution, xlim = x_range, geom="line", linewidth = 1.5, inherit.aes = FALSE, alpha = 0.8) +
    ggplot2::stat_ecdf(geom = "step", pad = TRUE, linewidth = 1.0, alpha = 0.8) +
    ggplot2::scale_x_continuous(limits = x_range, breaks=x_breaks) +
    ggplot2::scale_y_continuous(limits = 0:1) +
    ggplot2::ylab(gettext("Cumulative Probability")) +
    ggplot2::xlab(name)

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
    ggplot2::geom_segment(
      data = data.frame(x = 0, y = 0, xend = 1, yend = 1), 
      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = 1) + 
    ggplot2::geom_point(mapping = ggplot2::aes(x = theoretical, y = sample), shape=21, fill = "grey", size=3) +
    ggplot2::scale_x_continuous(name = gettext("Theoretical"), limits = 0:1) +
    ggplot2::scale_y_continuous(name = gettext("Sample"),      limits = 0:1)

  return(plot)
}
