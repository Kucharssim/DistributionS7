plot_distribution <- S7::new_generic("plot", "distribution")
plot_pdf <- S7::new_generic("plot_pdf", "distribution")
plot_cdf <- S7::new_generic("plot_cdf", "distribution")
plot_qf  <- S7::new_generic("plot_qf",  "distribution")

S7::method(plot_distribution, Distribution) <- function(distribution, type=c("all", "pdf", "cdf", "qf"), ...) {
  type <- rlang::arg_match(type, multiple=FALSE)
  plot <- switch(
    type,
    pdf = plot_pdf(distribution, ...),
    cdf = plot_cdf(distribution, ...),
    qf  = plot_qf (distribution, ...),
    all = list(plot_pdf(distribution, ...), plot_cdf(distribution, ...), plot_qf(distribution, ...))
  )

  if(type != "all") {
    plot <- plot +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe()
  } else {
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

  plot <- ggplot2::ggplot() +
    ggplot2::xlim(x_range(distribution, xlim))

  if (!is.null(probability)) {
    probability[[".group"]] <- seq_len(nrow(probability))
    plot <- plot +
      stat_pdf(distribution, data=probability,
               mapping=ggplot2::aes(xmin=xmin, xmax=xmax, group=.group),
               geom="ribbon", fill="steelblue", alpha=0.8) +
      stat_cdf_interval(distribution, data=probability,
                        mapping=ggplot2::aes(xmin=xmin, xmax=xmax, group=.group,
                                             label=ggplot2::after_stat(sprintf("%.3f", probability))
                                             )
      )
  }

  plot <- plot +
    stat_pdf(distribution, geom="line", linewidth=1.5)

  if (!is.null(density)) {
    df_density <- data.frame(x=density)
    plot <- plot +
      stat_pdf(distribution, data=df_density,
               mapping = ggplot2::aes(x=x, y=0, yend=ggplot2::after_stat(density)),
               geom = "segment", linetype=2) +
      stat_pdf(distribution, data=df_density,
               mapping = ggplot2::aes(x=x),
               size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5) +
      stat_pdf(distribution, data=df_density,
               mapping = ggplot2::aes(x=x, label=ggplot2::after_stat(sprintf("%.3f", density))),
               geom = ggrepel::GeomTextRepel
               )
  }

  plot <- plot + ggplot2::ylab(gettext("Density")) + ggplot2::xlab(gettext("X"))


  return(plot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe())
}

S7::method(plot_cdf, DistributionContinuous) <- function(distribution, ..., xlim, density=NULL, probability=NULL) {

  plot <- ggplot2::ggplot() +
    ggplot2::xlim(x_range(distribution, xlim))

  if (!is.null(density)) {
    df_density <- data.frame(x=density, g=seq_along(density))
    plot <- plot +
      stat_pdf(distribution, data = df_density,
               mapping = ggplot2::aes(x=x, group=g, color=factor(g)), geom="abline") +
      ggplot2::scale_color_discrete(name="Slope (density)", labels=sprintf("%.3f", pdf(distribution, density)))
  }

  plot <- plot +
    stat_cdf(distribution, geom="line", linewidth=1.5)

  if (!is.null(probability)) {
    df_probability <- data.frame(x=probability)
    plot <- plot +
      stat_cdf(distribution, data=df_probability,
               mapping = ggplot2::aes(x=x, y=0, yend=ggplot2::after_stat(probability)),
               geom = "segment", linetype=2) +
      stat_cdf(distribution, data=df_probability,
               mapping = ggplot2::aes(x=x),
               size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5) +
      stat_cdf(distribution, data=df_probability,
               mapping = ggplot2::aes(x=x, label=ggplot2::after_stat(sprintf("%.3f", probability))),
               geom = ggrepel::GeomTextRepel
      )
  }

  plot <- plot + ggplot2::ylab(gettext("Cumulative probability")) + ggplot2::xlab(gettext("X"))

  return(plot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe())
}

S7::method(plot_qf, DistributionContinuous) <- function(distribution, ..., xlim) {
  plot <- plot_cdf(distribution, xlim=xlim, ...)
  plot <- plot + ggplot2::coord_flip()

  return(plot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe())
}


x_range <- S7::new_generic("x_range", "distribution")

S7::method(x_range, DistributionContinuous) <- function(distribution, xlim, coverage=0.99) {
  if(missing(xlim)) {
    tail <- (1-coverage) / 2
    xlim <- qf(distribution, c(tail, 1-tail))
  }

  return(xlim)
}
