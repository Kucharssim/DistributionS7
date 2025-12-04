S7::new_generic("plot", "x")
plot_pdf <- S7::new_generic("plot_pdf", "distribution")
plot_cdf <- S7::new_generic("plot_cdf", "distribution")
plot_qf  <- S7::new_generic("plot_qf",  "distribution")

S7::method(plot, Distribution) <- function(x, type=c("all", "pdf", "cdf", "qf"), ...) {
  type <- rlang::arg_match(type, multiple=FALSE)
  plot <- switch(
    type,
    pdf = plot_pdf(x, ...),
    cdf = plot_cdf(x, ...),
    qf  = plot_qf (x, ...),
    all = list(plot_pdf(x), plot_cdf(x), plot_qf(x))
  )

  if(type != "all") {
    plot <- plot +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe()
  } else {
    plot <- lapply(plot, \(p) p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe())
    plot[[4]] <- plot[[3]]
    plot[[3]] <- patchwork::plot_spacer()
    plot <- patchwork::wrap_plots(plot, ncol=2, nrow=2, byrow = FALSE)
  }

  return(plot)
}

S7::method(plot_pdf, DistributionContinuous) <- function(distribution, ..., xlim, points=NULL, intervals=NULL) {

  plot <- ggplot2::ggplot() +
    ggplot2::xlim(x_range(distribution, xlim))

  if (!is.null(intervals)) {
    intervals[[".group"]] <- seq_len(nrow(intervals))
    plot <- plot +
      stat_pdf(distribution, data=intervals,
               mapping=ggplot2::aes(xmin=xmin, xmax=xmax, group=.group),
               geom="ribbon", fill="steelblue", alpha=0.8) +
      stat_cdf_interval(distribution, data=intervals,
                        mapping=ggplot2::aes(xmin=xmin, xmax=xmax, group=.group,
                                             label=ggplot2::after_stat(sprintf("%.3f", probability))
                                             )
      )
  }

  plot <- plot +
    stat_pdf(distribution, geom="line", linewidth=1.5)

  if (!is.null(points)) {
    df_points <- data.frame(x=points)
    plot <- plot +
      stat_pdf(distribution, data=df_points,
               mapping = ggplot2::aes(x=x),
               size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5) +
      stat_pdf(distribution, data=df_points,
               mapping = ggplot2::aes(x=x, label=ggplot2::after_stat(sprintf("%.3f", density))),
               geom = ggrepel::GeomTextRepel
               )
  }


  return(plot)
}


x_range <- S7::new_generic("x_range", "distribution")

S7::method(x_range, DistributionContinuous) <- function(distribution, xlim, coverage=0.99) {
  if(missing(xlim)) {
    tail <- (1-coverage) / 2
    xlim <- qf(distribution, c(tail, 1-tail))
  }

  return(xlim)
}
