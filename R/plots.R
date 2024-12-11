S7::new_generic("plot", "x")

S7::method(plot, distribution) <- function(x, type=c("pdf", "cdf", "qf", "all"), ...) {

  plot <- switch(
    rlang::arg_match(type, multiple=FALSE),
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


plot_pdf <- S7::new_generic("plot_pdf", "distribution")

S7::method(plot_pdf, distribution_continuous) <- function(distribution, x_range=qf(distribution, p=c(0.01, 0.99)), density=NULL, probability=NULL) {
  x_axis = seq(x_range[1], x_range[2], length.out=101)
  x_axis = sort(c(x_axis, density))
  y_axis = pdf(distribution, x_axis)


  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x,y=y), linewidth=1)

  if(!is.null(density)) {
    df_points = data.frame(x=density, y=pdf(distribution, density))
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(
          x = density, y = 0,
          xend = density, yend = df_points[["y"]]
          ),
        mapping = ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
        linetype = 2
      ) +
      ggplot2::geom_segment(
        data = data.frame(
          x = x_axis[1], y = df_points[["y"]],
          xend = density, yend = df_points[["y"]]
        ),
        mapping = ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
        linetype = 2
      ) +
      jaspGraphs::geom_point(data = df_points, mapping = ggplot2::aes(x=x, y=y), size=2.5)
  }

  plot <- plot +
    jaspGraphs::scale_x_continuous() +
    jaspGraphs::scale_y_continuous() +
    ggplot2::xlab("X") +
    ggplot2::ylab("Density")


  return(plot)


}

S7::method(plot_pdf, distribution_discrete) <- function(distribution, density=NULL, probability=NULL) {
  rlang::abort("Not implemented yet!")
}


plot_cdf <- S7::new_generic("plot_cdf", "distribution")

S7::method(plot_cdf, distribution_continuous) <- function(distribution, x_range=qf(distribution, p=c(0.01, 0.99)), density=NULL, probability=NULL) {
  x_axis <- seq(x_range[1], x_range[2], length.out=101)
  y_axis <- cdf(distribution, x_axis)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x, y=y), linewidth=1)

  plot <- plot +
    jaspGraphs::scale_x_continuous() +
    ggplot2::scale_y_continuous(limits=c(0,1)) +
    ggplot2::xlab("X") +
    ggplot2::ylab("Cumulative probability")

  return(plot)
}

plot_qf <- S7::new_generic("plot_qf", "distribution")

S7::method(plot_qf, distribution_continuous) <- function(distribution, x_range=c(0.01, 0.99), ...) {
  x_axis <- seq(x_range[1], x_range[2], length.out=101)
  y_axis <- qf(distribution, x_axis)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x, y=y), linewidth=1)

  plot <- plot +
    jaspGraphs::scale_y_continuous() +
    jaspGraphs::scale_x_continuous() +
    ggplot2::xlab("Cumulative probability") +
    ggplot2::ylab("X")

  return(plot)
}

