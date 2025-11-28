


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

S7::method(plot_pdf, DistributionContinuous) <- function(distribution, xlim, points=NULL, intervals=list(), ...) {

  plot <- ggplot2::ggplot() +
    ggplot2::xlim(x_range(distribution, xlim))

  for (interval in intervals) {
    plot <- plot +
      stat_pdf(n, geom="ribbon", fill="steelblue", xlim=interval) +
      stat_cdf_interval(n, xlim=interval, mapping=ggplot2::aes(label = ggplot2::after_stat(area)))
  }


  plot <- plot +
    stat_pdf(distribution, geom="line", linewidth=1.5)

  if (!is.null(points))
    plot <- plot +
      stat_pdf(distribution, data=data.frame(x=points), mapping = ggplot2::aes(x=x),
               size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5)


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






# plot_pdf <- S7::new_generic("plot_pdf", "distribution")
#
# S7::method(plot_pdf, distribution_continuous) <- function(distribution, x_range=qf(distribution, p=c(0.01, 0.99))) {
#   plot <- ggplot2::ggplot() +
#     jaspGraphs::scale_x_continuous(limits=x_range) +
#     jaspGraphs::scale_y_continuous() +
#     ggplot2::xlab("X") +
#     ggplot2::ylab("Density")
#
#   result <- list(plot=plot, x_range=x_range)
#
#   class(result) <- c("plot_pdf", class(result))
#   return(result)
# }
#
# S7::method(plot_pdf, distribution_discrete) <- function(distribution, density=NULL, probability=NULL) {
#   rlang::abort("Not implemented yet!")
# }


# plot_cdf <- S7::new_generic("plot_cdf", "distribution")
#
# S7::method(plot_cdf, distribution_continuous) <- function(distribution, x_range=qf(distribution, p=c(0.01, 0.99)), density=NULL, probability=NULL) {
#   x_axis <- seq(x_range[1], x_range[2], length.out=101)
#   y_axis <- cdf(distribution, x_axis)
#
#   plot <- ggplot2::ggplot() +
#     ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x, y=y), linewidth=1)
#
#   plot <- plot +
#     jaspGraphs::scale_x_continuous() +
#     ggplot2::scale_y_continuous(limits=c(0,1)) +
#     ggplot2::xlab("X") +
#     ggplot2::ylab("Cumulative probability")
#
#   class(plot) <- c("plot_cdf", class(plot))
#   return(plot)
# }
#
# plot_qf <- S7::new_generic("plot_qf", "distribution")
#
# S7::method(plot_qf, distribution_continuous) <- function(distribution, x_range=c(0.01, 0.99), ...) {
#   x_axis <- seq(x_range[1], x_range[2], length.out=101)
#   y_axis <- qf(distribution, x_axis)
#
#   plot <- ggplot2::ggplot() +
#     ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x, y=y), linewidth=1)
#
#   plot <- plot +
#     jaspGraphs::scale_y_continuous() +
#     jaspGraphs::scale_x_continuous() +
#     ggplot2::xlab("Cumulative probability") +
#     ggplot2::ylab("X")
#
#   return(plot)
# }
#
# highlight_function <- S7::new_generic("highlight_function", c("plot", "distribution"))
#
# S7::method(highlight_function, list(S7::new_S3_class("plot_pdf"), distribution_continuous)) <- function(plot, distribution) {
#   plot <- plot
#     ggplot2::geom_line(data = data.frame(x=x_axis, y=y_axis), mapping = ggplot2::aes(x=x,y=y), linewidth=1)
#   x_axis = seq(x_range[1], x_range[2], length.out=101)
#   y_axis = pdf(distribution, x_axis)
#   df = data.frame(x = x_axis, y = y_axis)
# }
#
#
# highlight_density <- S7::new_generic("highlight_density", c("plot", "distribution"))
#
# S7::method(highlight_density, list(S7::new_S3_class("plot_pdf"), distribution_continuous)) <- function(plot, distribution, x){
#   df_points = data.frame(x=x, y=pdf(distribution, x))
#   plot <- plot +
#     ggplot2::geom_segment(
#       data = data.frame(
#         x = x, y = 0,
#         xend = x, yend = df_points[["y"]]
#       ),
#       mapping = ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
#       linetype = 2
#     ) +
#     ggplot2::geom_segment(
#       data = data.frame(
#         x = x-1, y = df_points[["y"]],
#         xend = x, yend = df_points[["y"]]
#       ),
#       mapping = ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
#       linetype = 2
#     ) +
#     jaspGraphs::geom_point(data = df_points, mapping = ggplot2::aes(x=x, y=y), size=2.5)
#
#   return(plot)
# }
