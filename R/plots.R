


S7::new_generic("plot", "x")

S7::method(plot, distribution) <- function(x, type=c("all", "pdf", "cdf", "qf"), ...) {
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

plot_distribution <- S7::new_class(
  name = "plot_distribution",
  properties = list(
    layers = S7::class_list,
    xlab = S7::class_character,
    ylab = S7::class_character,
    x_range= S7::class_numeric | NULL
  ),
  abstract = TRUE
)

S7::method(print, plot_distribution) <- function(x, ...) {
  plot <- plot_assemble(x, ...)
  print(plot)
}


plot_pdf <- S7::new_class(
  name = "plot_pdf",
  parent = plot_distribution,
  constructor = function(x_range=NULL) {
    S7::new_object(
      S7::S7_object(),
      layers = list(),
      xlab = "X",
      ylab = "Density",
      x_range = x_range
    )
  }
)

plot_assemble <- S7::new_generic("plot_assemble", "plot")

S7::method(plot_assemble, plot_pdf) <- function(plot, ...) {
  ggplot2::ggplot() +
    plot@layers +
    ggplot2::xlab(plot@xlab) +
    ggplot2::ylab(plot@ylab) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
}

draw_function <- S7::new_generic("draw_function", c("plot", "distribution"))

S7::method(draw_function, list(plot_pdf, distribution_continuous)) <- function(
    plot, distribution,
    range=qf(distribution, p=c(0.01, 0.99)), length=101,
    ...
    ) {
  x = seq(range[1], range[2], length.out = length)
  y = pdf(distribution, x)
  df <- data.frame(x=x,y=y)

  l <- length(plot@layers)
  plot@layers[[l+1]] <- ggplot2::geom_line(
    mapping = ggplot2::aes(x=x,y=y),
    data = df, ...)

  return(plot)
}

draw_density <- S7::new_generic("draw_density", c("plot", "distribution"))

S7::method(draw_density, list(plot_pdf, distribution_continuous)) <- function(
    plot, distribution, x,
    point_args=list(), segment_args=list()) {

  segment_args[["data"]] <- data.frame(x=x, xend=x, y=0, yend=pdf(distribution, x))
  segment_args[["mapping"]] <- ggplot2::aes(x=x,y=y, xend=xend, yend=yend)
  l <- length(plot@layers)
  plot@layers[[l+1]] <-
    do.call(ggplot2::geom_segment, segment_args)


  point_args[["data"]] <- data.frame(x=x, y=pdf(distribution, x))
  point_args[["mapping"]] <- ggplot2::aes(x=x,y=y)
  l <- length(plot@layers)
  plot@layers[[l+1]] <-
    do.call(jaspGraphs::geom_point, point_args)
  return(plot)
}

draw_probability <- S7::new_generic("draw_probability", c("plot", "distribution"))

S7::method(draw_probability, list(plot_pdf, distribution_continuous)) <- function(
    plot, distribution, x) {

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
