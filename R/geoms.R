# Define StatPdf ggproto
StatPdf <- ggplot2::ggproto(
  "StatPdf", ggplot2::Stat,
  default_aes = ggplot2::aes(x=NULL, y = ggplot2::after_scale(y)),
  compute_group = function(data, scales, dist, log=FALSE, n=101, p=c(0.01, 0.99)) {
    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else {
      q_vals <- qf(dist, p)
      x_vals <- seq(q_vals[1], q_vals[2], length.out = n)
    }
    y_vals <- pdf(dist, x_vals, log)
    data.frame(x = x_vals, y = y_vals)
  }
)

# Wrapper function for PDF
stat_pdf <- function(dist, mapping = NULL, data = NULL, geom = "point",
                     position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data <- ggplot2:::ensure_nonempty_data

  ggplot2::layer(
    stat = StatPdf, data = data, mapping = mapping, geom = geom,
    position = position, inherit.aes = inherit.aes, show.legend = show.legend,
    params = list(dist = dist, ...)
  )
}


ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    data.frame(group = 1, .size = 1)
  } else {
    data
  }
}
