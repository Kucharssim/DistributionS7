StatPdf <- ggplot2::ggproto(
  "StatPdf", ggplot2::Stat,
  default_aes = ggplot2::aes(
    x=NULL,
    y = ggplot2::after_scale(y), ymin = ggplot2::after_stat(0), ymax = ggplot2::after_stat(y),
    slope = ggplot2::after_stat(y), intercept = ggplot2::after_scale(intercept)),
  compute_group = function(data, scales, dist, log=FALSE, n=101, xlim=NULL, coverage=0.99) {
    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else if(!is.null(xlim)) {
      x_vals = seq(xlim[1], xlim[2], length.out = n)
    } else if (!is.null(scales$x)) {
      xlim <- scales$x$get_limits()
      x_vals = seq(xlim[1], xlim[2], length.out = n)
    } else {
      tail <- (1-coverage) / 2
      xlim <- qf(dist, c(tail, 1-tail))
      x_vals <- seq(xlim[1], xlim[2], length.out = n)
    }
    y_vals <- pdf(dist, x_vals, log)
    intercept <- cdf(dist, x_vals) - y_vals*x_vals
    data.frame(x = x_vals, y = y_vals, intercept = intercept)
  }
)

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


StatCdf <- ggplot2::ggproto(
  "StatCdf", ggplot2::Stat,
  default_aes = ggplot2::aes(x=NULL, y=ggplot2::after_scale(y)),
  compute_group = function(data, scales, dist, lower.tail=TRUE, log.p=FALSE, n=101, xlim=NULL, coverage=0.99) {
    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else if(!is.null(xlim)) {
      x_vals = seq(xlim[1], xlim[2], length.out = n)
    } else if (!is.null(scales$x)) {
      xlim <- scales$x$get_limits()
      x_vals = seq(xlim[1], xlim[2], length.out = n)
    } else {
      tail <- (1-coverage) / 2
      xlim <- qf(dist, c(tail, 1-tail))
      x_vals <- seq(xlim[1], xlim[2], length.out = n)
    }
    y_vals = cdf(dist, x_vals, lower.tail=lower.tail, log.p=log.p)
    data.frame(x=x_vals, y=y_vals)
  }
)

stat_cdf <- function(dist, mapping = NULL, data = NULL, geom = "point",
                     position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data = ggplot2:::ensure_nonempty_data

  ggplot2::layer(
    stat = StatCdf, data = data, mapping = mapping, geom = geom,
    position = position, inherit.aes = inherit.aes, show.legend = show.legend,
    params = list(dist = dist, ...)
  )
}


StatCdfInterval <- ggplot2::ggproto(
  "StatCdfInterval", ggplot2::Stat,
  default_aes = ggplot2::aes(x = ggplot2::after_stat(x_center), y = ggplot2::after_stat(y_center), label = ggplot2::after_stat(area)),
  compute_group = function(data, scales, dist, xlim=NULL, digits=3) {
    if ("xmin" %in% colnames(data)) {
      xmin <- data$xmin
    } else if (!is.null(xlim)) {
      xmin <- xlim[1]
    } else {
      xmin <- dist@support@min
    }

    if ("xmax" %in% colnames(data)) {
      xmax <- data$xmax
    } else if (!is.null(xlim)) {
      xmax <- xlim[2]
    } else {
      xmax <- dist@support@max
    }

    area <- 1 - cdf(dist, xmin) - cdf(dist, xmax, lower.tail=FALSE)

    if (!is.null(scales$x)) {
      plot_lim <- scales$x$get_limits()
    } else {
      cli::cli_abort("stat_cdf_interval cannot itself determine plot limits")
    }

    if (xmin < plot_lim[1]) {
      xmin <- plot_lim[1]
    } else if (xmin > plot_lim[2]) {
      cli::cli_abort(
        c("Stat interval is outside of the plot limits",
          "i" = "The plot limits are {plot_lim}",
          "x" = "The interval is {c(xmin, xmax)}")
      )
    }

    if (xmax > plot_lim[2]) {
      xmax <- plot_lim[2]
    } else if(xmax < plot_lim[1]) {
      cli::cli_abort(
        c("Stat interval is outside of the plot limits",
          "i" = "The plot limits are {xlim}",
          "x" = "The interval is {c(xmin, xmax)}")
      )
    }

    x_vals <- seq(xmin, xmax, length.out=101)
    y_vals <- pdf(dist, x_vals)

    x_center <- sum(x_vals * y_vals) / (sum(y_vals))
    y_center <- pdf(dist, x_center) / 2

    data.frame(x_center=x_center, y_center=y_center, area=signif(area, digits=digits))
  }
)

stat_cdf_interval <- function(dist, mapping = NULL, data = NULL, geom = "text",
                              position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data = ggplot2:::ensure_nonempty_data

  ggplot2::layer(
    stat = StatCdfInterval, data = data, mapping = mapping, geom = geom,
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
