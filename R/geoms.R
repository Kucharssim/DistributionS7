StatPdf <- ggplot2::ggproto(
  "StatPdf", ggplot2::Stat,
  default_aes = ggplot2::aes(
    x=NULL,xmin=NULL,xmax=NULL,
    y = ggplot2::after_stat(density), ymin = ggplot2::after_stat(0), ymax = ggplot2::after_stat(density),
    slope = ggplot2::after_stat(density), intercept = ggplot2::after_stat(intercept)),
  compute_group = function(data, scales, dist, log=FALSE, n=101, xlim=NULL, coverage=0.99) {
    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else {
      if ("xmin" %in% colnames(data) && "xmax" %in% colnames(data)) {
        xmin <- data[["xmin"]]
        xmax <- data[["xmax"]]
      } else if (!is.null(xlim)) {
        xmin <- xlim[1]
        xmax <- xlim[2]
      } else if (!is.null(scales$x)) {
        xlim <- scales$x$get_limits()
        xmin <- xlim[1]
        xmax <- xlim[2]
      } else {
        tail <- (1-coverage) / 2
        xmin <- qf(dist, tail)
        xmax <- qf(dist, 1-tail)
      }

      xlim <- scales$x$get_limits()

      if (!is.finite(xmin) || xmin < xlim[1]) xmin <- xlim[1]
      if (!is.finite(xmax) || xmax > xlim[2]) xmax <- xlim[2]

      x_vals <- seq(xmin, xmax, length.out = n)
    }

    density <- pdf(dist, x_vals, log)
    intercept <- cdf(dist, x_vals) - density*x_vals
    data.frame(x = x_vals, density = density, intercept = intercept)
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
  default_aes = ggplot2::aes(xmin=NULL, xmax=NULL, y=ggplot2::after_scale(y)),
  compute_group = function(data, scales, dist, log.p=FALSE, n=101, xlim=NULL, coverage=0.99) {
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
  default_aes = ggplot2::aes(
    xmin=NULL,xmax=NULL,
    x = ggplot2::after_stat(x_center), y = ggplot2::after_stat(y_center), label = ggplot2::after_stat(probability)),
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

    probability <- 1 - cdf(dist, xmin) - cdf(dist, xmax, lower.tail=FALSE)

    if (!is.null(scales$x)) {
      plot_lim <- scales$x$get_limits()
    } else {
      cli::cli_abort("stat_cdf_interval cannot itself determine plot limits")
    }

    if (any(xmin > xmax)) cli::cli_abort("Minimum of an interval must be smmaler than the maximum of an interval")

    # truncate the intervals to plotable area
    xminplot <- ifelse(xmin < plot_lim[1], plot_lim[1], xmin)
    xmaxplot <- ifelse(xmax > plot_lim[2], plot_lim[2], xmax)

    # for each interval, calculate the center of the plottable area
    # this is done by calculating the weighted density
    x_vals <- mapply(seq, from=xminplot, to=xmaxplot, length.out=101, SIMPLIFY = TRUE)
    y_vals <- apply(x_vals, 2, function(x) pdf(dist, x))

    x_center <- colSums(x_vals * y_vals) / colSums(y_vals)
    y_center <- pdf(dist, x_center) / 2

    data.frame(x_center=x_center, y_center=y_center, probability=probability)
  }
)

stat_cdf_interval <- function(dist, mapping = NULL, data = NULL, geom = ggrepel::GeomLabelRepel,
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
