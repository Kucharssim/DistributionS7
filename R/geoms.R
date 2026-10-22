#' @title Distribution stats for ggplot2
#' @description
#' ggplot2 layers that compute the density (`stat_pdf()`), the cumulative probability (`stat_cdf()`), and the
#' probability of intervals (`stat_cdf_interval()`) of a [Distribution()].
#'
#' Positions are truncated to the limits of the x scale, but values (e.g., probabilities of intervals) are computed
#' from the original inputs.
#'
#' @inheritParams ggplot2::layer
#' @param dist Object of class [Distribution()].
#' @param ... Other arguments passed on to the stat (e.g., `n`, `xlim`, `coverage`, `log`, `log.p`) or to the geom.
#'
#' @section Aesthetics:
#' `stat_pdf()` and `stat_cdf()` evaluate the distribution at `x` if supplied. Otherwise they evaluate it on a grid
#' of `n` points between `xmin` and `xmax` (if supplied), or across the x scale.
#'
#' `stat_cdf_interval()` requires `xmin` and `xmax`; intervals that lie entirely outside of the x scale are dropped.
#'
#' @section Computed variables:
#' `stat_pdf()`:
#' - `density`: the density at `x`.
#' - `intercept`: intercept of the tangent of the cdf at `x` (its slope is `density`).
#'
#' `stat_cdf()`:
#' - `probability`: the cumulative probability at `x`.
#' - `xleft`: the left limit of the x scale.
#'
#' `stat_cdf_interval()`:
#' - `probability`: probability of the interval (untruncated).
#' - `xminplot`, `xmaxplot`: interval truncated to the x scale.
#' - `pmin`, `pmax`: cumulative probability at `xminplot` and `xmaxplot`.
#' - `pmin_visible`, `pmax_visible`: as `pmin` and `pmax`, but `NA` if that end of the interval was truncated.
#' - `x_center`, `y_center`: label position within the area under the density.
#' - `x_center_cdf`, `y_center_cdf`: label position within the area between the left limit and the cdf.
#' - `xleft`: the left limit of the x scale.
#'
#' @name distribution-stats
NULL

#' @rdname distribution-stats
#' @format NULL
#' @usage NULL
#' @export
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

      # intervals that lie entirely outside of the plot are not drawn
      if (xmin > xlim[2] || xmax < xlim[1]) return(data.frame())

      if (!is.finite(xmin) || xmin < xlim[1]) xmin <- xlim[1]
      if (!is.finite(xmax) || xmax > xlim[2]) xmax <- xlim[2]

      x_vals <- seq(xmin, xmax, length.out = n)
    }

    density <- pdf(dist, x_vals, log)
    intercept <- try(cdf(dist, x_vals) - density*x_vals, silent = TRUE)
    if (inherits(intercept, "try-error")) {
      rlang::warn("Distribution does not support cdf() for intercept stat calculation; using NA instead")
      intercept <- NA
    }
    data.frame(x = x_vals, density = density, intercept = intercept)
  }
)

#' @rdname distribution-stats
#' @export
stat_pdf <- function(dist, mapping = NULL, data = NULL, geom = "point",
                     position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data <- ensure_nonempty_data

  ggplot2::layer(
    stat = StatPdf, data = data, mapping = mapping, geom = geom,
    position = position, inherit.aes = inherit.aes, show.legend = show.legend,
    params = list(dist = dist, ...)
  )
}


#' @rdname distribution-stats
#' @format NULL
#' @usage NULL
#' @export
StatCdf <- ggplot2::ggproto(
  "StatCdf", ggplot2::Stat,
  default_aes = ggplot2::aes(xmin=NULL, xmax=NULL, y=ggplot2::after_stat(probability)),
  compute_group = function(data, scales, dist, log.p=FALSE, n=101, xlim=NULL, coverage=0.99) {
    plot_lim <- if (!is.null(scales$x)) scales$x$get_limits() else NULL

    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else if ("xmin" %in% colnames(data) && "xmax" %in% colnames(data)) {
      if (is.null(plot_lim)) cli::cli_abort("stat_cdf cannot itself determine plot limits")
      xmin <- data[["xmin"]]
      xmax <- data[["xmax"]]

      # intervals that lie entirely outside of the plot are not drawn
      if (xmin > plot_lim[2] || xmax < plot_lim[1]) return(data.frame())

      # truncate the interval to the plotable area
      if (xmin < plot_lim[1]) xmin <- plot_lim[1]
      if (xmax > plot_lim[2]) xmax <- plot_lim[2]

      x_vals <- seq(xmin, xmax, length.out = n)
    } else if(!is.null(xlim)) {
      x_vals = seq(xlim[1], xlim[2], length.out = n)
    } else if (!is.null(plot_lim)) {
      x_vals = seq(plot_lim[1], plot_lim[2], length.out = n)
    } else {
      tail <- (1-coverage) / 2
      xlim <- qf(dist, c(tail, 1-tail))
      x_vals <- seq(xlim[1], xlim[2], length.out = n)
    }
    probability = cdf(dist, x_vals, log.p=log.p)
    xleft <- if (!is.null(plot_lim)) plot_lim[1] else min(x_vals)
    data.frame(x=x_vals, probability=probability, xleft=xleft)
  }
)

#' @rdname distribution-stats
#' @export
stat_cdf <- function(dist, mapping = NULL, data = NULL, geom = "point",
                     position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data = ensure_nonempty_data

  ggplot2::layer(
    stat = StatCdf, data = data, mapping = mapping, geom = geom,
    position = position, inherit.aes = inherit.aes, show.legend = show.legend,
    params = list(dist = dist, ...)
  )
}


#' @rdname distribution-stats
#' @format NULL
#' @usage NULL
#' @export
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

    if (any(xmin > xmax)) cli::cli_abort("Minimum of an interval must be smaller than the maximum of an interval")

    # intervals that lie entirely outside of the plot are not drawn
    visible <- xmin <= plot_lim[2] & xmax >= plot_lim[1]
    if (!any(visible)) return(data.frame())
    xmin <- xmin[visible]
    xmax <- xmax[visible]
    probability <- probability[visible]

    # truncate the intervals to plotable area
    xminplot <- ifelse(xmin < plot_lim[1], plot_lim[1], xmin)
    xmaxplot <- ifelse(xmax > plot_lim[2], plot_lim[2], xmax)

    # for each interval, calculate the center of the plottable area
    # this is done by calculating the weighted density
    x_vals <- mapply(seq, from=xminplot, to=xmaxplot, length.out=101, SIMPLIFY = TRUE)
    y_vals <- apply(x_vals, 2, function(x) pdf(dist, x))

    x_center <- colSums(x_vals * y_vals) / colSums(y_vals)
    y_center <- colMeans(y_vals) / 2

    # on the cdf, the area spans vertically from pmin to pmax and horizontally from the left edge to the curve:
    # place the label in the middle of the rise, halfway between the left edge and the curve
    pmin <- cdf(dist, xminplot)
    pmax <- cdf(dist, xmaxplot)
    xleft <- plot_lim[1]
    y_center_cdf <- (pmin + pmax) / 2
    x_center_cdf <- (xleft + qf(dist, y_center_cdf)) / 2

    data.frame(
      x_center=x_center, y_center=y_center,
      x_center_cdf=x_center_cdf, y_center_cdf=y_center_cdf,
      probability=probability,
      xminplot=xminplot, xmaxplot=xmaxplot,
      pmin=pmin, pmax=pmax,
      # boundaries of the interval that are visible in the plot (NA if truncated)
      pmin_visible=ifelse(xmin < plot_lim[1], NA, pmin),
      pmax_visible=ifelse(xmax > plot_lim[2], NA, pmax),
      xleft=xleft
    )
  }
)

#' @rdname distribution-stats
#' @export
stat_cdf_interval <- function(dist, mapping = NULL, data = NULL, geom = "label",
                              position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  if(is.null(data))
    data = ensure_nonempty_data

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

empty <- function (df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || ggplot2::is_waiver(df)
}
