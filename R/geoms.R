#' @title Distribution stats for ggplot2
#' @description
#' ggplot2 layers that compute the density (`stat_pdf()`), the cumulative probability (`stat_cdf()`), and the
#' probability of intervals (`stat_cdf_interval()`) of a [Distribution()].
#'
#' Like [ggplot2::stat_function()], the stats compute the distribution over the limits of the x scale, or over `xlim`
#' if supplied. `xlim` is intersected with the limits of the x scale (`-Inf` and `Inf` extend to the edge of the plot),
#' and layers whose `xlim` lies entirely outside of the x scale are not drawn. Because `xlim` is a parameter rather
#' than an aesthetic, it neither trains nor is censored by the x scale. Values (e.g., probabilities of intervals) are
#' computed from the original `xlim`.
#'
#' @inheritParams ggplot2::layer
#' @param dist Object of class [Distribution()].
#' @param ... Other arguments passed on to the stat (e.g., `xlim`, `n`, `log`, `log.p`) or to the geom.
#'
#' @section Aesthetics:
#' `stat_pdf()` and `stat_cdf()` evaluate the distribution at `x` if supplied. Otherwise they evaluate it on a grid
#' of `n` points across `xlim` or the x scale.
#'
#' `stat_cdf_interval()` takes the interval as `xlim` (default: the support of the distribution).
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
    x=NULL,
    y = ggplot2::after_stat(density), ymin = ggplot2::after_stat(0), ymax = ggplot2::after_stat(density),
    slope = ggplot2::after_stat(density), intercept = ggplot2::after_stat(intercept)),
  compute_group = function(data, scales, dist, log=FALSE, n=101, xlim=NULL, coverage=0.99) {
    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else {
      range <- stat_range(scales, xlim, dist, coverage)
      if (is.null(range)) return(data.frame())
      x_vals <- seq(range[1], range[2], length.out = n)
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
  default_aes = ggplot2::aes(y=ggplot2::after_stat(probability)),
  compute_group = function(data, scales, dist, log.p=FALSE, n=101, xlim=NULL, coverage=0.99) {
    plot_lim <- scale_limits(scales, dist, coverage)

    if ("x" %in% colnames(data)) {
      x_vals <- data$x
    } else {
      range <- stat_range(scales, xlim, dist, coverage)
      if (is.null(range)) return(data.frame())
      x_vals <- seq(range[1], range[2], length.out = n)
    }

    probability <- cdf(dist, x_vals, log.p=log.p)
    data.frame(x=x_vals, probability=probability, xleft=plot_lim[1])
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
    x = ggplot2::after_stat(x_center), y = ggplot2::after_stat(y_center), label = ggplot2::after_stat(probability)),
  compute_group = function(data, scales, dist, xlim=NULL, coverage=0.99) {
    if (is.null(xlim)) {
      s <- support(dist)
      xlim <- c(s@min, s@max)
    }
    if (xlim[1] > xlim[2]) cli::cli_abort("Minimum of an interval must be smaller than the maximum of an interval")

    # the probability is computed from the untruncated interval
    probability <- 1 - cdf(dist, xlim[1]) - cdf(dist, xlim[2], lower.tail=FALSE)

    # positions are truncated to the plotable area
    range <- stat_range(scales, xlim, dist, coverage)
    if (is.null(range)) return(data.frame())
    plot_lim <- scale_limits(scales, dist, coverage)
    xminplot <- range[1]
    xmaxplot <- range[2]

    # center of the area under the density
    x_vals <- seq(xminplot, xmaxplot, length.out = 101)
    y_vals <- pdf(dist, x_vals)
    x_center <- sum(x_vals * y_vals) / sum(y_vals)
    y_center <- mean(y_vals) / 2

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
      pmin_visible=if (xlim[1] < plot_lim[1]) NA_real_ else pmin,
      pmax_visible=if (xlim[2] > plot_lim[2]) NA_real_ else pmax,
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

# limits of the x scale; if there is no trained x scale, the central `coverage` of the distribution
scale_limits <- function(scales, dist, coverage) {
  if (!is.null(scales$x) && !scales$x$is_empty()) return(scales$x$get_limits())

  tail <- (1-coverage) / 2
  qf(dist, c(tail, 1-tail))
}

# range over which to compute a stat: `xlim` (if supplied) intersected with the limits of the x scale;
# NULL if `xlim` lies entirely outside of the x scale
stat_range <- function(scales, xlim, dist, coverage) {
  plot_lim <- scale_limits(scales, dist, coverage)
  if (is.null(xlim)) return(plot_lim)

  range <- c(max(xlim[1], plot_lim[1]), min(xlim[2], plot_lim[2]))
  if (range[1] > range[2]) return(NULL)

  range
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
