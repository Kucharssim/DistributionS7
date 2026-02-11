#' @title Compute weights from information criteria
#' @description Compute weights from [aic()] or [bic()] values.
#'
#' @param x Numeric vector of AIC/BIC values.
#'
#' @returns A vector of weights.
#' @export
weights_ic <- function(x) {
  x <- x-min(x)
  x <- exp(-0.5*x)
  return(x / sum(x))
}
