#' @title Compare distributions
#' @description
#' Compare distributions using information criteria.
#'
#' @param ... Objects of class [Distribution()].
#' @param data Numeric containing the data.
#' @param x Numberic vector of AIC/BIC values.
#'
#' @details
#' Computes [information_criteria()] for each distribution.
#' Note that prior to comparing the distributions, you ought to fit the distributions with [Mle()] (see [fit_distribution()]).
#'
#' @returns
#' A data.frame containing the results.
#'
#' @name compare
#' @export
compare <- function(..., data) {
  distributions <- rlang::dots_list(..., .named = TRUE)
  names <- names(distributions)

  ic <- lapply(distributions, function(distribution) information_criteria(distribution, data))
  ic <- do.call(rbind, ic)

  ic[["aic_weight"]] <- weights_ic(ic[["aic"]])
  ic[["bic_weight"]] <- weights_ic(ic[["bic"]])
  ic[["name"]] <- names
  return(ic)
}

#' @rdname compare
#' @export
weights_ic <- function(x) {
  x <- x-min(x)
  x <- exp(-0.5*x)
  return(x / sum(x))
}
