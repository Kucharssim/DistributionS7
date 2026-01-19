compare_ic <- function(..., data) {
  distributions <- rlang::dots_list(..., .named = TRUE)
  names <- names(distributions)

  ic <- lapply(distributions, function(distribution) information_criteria(distribution, data))
  ic <- do.call(rbind, ic)

  rownames(ic) <- names

  ic[["aic_weight"]] <- weights_ic(ic[["aic"]])
  ic[["bic_weight"]] <- weights_ic(ic[["bic"]])

  return(ic)
}

weights_ic <- function(x) {
  x <- exp(-0.5*x)
  return(x / sum(x))
}
