compare <- function(..., data, estimated=FALSE) {
  distributions <- rlang::dots_list(..., .named = TRUE)
  names <- names(distributions)

  ic <- lapply(distributions, function(distribution) information_criteria(distribution, data, estimated=estimated))
  ic <- do.call(rbind, ic)

  ic[["aic_weight"]] <- weights_ic(ic[["aic"]])
  ic[["bic_weight"]] <- weights_ic(ic[["bic"]])
  ic[["name"]] <- names
  ic <- tibble::tibble(ic)
  return(ic)
}

weights_ic <- function(x) {
  x <- x-min(x)
  x <- exp(-0.5*x)
  return(x / sum(x))
}
