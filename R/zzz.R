.onLoad <- function(...) {
  S7::methods_register()

  S7::method(fit, Distribution) <- function(object, estimator=Mle(), data) fit_distribution(object, estimator, data)
}
