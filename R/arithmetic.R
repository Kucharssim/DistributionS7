S7::method(`+`, list(normal, normal)) <- function(e1, e2) {
  normal(mu = e1@mu + e2@mu, sigma2 = e1@sigma2 + e2@sigma2)
}
