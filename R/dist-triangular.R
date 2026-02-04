Triangular <- S7::new_class(
  "Triangular",
  parent = DistributionContinuous,
  properties = list(
    a = Parameter,
    b = Parameter,
    c = Parameter,
    epsilon = S7::class_numeric
  ),
  constructor = function(a, b, c) {
    S7::new_object(
      S7::S7_object(),
      name = "Triangular",
      support = Real(min=expression(a), max=expression(b)),
      a = Parameter("a", "minimum", "a", a, Real()),
      b = Parameter("b", "maximum", "b", b, Real()),
      c = Parameter("c", "mode",    "c", c, Real()),
      epsilon = 1e-2
    )
  }
)

triangular <- function(a, b, c) {
  Triangular(a, b, c)
}

S7::method(pdf_fn, Triangular) <- function(distribution) function(x, a, b, c, log = FALSE) {
  lpdf <- rep(-Inf, length(x))

  in_range <- x >= a & x <= b
  left  <- in_range & (x <= c)
  right <- in_range & (x >  c)

  lpdf[left]  <- log(2) + log(x[left] - a) - log(b - a) - log(c - a)
  lpdf[right] <- log(2) + log(b - x[right]) - log(b - a) - log(b - c)

  if (log) lpdf else exp(lpdf)
}

S7::method(cdf_fn, Triangular) <- function(distribution) function(q, a, b, c, lower.tail = TRUE, log.p = FALSE) {
  lcdf <- rep(NA_real_, length(q))

  lcdf[q < a] <- -Inf
  lcdf[q > b] <- 0

  left  <- q >= a & q <= c
  right <- q <= b & q > c

  lcdf[left]  <- 2*log(q[left]-a)-log(b-a)-log(c-a)
  lcdf[right] <- log1p(-exp(2*log(b-q[right])-log(b-a)-log(b-c)))

  if(!lower.tail) out <- log1p(-exp(lcdf))

  if (log.p) lcdf else exp(lcdf)
}

S7::method(qf_fn, Triangular)  <- function(distribution) function(p, a, b, c, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p

  q <- rep(NA_real_, length(p))

  q[p <= 0] <- a
  q[p >= 1] <- b

  Fc <- (c - a) / (b - a)

  left  <- p > 0 & p <= Fc
  right <- p > Fc & p < 1

  q[left]  <- a + sqrt(p[left] * (b - a) * (c - a))
  q[right] <- b - sqrt((1 - p[right]) * (b - a) * (b - c))

  q
}

S7::method(rng_fn, Triangular) <- function(distribution) function(n, a, b, c) {
  p <- runif(n, 0, 1)
  q <- qf(distribution, p=p)

  return(q)
}

S7::method(rargs, Triangular) <- function(distribution) {
  parameter_values(distribution, which="all")
}

S7::method(expectation, Triangular) <- function(distribution, ...) with(parameter_values(distribution, which="all"), (a + b + c / 3))

S7::method(variance, Triangular)  <- function(distribution, ...) {
  with(
    parameter_values(distribution, which="all"),
    (a^2 + b^2 + c^2 - a*b - a*c - b*c)/18
  )
}

S7::method(parameter_start, Triangular) <- function(distribution, data) {
  if (!distribution@a@fixed) {
    m <- min(data)
    distribution@a@value <- m - distribution@epsilon
    distribution@a@support@max <- m
  } else {
    m <- distribution@a@value
  }

  if (!distribution@b@fixed) {
    M <- max(data)
    distribution@b@value <- M + distribution@epsilon
    distribution@b@support@min <- M
  } else {
    M <- distribution@b@value
  }

  if (!distribution@c@fixed) {
    distribution@c@value <- 3*mean(data) - m - M
    distribution@c@support@min <- m
    distribution@c@support@max <- M
  }

  return(distribution)
}
