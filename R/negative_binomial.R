negative_binomial <- function(n, p, mu) {
  call <- match.call()
  definition <- switch(
    rlang::check_exclusive(p, mu),
    p = pars(
      list(
        par(key="n", value=n, support = real(0)),
        par(key="p", value=p, support = real(0, 1))
      ),
      list(
        tpar(key="mu", value = expression(n / p - n), update = list(p = expression(n / (n + mu))))
      ),
      list(
        size = expression(n),
        prob = expression(p)
      )
    ),
    mu = pars(
      list(
        par(key="n",  value=n, support = real(0)),
        par(key="mu", value=p, support = real(0))
      ),
      list(
        tpar(key="p", value = expression(n / (n + mu)), update = list(mu = expression(n / p - n)))
      ),
      list(
        size = expression(n),
        mu   = expression(mu)
      )
    )
  )

  negative_binomial <- S7::new_class(
    "negative_binomial",
    parent = distribution_discrete,
    properties = definition[["properties"]],
    constructor = function(n=NA, p=NA, mu=NA) {
      S7::new_object(
        S7::S7_object(),
        name = "Negative binomial",
        support = int(0),
        parameters = definition[["pars"]],
        transformed_parameters = definition[["tpars"]],
        rargs = definition[["rargs"]]
      )
    }
  )

  return(eval(call))
}


S7::method(pdf, negative_binomial) <- function(distribution, x, log = FALSE) {
  stats::dnbinom(x=x, size=distribution@n, prob=distribution@p, log=log)
}

nb <- negative_binomial(n=10, p=0.5)


foo <- pars(
  list(
    par(key="n",  support = real(0),    value = expression(size)),
    par(key="p",  support = real(0, 1), value = expression(size / (size + mu)),
    par(key="mu", support = real(0),    value = expression(n / p + n))
  ),
  prob = list(
    size = expression(n),
    prob = expression(p),
    mu = expression(n / p + n)
  ),
  mu = list(
    size = expression(n),
    mu   = expression(mu)
  )
))


foo <- pars(
  list(
    par(key="mu",     support = real()),
    par(key="sigma",  support = real(0), value = expression(sd)    ),
    par(key="sigma2", support = real(0), value = expression(sd^2)  ),
    par(key="tau",    support = real(0), value = expression(1/sd^2)),
    par(key="kappa",  support = real(0), value = expression(1/sd)  )
  ),
  sigma = list(
    mean = expression(mu),
    sd   = expression(sd)
  ),
  sigma2 = list(
    mean = expression(mu),
    sd   = expression(sqrt(sigma2))
  ),
  tau = list(
    mean = expression(mu),
    sd   = expression(sqrt(1/tau))
  ),
  kappa = list(
    mean = expression(mu),
    sd   = expression(1/kappa)
  )
)
