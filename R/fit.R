# point estimation ----
Estimator <- S7::new_class(
  name = "Estimator",
  abstract = TRUE
)

Mle <- S7::new_class(
  name = "Mle",
  parent = Estimator,
  properties = list(
    optim = S7::class_logical,
    constrained = S7::class_logical,
    method = S7::class_character,
    control = S7::class_list
  ),
  constructor = function(optim=FALSE, constrained=FALSE, method=if(constrained) "L-BFGS-B" else "BFGS", control=list()) {
    S7::new_object(
      S7::S7_object(),
      optim=optim,
      constrained=constrained,
      method=method,
      control=control
    )
  }
)

BiasCorrected <- S7::new_class(
  name = "BiasCorrected",
  parent = Estimator
)


## point estimation generics ----
parameter_estimates <- S7::new_generic("parameter_estimates", c("distribution","estimator"), function(distribution, estimator, data) {
  data <- na.omit(data)
  # override custom methods if optim requested
  if (S7::S7_inherits(estimator, Mle) && estimator@optim)
    distribution <- S7::super(distribution, Distribution)

  S7::S7_dispatch()
})


S7::method(parameter_estimates, list(Distribution, Mle)) <- function(distribution, estimator, data) {
  if (!estimator@optim)
    rlang::inform(message = "Using numerical optimization to find parameter estimates...")

  if (estimator@constrained) {
    start <- parameter_values(distribution, which="free")

    objective <- function(par, d, data) {
      parameter_values(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }

    # determine limits of optimization
    support <- parameter_properties(distribution, "support")
    lower <- lapply(support, S7::prop, "min")
    upper <- lapply(support, S7::prop, "max")

    result <- try(
      optim(
        par=start, fn=objective, d=distribution, data=data,
        method=estimator@method, lower=lower, upper=upper,
        control=estimator@control, hessian=FALSE),
      silent=TRUE
    )
  } else {
    start <- parameter_uvalues(distribution, which="free")

    objective <- function(par, d, data) {
      parameter_uvalues(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }

    result <- try(
      optim(
        par=start, fn=objective, d=distribution, data=data,
        method=estimator@method,
        control=estimator@control, hessian=FALSE),
      silent=TRUE
    )
  }

  if (inherits(result, "try-error")) rlang::abort("Optimization failed")
  if (result[["convergence"]] != 0) rlang::warn(
    sprintf("Convergence code %i", result[["convergence"]])
  )

  if(estimator@constrained) {
    parameter_values(distribution) <- result[["par"]]
  } else {
    parameter_uvalues(distribution) <- result[["par"]]
  }

  estimates <- parameter_values(distribution)

  return(unlist(estimates))
}

# inference ----

InferenceMethod <- S7::new_class(
  name = "InferenceMethod",
  properties = list(
    estimator = S7::new_property(Estimator, default = Mle()),
    ci_level = S7::new_property(
      class = S7::class_double,
      validator = function(value) { assertthat::assert_that(value > 0, value < 1); return(NULL) },
      default = 0.95
    ),
    alpha = S7::new_property(getter = function(self) 1-self@ci_level),
    lower = S7::new_property(getter = function(self) self@alpha/2),
    upper = S7::new_property(getter = function(self) 1-self@alpha/2)
  )
)

DefaultMethod <- S7::new_class(
  name = "DefaultMethod",
  parent = InferenceMethod,
  properties = list(
    normal_theory = S7::new_property(S7::class_logical, default=FALSE),
    constrained = S7::new_property(S7::class_logical, default=FALSE),
    control = S7::new_property(S7::class_list, default=list())
  )
)

Bootstrap <- S7::new_class(
  name = "Bootstrap",
  parent = InferenceMethod
)

## inference generics ----
parameter_inference <- S7::new_generic("parameter_inference", c("distribution", "inference_method"), function(distribution, inference_method, data){
  data <- na.omit(data)
  # override custom method if normal theory requested
  if (S7::S7_inherits(inference_method, DefaultMethod) && inference_method@normal_theory) {
    # only valid for Mle estimators:
    S7::check_is_S7(inference_method@estimator, Mle)
    distribution <- S7::super(distribution, Distribution)
  }
  S7::S7_dispatch()
})

S7::method(parameter_inference, list(Distribution, DefaultMethod)) <- function(distribution, inference_method, data) {
  if (!inference_method@normal_theory)
    rlang::inform(message = "Computing normal theory SE and CIs...")

  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  parameter_values(distribution) <- estimates
  vcov <- vcov(distribution, data, inference_method@constrained, inference_method@control)

  npar <- length(estimates)
  keys <- names(estimates)

  if (inference_method@constrained) {
    se <- sqrt(diag(vcov))
    lower <- qnorm(inference_method@lower, estimates, se)
    upper <- qnorm(inference_method@upper, estimates, se)
    return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
  }

  # compute Jacobian of the inverse transform
  dxdy <- setNames(vector(length=npar), keys)
  for (key in keys) {
    par <- S7::prop(distribution, key)
    dxdy[key] <- derivative(par)(par@uvalue)
  }
  jac <- if (npar == 1) matrix(dxdy) else diag(dxdy)

  # compute limits on the unconstrained space
  uestimates <- unlist(parameter_uvalues(distribution, which="free"))
  se <- sqrt(diag(vcov))
  lower <- setNames(qnorm(inference_method@lower, uestimates, se), keys)
  upper <- setNames(qnorm(inference_method@upper, uestimates, se), keys)

  # convert to constrained space
  for (key in keys) {
    par <- S7::prop(distribution, key)
    fn <- constrain(par)
    lower[[key]] <- fn(lower[[key]])
    upper[[key]] <- fn(upper[[key]])
  }

  # compute SE on the constrained space using delta method
  vcov <- jac %*% vcov %*% jac
  se <- sqrt(diag(vcov))

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}

# helper functions ----
fit_distribution <- S7::new_generic("fit_distribution", c("distribution", "estimator"), function(distribution, estimator, data) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(fit_distribution, list(Distribution, Estimator)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, estimator, data)
  parameter_values(distribution) <- estimates

  # make a new dist object - this ensures all properties are valid
  dist_class <- S7::S7_class(distribution)
  pars <- parameter_values(distribution)
  is_fixed <- parameter_properties(distribution, property="fixed")
  for (name in names(pars)) {
    if (is_fixed[[name]]) pars[[name]] <- fixed(pars[[name]])
  }
  distribution <- do.call(dist_class, pars)
  return(distribution)
}

vcov <- function(distribution, data, constrained, control) {
  if (constrained) {
    start <- parameter_values(distribution, which="free")

    objective <- function(par, d, data) {
      parameter_values(d) <- par
      likelihood(d, data, log=TRUE, factor=-1)
    }
  } else {
    start <- parameter_uvalues(distribution, which="free")

    objective <- function(par, d, data) {
      parameter_uvalues(d) <- par
      likelihood(d, data, log=TRUE, factor=-1)
    }
  }

  hessian <- try(
    optimHess(par=start, fn=objective, d=distribution, data=data, control=control),
    silent=TRUE
  )

  if (inherits(hessian, "try-error")) rlang::abort("Approximating the hessian failed.")

  vcov <- try(solve(hessian))

  if (inherits(vcov, "try-error")) rlang::abort("Could not compute the variance-covariance matrix.")

  return(vcov)
}

estimates_table <- function(distribution, estimates, se, lower, upper) {
  output <- tibble::tibble(
    key = unlist(parameter_properties(distribution, property="key", which="free")),
    label = unlist(parameter_properties(distribution, property="label", which="free")),
    estimate = unlist(estimates),
  )
  if (!missing(se))
    output[["se"]] <- unlist(se)
  output[["lower"]] <- unlist(lower)
  output[["upper"]] <- unlist(upper)
  return(output)
}
