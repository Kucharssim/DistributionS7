# point estimation ----
Estimator <- S7::new_class(
  name = "Estimator",
  abstract = TRUE,
  properties = list(
    silent = S7::new_property(class = S7::class_logical, default=FALSE)
  )
)

Mom <- S7::new_class(
  name = "Mom",
  parent = Estimator
)

Mle <- S7::new_class(
  name = "Mle",
  parent = Estimator,
  properties = list(
    optim = S7::class_logical,
    constrained = S7::class_logical,
    method = S7::class_character,
    start = Estimator | S7::class_function | S7::class_list,
    control = S7::class_list
  ),
  constructor = function(optim=FALSE, constrained=FALSE, method=if(constrained) "L-BFGS-B" else "BFGS", start=parameter_start, control=list(), silent=FALSE) {
    S7::new_object(
      S7::S7_object(),
      optim=optim,
      constrained=constrained,
      method=method,
      start=start,
      control=control,
      silent=silent
    )
  }
)

BiasCorrected <- S7::new_class(
  name = "BiasCorrected",
  parent = Estimator
)


parameter_start <- S7::new_generic("parameter_start", "distribution", function(distribution, data) {
  S7::S7_dispatch()
})

S7::method(parameter_start, Distribution) <- function(distribution, data) {
  estimates <- try(parameter_estimates(distribution, Mom(), data), silent=TRUE)
  if (!inherits(estimates, "try-error")) parameter_values(distribution) <- estimates
  return(distribution)
}

## point estimation generics ----
parameter_estimates <- S7::new_generic("parameter_estimates", c("distribution","estimator"), function(distribution, estimator, data) {
  if (distribution@support@numeric)
    assertthat::assert_that(all(inside(distribution, data)), msg = "data are outside of the parameter support")
  if (all(unlist(parameter_properties(distribution, property="fixed")))) {
    if (estimator@silent) rlang::inform("All parameters are fixed, nothing to estimate")
    return(list())
  }

  data <- stats::na.omit(data)
  # override custom methods if optim requested
  if (S7::S7_inherits(estimator, Mle) && estimator@optim)
    distribution <- S7::super(distribution, Distribution)

  S7::S7_dispatch()
})


S7::method(parameter_estimates, list(Distribution, Mle)) <- function(distribution, estimator, data) {
  if (!estimator@optim && !estimator@silent)
    rlang::inform(message = "Using numerical optimization to find parameter estimates...")

  # try to set intelligent starting values
  if (S7::S7_inherits(estimator@start, Estimator)) {
    estimates <- try(parameter_estimates(distribution, estimator@start, data), silent = TRUE)
    if (!inherits(estimates, "try-error")) parameter_values(distribution) <- estimates
  } else if (is.function(estimator@start)) {
    distribution <- estimator@start(distribution, data)
  } else if (is.list(estimator@start)) {
    parameter_values(distribution) <- estimator@start
  }

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
      stats::optim(
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
      stats::optim(
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

  estimates <- parameter_values(distribution, which="free")

  return(estimates)
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
    args = S7::new_property(S7::class_list, default=list())
  )
)

NormalTheory <- S7::new_class(
  name = "NormalTheory",
  parent = InferenceMethod,
  properties = list(
    estimator = S7::new_property(Mle, default = Mle()),
    constrained = S7::new_property(S7::class_logical, default=FALSE),
    control = S7::new_property(S7::class_list, default=list())
  )
)

ProfileLikelihood <- S7::new_class(
  name = "Profile",
  parent = InferenceMethod
)

Bootstrap <- S7::new_class(
  name = "Bootstrap",
  parent = InferenceMethod,
  properties = list(
    samples = S7::new_property(
      class = S7::class_integer,
      validator = function(value) { assertthat::assert_that(value >= 0); return(NULL) },
      default = 1000L
    ),
    callback = S7::new_property(
      class = S7::class_function,
      default = function() {}
    )
  )
)

## inference generics ----
parameter_inference <- S7::new_generic("parameter_inference", c("distribution", "inference_method"), function(distribution, inference_method, data){
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(parameter_inference, list(Distribution, DefaultMethod)) <- function(distribution, inference_method, data) {
  rlang::inform(message = "Computing normal theory SE and CIs...")
  inference_method <- do.call(NormalTheory, inference_method@args)
  parameter_inference(distribution, inference_method, data)
}

S7::method(parameter_inference, list(Distribution, NormalTheory)) <- function(distribution, inference_method, data) {
  estimates <- parameter_estimates(distribution, inference_method@estimator, data)
  # in case we want to dynamically change support of pars
  distribution <- parameter_start(distribution, data)
  parameter_values(distribution) <- estimates
  vcov <- vcov(distribution, data, inference_method@constrained, inference_method@control)

  npar <- length(estimates)
  keys <- names(estimates)

  if (inference_method@constrained) {
    se <- sqrt(diag(vcov))
    lower <- qnorm(inference_method@lower, unlist(estimates), se)
    upper <- qnorm(inference_method@upper, unlist(estimates), se)
    return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
  }

  # compute limits on the unconstrained space
  uestimates <- unlist(parameter_uvalues(distribution, which="free"))
  se <- sqrt(diag(vcov))
  lower <- setNames(qnorm(inference_method@lower, uestimates, se), keys)
  upper <- setNames(qnorm(inference_method@upper, uestimates, se), keys)

  # convert to constrained space
  for (key in keys) {
    par <- S7::prop(distribution, key)
    lower[[key]] <- constrain(par, lower[[key]])
    upper[[key]] <- constrain(par, upper[[key]])
    # reverse limits when they are fliped (due to max-exp(uvalue) transform)
    if (lower[[key]] > upper[[key]]) {
      tmp <- upper[[key]]
      upper[[key]] <- lower[[key]]
      lower[[key]] <- tmp
    }
  }

  # compute Jacobian of the inverse transform
  derivatives <- unlist(parameter_properties(distribution, property="derivative", which="free"))
  jac <- if (npar == 1) matrix(derivatives) else diag(derivatives)
  # compute SE on the constrained space using delta method
  vcov <- jac %*% vcov %*% jac
  se <- sqrt(diag(vcov))

  return(estimates_table(distribution, estimates=estimates, se=se, lower=lower, upper=upper))
}

# helper functions ----
fit_distribution <- S7::new_generic("fit_distribution", c("distribution", "estimator"), function(distribution, estimator, data) {
  data <- stats::na.omit(data)
  S7::S7_dispatch()
})

S7::method(fit_distribution, list(Distribution, Estimator)) <- function(distribution, estimator, data) {
  estimates <- parameter_estimates(distribution, estimator, data)
  parameter_values(distribution) <- estimates


  # make a new dist object - this ensures all properties are valid
  dist_class <- S7::S7_class(distribution)
  parameters <- recreate_parameters(distribution)
  distribution <- do.call(dist_class, parameters)
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
    stats::optimHess(par=start, fn=objective, d=distribution, data=data, control=control),
    silent=TRUE
  )

  if (inherits(hessian, "try-error")) rlang::abort("Approximating the hessian failed.")

  vcov <- try(solve(hessian))

  if (inherits(vcov, "try-error")) rlang::abort("Could not compute the variance-covariance matrix.")

  return(vcov)
}

estimates_table <- function(distribution, estimates, se, lower, upper) {
  output <- data.frame(
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
