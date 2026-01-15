Estimates <- S7::new_class(
  "Estimates",
  properties = list(
    values = S7::class_list | S7::class_numeric,
    constrained = S7::class_logical
  ),
  constructor = function(values, constrained=TRUE) {
    S7::new_object(
      S7::S7_object(),
      values = values,
      constrained = constrained
    )
  }
)

EstimatesHessian <- S7::new_class(
  "EstimatesHessian",
  parent = Estimates,
  properties = list(
    hessian = S7::class_numeric
  ),
  constructor = function(values, hessian, constrained) {
    S7::new_object(
      S7::S7_object(),
      values  = values,
      hessian = hessian,
      constrained = constrained
    )
  }
)

S7::method(coef, Estimates) <- function(object, ..., distribution, constrained) {
  if (object@constrained) {
    parameter_values(distribution) <- object@values
  } else {
    parameter_uvalues(distribution) <- object@values
  }
  values <- if(constrained) parameter_values(distribution, which="free") else parameter_uvalues(distribution, which="free")
  return(unlist(values))
}

S7::method(vcov, Estimates) <- function(object, ..., distribution, data, constrained=FALSE) {
  if (object@constrained) {
    parameter_values(distribution) <- object@values
  } else {
    parameter_uvalues(distribution) <- object@values
  }

  if (constrained) {
    start <- parameter_values(distribution, which="free")
    objective <- function(par, d, data, ...) {
      parameter_values(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }
  } else {
    start <- parameter_uvalues(distribution, which="free")
    objective <- function(par, d, data, ...) {
      parameter_uvalues(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }
  }

  hessian <- try(
    optimHess(par=start, fn=objective, d=distribution, data=data, ...),
    silent=TRUE
  )
  if (inherits(hessian, "try-error")) rlang::abort("Could not determine Hessian")

  values <- if(constrained) parameter_values(distribution, which="free") else parameter_uvalues(distribution, which="free")
  values <- unlist(values)
  estimates <- EstimatesHessian(
    values = values,
    hessian = hessian,
    constrained = constrained
  )

  return(vcov(estimates))
}

S7::method(vcov, EstimatesHessian) <- function(object,...) {
  vcov <- try(solve(object@hessian,...))
  if (inherits(vcov, "try-error")) rlang::abort("Variance covariance matrix of the parameters could not be computed")
  return(vcov)
}

point_estimates <- S7::new_generic("point_estimates", "distribution", function(distribution, data, ...) {
  data <- na.omit(data)
  S7::S7_dispatch()
})

S7::method(point_estimates, Distribution) <- function(distribution, data, constrained=FALSE, method=if(constrained) "L-BFGS-B" else "BFGS", ...) {
  rlang::inform(message = "Analytic parameter estimates are not available/implemented, using numerical optimization...")

  if (constrained) {
    start <- parameter_values(distribution, which="free")

    objective <- function(par, d, data, ...) {
      parameter_values(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }

    # determine limits of optimization
    support <- parameter_properties(distribution, "support")
    lower <- lapply(support, slot, "min")
    upper <- lapply(support, slot, "max")

    result <- try(
      optim(par=start, fn=objective, d=distribution, data=data, hessian=TRUE, lower=lower, upper=upper, method=method, ...),
      silent=TRUE
    )
  } else {
    start <- parameter_uvalues(distribution, which="free")

    objective <- function(par, d, data, ...) {
      parameter_uvalues(d) <- par
      likelihood(d, data, log=TRUE, factor=-2)
    }

    result <- try(
      optim(par=start, fn=objective, d=distribution, data=data, hessian=TRUE, method=method, ...),
      silent=TRUE
    )
  }

  if (inherits(result, "try-error")) rlang::abort("Optimization failed")
  if (result[["convergence"]] != 0) rlang::warn(
    sprintf("Convergence code %i", result[["convergence"]])
  )

  estimates <- EstimatesHessian(
    values = unlist(result[["par"]]),
    hessian = result[["hessian"]],
    constrained = constrained
  )

  return(estimates)
}

fit_distribution <- S7::new_generic("fit_distribution", "distribution")

S7::method(fit_distribution, Distribution) <- function(distribution, data, ...) {
  parameters <- point_estimates(distribution, data, ...)
  if(parameters@constrained) {
    parameter_values(distribution) <- parameters@values
  } else {
    parameter_uvalues(distribution) <- parameters@values
  }
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

parameter_inference <- S7::new_generic(
  "parameter_inference", "distribution",
  function(distribution, data, ..., ci_level=0.95) {
    data <- na.omit(data)
    assertthat::assert_that(
      rlang::is_scalar_double(ci_level),
      ci_level > 0,
      ci_level < 1
    )
    S7::S7_dispatch()
  })

S7::method(parameter_inference, Distribution) <- function(
    distribution, data, ..., ci_level=0.95
    ) {
  rlang::inform("Normal theory SE and CIs")
  result <- normal_theory_inference(distribution, data, ci_level=ci_level, ...)
  return(result)
}

normal_theory_inference <- S7::new_generic(
  "normal_theory_inference", "distribution",
  function(distribution, data, ..., ci_level=0.95, symmetric=FALSE, constrained=FALSE){
    data <- na.omit(data)
    assertthat::assert_that(
      rlang::is_scalar_double(ci_level),
      ci_level > 0,
      ci_level < 1
    )
    S7::S7_dispatch()
  })

S7::method(normal_theory_inference, Distribution) <- function(distribution, data, ..., ci_level=0.95, symmetric=FALSE, constrained=FALSE) {
  estimates <- point_estimates(distribution, data=data, constrained=constrained, ...)
  vcov <- vcov(estimates, distribution=distribution, data=data, constrained=constrained, ...)
  estimates <- coef(estimates, distribution=distribution, constrained=constrained)

  alpha <- 1-ci_level
  npar <- length(estimates)
  keys <- names(estimates)

  # compute normal CIs assuming parameters are already on constrained space
  if (constrained) {
    if (!symmetric) rlang::warn("CIs on the constrained space are always symmetric, ignoring `symmetric=FALSE` argument.")
    se <- sqrt(diag(vcov))
    lower <- qnorm(  alpha/2, estimates, se)
    upper <- qnorm(1-alpha/2, estimates, se)
    return(data.frame(key=keys, estimate=estimates, se=se, lower=lower, upper=upper))
  }

  # compute Jacobian of the inverse transform
  dxdy <- setNames(vector(length=npar), keys)
  for (key in keys) {
    par <- S7::prop(distribution, key)
    dxdy[key] <- derivative(par)
  }
  jac <- if (npar == 1) matrix(dxdy) else diag(dxdy)

  if (!symmetric) {
    # compute limits on the unconstrained space
    se <- sqrt(diag(vcov))
    lower <- setNames(qnorm(  alpha/2, estimates, se), keys)
    upper <- setNames(qnorm(1-alpha/2, estimates, se), keys)

    # convert constrained space
    for (key in keys) {
      par <- S7::prop(distribution, key)
      fn <- constrain(par)
      estimates[[key]] <- fn(estimates[[key]])
      lower[[key]] <- fn(lower[[key]])
      upper[[key]] <- fn(upper[[key]])
    }

    # compute SE on the constrained space using delta method
    vcov <- jac %*% vcov %*% jac
    se <- sqrt(diag(vcov))

    return(data.frame(key = keys, estimate = estimates, se = se, lower=lower, upper=upper))
  }

  # compute CIs on the constrained space using delta method
  for (key in keys) {
    par <- S7::prop(distribution, key)
    estimates[[key]] <- constrain(par)(estimates[[key]])
  }
  vcov <- jac %*% vcov %*% jac
  se <- sqrt(diag(vcov))
  lower <- qnorm(  alpha/2, estimates, se)
  upper <- qnorm(1-alpha/2, estimates, se)

  return(data.frame(key=keys, estimate=estimates, se=se, lower=lower, upper=upper))
}

#
# parameter_inference_bootstrap_nonparametric <- S7::new_generic(
#   "parameter_inference_bootstrap_nonparametric", "distribution",
#   function(distribution, data, ..., ci_level=0.95, ci_type="bca", bootstrap_samples=1000){
#     S7::S7_dispatch()
#   })
#
# S7::method(parameter_inference_bootstrap_nonparametric, Distribution) <- function(distribution, data, ..., ci_level=0.95, ci_type=c("norm","basic", "stud", "perc", "bca"), bootstrap_samples=1000) {
#   distribution <- fit_distribution(distribution, data=data, ...)
#   estimates <- point_estimates(distribution, data=new_data, ...)
#
#   samples <- boot::boot(
#     data = data,
#     statistic = function(data, idx) {
#       new_data <- data[idx]
#       estimates <- point_estimates(distribution, data=new_data, ...)
#       coef(estimates)
#     },
#     type = rlang::arg_match(ci_type),
#     R=bootstrap_samples
#   )
#
#   hinv <- function(x) {
#     fn <- constrain(distribution, which="free")
#     out <- x
#     for (i in seq_along(x)) {
#       out[i] <- fn[[i]](x)
#     }
#     return(out)
#   }
#
#   if (estimates@constrained) {
#     h <- function(x) {
#       fn <- unconstrain(distribution, which="free")
#       out <- x
#       for (i in seq_along(x)) {
#         out[i] <- fn[[i]](x)
#       }
#       return(out)
#     }
#     hdot <- function(x) {
#       fn <- derivative(distribution, which="free")
#       out <- x
#       for (i in seq_along(x)) {
#         out[i] <- fn[[i]](x)
#       }
#       return(out)
#     }
#     ci <- boot::boot.ci(
#       samples,
#       type=ci_type,
#       h = h,
#       hdot = hdot,
#       hinv = hinv
#       )
#   } else {
#     ci <- boot::boot.ci(
#       samples,
#       type = ci_type,
#       hinv = hinv
#     )
#   }
#
#   return(ci)
# }
