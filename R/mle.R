mle <- S7::new_generic("mle", "distribution")

S7::method(mle, distribution) <- function(distribution, x, ...) {
  rlang::inform(message = "Analytic MLE is not available/implemented, using numerical optimization...")
  mle_optim(distribution, x, ...)
}

mle_optim <- S7::new_generic("mle_optim", "distribution")


S7::method(mle_optim, distribution) <- function(distribution, x, constrained=FALSE, method="L-BFGS-B", ...) {
  x <- na.omit(x)

  if (constrained) {
    start <- parameter_values(distribution, which="free")

    objective <- function(par, d, x, ...) {
      parameter_values(d) <- par
      likelihood(d, x, log=TRUE, factor=-2)
    }

    # determine limits of optimization
    support <- parameter_properties(distribution, "support")
    lower <- lapply(support, slot, "min")
    upper <- lapply(support, slot, "max")

    result <- try(
      optim(par=start, fn=objective, d=distribution, x=x, hessian=TRUE, lower=lower, upper=upper, method=method, ...),
      silent=TRUE
    )
  } else {
    start <- parameter_uvalues(distribution, which="free")

    objective <- function(par, d, x, ...) {
      parameter_uvalues(d) <- par
      likelihood(d, x, log=TRUE, factor=-2)
    }

    result <- try(
      optim(par=start, fn=objective, d=distribution, x=x, hessian=TRUE, method=method, ...),
      silent=TRUE
    )
  }

  if (inherits(result, "try-error")) rlang::abort("Optimization failed")
  if (result[["convergence"]] != 0) rlang::warn(
    sprintf("Convergence code %i", result[["convergence"]])
  )

  if (constrained) {
    parameter_values(distribution) <- result[["par"]]
  } else {
    parameter_uvalues(distribution) <- result[["par"]]
  }

  estimates <- normal_theory_uncertainty(
    distribution,
    estimates=result[["par"]],
    hessian=result[["hessian"]],
    constrained=constrained,
    ...
  )

  return(list(distribution=distribution, estimates=estimates, optim=result))
}


normal_theory_uncertainty <- S7::new_generic("parameter_uvalues", "distribution")

S7::method(normal_theory_uncertainty, distribution) <- function(
    distribution, estimates, hessian, constrained=FALSE, ciLevel=0.95, symmetric=TRUE
  ) {
  estimates <- unlist(estimates)
  alpha <- 1-ciLevel
  npar <- length(estimates)
  keys <- names(estimates)

  vcov <- try(solve(hessian))
  if (inherits(vcov, "try-error")) rlang::abort("Variance covariance matrix of the parameters could not be computed")

  # compute normal CIs assuming parameters are already on constrained space
  if (constrained) {
    se <- sqrt(diag(vcov))
    lower <- qnorm(  alpha/2, estimates, se)
    upper <- qnorm(1-alpha/2, estimates, se)
    return(data.frame(key = keys, estimate = estimates, se = se, lower=lower, upper=upper))
  }

  # compute Jacobian of the inverse transform
  dxdy <- setNames(vector(length=npar), keys)
  for (key in keys) {
    par <- distribution@parameters[[key]]
    dxdy[key] <- derivative(par)
  }
  jac <- if (npar == 1) matrix(dxdy) else diag(dxdy)


  if (!symmetric) {
    # compute limits on the unconstrained space
    se <- sqrt(diag(vcov))
    lower <- qnorm(  alpha/2, estimates, se)
    upper <- qnorm(1-alpha/2, estimates, se)

    # convert constrained space
    for (key in keys) {
      par <- distribution@parameters[[key]]
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
    par <- distribution@parameters[[key]]
    estimates[[key]] <- constrain(par)(estimates[[key]])
  }
  vcov <- jac %*% vcov %*% jac
  se <- sqrt(diag(vcov))
  lower <- qnorm(  alpha/2, estimates, se)
  upper <- qnorm(1-alpha/2, estimates, se)

  return(data.frame(key = keys, estimate = estimates, se = se, lower=lower, upper=upper))
}


