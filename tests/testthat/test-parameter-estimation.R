testthat::test_that("parameter_estimates works", {
  dist <- Normal(mu=0, sigma2=2)
  x <- withr::with_seed(93235, rng(dist, 100))
  x_bar <- mean(x)

  # analytic
  pars <- parameter_estimates(dist, data=x, estimator=BiasCorrected())
  testthat::expect_equal(
    pars,
    list(mu = x_bar, sigma2 = var(x))
  )

  # analytic mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle())
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2))
  )

  # optimizing mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle(optim=TRUE, start=list(mu=-1)))
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2)),
    tolerance=1e-3
  )

  # constrained optimizing mle
  pars <- parameter_estimates(dist, data=x, estimator=Mle(optim=TRUE, constrained=TRUE))
  testthat::expect_equal(
    pars, 
    list(mu = x_bar, sigma2 = mean((x-x_bar)^2)),
    tolerance=1e-3
  )

})

testthat::test_that("parameter_inference works", {
  dist <- Wald(mu=1, lambda=2)
  x <- withr::with_seed(34698, rng(dist, 20))

  # DefaultMethod switches to NormalTheory
  testthat::expect_message(
    parameter_inference(dist, DefaultMethod(args=list(constrained=TRUE)), data=x),
    "Computing normal theory SE and CIs..."
  )
  
  # and passess args forward
  result_default <- parameter_inference(dist, DefaultMethod(args=list(constrained=TRUE)), data=x)
  result_normal <- parameter_inference(dist, NormalTheory(constrained=TRUE), data=x)

  testthat::expect_equal(result_default, result_normal)

  
  dist <- Normal(mu=0, sigma=1)
  
  # test analytic estimates + analytic ci
  result <- parameter_inference(dist, DefaultMethod(), data=x)
  testthat::expect_snapshot(result)

  # analytic estimates + normal theory cis
  result <- parameter_inference(dist, NormalTheory(), data=x)
  testthat::expect_snapshot(result)

  # analytic estimates + normal theory cis on constrained space
  result <- parameter_inference(dist, NormalTheory(constrained=TRUE), data=x)
  testthat::expect_snapshot(result)

  # optim estimates + normal theory cis
  result <- parameter_inference(dist, NormalTheory(estimator=Mle(optim=TRUE)), data=x)
  testthat::expect_snapshot(result)
})
testthat::test_that("DefaultMethod forwards estimator and ci_level to NormalTheory", {
  dist <- Wald(mu=1, lambda=2)
  x <- withr::with_seed(34698, rng(dist, 20))

  testthat::expect_equal(
    suppressMessages(parameter_inference(dist, DefaultMethod(ci_level=0.9, args=list(constrained=TRUE)), data=x)),
    parameter_inference(dist, NormalTheory(ci_level=0.9, constrained=TRUE), data=x)
  )
  testthat::expect_equal(
    suppressMessages(parameter_inference(dist, DefaultMethod(estimator=Mle(optim=TRUE, silent=TRUE)), data=x)),
    parameter_inference(dist, NormalTheory(estimator=Mle(optim=TRUE, silent=TRUE)), data=x)
  )
})

testthat::test_that("NormalTheory warns for non-MLE estimators", {
  dist <- Gamma(alpha=2, theta=1)
  x <- withr::with_seed(2025, rng(dist, 50))

  testthat::expect_warning(
    parameter_inference(dist, NormalTheory(estimator=Mme()), data=x),
    "maximum likelihood"
  )
})

testthat::test_that("inference method classes are consistent", {
  testthat::expect_error(NormalTheory(ci_level=1))
  testthat::expect_error(NormalTheory(ci_level=0))
  testthat::expect_equal(S7::S7_class(ProfileLikelihood())@name, "ProfileLikelihood")

  dist <- Uniform(min=0, max=1)
  x <- withr::with_seed(2025, rng(dist, 20))
  result <- parameter_inference(dist, DefaultMethod(), data=x)
  testthat::expect_named(result, c("key", "label", "estimate", "se", "lower", "upper"))
})

testthat::test_that("Bootstrap class is validated", {
  testthat::expect_error(Bootstrap(type="jackknife"))
  testthat::expect_error(Bootstrap(ci_method="bca"))
  testthat::expect_equal(Bootstrap()@type, "parametric")
  testthat::expect_equal(Bootstrap()@ci_method, "percentile")
  testthat::expect_equal(Bootstrap(ci_method="basic")@ci_method, "basic")
})

testthat::test_that("nonparametric bootstrap inference works", {
  dist <- Normal(mu=0, sigma=1)
  x <- withr::with_seed(2025, rnorm(50, 1, 2))
  method <- Bootstrap(samples=200L, type="nonparametric")

  result <- withr::with_seed(1, parameter_inference(dist, method, data=x))
  boot <- attr(result, "bootstrap")

  testthat::expect_equal(result$estimate, unlist(parameter_estimates(dist, Mle(), x)), ignore_attr=TRUE)
  testthat::expect_true(all(result$lower < result$estimate & result$estimate < result$upper))
  testthat::expect_equal(boot$valid, 200)
  testthat::expect_equal(dim(boot$estimates), c(200, 2))
  testthat::expect_equal(
    result$lower,
    apply(boot$estimates, 2, stats::quantile, probs=0.025, names=FALSE),
    ignore_attr=TRUE
  )
  testthat::expect_equal(result$se, apply(boot$estimates, 2, stats::sd), ignore_attr=TRUE)

  # reproducible
  testthat::expect_equal(withr::with_seed(1, parameter_inference(dist, method, data=x)), result)
})

testthat::test_that("basic bootstrap intervals respect the parameter support", {
  dist <- Exponential(lambda=1)
  x <- withr::with_seed(2025, rexp(15, 5))

  result <- withr::with_seed(1, parameter_inference(dist, Bootstrap(samples=200L, ci_method="basic"), data=x))
  testthat::expect_gt(result$lower, 0)
  testthat::expect_lt(result$lower, result$upper)
})

testthat::test_that("bootstrap calls the callback on every sample", {
  dist <- Exponential(lambda=1)
  x <- withr::with_seed(2025, rexp(15, 5))

  counter <- 0
  method <- Bootstrap(samples=25L, type="nonparametric", callback=function() counter <<- counter + 1)
  withr::with_seed(1, parameter_inference(dist, method, data=x))
  testthat::expect_equal(counter, 25)
})
