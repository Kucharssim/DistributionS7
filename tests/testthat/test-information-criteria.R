testthat::test_that("information criteria work", {
  dist <- Normal(mu = 0, sigma = fixed(1))
  x <- rng(dist, 10)
  x <- c(x, NA)

  dist <- fit(dist, data=x)
  results <- information_criteria(dist, data=x)

  n_par <- 1
  n_obs <- 10
  log_lik <- sum(dnorm(x, mean=mean(x, na.rm=TRUE), log=TRUE), na.rm=TRUE)
  testthat::expect_equal(results[["n_par"]], n_par)
  testthat::expect_equal(results[["n_obs"]], n_obs)
  testthat::expect_equal(results[["log_lik"]], log_lik)
  testthat::expect_equal(results[["aic"]], -2*log_lik + n_par*2)
  testthat::expect_equal(results[["bic"]], -2*log_lik + n_par*log(n_obs))

})

testthat::test_that("weights work", {
  # cf. Table 1 in Wagenmakers & Farrell (2004). AIC model selection using Akaike weights. Psych Bull & Review
  testthat::expect_equal(
    weights_ic(c(204, 202, 206, 206, 214)),
    c(0.224176108413539, 0.609373841875187, 0.0824697814871613, 0.0824697814871613, 
0.00151048673695166)
  )
})