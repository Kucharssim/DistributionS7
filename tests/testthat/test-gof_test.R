test_that("gof tests work", {
  n <- Normal(0, 1)
  x <- rng(n, 1000)

  results <- gof_test(n, x, estimated=FALSE)

  ks <- ks.test(x, pnorm, mean=0, sd=1)
  testthat::expect_equal(results[1, "statistic"], ks[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[1, "p_value"],   ks[["p.value"]],   ignore_attr=TRUE)

  cvm <- goftest::cvm.test(x, pnorm, mean=0, sd=1)
  testthat::expect_equal(results[2, "statistic"], cvm[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[2, "p_value"],   cvm[["p.value"]],   ignore_attr=TRUE)

  ad <- goftest::ad.test(x, pnorm, mean=0, sd=1)
  testthat::expect_equal(results[3, "statistic"], ad[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[3, "p_value"],   ad[["p.value"]],   ignore_attr=TRUE)

  n <- fit_distribution(n, BiasCorrected(), x)
  results <- gof_test(n, x, estimated=TRUE)

  li <- nortest::lillie.test(x)
  testthat::expect_equal(results[1, "statistic"], li[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[1, "p_value"],   li[["p.value"]],   ignore_attr=TRUE)

  cvm <- nortest::cvm.test(x)
  testthat::expect_equal(results[2, "statistic"], cvm[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[2, "p_value"],   cvm[["p.value"]],   ignore_attr=TRUE)

  ad <- nortest::ad.test(x)
  testthat::expect_equal(results[3, "statistic"], ad[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[3, "p_value"],   ad[["p.value"]],   ignore_attr=TRUE)

  sw <- stats::shapiro.test(x)
  testthat::expect_equal(results[4, "statistic"], sw[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[4, "p_value"],   sw[["p.value"]],   ignore_attr=TRUE)

  sf <- nortest::sf.test(x)
  testthat::expect_equal(results[5, "statistic"], sf[["statistic"]], ignore_attr=TRUE)
  testthat::expect_equal(results[5, "p_value"],   sf[["p.value"]],   ignore_attr=TRUE)
})
