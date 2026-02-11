
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DistributionS7

<!-- badges: start -->

[![R-CMD-check](https://github.com/Kucharssim/DistributionS7/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Kucharssim/DistributionS7/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of DistributionS7 is to provide convenient functionality to
work with probability distributions.

## Installation

You can install the development version of DistributionS7 from
[GitHub](https://github.com/) with:

``` r
# renv::install("Kucharssim/DistributionS7")
```

# Example

``` r
library(DistributionS7)
#> 
#> Attaching package: 'DistributionS7'
#> The following objects are masked from 'package:stats':
#> 
#>     Gamma, qf
#> The following object is masked from 'package:grDevices':
#> 
#>     pdf

# create a distribution object
n <- Normal(0, 1)

# sample from a distribution (and distort to make the distribution not fitting well)
x <- rng(n, 100) * 0.3 + 1

# goodness-of-fit tests
gof_test(n, x, estimated=FALSE)
#>              test  statistic      p_value
#> ks_test   ks_test  0.6398649 5.478287e-36
#> cvm_test cvm_test 15.2805061 0.000000e+00
#> ad_test   ad_test 72.9246153 6.000000e-06

# fit to data using maximum likelihood
n <- fit_distribution(n, Mle(), x)

# get uncertainty around parameter estimates using normal theory intervals
parameter_inference(n, NormalTheory(), x)
#>         key   label  estimate         se     lower    upper
#> mu       mu    \\mu 0.9705081 0.02906247 0.9135468 1.027470
#> sigma sigma \\sigma 0.2906247 0.02055025 0.2530134 0.333827

# fit indices of the fitted distribution
gof_test(n, x, estimated=TRUE)
#>                                      test  statistic   p_value
#> lillie_test                   lillie_test 0.04763650 0.8344792
#> cvm_test                         cvm_test 0.03141533 0.8253144
#> ad_test                           ad_test 0.24730958 0.7472024
#> shapiro_wilk_test       shapiro_wilk_test 0.98870400 0.5614682
#> shapiro_francia_test shapiro_francia_test 0.99142378 0.6879283
information_criteria(n, x)
#>   n_par n_obs   log_lik      aic      bic
#> 1     2   100 -18.32159 40.64317 45.85351

# compare data to distribution
plot_empirical(n, x, ci=TRUE)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the DistributionS7 package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: `expand_scale()` was deprecated in ggplot2 3.3.0.
#> ℹ Please use `expansion()` instead.
#> ℹ The deprecated feature was likely used in the DistributionS7 package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
