
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
#>     binomial, qf
#> The following object is masked from 'package:grDevices':
#> 
#>     pdf
#> The following objects are masked from 'package:base':
#> 
#>     beta, gamma

# create a distribution object
n <- normal(0, 1)

# sample from a distribution (and distort to make the distribution not fitting well)
x <- rng(n, 100) * 0.3 + 1

# goodness-of-fit tests
gof_test(n, x, estimated=FALSE)
#>              test  statistic      p_value
#> ks_test   ks_test  0.6595587 3.280012e-38
#> cvm_test cvm_test 15.1462604 0.000000e+00
#> ad_test   ad_test 71.7494937 6.000000e-06

# fit to data using maximum likelihood
n <- fit_distribution(n, Mle(), x)

# get uncertainty around parameter estimates using normal theory intervals
parameter_inference(n, NormalTheory(), x)
#>         key   label  estimate         se     lower    upper
#> mu       mu    \\mu 0.9603700 0.02866496 0.9041877 1.016552
#> sigma sigma \\sigma 0.2866496 0.02026917 0.2495527 0.329261

# fit indices of the fitted distribution
gof_test(n, x, estimated=TRUE)
#>                                      test  statistic   p_value
#> lillie_test                   lillie_test 0.05901829 0.5318607
#> cvm_test                         cvm_test 0.05764614 0.4028020
#> ad_test                           ad_test 0.43365370 0.2964786
#> shapiro_wilk_test       shapiro_wilk_test 0.98147289 0.1728587
#> shapiro_francia_test shapiro_francia_test 0.98612874 0.3226297
information_criteria(n, x)
#>   n_par n_obs   log_lik      aic      bic
#> 1     2   100 -16.94437 37.88875 43.09909

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
