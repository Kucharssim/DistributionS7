
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
#> ks_test   ks_test  0.6446443 1.604742e-36
#> cvm_test cvm_test 14.7052995 0.000000e+00
#> ad_test   ad_test 69.6388496 6.000000e-06

# fit to data (maximum likelihood by default)
n <- fit(n, data=x)

# get uncertainty around parameter estimates using normal theory intervals
parameter_inference(n, NormalTheory(), x)
#>         key   label  estimate         se     lower     upper
#> mu       mu    \\mu 0.9491470 0.03048872 0.8893902 1.0089038
#> sigma sigma \\sigma 0.3048872 0.02155877 0.2654301 0.3502097

# fit indices of the fitted distribution
gof_test(n, x, estimated=TRUE)
#>                                      test  statistic   p_value
#> lillie_test                   lillie_test 0.05428551 0.6644937
#> cvm_test                         cvm_test 0.03436303 0.7790073
#> ad_test                           ad_test 0.29816661 0.5812252
#> shapiro_wilk_test       shapiro_wilk_test 0.98762523 0.4809980
#> shapiro_francia_test shapiro_francia_test 0.99008937 0.5808850
information_criteria(n, x)
#>   n_par n_obs   log_lik      aic      bic
#> 1     2   100 -23.11251 50.22503 55.43537

# compare data to distribution
plot_hist(n, x) + ggplot2::ggtitle("Histogram vs. Normal Density")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
plot_qq(n, x, ci=TRUE) + ggplot2::ggtitle("Q-Q plot") 
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />
