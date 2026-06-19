
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DistributionS7

<!-- badges: start -->

[![R-CMD-check](https://github.com/Kucharssim/DistributionS7/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Kucharssim/DistributionS7/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Kucharssim/DistributionS7/graph/badge.svg)](https://app.codecov.io/gh/Kucharssim/DistributionS7)
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
#>              test statistic      p_value
#> ks_test   ks_test  0.629647 7.334348e-35
#> cvm_test cvm_test 15.519860 0.000000e+00
#> ad_test   ad_test 75.337049 6.000000e-06

# fit to data (maximum likelihood by default)
n <- fit(n, data=x)

# get uncertainty around parameter estimates using normal theory intervals
parameter_inference(n, NormalTheory(), x)
#>         key   label  estimate         se     lower     upper
#> mu       mu    \\mu 1.0064618 0.03200656 0.9437301 1.0691935
#> sigma sigma \\sigma 0.3200656 0.02263204 0.2786442 0.3676444

# fit indices of the fitted distribution
gof_test(n, x, estimated=TRUE)
#>                                      test  statistic   p_value
#> lillie_test                   lillie_test 0.06877785 0.2904382
#> cvm_test                         cvm_test 0.07384191 0.2464632
#> ad_test                           ad_test 0.50914370 0.1937788
#> shapiro_wilk_test       shapiro_wilk_test 0.98044336 0.1439141
#> shapiro_francia_test shapiro_francia_test 0.98377711 0.2206845
information_criteria(n, x)
#>   n_par n_obs   log_lik      aic      bic
#> 1     2   100 -27.97091 59.94183 65.15217

# compare data to distribution
plot_hist(n, x) + ggplot2::ggtitle("Histogram vs. Normal Density")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

``` r
plot_qq(n, x, ci=TRUE) + ggplot2::ggtitle("Q-Q plot") 
```

<img src="man/figures/README-unnamed-chunk-3-2.png" alt="" width="100%" />
