
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

# sample from a distribution
x <- rng(n, 1000)

# goodness-of-fit tests
gof_test(n, x, estimated=FALSE)
#>              test statistic   p_value
#> ks_test   ks_test 0.0372936 0.1238460
#> cvm_test cvm_test 0.3091308 0.1274170
#> ad_test   ad_test 1.9198886 0.1017058

# fit to data using maximum likelihood
n <- fit_distribution(n, Mle(), x)

# get uncertainty around parameter estimates using normal theory intervals
parameter_inference(n, NormalTheory(), x)
#>         key   label    estimate         se      lower       upper
#> mu       mu    \\mu -0.05823803 0.03109232 -0.1191779 0.002701806
#> sigma sigma \\sigma  0.98322554 0.02198558  0.9410652 1.027274683

# fit indices of the fitted distribution
gof_test(n, x, estimated=TRUE)
#>                                      test  statistic    p_value
#> lillie_test                   lillie_test 0.02933822 0.04163899
#> cvm_test                         cvm_test 0.14544444 0.02732160
#> ad_test                           ad_test 0.76589234 0.04636819
#> shapiro_wilk_test       shapiro_wilk_test 0.99695697 0.05321971
#> shapiro_francia_test shapiro_francia_test 0.99697475 0.05275794
information_criteria(n, x)
#>   n_par n_obs   log_lik      aic      bic
#> 1     2  1000 -1402.022 2808.044 2817.859

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
