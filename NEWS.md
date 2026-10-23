# DitributionS7 0.1.5
## Added
* Theoretical plots are now exported for continuous distributions.
* Bootstrap parameter inference via `parameter_inference()` with `Bootstrap()`, which gains arguments `type` (`"parametric"` or `"nonparametric"`) and `ci_method` (`"percentile"` or `"basic"`). `gof_test()` always uses a parametric bootstrap.

## Changed
* Plots no longer set position scales; the plotted range is added as data (`ggplot2::expand_limits()`), and the theoretical curves follow the final limits of the x scale. Any position scale can be added to the plots without re-creating limits.
* `stat_pdf()`, `stat_cdf()`, and `stat_cdf_interval()` take the range to compute over as the parameter `xlim` (like `ggplot2::stat_function()`), instead of `xmin`/`xmax` aesthetics.
* `plot_hist()` now covers all histogram bins and the full density curve.
* `BiasCorrected()` for the Exponential rate is now the unbiased `(n-1)/sum(x)` (was `(n-2)/sum(x)`).
* `parameter_inference()` with `DefaultMethod()` for the Exponential distribution computes exact chi-squared intervals instead of normal theory intervals, and no longer warns for estimators other than `Mle()`.
* Log-logistic `parameter_estimates()` supports only `Mme()` and `Mle()`, and fails with a clear error for other estimators.

## Fixed
* `BiasCorrected()` for the Normal distribution no longer applies the degrees of freedom correction when the mean is fixed. The documentation states that only the variance estimate is unbiased.
* Bootstrap `gof_test()` no longer errors when re-fitting fails with an estimator that has no `start` (e.g., `BiasCorrected()` or `Mme()`).
* `Normal(mu, tau)` can evaluate the pdf, cdf, quantile function and random numbers.

# DistributionS7 0.1.4
* Added the Shash distribution.

# DistributionS7 0.1.3

* Removed dependency on `jaspGraphs`.
* Fixed implementation of bootstrap `gof_test` with `estimated=TRUE`. Previous implementation erroneously did not fit the distribution on each bootstrap replicate.
