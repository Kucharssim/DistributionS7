# DistributionS7 0.1.6

## Added
* Maximum spacing estimator `Mse()` for continuous distributions. Unlike the MLE, it remains well defined when the likelihood is unbounded, e.g., for the shift of shifted distributions with shape parameter below 1.

## Fixed
* Setting parameter values of shifted distributions (`parameter_values<-`, `parameter_uvalues<-`) now also sets the parameters of the unshifted distribution. Previously, generic numerical optimization (e.g., `Mle(optim = TRUE)`) only estimated the shift.

## Changed
* Labels in PDF/CDF/QF plots are drawn after everything else, so they are layed on top of all layers (and always visible).
* Labels in PDF/CDF/QF are prettier (larger fontsize, unified style: colored background, black text and borders).
* Tangent density lines in CDF/QF plots are drawn after the CDF/QF, so they are not overlayed by it.
* Improved fitting shifted distributions with MLE; including warnings if the MLE does not exist.



# DistributionS7 0.1.5
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
