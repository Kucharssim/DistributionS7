# DitributionS7 0.1.5
## Added
* Theoretical plots are now exported for continuous distributions.
* Bootstrap parameter inference via `parameter_inference()` with `Bootstrap()`, which gains arguments `type` (`"parametric"` or `"nonparametric"`) and `ci_method` (`"percentile"` or `"basic"`). `gof_test()` always uses a parametric bootstrap.

## Changed
* Plots no longer set position scales; the plotted range is added as data (`ggplot2::expand_limits()`), and the theoretical curves follow the final limits of the x scale. Any position scale can be added to the plots without re-creating limits.
* `stat_pdf()`, `stat_cdf()`, and `stat_cdf_interval()` take the range to compute over as the parameter `xlim` (like `ggplot2::stat_function()`), instead of `xmin`/`xmax` aesthetics.
* `plot_hist()` now covers all histogram bins and the full density curve.

# DistributionS7 0.1.4
* Added the Shash distribution.

# DistributionS7 0.1.3

* Removed dependency on `jaspGraphs`.
* Fixed implementation of bootstrap `gof_test` with `estimated=TRUE`. Previous implementation erroneously did not fit the distribution on each bootstrap replicate.
