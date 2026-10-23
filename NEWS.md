# DitributionS7 0.1.5
## Added
* Theoretical plots are now exported for continuous distributions.
* Bootstrap parameter inference via `parameter_inference()` with `Bootstrap()`, which gains arguments `type` (`"parametric"` or `"nonparametric"`) and `ci_method` (`"percentile"` or `"basic"`). `gof_test()` always uses a parametric bootstrap.

# DistributionS7 0.1.4
* Added the Shash distribution.

# DistributionS7 0.1.3

* Removed dependency on `jaspGraphs`.
* Fixed implementation of bootstrap `gof_test` with `estimated=TRUE`. Previous implementation erroneously did not fit the distribution on each bootstrap replicate.
