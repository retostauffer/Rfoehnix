
# TODO

* Regularized probability models (based on glmnet).
* Tests.

# foehnix 0.0-9

* Added a warning which will be shown if the EM algorithm stops
  in the first iteration (returns initial values). Quite likely
  a misspecified model (non-suitable formula given the data).

# foehnix 0.0-8

* Added vignette for the logistic regression model (IWLS).
* Added new references.
* In `iwls_logit`: renamed variable `mu` to `prob`.
* Added a Hovmoeller diagram method (`image.foehnix`) with some
  features. Has to be seen as "under development" at the moment!
* Removed some warnings/notes.
* Added inference to the summary output (asymptotic theory).
* Added vignette for the inference part.

# foehnix 0.0-5

* Extended manual pages for `Getting Started`, `Statistical Model`,
  and `References`.
* Updated `predict.foehnix`, new `phoenix_filter` was not yet implemented.
* Updated/fixed some smaller issues inside the `plot.foehnix` method
  (colors/line types/labeling), added `log = FALSE` option to plot the
  paths on the EM iteration scale rather than the log iteration scale.
* If `windfilter` is used as an argument to the method `foehnix` the
  script will now shout at you! Depricated option (this check should
  be removed before release).
* Added functions to convert `ddff2uv` and `uv2ddff` (plotting).
* Was working on the `windrose` and `probwindrose` plotting functions,
  however, not yet production ready. TODO!
* Added windrose function(s), most interesting is the windrose function
  for the foehnix objects.

# foehnix 0.0-4

* Initialization of `prob` ($\pi$) for the non-commitant mixture model
  is now `mean(z)` rather than `0.5`.
* EM/IWLS: if the log-likelihood improvement for a specific iteration
  falls below the threshold: ignore this iteration and return the
  information/coefficients/iteration count from the previous iteration
  (`iter - 1).
* `windfilter` renamed to `filter` when calling `foehnix`!
* `foehnix_filter` has been extended. So far an integer vector has
  been returned with the observations within filter (the 'good' ones).
  This made it hard to find out which ones have to be set to `0` 
  (where both, `y` was ont `NA` and all variables for the filter(s)
  were not `NA`). I made the return of the `foehnix_filter` slightly
  more complex. `foehnix_filter` now returns an object of class
  `foehnix.filter`, a list with three elements containing
  integer vectors which correspond to the row indizes of the data
  (input `x`, see [`foehnix_filter` manual](reference/foehnix_filter.html)).
  Returns:
  * `good`: observations/rows within filter
  * `bad`: observations outside filter, but all values required for the
     filter have been available (not `NA`).
  * `ugly`: if at least one of the variables used for the filter was
     missing.

# foehnix 0.0-3

* Manual pages/documentation now based on roxygen2.
* Nicer implementation of the `switch = TRUE` option. Simply
  use `z = f(y <= mean(y))` instead of `z = f(y >= mean(y))`
  when initializing the component membership to flip the components.
* Removed some smaller uncritical bugs.
* Dependencies/Suggestions corretly resolved.
* R CMD check does not shout on me anymore.

# foehnix 0.0-2

* Fist stable alpha version of the package. Some feature are still missing,
  some of the methods and functions might change in the near future!
