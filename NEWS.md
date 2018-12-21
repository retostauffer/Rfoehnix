
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
