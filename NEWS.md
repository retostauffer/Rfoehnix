
# TODO

* Regularized probability models (based on glmnet): implemented, test
* There is one import ":::" (cgaussian family) in one of the
  examples -> illegal!
* A separate `windrose` vignette might be nice.
    ... started a separate vignette for `tsplot`, implement separate
    vignettes for `windrose` and `image`.
* A Tyrolean and a Californian demo vignette? - draft added.
* `summary(..., detailed = TRUE)` does not show sigma coefficients.
* Seems I am having some problems with cgaussian/tgaussian/...
* Windrose: changed argument `mfcol` to `ncol`, adjusted handling of
    labels/legends.
* Updated a series of _R_ documentation pages.

# foehnix 0.1-4 (August 2019; development)

* Additional tests added.
* Added generic function for `IGN` (ignorance) as alternative score.
* Windrose: now allows for inputs \code{main}, \code{legend.pos} ("right" or "left"),
    \code{legend.title} (add legend title),
    and \code{labels.angle} (where to plot the labels on density plots), and
* Updated structure of the `pkgdown` site: combined `README.md` (`index.html` and 
    github readme), removed "getting started", added demo data sets and vignette
    "Import data".

# foehnix 0.1-3 (August 2019)

* Additional functionality added to `image.foehnix` (custom `xlim` and `ylim`,
    allow for custom `style` files).
* Revamped/simplified internal structure of `image.foehnix`.
* Updated the demo data sets (Ellboegen and Sattelberg); quality-controlled
    updated extended data set (by Deborah Detka). Moved from csv to rda
    using `usethis`; reduces package size; added data/scripts in `data-raw`.
* Moved generic `image` function (`image.foehnix`) to `R/image.R`.
* `image` allows to set custom `xlim`, `ylim` (and `zlim`) limits.
    Decreasing `xlim` values allow to plot over the new years period.
* Added generic `write.csv` method to save estimated probabilities to a CSV file.
    Will be ued to test output (and compare _R_/python implementation).
* New vignettes (currently in draft stage): each plotting routine gets its own
    vignette.
* Allow for custom wind speed breaks when calling `windrose`.
* Allow for adding wind sectors when calling `windrose.default`.
* Added second demo data set "Viejas and Lucky Five Ranch (CA, USA)";
    modified the `demodata(...)` function to be able to handle both
    (multiple) demo data sets.
* Data sets (`ellboegen`, `sattelberg`, `viejas`, `luckyfive`) now stored
    as binary time series objects (`.rda`; `zoo`).
* Fixed a bug in the p-value calculation in `summary.crch` (`abs` was missing).
* New test statistics in `summary.foehnix` (similar to `flexmix`).
* Additional plot type (`"posterior"`, `5L`) in `plot.foehnix`.
* Fixed an NA-handling bug in `foehnix_filter`. Only rows used have to be
    checked (controlled by new input argument \code{cols}).

# foehnix 0.1-2 (August 2019)

* Mainly a development release.
* Revamped the structure of the time series plot (`tsplot`)
* Added new features to `tsplot`: wind sectors, styles, better customization options
    (`tsplot.control`).
* Fixed few tests.
* Fixed a but in the Hovmoeller routine (`image.foehnix`); further adjustments
    required.

# foehnix 0.1-1 (April 2019)

* `uv2ddff` and `ff2dduv` now return a `zoo` object if the input
    is a single `zoo` object (`uv2ddff(data)` or `ddff2uv(data)` where `data`
    is of class `zoo`).
* Checking for constant values: new check on main variable (after applying
    the filter). The check for multiple constants for the concomitant model
    is now performed on the `logitX` model.matrix.
* Removed custom LASSO option (`lambda`) from the `foehnix` interface.
    `iwls_logit` would, technically, allow for iterative penalized
    estimates but is rather slow. Thus, an experimental control argument
    for regularized estimates based on the `glmnet` package has been
    implemented (see \code{\link{foehnix.control}}/\code{\link{glmnet.control}}).
* `windrose.default` allows for custom data filters (using the foehnix filter
    method) and custom variable names for wind direction and wind speed
    (`var.dd`/`var.ff`). Default `var.dd = "dd"` and `var.ff = "ff"`.
    Custom filters only allowed with multivariate objects (zoo or data.frame).
* windrose now allows to highlight specific wind sectors.
* Fixed tsplot: observed temperature is now on top, fixed the issue with the
    gray boxes (periods with a probability >= 0.5).

# foehnix 0.1-0 (January 2019)

* `windrose` allows to specify custom names.
* `tsplot` allows to specify custom variable names (rename defaults).
   Requires some more testing.
* `foehnix` objects (returned by `foehnix`) contain a new element
   `nobs`, the number of elements used for classification.
   Note that this is _not_ the same information as provided by
   the `good`, the `bad`, and the `ugly` from the foehnix filter
   (the `filter_obj` only contains information based on the variables
   used with the filter, not on the covariates used for the `foehnix`
   model).

# foehnix 0.0-9

* Added a warning which will be shown if the EM algorithm stops
  in the first iteration (returns initial values). Quite likely
  a misspecified model (non-suitable formula given the data).
* `windrose` manual extended, `windrose` also allows to plot a windrose
  of non-`foehnix` objects (e.g., univariate zoo objects of `dd` and `ff`
  or two numeric vectors). Examples included.
* Added inflation handling. foehnix tries to inflate the time series
  object provided by the user to create a strictly regular time series
  object. If the inflation ratio exceeds 2 (the data set would be inflated
  by a factor of 2 or more) the script will STOP! foehnix.control provides
  an option (force.inflate) to overrule this check, however, might cause
  memory issues.
* summary.foehnix shows "inflation" count.
* foehnix will stop if the data set is not regular (note: regular is not
  "strictly regular", but is one requirement to create a strictly regular
  time series (inflate feature).
* Fixed an issue with `foehnix_filter` where observations (rows) where not
  all elements have been NA have been treated as "outside wind sector"
  rather than "not all observations available" (FALSE has been returned
  instead of NA; now an NA will be returned by if multiple filters are
  used and at least one element is missing).
* Fixed a bug where `N_inflated` was missing when no inflation was needed.

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
