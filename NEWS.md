
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
