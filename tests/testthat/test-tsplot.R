
library("foehnix")
library("testthat")


# Prepare the models in the "global env" such that we only have
# to run the model estimation twice for all kinds of plots!
# Loading test data
data <- demodata("tyrol")
filter <- list(dd = c(43, 223), crest_dd = c(90, 270))

# Model: model will be added to the global 
# environment such that we can use it in the
# other test_that calls.
mod0 <- foehnix(ff ~ 1, data = data, switch = TRUE,
                filter = filter, verbose = FALSE)
mod1 <- foehnix(diff_t ~ ff + rh, data = data, switch = TRUE,
                filter = filter, verbose = FALSE)

# ------------------------------------------------------------
# ------ testing the tsplot plot function (time series) ------
# ------------------------------------------------------------
test_that("Testing tsplot.foehnix function", {

    expect_is(mod0, "foehnix")
    expect_is(mod1, "foehnix")

    # Plotting helper function for the tests
    tsplotfun <- function(x, ...) {
        pdf(file = tempfile())
        tsplot(x, ...)
        dev.off()
    }

    # Start and end for the period to be plotted
    start <- min(index(data))
    end   <- start + 20 * 86400
    expect_is(start, "POSIXt")
    expect_is(end,   "POSIXt")

    # Wrong usage
    expect_error(tsplotfun(mod0, start = "foo"))
    expect_error(tsplotfun(mod0, start = "foo"))
    expect_error(tsplotfun(mod0, end = "foo"))
    expect_error(tsplotfun(mod0, start = 11111))
    expect_error(tsplotfun(mod0, end = 11111))
    expect_error(tsplotfun(mod0, ndays = "foo"))
    expect_error(tsplotfun(mod0, ndays = -10))
    expect_error(tsplotfun(data))

    # Create random zoo time series and call with two inputs.
    # This one should result in an error as the first one has to be
    # a foehnix object.
    xtra <- zoo(runif(nrow(data), 0, 1), index(data))
    expect_error(tsplotfun(list(xtra, mod0), start = start, end = end, ask = FALSE))
    # This should work
    expect_silent(tsplotfun(list(mod0, xtra), start = start, end = end, ask = FALSE))
    # Should work as well
    expect_silent(tsplotfun(list(mod0, mod1, xtra), start = start, end = end, ask = FALSE))
    expect_error(tsplotfun(list(mod0, mod1, modX, xtra), start = start, end = end, ask = FALSE))

    # Correct usage
    expect_silent(tsplotfun(mod0, start = start, end = end, ask = FALSE))
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30, ask = FALSE))


})










