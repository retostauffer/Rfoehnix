
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

    # Testing tsplot.control
    expect_silent(res <- tsplot.control())
    expect_is(res, "tsplot.control")

    expect_silent(res <- tsplot.control(foo = "baa"))
    expect_silent(res <- tsplot.control(t = "T2m"))

    # Renaming covariates
    expect_identical(res$t$name, "T2m")
    expect_silent(res <- tsplot.control(t = "T2m", rh = "relhum"))
    expect_identical(res$t$name,  "T2m")
    expect_identical(res$rh$name, "relhum")
    expect_silent(res <- tsplot.control(t = list(name = "T2m"), rh = list(name = "relhum")))
    expect_identical(res$t$name,  "T2m")
    expect_identical(res$rh$name, "relhum")

    # Adding fresh color
    expect_silent(res <- tsplot.control(t = list(col = "red")))
    expect_identical(res$t$col, "#FF0000")
    expect_silent(res <- tsplot.control(t = list(col = 2)))
    expect_identical(res$t$col, "#DF536B")

    # Adding a new label
    expect_silent(res <- tsplot.control(t = list(ylab = "foo")))
    expect_identical(res$t$ylab, "foo")

    # Adding all in one go
    args <- list(dd = list(name = "winddir", col = 2, ylab = "my wind direction"),
                 ff = list(name = "windspd", col = "#001122", ylab = "my wind speed"))

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
    expect_error(tsplotfun(mod0, control = list(foo = "bar")))
    expect_error(tsplotfun(mod0, control = 1))

    # Renaming ALL covariates. Should result in an error as none of the
    # subplots can be plotted anymore.
    modX <- mod0; modX$prob$prob <- NA
    expect_silent(tsplotfun(modX, start = start, end = end, t = "myt", rh = "myrh",
              diff_t = "mydiff_t", dd = "mydd", ff = "myff", prob = "myprob", ask = FALSE))
    # Should result in an error as probabilities out of range
    modX$prob$prob <- 10
    expect_error(tsplotfun(modX, start = start, end = end, t = "myt", rh = "myrh",
              diff_t = "mydiff_t", dd = "mydd", ff = "myff", prob = "myprob", ask = FALSE))

    # Using the 'control' from above (with two misspecified/renamed variables, so the
    # wind direction and wind speed plot should just not show up)
    args <- list(dd = list(name = "winddir", col = 2,         ylab = "my wind direction"),
                 ff = list(name = "windspd", col = "#001122", ylab = "my wind speed"))
    expect_silent(control <- do.call(tsplot.control, args))
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            control = control, ask = FALSE))
    # Same but explicitly renaming the variables here
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            dd = "winddir", ff = "windspd", ask = FALSE))
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            dd = list(name = "winddir", col = 2, ylab = "my wind direction"),
                            ff = list(name = "windspd", col = 5, ylab = "my wind spedd"),
                            ask = FALSE))

    # Custom csv file 
    expect_silent(style_file <- system.file(package = "foehnix", "tsplot.control/custom_demo.csv"))
    expect_true(file.exists(style_file))
    expect_silent(tsplotfun(mod1, style = style_file, windsector = list(c(43, 223)),
                            start = "2018-01-01", end = "2018-01-10"))
})

