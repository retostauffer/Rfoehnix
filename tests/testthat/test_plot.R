
library("foehnix")
library("testthat")


# Prepare the models in the "global env" such that we only have
# to run the model estimation twice for all kinds of plots!
# Loading test data
data <- demodata()
filter <- list(dd = c(43, 223), crest_dd = c(90, 270))

# Model: model will be added to the global 
# environment such that we can use it in the
# other test_that calls.
mod0 <- foehnix(ff ~ 1, data = data, switch = TRUE,
                filter = filter, verbose = FALSE)
mod1 <- foehnix(diff_t ~ ff + rh, data = data, switch = TRUE,
                filter = filter, verbose = FALSE)

# ------------------------------------------------------------
# --------- testing the foehnix plot function ----------------
# ------------------------------------------------------------
test_that("Testing plot.foehnix function", {

    expect_is(mod0, "foehnix")
    expect_is(mod1, "foehnix")

    # Test plotting functions
    # function(x, which = NULL, log = TRUE, ...) 
    plotfun <- function(x, ...) {
        pdf(file = tempfile())
        plot(x, ...)
        dev.off()
    }

    # Wrong usage
    expect_error(plotfun(mod0, which = 9))
    expect_error(plotfun(mod0, which = c(1,8)))
    expect_error(plotfun(mod0, which = list(1, 2)))
    expect_error(plotfun(mod0, which = "foo"))

    # Testing the different plot types for both models
    expect_silent(plotfun(mod0, ask = FALSE))
    expect_silent(plotfun(mod0, which = "loglik", ask = FALSE))
    expect_silent(plotfun(mod1, which = "loglik", ask = FALSE))
    expect_silent(plotfun(mod0, which = "loglikcontribution", ask = FALSE))
    expect_silent(plotfun(mod1, which = "loglikcontribution", ask = FALSE))
    expect_silent(plotfun(mod0, which = "coef", ask = FALSE))
    expect_silent(plotfun(mod1, which = "coef", ask = FALSE))
    expect_silent(plotfun(mod0, which = "hist", ask = FALSE))
    expect_silent(plotfun(mod1, which = "hist", ask = FALSE))
    expect_silent(plotfun(mod0, which = c("hist", "loglik"), ask = FALSE))
    expect_silent(plotfun(mod1, which = c("hist", "loglik"), ask = FALSE))

    # Integers instead of plot type names
    expect_silent(plotfun(mod0, which = 1:2, ask = FALSE))
    expect_silent(plotfun(mod1, which = 1:2, ask = FALSE))

    # Setting log to FALSE
    expect_silent(plotfun(mod0, log = FALSE, ask = FALSE))
    expect_silent(plotfun(mod1, log = FALSE, ask = FALSE))

})



# ------------------------------------------------------------
# ------- testing the image plot function (Hovmoeller) -------
# ------------------------------------------------------------
test_that("Testing image.foehnix function", {

    expect_is(mod1, "foehnix")

    imagefun <- function(x, ...) {
        pdf(file = tempfile())
        res <- image(x, ...)
        dev.off()
        return(res)
    }

    # Test some errors (wrong usage)
    expect_error(foehnix::image.foehnix(data))
    # Wrong function argument
    expect_error(imagefun(mod1, FUN = 3))  
    expect_error(imagefun(mod1, FUN = "foo"))
    # Wrong deltat
    expect_error(imagefun(mod1, deltat = "foo"))
    expect_error(imagefun(mod1, deltat = -3))
    expect_error(imagefun(mod1, deltat = -3))
    expect_error(imagefun(mod1, deltat = 77777))
    # Wrong deltad
    expect_error(imagefun(mod1, deltad = "foo"))
    expect_error(imagefun(mod1, deltad = -3))
    expect_error(imagefun(mod1, deltad = -3))
    expect_error(imagefun(mod1, deltad = 77777))
    # Wrong colors
    expect_error(imagefun(mod1, col = list()))
    expect_error(imagefun(mod1, col = "#ff00ff"))

    expect_silent(res <- imagefun(mod1))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod1, deltad = 31))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod1, deltat = 3*3600))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod1, deltat = 3*3600, deltad = 31))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    # Test if our default functions are handled correctly
    expect_is(imagefun(mod1, FUN = "mean"), "list")
    expect_is(imagefun(mod1, FUN = "occ"), "list")
    expect_is(imagefun(mod1, FUN = "noocc"), "list")
    expect_is(imagefun(mod1, FUN = "freq"), "list")

    # Custom colors
    expect_is(imagefun(mod1, col = rainbow(100)), "list")
    expect_is(imagefun(mod1, contours = TRUE), "list")
    expect_is(imagefun(mod1, contours = TRUE, contour.col = "black"), "list")

    # Custom zlim
    expect_is(res <- imagefun(mod1), "list")
    expect_true(is.null(res$zlim))
    expect_is(res <- imagefun(mod1, zlim = c(0,1)), "list")
    expect_identical(res$zlim, c(0,1))

})


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
    expect_identical(res[which(res$var == "t"),"name"], "T2m")
    expect_silent(res <- tsplot.control(t = "T2m", rh = "relhum"))
    expect_identical(res[which(res$var == "t"),  "name"], "T2m")
    expect_identical(res[which(res$var == "rh"), "name"], "relhum")
    expect_silent(res <- tsplot.control(t = list(name = "T2m"), rh = list(name = "relhum")))
    expect_identical(res[which(res$var == "t"),  "name"], "T2m")
    expect_identical(res[which(res$var == "rh"), "name"], "relhum")

    # Adding fresh color
    expect_silent(res <- tsplot.control(t = list(color = "red")))
    expect_identical(res[which(res$var == "t"), "color"], "#FF0000")
    expect_silent(res <- tsplot.control(t = list(color = 2)))
    expect_identical(res[which(res$var == "t"), "color"], "#FF0000")

    # Adding a new label
    expect_silent(res <- tsplot.control(t = list(label = "foo")))
    expect_identical(res[which(res$var == "t"), "label"], "foo")

    # Adding all in one go
    args <- list(dd = list(name = "winddir", color = 2, label = "my wind direction"),
                 ff = list(name = "windspd", color = "#001122", label = "my wind speed"))
    expect_silent(res <- do.call(tsplot.control, args))
    expect_identical(unname(unlist(res[which(res$var == "dd"),])),
                     c("dd", "winddir", "#FF0000", "my wind direction"))
    expect_identical(unname(unlist(res[which(res$var == "ff"),])),
                     c("ff", "windspd", "#001122", "my wind speed"))

    # tsplot <- function(x, start = NULL, end = NULL, ndays = 10,
    #                control = tsplot.control(...),
    #                ..., ask = TRUE) {

    tsplotfun <- function(x, ...) {
        pdf(file = tempfile())
        tsplot(x, ...)
        dev.off()
    }


    # Wrong usage
    expect_error(tsplotfun(mod0, start = "foo"))
    expect_error(tsplotfun(mod0, end = "foo"))
    expect_error(tsplotfun(mod0, start = 11111))
    expect_error(tsplotfun(mod0, end = 11111))
    expect_error(tsplotfun(mod0, ndays = "foo"))
    expect_error(tsplotfun(mod0, ndays = -10))
    expect_error(tsplotfun(mod0, control = list(foo = "bar")))
    expect_error(tsplotfun(mod0, control = 1))

    # Correct usage
    start <- min(index(data))
    end   <- start + 20 * 86400
    expect_silent(tsplotfun(mod0, start = start, end = end, ask = FALSE))
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30, ask = FALSE))

    # Using the 'control' from above (with two misspecified/renamed variables, so the
    # wind direction and wind speed plot should just not show up)
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            control = res, ask = FALSE))
    # Same but explicitly renaming the variables here
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            dd = "winddir", ff = "windspd", ask = FALSE))
    expect_silent(tsplotfun(mod0, start = start, end = end, ndays = 30,
                            dd = list(name = "winddir", color = 2, label = "my wind direction"),
                            ff = list(name = "windspd", color = 5, label = "my wind spedd"),
                            ask = FALSE))

})










