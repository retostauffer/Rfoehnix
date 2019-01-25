
library("foehnix")
library("testthat")

test_that("Testing image plot", {

    # Loading test data
    expect_silent(data <- demodata())
    # Specify our wind filter rule(s)
    expect_silent(filter <- list(dd = c(43, 223), crest_dd = c(90, 270)))
    expect_silent(mod <- foehnix(diff_t ~ ff + rh, data = data, switch = TRUE,
                                 filter = filter, verbose = FALSE))
    expect_is(mod, "foehnix")

    # Test some errors (wrong usage)
    expect_error(foehnix::image.foehnix(data))
    # Wrong function argument
    expect_error(image(mod, FUN = 3))  
    expect_error(image(mod, FUN = "foo"))
    # Wrong deltat
    expect_error(image(mod, deltat = "foo"))
    expect_error(image(mod, deltat = -3))
    expect_error(image(mod, deltat = -3))
    expect_error(image(mod, deltat = 77777))
    # Wrong deltad
    expect_error(image(mod, deltad = "foo"))
    expect_error(image(mod, deltad = -3))
    expect_error(image(mod, deltad = -3))
    expect_error(image(mod, deltad = 77777))
    # Wrong colors
    expect_error(image(mod, col = list()))
    expect_error(image(mod, col = "#ff00ff"))

    # Testing image plot and it's options.
    # With plot = FALSE we will get a list as the return
    # With some control arguments and data objects used
    # for plotting. Check if we (internally) calculate what
    # is expected:
    expect_silent(res <- image(mod, plot = FALSE))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- image(mod, deltad = 31, plot = FALSE))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- image(mod, deltat = 3*3600, plot = FALSE))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- image(mod, deltat = 3*3600, deltad = 31, plot = FALSE))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    # Test if our default functions are handled correctly
    expect_is(image(mod, FUN = "mean",  plot = FALSE), "list")
    expect_is(image(mod, FUN = "occ",   plot = FALSE), "list")
    expect_is(image(mod, FUN = "noocc", plot = FALSE), "list")
    expect_is(image(mod, FUN = "freq",  plot = FALSE), "list")

    # Custom colors
    expect_is(image(mod, col = rainbow(100), plot = FALSE), "list")
    expect_is(image(mod, contours = TRUE, plot = FALSE), "list")
    expect_is(image(mod, contours = TRUE, contour.col = "black", plot = FALSE), "list")

    # Custom zlim
    expect_is(res <- image(mod, plot = FALSE), "list")
    expect_true(is.null(res$zlim))
    expect_is(res <- image(mod, zlim = c(0,1), plot = FALSE), "list")
    expect_identical(res$zlim, c(0,1))

})
