
library("foehnix")
library("testthat")

test_that("Testing image plot", {

    # Testing image plot and it's options.
    imagefun <- function(x, ...) {
        pdf(file = tempfile(), width = 10, height = 6)
        res <- image(x, ...)
        dev.off()
        return(res)
    }

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
    expect_error(imagefun(mod, FUN = 3))  
    expect_error(imagefun(mod, FUN = "foo"))
    # Wrong deltat
    expect_error(imagefun(mod, deltat = "foo"))
    expect_error(imagefun(mod, deltat = -3))
    expect_error(imagefun(mod, deltat = -3))
    expect_error(imagefun(mod, deltat = 77777))
    # Wrong deltad
    expect_error(imagefun(mod, deltad = "foo"))
    expect_error(imagefun(mod, deltad = -3))
    expect_error(imagefun(mod, deltad = -3))
    expect_error(imagefun(mod, deltad = 77777))
    # Wrong colors
    expect_error(imagefun(mod, col = list()))
    expect_error(imagefun(mod, col = "#ff00ff"))

    expect_silent(res <- imagefun(mod))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod, deltad = 31))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod, deltat = 3*3600))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    expect_silent(res <- imagefun(mod, deltat = 3*3600, deltad = 31))
    expect_is(res, "list")
    expect_is(res$agg, "data.frame")
    expect_equal((length(res$breaks.time)-1) * (length(res$breaks.date)-1), nrow(res$agg))

    # Test if our default functions are handled correctly
    expect_is(imagefun(mod, FUN = "mean"), "list")
    expect_is(imagefun(mod, FUN = "occ"), "list")
    expect_is(imagefun(mod, FUN = "noocc"), "list")
    expect_is(imagefun(mod, FUN = "freq"), "list")

    # Custom colors
    expect_is(imagefun(mod, col = rainbow(100)), "list")
    expect_is(imagefun(mod, contours = TRUE), "list")
    expect_is(imagefun(mod, contours = TRUE, contour.col = "black"), "list")

    # Custom zlim
    expect_is(res <- imagefun(mod), "list")
    expect_true(is.null(res$zlim))
    expect_is(res <- imagefun(mod, zlim = c(0,1)), "list")
    expect_identical(res$zlim, c(0,1))

})
