

library("testthat")
library("foehnix")


test_that("Testing wind rose plot functionality (basic type)", {

    # Testing errors if wrong inputs are used
    expect_error(windrose())
    expect_error(windrose(3))
    expect_error(windrose(list()))
    expect_error(windrose(c(1,2,3)))

    # For the default windrose plot: test for error
    # if length(dd) != length(ff)
    expect_error(windrose(dd = c(1,2,3)))
    expect_error(windrose(ff = c(1,2,3)))
    expect_error(windrose(dd = c(1,2,3)), ff = c(1,2))
    expect_error(windrose(dd = c(1,2)), ff = c(1,2,3))
    # Wind direction outside defined range of 0 - 360
    expect_error(windrose(dd = c(-10,180), ff = c(1,1)))
    expect_error(windrose(dd = c(10,580), ff = c(1,1)))
    # Wind speed < 0
    expect_error(windrose(dd = c(10,180), ff = c(-3,1)))

    # Loading some data
    expect_silent(data <- demodata("Ellboegen"))

    # -------------------------------
    # Testing the two basic types
    # First: the density plot 
    expect_silent(res <- windrose(as.numeric(data$dd), as.numeric(data$ff),
                                  type = "density", plot = FALSE))
    expect_is(res$tab, "matrix")
    expect_true(nrow(res$tab) == (360 / res$interval))
    expect_identical(dim(res$tab), c(length(res$dd.breaks), length(res$ff.breaks)) - 1L)
    expect_true(is.null(res$counts))


    # Second: the histogram plot
    expect_silent(res <- windrose(as.numeric(data$dd), as.numeric(data$ff),
                                  type = "histogram", plot = FALSE))
    expect_is(res$tab, "list")
    expect_is(res$tab$colormatrix, "matrix")
    expect_is(res$tab$legend, "data.frame")
    expect_is(res$counts, "xtabs")
    expect_identical(nrow(res$counts), as.integer(360 / res$interval))
    expect_identical(dim(res$counts), c(length(res$dd.breaks), length(res$ff.breaks)) - 1L)

})

# Testing windrose plot on foehnix object.
test_that("Testing foehnix plot on windrose object", {

    expect_silent(data <- head(demodata("Ellboegen"), 10000))
    expect_silent(filter <- list(dd = c(43, 223)))
    expect_silent(mod <- foehnix(ff ~ rh, data = data,
                                 filter = filter, verbose = FALSE))

    # Thest error, wrong input
    expect_error(windrose(mod, type = list()))
    expect_error(windrose(mod, type = "foo"))
    expect_error(windrose(mod, which = list()))
    expect_error(windrose(mod, which = "foo"))
    # Rename the variables dd/ff -> should result in an error
    expect_error(windrose(mod, ddvar = "foo"))
    expect_error(windrose(mod, ffvar = "foo"))
    expect_error(windrose(mod, mfcol =  -10))
    expect_error(windrose(mod, mfcol =  "foo"))
    expect_error(windrose(mod, maxpp =  -10))
    expect_error(windrose(mod, maxpp = -Inf))
    expect_error(windrose(mod, plot = "foo"))

    # Make the default plot (without plotting)
    expect_silent(res <- windrose(mod, plot = FALSE))
    expect_is(res, "list")
    expect_identical(length(res), 6L)
    expect_identical(unname(sapply(res, class)), rep("list", 6L))

    # Return of the density plots
    expect_silent(tmp <- res[grep("^density.*", names(res))])
    expect_identical(unname(sapply(tmp, function(x) class(x$counts))), rep("NULL", 3L))
    expect_identical(unname(sapply(tmp, function(x) class(x$tab))), rep("matrix", 3L))
    expect_true(all(all(sapply(tmp, function(x) nrow(x$tab) == (360L/x$interval)))))
    expect_true(all(all(sapply(tmp, function(x) ncol(x$tab) == (length(x$ff.breaks) - 1L)))))

    # Return of the histogram plots
    expect_silent(tmp <- res[grep("^histogram.*", names(res))])
    expect_identical(unname(sapply(tmp, function(x) class(x$counts)[1L])), rep("xtabs", 3L))
    expect_identical(unname(sapply(tmp, function(x) class(x$tab))), rep("list", 3L))
    expect_identical(unname(sapply(tmp, function(x) class(x$tab$colormatrix))), rep("matrix", 3L))
    expect_identical(unname(sapply(tmp, function(x) class(x$tab$legend))), rep("data.frame", 3L))
    expect_true(all(all(sapply(tmp, function(x) nrow(x$tab$colormatrix) == (360L/x$interval)))))
    expect_true(all(all(sapply(tmp, function(x) ncol(x$tab$colormatrix) == (length(x$ff.breaks) - 1L)))))

    # Subsetting (only a subset of all plots)
    expect_identical(length(windrose(mod, which = "unconditional", plot = FALSE)), 2L)
    expect_identical(length(windrose(mod, which = "nofoehn",       plot = FALSE)), 2L)
    expect_identical(length(windrose(mod, which = "foehn",         plot = FALSE)), 2L)
    expect_identical(length(windrose(mod, type = "density",        plot = FALSE)), 3L)
    expect_identical(length(windrose(mod, type = "histogram",      plot = FALSE)), 3L)

    expect_identical(length(windrose(mod, which = "unconditional", type = "density", plot = FALSE)), 1L)
    expect_identical(length(windrose(mod, which = "nofoehn",       type = "density", plot = FALSE)), 1L)
    expect_identical(length(windrose(mod, which = "foehn",         type = "density", plot = FALSE)), 1L)
    expect_identical(length(windrose(mod, which = "unconditional", type = "histogram", plot = FALSE)), 1L)
    expect_identical(length(windrose(mod, which = "nofoehn",       type = "histogram", plot = FALSE)), 1L)
    expect_identical(length(windrose(mod, which = "foehn",         type = "histogram", plot = FALSE)), 1L)

})


