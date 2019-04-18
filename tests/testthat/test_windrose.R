

library("testthat")
library("foehnix")

# Function for testing, creates a pdf output.
windrosefun <- function(x, ...) {
    pdf(file = tempfile(), width = 10, height = 6)
    res <- windrose(x, ...)
    dev.off()
    return(res)
}

test_that("Testing wind rose plot functionality (basic type)", {

    # Testing errors if wrong inputs are used
    expect_error(windrosefun())
    expect_error(windrosefun(3))
    expect_error(windrosefun(list()))
    expect_error(windrosefun(c(1,2,3)))

    # For the default windrose plot: test for error
    # if length(dd) != length(ff)
    expect_error(windrosefun(dd = c(1,2,3)))
    expect_error(windrosefun(ff = c(1,2,3)))
    expect_error(windrosefun(dd = c(1,2,3)), ff = c(1,2))
    expect_error(windrosefun(dd = c(1,2)), ff = c(1,2,3))
    # Wind direction outside defined range of 0 - 360
    expect_error(windrosefun(dd = c(-10,180), ff = c(1,1)))
    expect_error(windrosefun(dd = c(10,580), ff = c(1,1)))
    # Wind speed < 0
    expect_error(windrosefun(dd = c(10,180), ff = c(-3,1)))

    # Loading some data
    expect_silent(data <- demodata("Ellboegen"))

    # -------------------------------
    # Testing the two basic types
    # First: the density plot 
    expect_silent(res <- windrosefun(as.numeric(data$dd), as.numeric(data$ff),
                                  type = "density"))
    expect_is(res$tab, "matrix")
    expect_true(nrow(res$tab) == (360 / res$interval))
    expect_identical(dim(res$tab), c(length(res$dd.breaks), length(res$ff.breaks)) - 1L)
    expect_true(is.null(res$counts))


    # Second: the histogram plot
    expect_silent(res <- windrosefun(as.numeric(data$dd), as.numeric(data$ff),
                                  type = "histogram"))
    expect_is(res$tab, "list")
    expect_is(res$tab$colormatrix, "matrix")
    expect_is(res$tab$legend, "data.frame")
    expect_is(res$counts, "xtabs")
    expect_identical(nrow(res$counts), as.integer(360 / res$interval))
    expect_identical(dim(res$counts), c(length(res$dd.breaks), length(res$ff.breaks)) - 1L)

    # Testing windrose with zoo input
    expect_silent(windrosefun(data))
    # With data.frame
    expect_silent(windrosefun(as.data.frame(data)))
    # Renaming variables
    copy <- data
    names(copy)[grep("^ff$", names(copy))] <- "wind_spd"
    names(copy)[grep("^dd$", names(copy))] <- "wind_dir"
    expect_silent(windrosefun(copy, var.ff = "wind_spd", var.dd = "wind_dir"))

})

# Testing windrose plot on foehnix object.
test_that("Testing foehnix plot on windrose object", {

    expect_silent(data <- head(demodata("Ellboegen"), 10000))
    expect_silent(filter <- list(dd = c(43, 223)))
    expect_silent(mod <- foehnix(ff ~ rh, data = data,
                                 filter = filter, verbose = FALSE))

    # Thest error, wrong input
    expect_error(windrosefun(mod, type = list()))
    expect_error(windrosefun(mod, type = "foo"))
    expect_error(windrosefun(mod, which = list()))
    expect_error(windrosefun(mod, which = "foo"))
    # Rename the variables dd/ff -> should result in an error
    expect_error(windrosefun(mod, ddvar = "foo"))
    expect_error(windrosefun(mod, ffvar = "foo"))
    expect_error(windrosefun(mod, mfcol =  -10))
    expect_error(windrosefun(mod, mfcol =  "foo"))
    expect_error(windrosefun(mod, maxpp =  -10))
    expect_error(windrosefun(mod, maxpp = -Inf))

    # Make the default plot (without plotting)
    expect_silent(res <- windrosefun(mod))
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
    expect_identical(length(windrosefun(mod, which = "unconditional")), 2L)
    expect_identical(length(windrosefun(mod, which = "nofoehn")),       2L)
    expect_identical(length(windrosefun(mod, which = "foehn")),         2L)
    expect_identical(length(windrosefun(mod, type = "density")),        3L)
    expect_identical(length(windrosefun(mod, type = "histogram")),      3L)

    expect_identical(length(windrosefun(mod, which = "unconditional", type = "density")),   1L)
    expect_identical(length(windrosefun(mod, which = "nofoehn",       type = "density")),   1L)
    expect_identical(length(windrosefun(mod, which = "foehn",         type = "density")),   1L)
    expect_identical(length(windrosefun(mod, which = "unconditional", type = "histogram")), 1L)
    expect_identical(length(windrosefun(mod, which = "nofoehn",       type = "histogram")), 1L)
    expect_identical(length(windrosefun(mod, which = "foehn",         type = "histogram")), 1L)

})


test_that("Testing wind sector highlighting", {

    # Loading some data
    expect_silent(data <- head(demodata("Ellboegen"), 1000))

    # Expect error
    expect_error(windrosefun(data, windsector = c(1, 2, 3)))
    expect_error(windrosefun(data, windsector = "foo"))

    # These should work
    # Lists
    expect_silent(windrosefun(data, windsector = list(c(90, 180))))
    expect_silent(windrosefun(data, windsector = list(c(90, 180), c(340, 30))))
    expect_silent(windrosefun(data, windsector = list(A = c(90, 180))))
    expect_silent(windrosefun(data, windsector = list(A = c(90, 180), B = c(340, 30))))
    # Matrices
    tmp <- matrix(c(90, 180), ncol = 2)
    expect_silent(windrosefun(data, windsector = tmp))
    tmp <- matrix(c(90, 180, 340, 30), ncol = 2)
    expect_silent(windrosefun(data, windsector = tmp))
    tmp <- matrix(c(90, 180), ncol = 2,
                  dimnames = list(c("A"), NULL))
    expect_silent(windrosefun(data, windsector = tmp))
    tmp <- matrix(c(90, 180, 340, 30), ncol = 2,
                  dimnames = list(c("A", "B"), NULL))
    expect_silent(windrosefun(data, windsector = tmp))


})


test_that("Testing custom filters for windrose", {

    # Loading some data
    expect_silent(data <- head(demodata("Ellboegen"), 5000))

    # Custom filter: for details, see ?foehnix_filter
    # Example 1:
    # - Plot windrose for all observations where the wind
    #   direction was within 43-233 degrees (south southeast)
    filter1 <- list(dd = c(43, 223))
    expect_silent(windrosefun(data, filter = filter1))
    
    # Example 2:
    # - Plot windrose for all observations where the wind
    #   speed was > 5 meters per second.
    filter2 <- list(ff = function(x) x > 10)
    expect_silent(windrosefun(data, filter = filter2))
    
    # Example 3:
    # - Plot windrose for specific months (create new variable
    #   'month' in advance):
    data$month <- as.POSIXlt(index(data))$mon + 1
    expect_silent(windrosefun(data, filter = list(month = function(x) x == 1)))
    expect_silent(windrosefun(data, filter = list(month = function(x) x == 7)))
    
    # Example 4:
    # Similar to example 3, but for 
    data$hour <- as.POSIXlt(index(data))$hour
    expect_silent(windrosefun(data, filter = list(hour = function(x) x == 0)))
    expect_silent(windrosefun(data, filter = list(hour = function(x) x == 12)))

})
