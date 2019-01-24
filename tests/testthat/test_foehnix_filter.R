

library("testthat")
library("foehnix")

test_that("Testing foehnix 'wind filter' function (univariate filter)", {

    # Helper function, extracts the counts for 'good', 'bad', and 'ugly'
    counts <- function(x) return(sapply(x[c('good','bad','ugly')], length))

    # Small test data set with NA, north, and south wind directions, 5 each.
    data <- data.frame(dd = sample(rep(c(NA, 0, 180), each = 5)))

    # Misusage
    expect_error(foehnix_filter()) # Missing inputs
    expect_error(foehnix_filter("foo")) # Wrong data type
    expect_error(foehnix_filter(matrix(NA, ncol = 2, nrow = 2))) # Wrong data type
    expect_error(foehnix_filter(data, )) # Missing filter specification
    expect_error(foehnix_filter(data, "foo")) # Wrong class
    expect_error(foehnix_filter(data, matrix(NA, ncol = 2, nrow = 2))) # Wrong class
    expect_error(foehnix_filter(data, list(c(90, 170))))  # Unnamed list
    expect_error(foehnix_filter(data, list(c(90, 170), dd = c(90, 170))))  # Unnamed list
    expect_error(foehnix_filter(data, list(dd = c(90, Inf)))) # Infinite value in filter
    expect_error(foehnix_filter(data, list(dd = c(90, NA))))  # Infinite value in filter
    expect_error(foehnix_filter(data, list(dd = c(90, list())))) # Wrong 'list of two numerics'
    expect_error(foehnix_filter(data, list(dd = function(x) return("wrong return type"))))
    expect_error(foehnix_filter(data, list(dd = function(x) return(c(NA, TRUE)))))
    expect_error(foehnix_filter(data, function(x) return("wrong return type")))
    expect_error(foehnix_filter(data, function(x) c(NA, TRUE, FALSE)))
    expect_error(foehnix_filter(data, list(cc = c(1,2), foo = c(2,3)))) # foo does not exist

    # Misspecification as variable will not be found
    expect_error(foehnix_filter(data, list(foo = c(90, Inf))))
    expect_error(foehnix_filter(data, function(x) return(x$foo >= 90 & x$foo <= 180)))

    # Check silence and correct return
    expect_silent(res <- foehnix_filter(data, list(dd = c(90, 270))))
    expect_identical(counts(res), structure(rep(5L, 3), names = c("good", "bad", "ugly")))
    expect_identical(sum(counts(res)), res$total)

    # Same filter but as function
    expect_silent(res2 <- foehnix_filter(data, list(dd = function(x) return(x >= 90 & x <= 270))))
    expect_identical(counts(res), counts(res2))
    expect_identical(res$total, res2$total)

    # Same filter but as a complex function
    expect_silent(res3 <- foehnix_filter(data, function(x) return(x$dd >= 90 & x <= 270)))
    expect_identical(counts(res), counts(res3))
    expect_identical(res$total, res3$total)

    # A filter of 'dd = c(90, 270)' should be identical to
    # 'dd = function(x) return(x >= 90 & x <= 270)'.
    # Test if this is true. Same for 'dd = c(270, 90)'.
    data2 <- data.frame(dd = seq(0, 360, by = 1))
    res3 <- foehnix_filter(data2, list(dd = c(90, 270)))
    res4 <- foehnix_filter(data2, list(dd = function(x) return(x >= 90 & x <= 270)))
    res5 <- foehnix_filter(data2, list(dd = c(270, 90)))
    res6 <- foehnix_filter(data2, list(dd = function(x) return(x >= 270 | x <= 90)))
    expect_identical(counts(res3), counts(res4))
    expect_identical(counts(res5), counts(res6))

    # Check internal "apply_foehnix_filter" method.
    expect_silent(res7 <- foehnix:::apply_foehnix_filter(data, function(x) return(x >= 90 & x <= 270), "dd"))
    expect_identical(counts(res),
        structure(c(sum(res7, na.rm = TRUE), sum(!res7, na.rm = TRUE), sum(is.na(res7))), names = c("good", "bad", "ugly")))

    # Should produce an output, check output
    expect_output(print(res))
    expect_is(res, "foehnix.filter")
    expect_identical(typeof(res), "list")

});


test_that("Testing foehnix 'wind filter' function (multivariate filter)", {

    # Helper function, extracts the counts for 'good', 'bad', and 'ugly'
    counts <- function(x) return(sapply(x[c('good','bad','ugly')], length))

    # Set of wind directions (NA, 0, 180) and wind speeds (1 - 10),
    # all combinations. Yields 30 observations in tota.
    data <- expand.grid(dd = c(NA, 0, 180), ff = 1:10)

    # ff >= 0, so ff has 'no effect' (all will be TRUE).
    # Thus, we should have 10 each (good, bad, ugly)
    expect_silent(res <- foehnix_filter(data, list(dd = c(90, 270), ff = function(x) return(x >= 0))))
    expect_identical(counts(res), structure(rep(10L, 3), names = c("good", "bad", "ugly")))

    # Only allow ff > 5. Thus, 10 are 'ugly' due to NA,
    # 10 are 'bad' as they are out of the wind sector.
    # Another 5 are 'bad' because wind speed ff is to low,
    # and only 5 will get a 'good' (wind sector good, wind speed good).
    expect_silent(res2 <- foehnix_filter(data, list(dd = c(90, 270), ff = c(6, 10))))
    expect_identical(counts(res2), structure(c(5L, 15L, 10L), names = c("good", "bad", "ugly")))

    # Testing different multi-variate filter specifications
    expect_silent(res3 <- foehnix_filter(data, list(dd = c(90, 270), ff = c(5.1, 100))))
    expect_silent(res4 <- foehnix_filter(data, list(dd = c(90, 270), ff = function(x) return(x >= 6))))
    expect_silent(res5 <- foehnix_filter(data, list(dd = c(90, 270), ff = function(x) return(x >= 5.1))))
    fun <- function(x) {
        res <- x$dd >= 90 & x$dd <= 270 & x$ff >= 5.1
        res[which(apply(data, 1, function(x) any(is.na(x))))] <- NA
        return(res)
    }
    expect_silent(res6 <- foehnix_filter(data, fun))
    expect_identical(counts(res2), counts(res3))
    expect_identical(counts(res2), counts(res4))
    expect_identical(counts(res2), counts(res5))
    expect_identical(counts(res2), counts(res6))
    expect_identical(res2$total, res3$total)
    expect_identical(res2$total, res4$total)
    expect_identical(res2$total, res5$total)
    expect_identical(res2$total, res6$total)

});
