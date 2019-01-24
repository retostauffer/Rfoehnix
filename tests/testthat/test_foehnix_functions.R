

library("testthat")
library("foehnix")

test_that("Foehnix functions: standardize/destandardize", {

    # Wrong inputs
    expect_error(standardize(3))
    expect_error(standardize(data.frame(a = c(1,2), b = c(2,3))))
    expect_error(standardize(3, "foo"))

    # Take the airquality data set to test standardization
    expect_silent(X <- as.matrix(na.omit(airquality)))
    expect_silent(S <- standardize(X))
    expect_is(S, c("standardized", "matrix"))
    expect_identical(dim(X), dim(S))
    expect_is(attr(S, "scaled:center"), "numeric")
    expect_is(attr(S, "scaled:scale"), "numeric")
    expect_identical(attr(S, "scaled:center"), apply(X, 2, mean))
    expect_identical(attr(S, "scaled:scale"), apply(X, 2, sd))
    expect_equal(apply(S, 2, mean), structure(rep(0, ncol(S)), names = colnames(S)))
    expect_equal(apply(S, 2, sd),   structure(rep(1, ncol(S)), names = colnames(S)))

    # Extract attributes
    expect_equal(center(S), apply(X, 2, mean))
    expect_equal(scale(S),  apply(X, 2, sd))

    # Destandardize
    expect_silent(D <- destandardize(S))
    expect_identical(dim(D), dim(X))
    expect_equal(unclass(D), unclass(X))

    # Checking standardization
    expect_false(is.standardized(X))
    expect_true(is.standardized(S))
    expect_false(is.standardized(D))


})

test_that("Foehnix functions: destandardize coefficients", {

    # Generate a fake standardized matrix
    set.seed(10)
    S <- matrix(c(rnorm(500, 10, 2), rnorm(500, 5, 3), rnorm(500, -5, 1)),
                ncol = 3, dimnames = list(NULL, c("(Intercept)", "A", "B")))
    S <- standardize(S)
    # Fake scaled:center and scaled:scale
    attr(S, "scaled:center") <- structure(c(10, 5, -20), names = colnames(S))
    attr(S, "scaled:scale")  <- structure(c(1, 2, 5), names = colnames(S))
    expect_true(is.standardized(S))

    # Check if conversion works: both standardized with an
    # offset of 20 and a scale of 2. The mean is meaningless
    # for the coefficients except for '(Intercept)'. 
    beta <- structure(matrix(c(2, -5, -5), ncol = 1), rownames = c("(Intercept)", "A", "B"))
    expect_silent(beta2 <- destandardize_coefficients(beta, S))
    expect_equal(as.vector(beta2), as.vector(beta) / as.vector(scale(S)))

})

