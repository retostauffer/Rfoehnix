

library("foehnix")
library("testthat")


test_that("Tests for foehnix family objects", {

    expect_silent(g1 <- foehnix:::foehnix_gaussian())
    expect_silent(g2 <- foehnix:::foehnix_cgaussian())
    expect_silent(g3 <- foehnix:::foehnix_tgaussian())
    expect_silent(l1 <- foehnix:::foehnix_logistic())
    expect_silent(l2 <- foehnix:::foehnix_clogistic())
    expect_silent(l3 <- foehnix:::foehnix_tlogistic())

    # Output check
    expect_output(print(g1)); expect_output(print(g2)); expect_output(print(g3))
    expect_output(print(l1)); expect_output(print(l2)); expect_output(print(l3))

    # Testing some S3 methods for the family objects
    fams <- list(g1, g2, g3, l1, l2, l3)
    expect_identical(sapply(fams, class), rep("foehnix.family", length(fams)))
    expect_identical(sapply(fams, function(x) class(x$name)), rep("character", length(fams)))

    # Test truncation
    expect_true(!any(sapply(list(g1, g2, l1, l2), is.truncated)))
    expect_true(all(sapply(list(g3, l3),          is.truncated)))

    # None of these objects should have left/right
    # censoring or truncation.
    expect_true(!any(sapply(fams, has.left)))
    expect_true(!any(sapply(fams, has.right)))

    # Calculate some properties for a set of observations/distributions.
    # Note that, for now, no truncation/censoring points have been set.
    # Thus, all Gaussian should return the same. True for logistic as well.
    y    <- seq(-5, 5, length = 100)
    prob <- exp(y) / (1 + exp(y))  # Just some probabilities

    # Gaussian ---------------------------------------
    # Distribution function
    expect_silent(p1 <- g1$p(y, mu = 1, sigma = 2))
    expect_silent(p2 <- g2$p(y, mu = 1, sigma = 2))
    expect_silent(p3 <- g3$p(y, mu = 1, sigma = 2))
    expect_equal(p1, p2)
    expect_equal(p1, p3)

    # Density
    expect_silent(d1 <- g1$d(y, mu = 1, sigma = 2))
    expect_silent(d2 <- g2$d(y, mu = 1, sigma = 2))
    expect_silent(d3 <- g3$d(y, mu = 1, sigma = 2))
    expect_equal(d1, d2)
    expect_equal(d1, d3)

    # Log-density
    expect_silent(ld1 <- g1$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_silent(ld2 <- g2$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_silent(ld3 <- g3$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_equal(ld1, ld2)
    expect_equal(ld1, ld3)

    # Posterior
    expect_silent(p1 <- g1$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_silent(p2 <- g2$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_silent(p3 <- g3$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_equal(p1, p2)
    expect_equal(p1, p3)
    
    # Theta estimate
    set.seed(100); yy <- c(rnorm(1000, -5, 3), rnorm(1000, 6, 5))
    expect_silent(t1 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_silent(t2 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_silent(t3 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_equal(as.numeric(t1), c(-5.239967, 0.9972708, 6.311117, 1.490218), tolerance = 0.001)
    expect_identical(t1, t2)
    expect_identical(t1, t3)



    # Logistic distribution ----------------------------------
    # Distribution function
    expect_silent(p1 <- l1$p(y, mu = 1, sigma = 2))
    expect_silent(p2 <- l2$p(y, mu = 1, sigma = 2))
    expect_silent(p3 <- l3$p(y, mu = 1, sigma = 2))
    expect_equal(p1, p2)
    expect_equal(p1, p3)

    # Density
    expect_silent(d1 <- l1$d(y, mu = 1, sigma = 2))
    expect_silent(d2 <- l2$d(y, mu = 1, sigma = 2))
    expect_silent(d3 <- l3$d(y, mu = 1, sigma = 2))
    expect_equal(d1, d2)
    expect_equal(d1, d3)

    # Log-density
    expect_silent(ld1 <- l1$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_silent(ld2 <- l2$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_silent(ld3 <- l3$d(y, mu = 1, sigma = 2, log = TRUE))
    expect_equal(ld1, ld2)
    expect_equal(ld1, ld3)

    # Posterior
    expect_silent(p1 <- l1$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_silent(p2 <- l2$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_silent(p3 <- l3$posterior(y, prob, list(mu1 = -2, logsd1 = log(3), mu2 = 4, logsd2 = log(5))))
    expect_equal(p1, p2)
    expect_equal(p1, p3)

    # Theta estimate
    set.seed(100); yy <- c(rlogis(1000, -5, 3), rlogis(1000, 6, 5))
    expect_silent(t1 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_silent(t2 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_silent(t3 <- g1$theta(yy, post = as.numeric(yy > median(yy))))
    expect_equal(as.numeric(t1), c(-6.534235, 1.471838, 8.058779, 1.964831), tolerance = 0.001)
    expect_equal(t1, t2)
    expect_equal(t1, t3)



});











