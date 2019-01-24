

library("foehnix")
library("testthat")


test_that("Tests for foehnix family objects, random errors, wrong initialization", {

    # Wrong initialization
    expect_error(foehnix:::foehnix_gaussian(0))
    expect_error(foehnix:::foehnix_cgaussian(c(1,2), 1))
    expect_error(foehnix:::foehnix_tgaussian(1, c(1,2)))

    expect_error(foehnix:::foehnix_logistic(0))
    expect_error(foehnix:::foehnix_clogistic(c(1,2), 1))
    expect_error(foehnix:::foehnix_tlogistic(1, c(1,2)))

    expect_error(g1$r(100, 1, 2))
    expect_error(g2$r(100, 1, 2))
    expect_error(g3$r(100, 1, 2))
    expect_error(l1$r(100, 1, 2))
    expect_error(l2$r(100, 1, 2))
    expect_error(l3$r(100, 1, 2))

    # Random number generator
    expect_silent(g1 <- foehnix:::foehnix_gaussian())
    expect_silent(g2 <- foehnix:::foehnix_cgaussian())
    expect_silent(g3 <- foehnix:::foehnix_tgaussian())
    expect_silent(l1 <- foehnix:::foehnix_logistic())
    expect_silent(l2 <- foehnix:::foehnix_clogistic())
    expect_silent(l3 <- foehnix:::foehnix_tlogistic())

    expect_silent(r <- g1$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)
    expect_silent(r <- g2$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)
    expect_silent(r <- g3$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)
    expect_silent(r <- l1$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)
    expect_silent(r <- l2$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)
    expect_silent(r <- l3$r(100, c(5, 10), c(1, 2)))
    expect_length(r, 100); expect_is(r, "numeric"); expect_true(sum(is.na(r)) == 0)


})

test_that("Tests for foehnix family objects (without truncation/censoring points)", {

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


# Left and right censored gaussian distribution
test_that("Tests for censored gaussian family", {

    # Create new family objects.
    expect_silent(left1  <- foehnix:::foehnix_cgaussian(left  = -5))
    expect_silent(left2  <- foehnix:::foehnix_cgaussian(left  =  2))
    expect_silent(right1 <- foehnix:::foehnix_cgaussian(right = -2))
    expect_silent(right2 <- foehnix:::foehnix_cgaussian(right =  5))
    expect_silent(both1  <- foehnix:::foehnix_cgaussian(left = -5, right =  7))
    expect_silent(both2  <- foehnix:::foehnix_cgaussian(left =  0, right =  4))

    # Just checking a flag
    expect_true(left1$censored)

    # Checking left/right
    fams <- list(left1, left2, right1, right2, both1, both2)
    expect_identical(sapply(fams, has.left),     c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
    expect_identical(sapply(fams, has.right),    c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
    expect_identical(sapply(fams, is.truncated), rep(FALSE, 6))

    # Check left/right
    expect_identical(left1[c("left", "right")], list(left = -5, right = Inf))
    expect_identical(right1[c("left", "right")], list(left = -Inf, right = -2))
    expect_identical(both1[c("left", "right")], list(left = -5, right = 7))


    # Left censored at -5
    x <- seq(-8, 8, length = 100); mu <- -4; sigma <- 2
    expect_equal(left1$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, pnorm(-5, mu, sigma), dnorm(x, mu, sigma)))
    expect_equal(left1$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x <= -5, pnorm(-5, mu, sigma, log = TRUE), dnorm(x, mu, sigma, log = TRUE)))

    expect_equal(left1$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, 0, pnorm(x, mu, sigma)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x <= -5, -Inf, pnorm(x, mu, sigma, log.p = TRUE)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x <= -5, 0, pnorm(x, mu, sigma)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x <= -5, 1, pnorm(x, mu, sigma, lower.tail = FALSE)))

    # Left censored at +2
    x <- seq(-8, 8, length = 100); mu <- 1; sigma <- 2
    expect_equal(left2$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= 2, pnorm( 2, mu, sigma), dnorm(x, mu, sigma)))
    expect_equal(left2$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x <= 2, pnorm(2, mu, sigma, log = TRUE), dnorm(x, mu, sigma, log = TRUE)))

    expect_equal(left2$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= 2, 0, pnorm(x, mu, sigma)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x <= 2, -Inf, pnorm(x, mu, sigma, log.p = TRUE)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x <= 2, 0, pnorm(x, mu, sigma)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x <= 2, 1, pnorm(x, mu, sigma, lower.tail = FALSE)))

    # Right censored at -2
    x <- seq(-8, 8, length = 100); mu <- -2; sigma <- 2
    expect_equal(right1$d(x, mu = mu, sigma = sigma),
                 ifelse(x >= -2, pnorm(-2, mu, sigma), dnorm(x, mu, sigma)))
    expect_equal(right1$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x >= -2, pnorm(-2, mu, sigma, log = TRUE), dnorm(x, mu, sigma, log = TRUE)))

    expect_equal(right1$p(x, mu = mu, sigma = sigma),
                 ifelse(x >= -2, 1, pnorm(x, mu, sigma)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x >= -2, 0, pnorm(x, mu, sigma, log.p = TRUE)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x >= -2, 1, pnorm(x, mu, sigma)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x >= -2, 0, pnorm(x, mu, sigma, lower.tail = FALSE)))

                                                               
    # Right censored at 5
    x <- seq(-8, 8, length = 100); mu <- 5; sigma <- 2
    expect_equal(right2$d(x, mu = mu, sigma = sigma),
                 ifelse(x >= 5, pnorm(5, mu, sigma), dnorm(x, mu, sigma)))
    expect_equal(right2$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x >= 5, pnorm(5, mu, sigma, log = TRUE), dnorm(x, mu, sigma, log = TRUE)))

    expect_equal(right2$p(x, mu = mu, sigma = sigma),
                 ifelse(x >= 5, 1, pnorm(x, mu, sigma)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x >= 5, 0, pnorm(x, mu, sigma, log.p = TRUE)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x >= 5, 1, pnorm(x, mu, sigma)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x >= 5, 0, pnorm(x, mu, sigma, lower.tail = FALSE)))

    # Left at -5 and right at 7
    x <- seq(-8, 8, length = 100); mu <- 3; sigma <- 2
    expect_equal(both1$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, pnorm(-5, mu, sigma),
                        ifelse(x >= 7, 1 - pnorm(7, mu, sigma), dnorm(x, mu, sigma))))
    expect_equal(both1$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, 0,
                        ifelse(x >= 7, 1, pnorm(x, mu, sigma))))

    # Left at 0 and right at 4
    x <- seq(-8, 8, length = 100); mu <- 3; sigma <- 2
    expect_equal(both2$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= 0, pnorm(0, mu, sigma),
                        ifelse(x >= 4, 1 - pnorm(4, mu, sigma), dnorm(x, mu, sigma))))
    expect_equal(both2$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= 0, 0,
                        ifelse(x >= 4, 1, pnorm(x, mu, sigma))))

})


# Left and right censored logistic distribution
test_that("Tests for censored gaussian family", {

    # Create new family objects.
    expect_silent(left1  <- foehnix:::foehnix_clogistic(left  = -5))
    expect_silent(left2  <- foehnix:::foehnix_clogistic(left  =  2))
    expect_silent(right1 <- foehnix:::foehnix_clogistic(right = -2))
    expect_silent(right2 <- foehnix:::foehnix_clogistic(right =  5))
    expect_silent(both1  <- foehnix:::foehnix_clogistic(left = -5, right =  7))
    expect_silent(both2  <- foehnix:::foehnix_clogistic(left =  0, right =  4))

    # Just checking a flag
    expect_true(left1$censored)

    # Checking left/right
    fams <- list(left1, left2, right1, right2, both1, both2)
    expect_identical(sapply(fams, has.left),     c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
    expect_identical(sapply(fams, has.right),    c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
    expect_identical(sapply(fams, is.truncated), rep(FALSE, 6))

    # Check left/right
    expect_identical(left1[c("left", "right")], list(left = -5, right = Inf))
    expect_identical(right1[c("left", "right")], list(left = -Inf, right = -2))
    expect_identical(both1[c("left", "right")], list(left = -5, right = 7))


    # Left censored at -5
    x <- seq(-8, 8, length = 100); mu <- -4; sigma <- 2
    expect_equal(left1$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, plogis(-5, mu, sigma), dlogis(x, mu, sigma)))
    expect_equal(left1$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x <= -5, plogis(-5, mu, sigma, log = TRUE), dlogis(x, mu, sigma, log = TRUE)))

    expect_equal(left1$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, 0, plogis(x, mu, sigma)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x <= -5, -Inf, plogis(x, mu, sigma, log.p = TRUE)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x <= -5, 0, plogis(x, mu, sigma)))
    expect_equal(left1$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x <= -5, 1, plogis(x, mu, sigma, lower.tail = FALSE)))

    # Left censored at +2
    x <- seq(-8, 8, length = 100); mu <- 1; sigma <- 2
    expect_equal(left2$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= 2, plogis( 2, mu, sigma), dlogis(x, mu, sigma)))
    expect_equal(left2$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x <= 2, plogis(2, mu, sigma, log = TRUE), dlogis(x, mu, sigma, log = TRUE)))

    expect_equal(left2$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= 2, 0, plogis(x, mu, sigma)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x <= 2, -Inf, plogis(x, mu, sigma, log.p = TRUE)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x <= 2, 0, plogis(x, mu, sigma)))
    expect_equal(left2$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x <= 2, 1, plogis(x, mu, sigma, lower.tail = FALSE)))

    # Right censored at -2
    x <- seq(-8, 8, length = 100); mu <- -2; sigma <- 2
    expect_equal(right1$d(x, mu = mu, sigma = sigma),
                 ifelse(x >= -2, plogis(-2, mu, sigma), dlogis(x, mu, sigma)))
    expect_equal(right1$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x >= -2, plogis(-2, mu, sigma, log = TRUE), dlogis(x, mu, sigma, log = TRUE)))

    expect_equal(right1$p(x, mu = mu, sigma = sigma),
                 ifelse(x >= -2, 1, plogis(x, mu, sigma)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x >= -2, 0, plogis(x, mu, sigma, log.p = TRUE)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x >= -2, 1, plogis(x, mu, sigma)))
    expect_equal(right1$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x >= -2, 0, plogis(x, mu, sigma, lower.tail = FALSE)))

                                                               
    # Right censored at 5
    x <- seq(-8, 8, length = 100); mu <- 5; sigma <- 2
    expect_equal(right2$d(x, mu = mu, sigma = sigma),
                 ifelse(x >= 5, plogis(5, mu, sigma), dlogis(x, mu, sigma)))
    expect_equal(right2$d(x, mu = mu, sigma = sigma, log = TRUE),
                 ifelse(x >= 5, plogis(5, mu, sigma, log = TRUE), dlogis(x, mu, sigma, log = TRUE)))

    expect_equal(right2$p(x, mu = mu, sigma = sigma),
                 ifelse(x >= 5, 1, plogis(x, mu, sigma)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, log.p = TRUE),
                 ifelse(x >= 5, 0, plogis(x, mu, sigma, log.p = TRUE)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, lower.tail = TRUE),
                 ifelse(x >= 5, 1, plogis(x, mu, sigma)))
    expect_equal(right2$p(x, mu = mu, sigma = sigma, lower.tail = FALSE),
                 ifelse(x >= 5, 0, plogis(x, mu, sigma, lower.tail = FALSE)))

    # Left at -5 and right at 7
    x <- seq(-8, 8, length = 100); mu <- 3; sigma <- 2
    expect_equal(both1$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, plogis(-5, mu, sigma),
                        ifelse(x >= 7, 1 - plogis(7, mu, sigma), dlogis(x, mu, sigma))))
    expect_equal(both1$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= -5, 0,
                        ifelse(x >= 7, 1, plogis(x, mu, sigma))))

    # Left at 0 and right at 4
    x <- seq(-8, 8, length = 100); mu <- 3; sigma <- 2
    expect_equal(both2$d(x, mu = mu, sigma = sigma),
                 ifelse(x <= 0, plogis(0, mu, sigma),
                        ifelse(x >= 4, 1 - plogis(4, mu, sigma), dlogis(x, mu, sigma))))
    expect_equal(both2$p(x, mu = mu, sigma = sigma),
                 ifelse(x <= 0, 0,
                        ifelse(x >= 4, 1, plogis(x, mu, sigma))))

})



