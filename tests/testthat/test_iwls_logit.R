
library("testthat")
library("foehnix")

# ------------------------------------------------------------------
# Testing IWLS logistic regression method
# ------------------------------------------------------------------
test_that("Testing 'iwls_logit' model estimation.", {

    # data set size (size of x, or x-size)
    xs <- c(111L, 6L)

    # Load and prepare demo data set
    expect_silent(data("airquality"))
    expect_identical(dim(airquality <- na.omit(airquality)), xs)
    expect_identical(sum(airquality$Ozone <- as.numeric(airquality$Ozone > 50)), 32)
    
    # Estimate logistic regression model 'out of the box'.
    expect_is(m1 <- glm(Ozone ~ ., data = airquality, family = binomial(link = "logit")), "glm")
    
    # Create model 'by hand' calling the 'iwls_logit' function.
    # Setting up model.frame, response, and model matrix
    mf <- model.frame(Ozone ~ ., data = airquality)
    X  <- model.matrix(Ozone ~ ., data = airquality)
    y  <- model.response(mf)
    expect_is(mf, "data.frame"); expect_identical(dim(mf), xs) 
    expect_is(X, "matrix");      expect_identical(dim(mf), xs)
    expect_is(y, "numeric");     expect_identical(length(y), xs[1L])

    # Catching some errors: data with NA
    Xerr <- X; Xerr[5,] <- NA
    yerr <- y; yerr[10] <- NA
    expect_error(iwls_logit(Xerr, y))
    expect_error(iwls_logit(Xerr, yerr))
    expect_error(iwls_logit(X, yerr))
    # Error: multiple columns with constant values
    Xerr <- X; Xerr[,5] <- 3
    expect_error(iwls_logit(Xerr, y))

    # Default call
    m2 <- iwls_logit(X, y, standardize = FALSE)
    # With standardized coefficients
    m3 <- iwls_logit(X, y, standardize = TRUE)
    # No early stop, stop when maxit = 100 is reached. Will through
    expect_warning(m4 <- iwls_logit(X, y, standardize = TRUE, tol = -Inf, maxit = 100))

    # Summary produces output
    expect_output(summary(m2))
    expect_output(print(m2))

    # Testing S3 methods
    expect_is(logLik(m2), "numeric");   expect_identical(length(logLik(m2)), 1L)
    expect_is(AIC(m2), "numeric");      expect_identical(length(AIC(m2)), 1L)
    expect_is(BIC(m2), "numeric");      expect_identical(length(BIC(m2)), 1L)
    expect_is(edf(m2), "numeric");      expect_identical(length(edf(m2)), 1L)

    # Model list for later
    ml <- list(m2, m3, m4)

    # Converged (m1, m2, m3, should, m4 is not allowed to)?
    expect_true(m1$converged);   expect_true(m2$converged)
    expect_true(m3$converged);   expect_false(m4$converged)

    # Check number of iterations for the IWLS models
    expect_identical(sapply(ml, function(x) x$iterations), c(7L, 7L, 100L))

    # Checking classes
    expect_identical(sapply(ml, class), rep("ccmodel", 3))
    # Make sure we estimated a logistic model
    expect_identical(family(m1)$family, "binomial")

    # Return type of logLik/AIC/BIC/edf
    expect_equal(sapply(ml, function(x) class(logLik(x))), rep("numeric", 3L))
    expect_equal(sapply(ml, function(x) class(AIC(x))),    rep("numeric", 3L))
    expect_equal(sapply(ml, function(x) class(BIC(x))),    rep("numeric", 3L))
    expect_equal(sapply(ml, function(x) class(edf(x))),    rep("numeric", 3L))

    # Value of logLik/AIC/BIC/edf
    expect_equal(sapply(ml, function(x) unname(logLik(x))), rep(unname(logLik(m1)), 3L))
    expect_equal(sapply(ml, function(x) unname(AIC(x))),    rep(unname(AIC(m1)), 3L))
    expect_equal(sapply(ml, function(x) unname(BIC(x))),    rep(unname(BIC(m1)), 3L))
    expect_equal(sapply(ml, function(x) unname(edf(x))),    rep(length(coef(m1)), 3L))

    # Testing names of coefficients
    expect_equal(sapply(ml, function(x) class(coef(x))), rep("numeric", 3L))
    expect_equal(sapply(ml, function(x) length(coef(x))), rep(length(coef(m1)), 3L))
    for ( i in seq.int(length(coef(m1))) ) {
        expect_match(names(coef(m2))[i], names(coef(m1))[i])
        expect_match(names(coef(m3))[i], names(coef(m1))[i])
        expect_match(names(coef(m4))[i], names(coef(m1))[i])
    }

    # Checking coefficients with a specific tolerance of 1e-4
    expect_equal(as.numeric(m2$coef), as.numeric(m1$coef), tolerance = 1e-4)
    expect_equal(as.numeric(m2$coef), as.numeric(m3$coef), tolerance = 1e-4)
    expect_equal(as.numeric(m2$coef), as.numeric(m4$coef), tolerance = 1e-4)

    # Checking standardized coefficients.
    # m2 uses standardized = FALSE, thus m2$coef must be m2$beta
    expect_equal(as.numeric(m2$beta), as.numeric(m2$coef))
    # m2$beta must also be m1$coef
    expect_equal(as.numeric(m2$beta), as.numeric(m1$coef), tolerance = 1e-4)
    # m3$beta musst be m4$beta
    expect_equal(as.numeric(m3$beta), as.numeric(m4$beta), tolerance = 1e-4)
    
    # Checking estimated standard deviation of the regression coefficients
    expect_equal(as.numeric(m2$beta.se), as.numeric(m3$beta.se), tolerance = 1e-4)
    expect_equal(as.numeric(m2$beta.se), as.numeric(m4$beta.se), tolerance = 1e-4)

})



