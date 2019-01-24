

library("foehnix")
library("testthat")
library("zoo")

test_that("Testing foehnix function (basic, S3)", {

    # Loading test data
    expect_silent(data <- demodata())

    # Specify our wind filter rule(s)
    expect_silent(filter <- list(dd = c(43, 223), crest_dd = c(90, 270)))

    # Testing wrong calls
    # Input 'formula' is not a formula
    expect_error(foehnix(3, data, TRUE))
    expect_error(foehnix("wrong formula", data, TRUE))
    # Data ste is not a 'zoo' object
    expect_error(foehnix(ff ~ rh, data = data.frame(ff = c(1,2), rh = c(50,90))))
    expect_error(foehnix(ff ~ rh, data = matrix(rnorm(10), ncol = 2)))
    # Wrong switch argument
    expect_error(foehnix(ff ~ rh, data, switch = "foo"))
    # Wrong filter
    expect_error(foehnix(ff ~ rh, data, filter = "foo"))
    expect_error(foehnix(ff ~ rh, data, filter = c(1,2,3)))
    # Wrong foehnix control
    expect_error(foehnix(ff ~ rh, data, control = "foo"))
    expect_error(foehnix(ff ~ rh, data, control = list(a = 1, b = 2)))
    # Inexistent family
    expect_error(foehnix(ff ~ rh, data, family = "foo family"))
    # Formula contains variables not in 'data'
    expect_error(foehnix("foo ~ bar", data, TRUE))


    # Model: model will be added to the global 
    # environment such that we can use it in the
    # other test_that calls.
    expect_silent(mod <- foehnix(diff_t ~ ff + rh, data = data, switch = TRUE,
                                 filter = filter, verbose = FALSE))
    # When maxit = 1 the algorithm cannot converge. Expect
    # a message to be shown and the foehnix object will be returned.
    expect_warning(x <- foehnix(diff_t ~ ff + rh, data = head(data,10000), 
                                switch = TRUE, filter = filter,
                                verbose = FALSE, maxit = 1))
    expect_is(x, "foehnix")

})


test_that("Testing foehnix S3 functions", {

    # Testdata and test model
    expect_silent(data <- demodata("Ellboegen"))
    expect_silent(mod <- foehnix(ff ~ rh, data = data, switch = TRUE,
                                 filter = list(dd = c(43, 223)), verbose = FALSE))

    # See if we can extract AIC/BIC/logLik
    expect_silent(ll <- logLik(mod))
    expect_silent(edf <- edf(mod))
    expect_silent(AIC <- AIC(mod))
    expect_silent(BIC <- BIC(mod))

    expect_equal(as.numeric(AIC), as.numeric(-2 * ll + 2 * edf))
    expect_equal(as.numeric(BIC), as.numeric(-2 * ll + edf * log(nobs(mod))))

    # edf and number of observations
    expect_silent(x <- c(edf(mod), nobs(mod)))
    expect_is(x, "integer")
    expect_true(length(x) == 2L)

    # AIC, BIC, and loglik
    expect_silent(x <- c(logLik(mod), AIC(mod), BIC(mod)))
    expect_is(x, "numeric")
    expect_true(length(x) == 3L)

    # return formula
    expect_silent(x <- formula(mod))
    expect_is(x, "formula")

    # return coefficients
    expect_silent(c1 <- coef(mod))
    expect_silent(c2 <- coefficients(mod))
    expect_identical(c1, c2)
    expect_is(c1, c("coef.coehnix", "numeric"))
    expect_identical(length(c1), unname(edf(mod)))

    # print method(s)
    expect_output(print(mod))
    expect_silent(x <- summary(mod))
    expect_is(x, "summary.foehnix")
    expect_output(print(x))

    # predict function
    expect_silent(x <- predict(mod))
    expect_is(x, "zoo")
    expect_identical(dim(x), c(nrow(mod$data), 2L))
    expect_identical(names(x), c("prob", "flag"))
    expect_true(all(as.numeric(x) >= 0 & as.numeric(x) <= 1, na.rm = T))

});


test_that("Testing foehnix families (simple test)", {

    # Loading test data
    expect_silent(data <- demodata("Ellboegen"))

    # Gaussian
    expect_output(g1 <- foehnix(ff ~ rh, data = head(data, 1000), family = "gaussian"))
    expect_output(g2 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_gaussian()))
    expect_output(g3 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_cgaussian()))
    expect_output(g4 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_tgaussian()))

    expect_identical(c(AIC(g1), BIC(g1), logLik(g1)), c(AIC(g2), BIC(g2), logLik(g1)))
    expect_identical(c(AIC(g3), BIC(g3), logLik(g3)), c(AIC(g4), BIC(g4), logLik(g4)))
    expect_identical(sapply(list(g1, g2, g3, g4), class), rep("foehnix", 4))

    expect_true(all(fitted(g1) >= 0 & fitted(g1) <= 1))
    expect_true(all(fitted(g2) >= 0 & fitted(g2) <= 1))
    expect_true(all(fitted(g3) >= 0 & fitted(g3) <= 1))
    expect_true(all(fitted(g4) >= 0 & fitted(g4) <= 1))

    # Logistic
    expect_output(l1 <- foehnix(ff ~ rh, data = head(data, 1000), family = "logistic"))
    expect_output(l2 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_logistic()))
    expect_output(l3 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_clogistic()))
    expect_output(l4 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_tlogistic()))

    expect_identical(c(AIC(l1), BIC(l1), logLik(l1)), c(AIC(l2), BIC(l2), logLik(l1)))
    expect_identical(c(AIC(l3), BIC(l3), logLik(l3)), c(AIC(l4), BIC(l4), logLik(l4)))
    expect_identical(sapply(list(l1, l2, l3, l4), class), rep("foehnix", 4))

    expect_true(all(fitted(l1) >= 0 & fitted(l1) <= 1))
    expect_true(all(fitted(l2) >= 0 & fitted(l2) <= 1))
    expect_true(all(fitted(l3) >= 0 & fitted(l3) <= 1))
    expect_true(all(fitted(l4) >= 0 & fitted(l4) <= 1))

});





