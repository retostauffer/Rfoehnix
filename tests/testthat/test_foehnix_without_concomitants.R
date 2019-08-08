

library("foehnix")
library("testthat")
library("zoo")

# Testing a model without concomitants
test_that("Testing foehnix S3 functions (model without concomitants)", {

    # Testdata and test model
    expect_silent(data <- demodata("ellboegen"))
    expect_silent(mod <- foehnix(ff ~ 1, data = data, switch = TRUE,
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

    # return coefficients, parameter scale
    expect_silent(c1 <- coef(mod))
    expect_silent(c2 <- coefficients(mod))
    expect_identical(c1, c2)
    expect_is(c1, c("coef.coehnix", "numeric"))
    expect_identical(length(c1), unname(edf(mod)))
    expect_output(print(c1))
    expect_output(print(c2))

    # return coefficients, coefficient scale
    expect_silent(c1 <- coef(mod, type = "coefficient"))
    expect_silent(c2 <- coefficients(mod, type = "coefficient"))
    expect_identical(c1, c2)
    expect_is(c1, c("coef.coehnix", "numeric"))
    expect_identical(length(c1), unname(edf(mod)))
    expect_output(print(c1))
    expect_output(print(c2))

    # print method(s)
    expect_output(print(mod))
    expect_silent(x <- summary(mod))
    expect_is(x, "summary.foehnix")
    expect_output(print(x))
    expect_silent(x <- summary(mod, detailed = TRUE))
    expect_is(x, "summary.foehnix")
    expect_output(print(x))

    # predict function
    expect_silent(x <- predict(mod))
    expect_is(x, "zoo")
    expect_identical(dim(x), c(nrow(mod$data), 2L))
    expect_identical(names(x), c("prob", "flag"))
    expect_true(all(as.numeric(x) >= 0 & as.numeric(x) <= 1, na.rm = T))

});

