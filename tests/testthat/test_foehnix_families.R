

library("foehnix")
library("testthat")
library("zoo")


test_that("Testing foehnix families (simple test)", {

    # Loading test data
    expect_silent(data <- demodata("ellboegen"))

    # Gaussian
    expect_output(g1 <- foehnix(ff ~ rh, data = head(data, 1000), family = "gaussian"))
    expect_output(g2 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_gaussian()))
    expect_output(g3 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_cgaussian()))
    expect_output(g4 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_tgaussian()))

    expect_identical(c(AIC(g1), BIC(g1), logLik(g1)), c(AIC(g2), BIC(g2), logLik(g1)))
    expect_identical(c(AIC(g3), BIC(g3), logLik(g3)), c(AIC(g4), BIC(g4), logLik(g4)))
    expect_identical(sapply(list(g1, g2, g3, g4), class), rep("foehnix", 4))

    expect_true(all(is.na(fitted(g1)) | (fitted(g1) >= 0 & fitted(g1) <= 1)))
    expect_true(all(is.na(fitted(g2)) | (fitted(g2) >= 0 & fitted(g2) <= 1)))
    expect_true(all(is.na(fitted(g3)) | (fitted(g3) >= 0 & fitted(g3) <= 1)))
    expect_true(all(is.na(fitted(g4)) | (fitted(g4) >= 0 & fitted(g4) <= 1)))


    # Logistic
    expect_output(l1 <- foehnix(ff ~ rh, data = head(data, 1000), family = "logistic"))
    expect_output(l2 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_logistic()))
    expect_output(l3 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_clogistic()))
    expect_output(l4 <- foehnix(ff ~ rh, data = head(data, 1000), family = foehnix:::foehnix_tlogistic()))

    expect_identical(c(AIC(l1), BIC(l1), logLik(l1)), c(AIC(l2), BIC(l2), logLik(l1)))
    expect_identical(c(AIC(l3), BIC(l3), logLik(l3)), c(AIC(l4), BIC(l4), logLik(l4)))
    expect_identical(sapply(list(l1, l2, l3, l4), class), rep("foehnix", 4))

    expect_true(all(is.na(fitted(l1)) | (fitted(l1) >= 0 & fitted(l1) <= 1)))
    expect_true(all(is.na(fitted(l1)) | (fitted(l2) >= 0 & fitted(l2) <= 1)))
    expect_true(all(is.na(fitted(l1)) | (fitted(l3) >= 0 & fitted(l3) <= 1)))
    expect_true(all(is.na(fitted(l1)) | (fitted(l4) >= 0 & fitted(l4) <= 1)))

    # An NA in the probabilities is only possible if we have at least one
    # missing value in the data!
    check_NA <- function(mod) {
        fn <- function(x) any(is.na(x))
        all(apply(coredata(mod$data), 1, fn)[is.na(fitted(mod))])
    }
    expect_true(check_NA(g1))
    expect_true(check_NA(g2))
    expect_true(check_NA(g3))
    expect_true(check_NA(g4))

    expect_true(check_NA(l1))
    expect_true(check_NA(l2))
    expect_true(check_NA(l3))
    expect_true(check_NA(l4))

});






