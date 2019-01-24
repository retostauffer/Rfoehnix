
library("foehnix")
library("testthat")

test_that("Testing time series image plot", {

    # Loading test data
    expect_silent(data <- demodata())

    # Specify our wind filter rule(s)
    expect_silent(filter <- list(dd = c(43, 223), crest_dd = c(90, 270)))

    # Model: model will be added to the global 
    # environment such that we can use it in the
    # other test_that calls.
    expect_silent(mod <- foehnix(diff_t ~ ff + rh, data = data, switch = TRUE,
                                 filter = filter, verbose = FALSE))
    expect_is(mod, "foehnix")


})
