

library("foehnix")
library("testthat")
library("zoo")

test_that("Testing foehnix function (main calls)", {

    # Loading test data
    expect_silent(data <- demodata("ellboegen"))
    expect_silent(data <- demodata("sattelberg"))
    expect_silent(data <- demodata("tyrol"))

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

