

library("testthat")
library("foehnix")

# Testing wind function uv2ddff (data.frame)
test_that("Testing uv2ddff foehnix wind conversion support functions (data.frame)", {

    # Testing uv -> ddff, data.frame
    uv <- data.frame(u = c(-1, 0, 1, 0), v = c(0, 1, 0, -1))
    expect_silent(ddff <- uv2ddff(uv))
    expect_is(ddff, "data.frame")
    expect_identical(names(ddff), c("dd", "ff"))

    # Capture some errors
    expect_error(uv2ddff(u = as.numeric(uv$u)))
    expect_error(uv2ddff(v = as.numeric(uv$v)))
    expect_error(uv2ddff(c(1,2,3), c(1,2)))
    expect_error(uv2ddff(c(1,2), c(1,2,3)))

    # Input as vector, or data.frame
    expect_identical(uv2ddff(as.numeric(uv$u), as.numeric(uv$v)),         ddff)
    expect_identical(uv2ddff(u = as.numeric(uv$u), v = as.numeric(uv$v)), ddff)
    expect_identical(uv2ddff(v = as.numeric(uv$v), u = as.numeric(uv$u)), ddff)

    # Unnamed matrix
    tmp <- as.matrix(uv); colnames(tmp) <- NULL
    expect_identical(uv2ddff(uv),            ddff)

    # Should work
    expect_is(ddff2uv(c(90, 180, 270), 5), "data.frame")
    expect_is(ddff2uv(90, c(0, 5, 10)), "data.frame")

    # Misspecified input
    expect_error(uv2ddff(data.frame(u = 1)))
    expect_error(uv2ddff(data.frame(aa = 1, bb = 2)))
    expect_error(uv2ddff(u = c(1,2)))
    expect_error(uv2ddff(u = c(1,2), v = c(1,2,3,5)))
    expect_error(uv2ddff(v = c(1,2)))
    expect_error(uv2ddff(u = c(1,2,3,5), v = c(1,2)))

    # Works as u/v/ will be recycled
    expect_silent(uv2ddff(u = c(1,2,3,5), v = 1))
    expect_silent(uv2ddff(v = c(1,2,3,5), u = 1))

    # Checking return
    expect_equal(ddff$dd, c(90, 180, 270, 360))
    expect_equal(ddff$ff, rep(1, 4))

})

# Testing wind function uv2ddff (zoo)
test_that("Testing uv2ddff foehnix wind conversion support functions (zoo)", {

    # Testing uv -> ddff, data.frame
    uv <- zoo(data.frame(u = c(-1, 0, 1, 0), v = c(0, 1, 0, -1)), 11:14)
    expect_silent(ddff <- uv2ddff(uv))
    expect_is(ddff, "zoo")
    expect_identical(names(ddff), c("dd", "ff"))

    # Capture some errors
    expect_warning(uv2ddff(uv, v = c(1, 2, 3, 4)))
    expect_error(uv2ddff(uv$u, uv$v))
    tmp <- uv; names(tmp)[1L] <- "foo"
    expect_error(uv2ddff(tmp))
    expect_identical(uv2ddff(zoo::zoo(uv)), ddff)

})


# Testing wind function uv2ddff 
test_that("Testing ddff2uv foehnix wind conversion support functions", {

    # Testing ddff -> uv
    ddff <- data.frame(dd = c(0, 90, 180, 270, 360), ff = rep(1, 5))
    expect_silent(uv <- ddff2uv(ddff))
    expect_is(uv, "data.frame")
    expect_identical(names(uv), c("u", "v", "rad"))

    # Capture some errors
    expect_error(ddff2uv(dd = as.numeric(ddff$dd)))
    expect_error(ddff2uv(ff = as.numeric(ddff$dd)))
    expect_error(uv2ddff(c(1,2,3), c(1,2)))
    expect_error(uv2ddff(c(1,2), c(1,2,3)))

    # Should work
    expect_is(uv2ddff(c(-1, 0, 1), 1), "data.frame")
    expect_is(uv2ddff(1, c(-1, 0, 1)), "data.frame")

    # Input as vector, data.frame, or zoo
    expect_identical(ddff2uv(as.numeric(ddff$dd), as.numeric(ddff$ff)),           uv)
    expect_identical(ddff2uv(dd = as.numeric(ddff$dd), ff = as.numeric(ddff$ff)), uv)
    expect_identical(ddff2uv(ff = as.numeric(ddff$ff), dd = as.numeric(ddff$dd)), uv)
    expect_identical(ddff2uv(zoo::zoo(ddff)), zoo::zoo(uv))

    # Unnamed matrix
    tmp <- as.matrix(ddff); colnames(tmp) <- NULL
    expect_identical(ddff2uv(tmp),            uv)

    # Misspecified input
    expect_error(ddff2uv(data.frame(dd = 1)))
    expect_error(ddff2uv(data.frame(aa = 1, bb = 2)))
    expect_error(ddff2uv(u = 1))
    expect_error(ddff2uv(u = 1, v = c(1,2)))
    expect_error(ddff2uv(v = 1))
    expect_error(ddff2uv(u = c(1,2), v = 1))

    # Checking return
    expect_equal(uv$u, c(0, -1, 0, 1, 0))
    expect_equal(uv$v, c(-1, 0, 1, 0, -1))

})

# Testing wind function uv2ddff (zoo)
test_that("Testing ddff2uv foehnix wind conversion support functions (zoo)", {

    # Testing uv -> ddff, data.frame
    ddff <- zoo(data.frame(dd = c(0, 90, 180, 270, 360), ff = rep(1, 5)), 11:15)
    expect_silent(uv <- ddff2uv(ddff))
    expect_is(uv, "zoo")
    expect_identical(names(uv), c("u", "v", "rad"))

    # Capture some errors
    expect_error(ddff2uv(uv, dd = c(1, 2, 3, 4)))
    expect_error(ddff2uv(uv$u, uv$v))
    tmp <- uv; names(tmp)[1L] <- "foo"
    expect_error(uv2ddff(tmp))
    expect_identical(ddff2uv(zoo::zoo(ddff)), uv)

})
