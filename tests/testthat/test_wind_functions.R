

library("testthat")
library("foehnix")

# Testing wind function uv2ddff 
test_that("Testing uv2ddff foehnix wind conversion support functions", {

    # Testing uv -> ddff
    uv <- data.frame(u = c(-1, 0, 1, 0), v = c(0, 1, 0, -1))
    expect_silent(ddff <- uv2ddff(uv))
    expect_is(ddff, "data.frame")
    expect_identical(names(ddff), c("dd", "ff"))

    expect_identical(uv2ddff(as.numeric(uv$u), as.numeric(uv$v)),         ddff)
    expect_identical(uv2ddff(u = as.numeric(uv$u), v = as.numeric(uv$v)), ddff)
    expect_identical(uv2ddff(v = as.numeric(uv$v), u = as.numeric(uv$u)), ddff)
    expect_identical(uv2ddff(zoo::zoo(uv, seq.int(nrow(uv)))),            ddff)

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

# Testing wind function uv2ddff 
test_that("Testing ddff2uv foehnix wind conversion support functions", {

    # Testing ddff -> uv
    ddff <- data.frame(dd = c(0, 90, 180, 270, 360), ff = rep(1, 5))
    expect_silent(uv <- ddff2uv(ddff))
    expect_is(uv, "data.frame")
    expect_identical(names(uv), c("u", "v", "rad"))

    expect_identical(ddff2uv(as.numeric(ddff$dd), as.numeric(ddff$ff)),           uv)
    expect_identical(ddff2uv(dd = as.numeric(ddff$dd), ff = as.numeric(ddff$ff)), uv)
    expect_identical(ddff2uv(ff = as.numeric(ddff$ff), dd = as.numeric(ddff$dd)), uv)
    expect_identical(ddff2uv(zoo::zoo(ddff, seq.int(nrow(ddff)))),        uv)

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
