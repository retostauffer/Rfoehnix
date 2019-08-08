
library("foehnix")
library("testthat")

# ------------------------------------------------------------
# ------- testing the csv files in data-raw (demodata) -------
# ------------------------------------------------------------
test_that("md5 sums of CSV data sets", {

    test <- list("ellboegen.csv"    = "a48b770830a688cb31dd56b9900bd46a",
                 "sattelberg.csv"   = "2cc74fcb6c02361f0fed26e18be247a2",
                 "viejas.csv"       = "f1a19ec04637fc231bc33b052ca2aeb5",
                 "luckyfive.csv"    = "529ef6465418c45a7656c8dc2c027c73")
    for (file in names(test)) {
        expect_true(tools::md5sum(sprintf("../../data-raw/%s", file)) == test[[file]])
    }

})


# ------------------------------------------------------------
# --------- testing the rda files in data (demodata) ---------
# ------------------------------------------------------------
test_that("md5 sums of CSV data sets", {

    test <- list("ellboegen.rda"    = "9ab817dd24b80297c06d7108c241c45e",
                 "sattelberg.rda"   = "cc65d0e08ab8be6dd882a98d5bf2766b",
                 "viejas.rda"       = "842165bf047d8d8a9a51b039b7dda100",
                 "luckyfive.rda"    = "7c939315ef2fb3b7f01ff9e98d05068c")
    for (file in names(test)) {
        expect_true(tools::md5sum(sprintf("../../data/%s", file)) == test[[file]])
    }

})
