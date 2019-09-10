

library("foehnix")
library("testthat")

test_that("Testing results for models, Tyrolean data set", {

    # MD5 only works on linux (windows calculates the hash different)
    if (.Platform$OS.type == "unix") {
        # Loading test data
        expect_silent(data_ell   <- demodata("ellboegen"))
        expect_silent(data_tyrol <- demodata("tyrol"))

        # Specify our wind filter rule(s)
        expect_silent(filter_ell   <- list(dd = c(43, 223)))
        expect_silent(filter_tyrol <- list(dd = c(43, 223), crest_dd = c(90, 270)))

        # Temporary file to store the results
        tmpfile <- tempfile()

        expect_silent(ell1 <- foehnix(ff ~ 1, data = data_ell, filter = filter_ell, verbose = FALSE))
        expect_silent(write.csv(ell1, file = tmpfile, info = FALSE))
        #print(tools::md5sum(tmpfile))
        expect_true(tools::md5sum(tmpfile) == "02d5c0c4ab6afafb27e178977e89fe1b")

        expect_silent(ell2 <- foehnix(ff ~ rh, data = data_ell, filter = filter_ell, verbose = FALSE))
        expect_silent(write.csv(ell2, file = tmpfile, info = FALSE))
        #print(tools::md5sum(tmpfile))
        expect_true(tools::md5sum(tmpfile) == "1807cfb60f0b28438ab0fe80621a8ca1")

        expect_silent(tyr1 <- foehnix(ff ~ 1, data = data_tyrol, filter = filter_tyrol, verbose = FALSE))
        expect_silent(write.csv(tyr1, file = tmpfile, info = FALSE))
        #print(tools::md5sum(tmpfile))
        expect_true(tools::md5sum(tmpfile) == "0825a50c768fca512590f1ce49d0f37d")

        expect_silent(tyr2 <- foehnix(ff ~ 1, data = data_tyrol, filter = filter_tyrol, verbose = FALSE))
        expect_silent(write.csv(tyr2, file = tmpfile, info = FALSE))
        #print(tools::md5sum(tmpfile))
        expect_true(tools::md5sum(tmpfile) == "0825a50c768fca512590f1ce49d0f37d")
    }

})

