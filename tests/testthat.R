# Run all unit tests


# Test filter
filter <- "[^(md5)]"

#files <- list.files("testthat")
#files <- gsub("(test-|\\.R)", "", files)
#take  <- files[grep(filter, files)]
#print(c(files = length(files), take = length(take)))

# This is a wrapper around all the tests specified
# in the tests/testthat directory. These tests are visible
# to users and serve as small examples as well.
testthat::test_check("foehnix", filter = filter)
