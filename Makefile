
# Reading version number from the package DESCRIPTION file
VERSION := $(shell grep '^Version:' DESCRIPTION | awk '{print $$2}')

# Export user login details, used for testing
test:
	make install
	Rscript -e "library('foehnix'); testthat::test_dir('tests/testthat')"

# testing md5 idea
md5:
	make install
	Rscript -e "library('foehnix'); testthat::test_file('tests/testthat/test-md5.R')"
	Rscript -e "library('foehnix'); testthat::test_file('tests/testthat/test-results.R')"

# Code coverage and report.
# Requires 'covr' to be installed. Saves the coverage summary to '_coverage.rds'.
coverage:
	##make install &&
	Rscript -e "library('covr'); x <- package_coverage(); saveRDS(x, file = '_coverage.rds'); report(x)"
	#Rscript -e "devtools::test_coverage(show_report = TRUE)"

# Build site, create documentation.
document:
	Rscript -e "pkgdown::build_site()"

.PHONY: build install check
build: document
	@echo Building current version: $(VERSION)
	(cd ../ && R CMD build Rfoehnix)
install: build
	@echo Installing current version: $(VERSION)
	(cd ../ && R CMD INSTALL foehnix_$(VERSION).tar.gz)
check: build
	@echo Checking current version: $(VERSION)
	(cd ../ && R CMD check --as-cran foehnix_$(VERSION).tar.gz)


