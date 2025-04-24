library(testthat)
library(dafr)


# Skip tests on CRAN because we need Julia
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    setup_daf()
    test_check("dafr")
}
