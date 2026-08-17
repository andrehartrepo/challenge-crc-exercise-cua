# Run testthat tests for the CRC exercise CUA model
# Usage: testthat::test_dir("tests/testthat")
#   or:  source("tests/testthat.R")

library(testthat)

# Source ALL R files that define functions used in tests.
# Previously only sourced 00-parameters.R and 06b-structural-sa.R,
# meaning tests for functions in 03-build-psm.R or 04-costs-qalys.R
# would fail because those functions were not in scope.
# Added 01-digitize-km.R and 02-fit-survival.R
# so tests for functions defined there (fit_distributions, compare_aic_bic)
# will work when written.
source("R/00-parameters.R")
source("R/01-digitize-km.R")
source("R/02-fit-survival.R")
source("R/03-build-psm.R")
source("R/04-costs-qalys.R")
source("R/05-run-model.R")
source("R/06-psa.R")
source("R/06b-structural-sa.R")

# Run all tests
test_dir("tests/testthat")
