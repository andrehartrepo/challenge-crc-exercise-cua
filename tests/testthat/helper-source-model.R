# =============================================================================
# helper-source-model.R
# Automatically sourced by testthat::test_dir() before running tests.
# Sources all R model files so functions are in scope for test files.
#
# This helper ensures testthat::test_dir() works
# the same way as source('tests/testthat.R'). Previously, test_dir() failed
# because functions were not in scope (no helper file existed).
# =============================================================================

Sys.setenv(TESTTHAT = "true")

# Determine project root (the repository root) from test dir (tests/testthat/)
project_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
if (!file.exists(file.path(project_root, "R", "00-parameters.R"))) {
  stop("helper-source-model.R: expected the repository root at ", project_root,
       " but R/00-parameters.R is missing; helper was sourced with wd = ",
       getwd(), " instead of tests/testthat/")
}

# TRAP: model files read data with wd-relative paths (e.g. 00-parameters.R
# reads data/raw/dmp-eq5d5l-norms-current.csv), but testthat sources this
# helper with wd = tests/testthat/. Move to the project root before
# sourcing, mirroring the setwd contract used by the test files that
# source model code directly (e.g. test_cost_component_ownership.R line 2).
# testthat re-asserts the test-dir wd before each test file, so no restore
# is needed (probe-verified, testthat 3.3.2).
setwd(project_root)

# Source all R model files in dependency order
source(file.path(project_root, "R", "00-parameters.R"))
source(file.path(project_root, "R", "01-digitize-km.R"))
source(file.path(project_root, "R", "02-fit-survival.R"))
source(file.path(project_root, "R", "03-build-psm.R"))
source(file.path(project_root, "R", "04-costs-qalys.R"))
source(file.path(project_root, "R", "05-run-model.R"))
source(file.path(project_root, "R", "06-psa.R"))
source(file.path(project_root, "R", "06b-structural-sa.R"))
