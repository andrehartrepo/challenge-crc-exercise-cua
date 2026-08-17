Sys.setenv(TESTTHAT = "true")
setwd(normalizePath(file.path("..", ".."), mustWork = TRUE))
source("R/00-parameters.R")

testthat::test_that("progressed utility mapping arithmetic is exact", {
  testthat::expect_equal(u_prog_mean, 0.742726, tolerance = 1e-15)
  testthat::expect_equal(
    u_prog_sampling_se, 0.773 * (0.298 / sqrt(57)), tolerance = 1e-15)
  testthat::expect_equal(u_prog_powsa_bounds,
                         c(0.683205, 0.806885), tolerance = 1e-15)
  testthat::expect_equal(
    u_prog_se_mapping_sensitivity,
    sqrt(u_prog_sampling_se^2 + 0.00764^2), tolerance = 1e-15)
})

testthat::test_that("reference beta uses target-scale mean and sampling SE", {
  shape <- beta_params(u_prog_mean, u_prog_sampling_se)
  implied_mean <- shape$alpha / (shape$alpha + shape$beta)
  implied_var <- shape$alpha * shape$beta /
    ((shape$alpha + shape$beta)^2 * (shape$alpha + shape$beta + 1))
  testthat::expect_equal(implied_mean, u_prog_mean, tolerance = 1e-14)
  testthat::expect_equal(sqrt(implied_var), u_prog_sampling_se,
                         tolerance = 1e-14)
})

testthat::test_that("mapping sensitivity changes dispersion only", {
  testthat::expect_equal(rho_endpoint_latent_reference, 0)
  testthat::expect_gt(u_prog_se_mapping_sensitivity, u_prog_sampling_se)
  testthat::expect_identical(u_prog_mean,
                            u_prog_mapping_intercept +
                              u_prog_mapping_slope * u_prog_3l_mean)
  source_text <- paste(readLines("R/00-parameters.R", warn = FALSE),
                       collapse = "\n")
  testthat::expect_false(grepl("coefficient covariance|mean \\+/- RMSE",
                               source_text, ignore.case = TRUE))
})

testthat::test_that("exercise disutility is fixed and not sampled", {
  testthat::expect_identical(unique(rep(u_exercise_decrement, 100L)), 0)
  testthat::expect_false(exists("u_exercise_decrement_se", inherits = FALSE))
})
