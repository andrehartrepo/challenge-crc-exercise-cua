# =============================================================================
# test_structural_cure_point.R
# Unit tests for apply_structural_cure_point() and the cure_t_star_years
# pass-through on the three PSM trace builders.
#
# imposed deterministic year-10 structural statistical-cure
#   point, both arms, DFS and OS, excess hazard 0 after t*, Norwegian
#   background mortality thereafter, scenario only with a NULL default.
# Caution: run-gate-tests.R calls test_file(load_helpers = FALSE), so this file
#   sources the model itself, exactly like test_utility_uncertainty.R.
# =============================================================================

Sys.setenv(TESTTHAT = "true")
setwd(normalizePath(file.path("..", ".."), mustWork = TRUE))
source("R/00-parameters.R")
source("R/03-build-psm.R")
source("R/06b-structural-sa.R")

# --- Synthetic fixture --------------------------------------------------------
# Exponential survival makes every interval hazard exact: for S(t) = exp(-r t)
# on a uniform grid, -diff(log(S)) is r * dt at every interval. That lets the
# regime tests assert exact hazard values rather than tolerances.
.cp_dt      <- 1 / 12
.cp_times   <- seq(0, 20, by = .cp_dt)
.cp_tstar   <- 10
.cp_rate_dfs <- 0.08
.cp_rate_os  <- 0.05
.cp_rate_gp  <- 0.02
.cp_s_dfs   <- exp(-.cp_rate_dfs * .cp_times)
.cp_s_os    <- exp(-.cp_rate_os  * .cp_times)
.cp_s_gp    <- exp(-.cp_rate_gp  * .cp_times)
.cp_pre     <- .cp_times[-length(.cp_times)] <  .cp_tstar - 1e-8
.cp_post    <- .cp_times[-length(.cp_times)] >= .cp_tstar - 1e-8

# Deterministic flexsurvreg fixtures for the builder-level tests. No RNG:
# survival times are exponential quantiles on a fixed probability grid, so the
# fits are reproducible across sessions and machines. Cached because four
# tests reuse them.
.cp_fixture_cache <- new.env(parent = emptyenv())
.cp_builder_fixture <- function() {
  if (!is.null(.cp_fixture_cache$fx)) return(.cp_fixture_cache$fx)
  p <- seq(0.005, 0.995, length.out = 200)
  d_dfs <- data.frame(time = qexp(p, rate = 0.10), status = 1)
  d_os  <- data.frame(time = qexp(p, rate = 0.06), status = 1)
  cl <- 1 / 12
  nc <- 240
  tt <- seq(0, nc * cl, by = cl)
  fx <- list(
    fit_dfs = flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1,
                                    data = d_dfs, dist = "exp"),
    fit_os  = flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1,
                                    data = d_os,  dist = "exp"),
    n_cycles = nc,
    cycle_length = cl,
    times = tt,
    s_genpop = exp(-0.02 * tt))
  .cp_fixture_cache$fx <- fx
  fx
}

testthat::test_that("synthetic hazards: hazard_max before t*, background after", {
  res <- apply_structural_cure_point(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                     times = .cp_times,
                                     t_star_years = .cp_tstar)
  h_dfs <- -diff(log(res$s_dfs))
  h_os  <- -diff(log(res$s_os))

  # Strictly before t*: the model hazard exceeds background, so hazard_max
  # keeps the model hazard for both endpoints.
  testthat::expect_equal(h_dfs[.cp_pre],
                         rep(.cp_rate_dfs * .cp_dt, sum(.cp_pre)),
                         tolerance = 1e-12)
  testthat::expect_equal(h_os[.cp_pre],
                         rep(.cp_rate_os * .cp_dt, sum(.cp_pre)),
                         tolerance = 1e-12)

  # At and after t*: excess hazard is exactly 0, background stands alone.
  testthat::expect_equal(h_dfs[.cp_post],
                         rep(.cp_rate_gp * .cp_dt, sum(.cp_post)),
                         tolerance = 1e-12)
  testthat::expect_equal(h_os[.cp_post],
                         rep(.cp_rate_gp * .cp_dt, sum(.cp_post)),
                         tolerance = 1e-12)
  testthat::expect_equal(res$max_abs_post_tstar_excess_hazard, 0,
                         tolerance = 1e-12)
  testthat::expect_identical(res$post_tstar_intervals, sum(.cp_post))
})

testthat::test_that("boundary: survival at exactly t* equals the capped path", {
  res    <- apply_structural_cure_point(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                        times = .cp_times,
                                        t_star_years = .cp_tstar)
  capped <- apply_mortality_cap(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                method = "hazard_max")
  idx <- which(abs(.cp_times - .cp_tstar) < 1e-8)
  testthat::expect_length(idx, 1L)

  # Everything up to and including t* is built from intervals whose LEFT
  # endpoint is below t*, so it must reproduce the current capped path.
  testthat::expect_equal(res$s_dfs[seq_len(idx)], capped$s_dfs[seq_len(idx)],
                         tolerance = 1e-12)
  testthat::expect_equal(res$s_os[seq_len(idx)], capped$s_os[seq_len(idx)],
                         tolerance = 1e-12)
  # After t* the imposed scenario must diverge upward (excess hazard removed).
  n <- length(.cp_times)
  testthat::expect_gt(res$s_os[n],  capped$s_os[n])
  testthat::expect_gt(res$s_dfs[n], capped$s_dfs[n])
})

testthat::test_that("interaction: the helper replaces the cap, floor imposed once", {
  res <- apply_structural_cure_point(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                     times = .cp_times,
                                     t_star_years = .cp_tstar)
  # Re-applying the mortality cap to the helper output changes nothing: the
  # background floor is already satisfied everywhere, so a second imposition
  # would be a no-op here and a double floor anywhere it was not.
  again <- apply_mortality_cap(res$s_dfs, res$s_os, .cp_s_gp,
                               method = "hazard_max")
  testthat::expect_equal(again$s_dfs, res$s_dfs, tolerance = 1e-12)
  testthat::expect_equal(again$s_os,  res$s_os,  tolerance = 1e-12)
  testthat::expect_true(res$background_interaction_pass)

  # Post-t* survival is exactly background-driven: the ratio of successive
  # values equals the background survival ratio, not its square.
  h_os <- -diff(log(res$s_os))
  h_gp <- -diff(log(.cp_s_gp))
  testthat::expect_equal(h_os[.cp_post], h_gp[.cp_post], tolerance = 1e-12)
  testthat::expect_false(isTRUE(all.equal(h_os[.cp_post],
                                          2 * h_gp[.cp_post])))
})

testthat::test_that("both arms receive the imposed cure point", {
  fx <- .cp_builder_fixture()
  tr_ctrl <- build_psm_trace(
    fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
    n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
    s_genpop = fx$s_genpop, arm_label = "Standard Care",
    mortality_method = "hazard_max", cure_t_star_years = .cp_tstar)
  tr_int <- build_psm_trace_from_ctrl(
    fit_dfs_ctrl = fx$fit_dfs, fit_os_ctrl = fx$fit_os,
    hr_dfs = 0.72, hr_os = 0.63,
    n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
    s_genpop = fx$s_genpop, arm_label = "Exercise",
    mortality_method = "hazard_max", cure_t_star_years = .cp_tstar)

  testthat::expect_identical(unique(tr_ctrl$arm), "Standard Care")
  testthat::expect_identical(unique(tr_int$arm),  "Exercise")

  h_gp  <- -diff(log(pmax(fx$s_genpop, 1e-15)))
  post  <- fx$times[-length(fx$times)] >= .cp_tstar - 1e-8
  for (tr in list(tr_ctrl, tr_int)) {
    h_dfs <- -diff(log(pmax(tr$p_dfs, 1e-15)))
    h_os  <- -diff(log(pmax(tr$p_dfs + tr$p_prog, 1e-15)))
    testthat::expect_lt(max(abs(h_dfs[post] - h_gp[post])), 1e-10)
    testthat::expect_lt(max(abs(h_os[post]  - h_gp[post])), 1e-10)
  }
})

testthat::test_that("state validity holds under the imposed cure point", {
  fx <- .cp_builder_fixture()
  tr <- build_psm_trace(
    fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
    n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
    s_genpop = fx$s_genpop, arm_label = "Standard Care",
    mortality_method = "hazard_max", cure_t_star_years = .cp_tstar)

  testthat::expect_true(all(diff(tr$p_dfs) <= 1e-10))       # monotone DFS
  testthat::expect_true(all(diff(tr$p_dead) >= -1e-10))     # dead absorbing
  testthat::expect_true(all(tr$p_dfs >= 0 & tr$p_prog >= 0 & tr$p_dead >= 0))
  testthat::expect_true(all(tr$p_dfs <= tr$p_dfs + tr$p_prog))  # S_DFS <= S_OS
  testthat::expect_lt(max(abs(tr$p_dfs + tr$p_prog + tr$p_dead - 1)), 1e-8)
  # assert_psm_trace() carries the model's own current tolerance.
  testthat::expect_true(assert_psm_trace(tr))
})

testthat::test_that("NULL default leaves all three builders byte-invariant", {
  fx <- .cp_builder_fixture()
  # Caution: identical() is strictly stronger than hash equality; serialize()
  #   equality is asserted alongside it so the claim is byte-level, not
  #   just structural.
  same <- function(a, b) {
    testthat::expect_identical(a, b)
    testthat::expect_identical(serialize(a, NULL, version = 3L),
                               serialize(b, NULL, version = 3L))
  }

  same(build_psm_trace(
         fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Standard Care",
         mortality_method = "hazard_max"),
       build_psm_trace(
         fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Standard Care",
         mortality_method = "hazard_max", cure_t_star_years = NULL))

  same(build_psm_trace_from_ctrl(
         fit_dfs_ctrl = fx$fit_dfs, fit_os_ctrl = fx$fit_os,
         hr_dfs = 0.72, hr_os = 0.63,
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Exercise",
         mortality_method = "hazard_max"),
       build_psm_trace_from_ctrl(
         fit_dfs_ctrl = fx$fit_dfs, fit_os_ctrl = fx$fit_os,
         hr_dfs = 0.72, hr_os = 0.63,
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Exercise",
         mortality_method = "hazard_max", cure_t_star_years = NULL))

  same(build_psm_trace_waning(
         fit_dfs_ctrl = fx$fit_dfs, fit_os_ctrl = fx$fit_os,
         hr_dfs_original = 0.72, hr_os_original = 0.63,
         waning_scenario = "constant",
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Exercise",
         mortality_method = "hazard_max"),
       build_psm_trace_waning(
         fit_dfs_ctrl = fx$fit_dfs, fit_os_ctrl = fx$fit_os,
         hr_dfs_original = 0.72, hr_os_original = 0.63,
         waning_scenario = "constant",
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Exercise",
         mortality_method = "hazard_max", cure_t_star_years = NULL))

  # The NULL path must also reproduce the pre-existing algorithm exactly:
  # summary -> apply_mortality_cap -> the same trace frame.
  s_dfs <- summary(fx$fit_dfs, t = fx$times, type = "survival",
                   ci = FALSE)[[1]]$est
  s_os  <- summary(fx$fit_os,  t = fx$times, type = "survival",
                   ci = FALSE)[[1]]$est
  capped <- apply_mortality_cap(s_dfs, s_os, fx$s_genpop,
                                method = "hazard_max")
  reference <- data.frame(
    arm    = "Standard Care",
    cycle  = seq_along(fx$times) - 1,
    time   = fx$times,
    p_dfs  = capped$s_dfs,
    p_prog = pmax(capped$s_os - capped$s_dfs, 0),
    p_dead = 1 - capped$s_os)
  same(build_psm_trace(
         fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
         n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
         s_genpop = fx$s_genpop, arm_label = "Standard Care",
         mortality_method = "hazard_max"),
       reference)
})

testthat::test_that("fail-closed: every invalid input raises, none coerces", {
  fx <- .cp_builder_fixture()

  # Missing background survival.
  testthat::expect_error(
    apply_structural_cure_point(.cp_s_dfs, .cp_s_os, NULL,
                                times = .cp_times, t_star_years = .cp_tstar),
    "s_genpop is required")
  # Caution: the builders reach the cure branch only inside their existing
  #   `if (!is.null(s_genpop))` block, so the NULL-s_genpop negative is a
  #   helper-contract obligation and is asserted there.

  # t* not on the model time grid.
  testthat::expect_error(
    apply_structural_cure_point(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                times = .cp_times, t_star_years = 10.004),
    "not a point on the model time grid")

  # Wrong mortality method.
  testthat::expect_error(
    build_psm_trace(fit_dfs = fx$fit_dfs, fit_os = fx$fit_os,
                    n_cycles = fx$n_cycles, cycle_length = fx$cycle_length,
                    s_genpop = fx$s_genpop, arm_label = "Standard Care",
                    mortality_method = "pmin",
                    cure_t_star_years = .cp_tstar),
    "requires mortality_method")

  # Unequal vector lengths.
  testthat::expect_error(
    apply_structural_cure_point(.cp_s_dfs[-1], .cp_s_os, .cp_s_gp,
                                times = .cp_times, t_star_years = .cp_tstar),
    "must each")

  # Increasing (non-monotone) survival input.
  bad_os <- .cp_s_os
  bad_os[50] <- bad_os[49] + 0.01
  testthat::expect_error(
    apply_structural_cure_point(.cp_s_dfs, bad_os, .cp_s_gp,
                                times = .cp_times, t_star_years = .cp_tstar),
    "non-increasing")

  # Non-finite survival input.
  bad_dfs <- .cp_s_dfs
  bad_dfs[20] <- NA_real_
  testthat::expect_error(
    apply_structural_cure_point(bad_dfs, .cp_s_os, .cp_s_gp,
                                times = .cp_times, t_star_years = .cp_tstar),
    "non-finite value")

  # Non-strictly-increasing time grid.
  bad_times <- .cp_times
  bad_times[30] <- bad_times[29]
  testthat::expect_error(
    apply_structural_cure_point(.cp_s_dfs, .cp_s_os, .cp_s_gp,
                                times = bad_times, t_star_years = .cp_tstar),
    "strictly increasing")
})
