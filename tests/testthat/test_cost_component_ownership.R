Sys.setenv(TESTTHAT = "true")
setwd(normalizePath(file.path("..", ".."), mustWork = TRUE))
source("R/00-parameters.R")
source("R/04-costs-qalys.R")
source("R/06-psa.R")

testthat::test_that("schedule boundaries and arm ownership are exact", {
  months <- c(1, 36, 37, 60, 61)
  exercise <- dfs_annual_cost_components(
    months / 12, 5060, 0, 5, 1000, 3)
  control <- dfs_annual_cost_components(
    months / 12, 5060, 0, 5, 0, 0)
  testthat::expect_identical(exercise$surveillance, control$surveillance)
  testthat::expect_true(all(exercise$exercise[months <= 36] > 0))
  testthat::expect_true(all(exercise$exercise[months >= 37] == 0))
  testthat::expect_identical(exercise$surveillance[months == 61], 0)
  testthat::expect_identical(control$surveillance[months == 61], 0)
})

# SOURCE: component ownership and Table-2 reporting contract.
# DECISION: decomposition is valid only when components reproduce existing totals.
testthat::test_that("reporting decomposition preserves totals and PSA arithmetic", {
  times <- 0:2
  trace <- rbind(
    data.frame(arm = "Exercise", time = times,
               p_dfs = c(1, 0.80, 0.60),
               p_prog = c(0, 0.10, 0.20),
               p_dead = c(0, 0.10, 0.20)),
    data.frame(arm = "Standard Care", time = times,
               p_dfs = c(1, 0.75, 0.50),
               p_prog = c(0, 0.15, 0.25),
               p_dead = c(0, 0.10, 0.25))
  )
  fixed_draw <- data.frame(
    u_dfs = 0.82, u_prog = 0.70,
    c_surveillance_early = 100, c_surveillance_late = 20,
    surveillance_cutoff_years = 1,
    c_exercise_annual = 50, intervention_duration_years = 1,
    c_progressed_annual = 300, c_terminal = 400,
    c_intervention_setup = 25, wtp = 385000
  )
  calculate_arm <- function(arm) {
    calculate_costs_qalys(
      trace, arm, fixed_draw$u_dfs, fixed_draw$u_prog, 0,
      c_surveillance_early = fixed_draw$c_surveillance_early,
      c_surveillance_late = fixed_draw$c_surveillance_late,
      surveillance_cutoff_years = fixed_draw$surveillance_cutoff_years,
      c_exercise_annual = if (arm == "Exercise") {
        fixed_draw$c_exercise_annual
      } else 0,
      intervention_duration_years = if (arm == "Exercise") {
        fixed_draw$intervention_duration_years
      } else 0,
      c_progressed_annual = fixed_draw$c_progressed_annual,
      c_terminal = fixed_draw$c_terminal,
      c_intervention_setup = if (arm == "Exercise") {
        fixed_draw$c_intervention_setup
      } else 0,
      discount_weights = c(1, 0.95, 0.90), cycle_length = 1,
      validate = FALSE
    )
  }
  exercise <- calculate_arm("Exercise")
  control <- calculate_arm("Standard Care")

  raw_names <- c("cost_surveillance_raw", "cost_exercise_raw",
                 "cost_progressed_raw", "cost_terminal_raw")
  disc_names <- c("cost_surveillance_disc", "cost_exercise_disc",
                  "cost_progressed_disc", "cost_terminal_disc")
  for (arm_result in list(exercise, control)) {
    testthat::expect_equal(rowSums(arm_result[raw_names]),
                           arm_result$costs_raw, tolerance = 1e-8)
    testthat::expect_equal(rowSums(arm_result[disc_names]),
                           arm_result$costs_disc, tolerance = 1e-8)
  }
  testthat::expect_true(all(control$cost_exercise_raw == 0))
  recurring_exercise <- exercise$eff_p_dfs * exercise$exercise
  testthat::expect_equal(
    sum(exercise$cost_exercise_raw - recurring_exercise),
    fixed_draw$c_intervention_setup, tolerance = 1e-8)

  # SOURCE: Bjornelv 2020 Table 1 (printed p. 3) row "Total costs" makes
  #   c_terminal all-inclusive for the decedent's last month, so the
  #   decedents' trapezoidal half-cycle is netted out of the two occupancies
  #   that carry registry-scope costs (04-costs-qalys.R death-cycle block).
  # Caution: surveillance and exercise no longer share one occupancy multiplier.
  #   Exercise is NOT netted (patient travel and patient time sit outside
  #   Bjornelv's registry scope), so the two terms must stay separate here or
  #   this independent recomputation stops pinning the corrected accrual.
  legacy_costs <- function(arm_result, arm_trace, setup) {
    overlap <- pmax(diff(arm_trace$p_dead), 0) / 2
    alive <- arm_result$eff_p_dfs + arm_result$eff_p_prog
    overlap_dfs <- overlap *
      ifelse(alive > 0, arm_result$eff_p_dfs / alive, 0)
    raw <- (arm_result$eff_p_dfs - overlap_dfs) * arm_result$surveillance +
      arm_result$eff_p_dfs * arm_result$exercise +
      (arm_result$eff_p_prog - (overlap - overlap_dfs)) *
        fixed_draw$c_progressed_annual + setup
    raw + pmax(diff(arm_trace$p_dead), 0) * fixed_draw$c_terminal
  }
  legacy_exercise_raw <- legacy_costs(
    exercise, trace[trace$arm == "Exercise", ],
    c(fixed_draw$c_intervention_setup, 0))
  legacy_control_raw <- legacy_costs(
    control, trace[trace$arm == "Standard Care", ], c(0, 0))
  legacy <- data.frame(
    total_costs_int = sum(legacy_exercise_raw * c(0.95, 0.90)),
    total_costs_ctrl = sum(legacy_control_raw * c(0.95, 0.90)),
    total_qalys_int = sum(exercise$qalys_disc),
    total_qalys_ctrl = sum(control$qalys_disc)
  )
  legacy$inc_costs <- legacy$total_costs_int - legacy$total_costs_ctrl
  legacy$inc_qalys <- legacy$total_qalys_int - legacy$total_qalys_ctrl
  legacy$icer <- legacy$inc_costs / legacy$inc_qalys
  legacy$inb <- legacy$inc_qalys * fixed_draw$wtp - legacy$inc_costs

  psa <- data.frame(
    iteration = 1L,
    inc_costs = sum(exercise$costs_disc) - sum(control$costs_disc),
    inc_qalys = sum(exercise$qalys_disc) - sum(control$qalys_disc),
    icer = NA_real_, inb = NA_real_,
    total_costs_int = sum(exercise$costs_disc),
    total_costs_ctrl = sum(control$costs_disc),
    total_qalys_int = sum(exercise$qalys_disc),
    total_qalys_ctrl = sum(control$qalys_disc),
    cost_surveillance_int = sum(exercise$cost_surveillance_disc),
    cost_surveillance_ctrl = sum(control$cost_surveillance_disc),
    cost_exercise_int = sum(exercise$cost_exercise_disc),
    cost_exercise_ctrl = sum(control$cost_exercise_disc),
    cost_progressed_int = sum(exercise$cost_progressed_disc),
    cost_progressed_ctrl = sum(control$cost_progressed_disc),
    cost_terminal_int = sum(exercise$cost_terminal_disc),
    cost_terminal_ctrl = sum(control$cost_terminal_disc)
  )
  psa$icer <- psa$inc_costs / psa$inc_qalys
  psa$inb <- psa$inc_qalys * fixed_draw$wtp - psa$inc_costs
  old_names <- c("inc_costs", "inc_qalys", "icer", "inb",
                 "total_costs_int", "total_costs_ctrl",
                 "total_qalys_int", "total_qalys_ctrl")
  testthat::expect_equal(psa[old_names], legacy[old_names], tolerance = 1e-8)
  for (suffix in c("int", "ctrl")) {
    components <- rowSums(psa[paste0(
      c("cost_surveillance_", "cost_exercise_", "cost_progressed_",
        "cost_terminal_"), suffix)])
    testthat::expect_equal(components, psa[[paste0("total_costs_", suffix)]],
                           tolerance = 1e-8)
  }
  summary <- summarise_psa(psa, wtp = fixed_draw$wtp)
  for (component in c("surveillance", "exercise", "progressed", "terminal")) {
    incremental <- summary[[paste0("expected_cost_", component, "_int")]] -
      summary[[paste0("expected_cost_", component, "_ctrl")]]
    testthat::expect_equal(
      incremental,
      mean(psa[[paste0("cost_", component, "_int")]] -
             psa[[paste0("cost_", component, "_ctrl")]]),
      tolerance = 1e-8)
  }
})

testthat::test_that("production trace uses cycle-end schedule", {
  times <- seq(0, 5.25, by = 0.25)
  trace <- data.frame(arm = "Exercise", time = times,
                      p_dfs = 1, p_prog = 0, p_dead = 0)
  out <- calculate_costs_qalys(
    trace, "Exercise", 0.82, 0.742726, 0,
    c_surveillance_early = 5060, c_surveillance_late = 0,
    surveillance_cutoff_years = 5,
    c_exercise_annual = 1000, intervention_duration_years = 3,
    c_progressed_annual = 84000, discount_weights = rep(1, nrow(trace)),
    cycle_length = 0.25, validate = FALSE)
  components <- out[, c("time_end", "surveillance", "exercise", "total")]
  testthat::expect_identical(names(components),
                            c("time_end", "surveillance", "exercise", "total"))
  testthat::expect_gt(components$exercise[components$time_end == 3], 0)
  testthat::expect_identical(components$exercise[components$time_end == 3.25], 0)
  testthat::expect_gt(components$surveillance[components$time_end == 5], 0)
  testthat::expect_identical(
    components$surveillance[components$time_end == 5.25], 0)
})

testthat::test_that("POWSA registry has eight component-owned rows", {
  registry <- data.frame(
    param = c("HR_DFS", "HR_OS", "u_dfs", "u_prog",
              "c_surveillance_early", "c_exercise_annual",
              "c_progressed_annual", "c_terminal"),
    low = c(HR_DFS_lower, HR_OS_lower, u_dfs_powsa_low, u_prog_powsa_low,
            powsa_c_surveillance_low, powsa_c_exercise_low,
            powsa_c_progressed_low, powsa_c_terminal_low),
    high = c(HR_DFS_upper, HR_OS_upper, u_dfs_powsa_high, u_prog_powsa_high,
             powsa_c_surveillance_high, powsa_c_exercise_high,
             powsa_c_progressed_high, powsa_c_terminal_high))
  testthat::expect_identical(nrow(registry), 8L)
  testthat::expect_identical(2L * nrow(registry), 16L)
  surveillance <- registry[registry$param == "c_surveillance_early", ]
  # SOURCE: surveillance re-anchor, c_surveillance_early = 5802 NOK/yr
  # (29,008.80 over 5 years), the amortised bundle carrying the DRG 710O
  # colonoscopy tariff of 3,814 NOK; bounds x0.40/x1.60 per
  # R/00-parameters.R section 6e. Expression form keeps the comparison
  # bitwise-identical to the production arithmetic while pinning an
  # independent literal base as the drift guardrail.
  # Caution: this literal is deliberately NOT c_surveillance_early. It is a
  # hand-carried duplicate whose whole purpose is to fail when the
  # production value moves, so it must be re-derived by hand, never
  # replaced by the symbol it guards.
  testthat::expect_identical(c(surveillance$low, surveillance$high),
                            c(5802 * 0.40, 5802 * 1.60))
})

testthat::test_that("retired bundled cost identifiers are absent from active code", {
  files <- c("main.R", sprintf("R/%s", c(
    "00-parameters.R", "03-build-psm.R", "04-costs-qalys.R", "06-psa.R",
    "06b-structural-sa.R", "07-visualization.R", "08-export-tables.R",
    "09-export-latex-commands.R")))
  active <- paste(unlist(lapply(files, readLines, warn = FALSE),
                         use.names = FALSE), collapse = "\n")
  retired <- paste(c(
    "c_dfs_intervention", "c_dfs_control", "c_dfs_intervention_standard",
    "c_dfs_int_se", "c_dfs_ctrl_se", "c_followup_early_int",
    "c_followup_early_ctrl", "c_followup_early_int_se",
    "c_followup_early_ctrl_se", "c_followup_late_int",
    "c_followup_late_ctrl", "c_followup_late_se", "c_prog_intervention",
    "c_prog_control", "c_prog_int_se", "c_prog_ctrl_se", "c_dfs_int",
    "c_dfs_ctrl", "c_prog_int", "c_prog_ctrl"), collapse = "|")
  testthat::expect_false(grepl(retired, active, perl = TRUE))
})
