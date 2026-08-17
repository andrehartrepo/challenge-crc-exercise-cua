# ==============================================================================
# 09-export-latex-commands.R
# Generate LaTeX \newcommand definitions from model outputs
#
# Input:  .rds files in data/processed/, .csv files in output/tables/
# Output: output/model-results.tex (LaTeX commands for thesis preamble)
#         output/model-results-manifest.csv (registry of all commands)
#
# Architecture: each model-derived number in the thesis chapters is replaced
# by a LaTeX command (e.g., \detICER instead of a hardcoded ICER literal). This
# script generates the command definitions; the thesis \input{}s this file
# from its own build tree. Any Rscript main.R rerun auto-updates all values
# across every chapter.
#
# Naming convention: category prefix + camelCase
#   det    = deterministic results
#   psa    = probabilistic analysis
#   evpi   = expected value of perfect information
#   evppi  = expected value of partial perfect information
#   ssa    = structural sensitivity analysis
#   powsa  = probabilistic one-way sensitivity analysis
#   val    = model validation / fit statistics
#   sev    = severity framework
#
# REFERENCE CODE PROVENANCE:
#   Beheim (2022). Adding dynamic computations to LaTeX documents.
#   DARTH-git/darthpack (R Markdown inline code approach, adapted here
#     to standalone LaTeX \newcommand workflow for non-Rmd thesis).
#   formatC() formatting: base R. No external dependencies.
#
# GUIDELINE: ISPOR Good Practices for transparency in modelling outputs
# GUIDELINE: CHEERS-VOI (Kunst et al. 2024) reporting standard
# ==============================================================================


# --- Formatting functions ----------------------------------------------------
# Pre-format display strings in R. LaTeX commands store the formatted result.
# No siunitx dependency; formatC handles thousands separators.

fmt_icer <- function(x) {
  # DECISION: true minus U+2212 for rendered negatives;
  # a text-mode ASCII hyphen typesets short and misreads as a range/hyphen.
  # Caution: anchored after any leading pad, so every element converts.
  sub("^( *)-", "\\1−",
      formatC(round(x), format = "f", digits = 0, big.mark = ","))
}

fmt_cost <- function(x) {
  # DECISION: same true-minus rule as fmt_icer.
  sub("^( *)-", "\\1−",
      formatC(round(x), format = "f", digits = 0, big.mark = ","))
}

fmt_qaly <- function(x) {
  # 3 decimal places for QALYs (standard HE reporting)
  formatC(x, format = "f", digits = 3)
}

fmt_pct <- function(x) {
  # 1 decimal place for percentages
  formatC(x, format = "f", digits = 1)
}

fmt_ly <- function(x) {
  # 2 decimal places for life-years
  formatC(x, format = "f", digits = 2)
}

fmt_millions <- function(x) {
  # 1 decimal place for population EVPI in millions
  formatC(x / 1e6, format = "f", digits = 1)
}

fmt_plain <- function(x) {
  # Comma-separated integer

  formatC(round(x), format = "f", digits = 0, big.mark = ",")
}

fmt_bic <- function(x) {
  # 1 decimal place for BIC/AIC; big.mark matches the rendered table, whose
  # four-figure cells have always printed a thousands separator.
  formatC(x, format = "f", digits = 1, big.mark = ",")
}


# --- Read data sources -------------------------------------------------------

det <- read.csv("output/tables/deterministic_results.csv",
                stringsAsFactors = FALSE)
ssa <- read.csv("output/tables/structural_sa_results.csv",
                stringsAsFactors = FALSE)
owpsa <- read.csv("output/tables/owpsa_v7_results.csv",
                  stringsAsFactors = FALSE)
aic_bic <- read.csv("output/tables/aic_bic_comparison.csv",
                    stringsAsFactors = FALSE)

psa_results   <- readRDS("data/processed/psa_results.rds")
evppi_results <- readRDS("data/processed/evppi_results.rds")
pop_evpi      <- readRDS("data/processed/pop_evpi_horizon.rds")
pd            <- readRDS("data/processed/perspective_delta.rds")
conv          <- readRDS("data/processed/convergence_check.rds")
inmb_conv     <- readRDS("data/processed/inmb_convergence.rds")
vc            <- readRDS("data/processed/validation_checks.rds")
psa_spec_provenance <- readRDS(
  "data/processed/psa_specification_sensitivity.rds")
psa_spec <- psa_spec_provenance$scenario_summaries
if (!is.data.frame(psa_spec)) {
  stop("09-export: invalid PSA-specification sensitivity RDS")
}

# Crossing-diagnostics inputs (stored artifacts; NO model re-run).
# SOURCE: base-case control-arm fits (02-fit-survival.R output) + per-cycle
#   discounted QALYs (04-costs-qalys.R output) -- the same stored objects the
#   base case itself consumes.
fits_dfs_ctrl <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
fits_os_ctrl  <- readRDS("data/processed/survival_fits_os_ctrl.rds")
costs_qalys   <- readRDS("data/processed/costs_qalys.rds")

# Extreme-value scenario results (appendix A.2.2 narrative; AdViSHE C2/C3).
# SOURCE: output/validation/extreme-value-results.csv, written by
#   R/10-extreme-value-tests.R. main.R does NOT source that harness, so this
#   file is an INPUT here, never a product of the canonical run.
# Caution: a model change not followed by a rerun of
#   `Rscript R/10-extreme-value-tests.R` would leave the CSV describing a
#   superseded model. The currency assertion below refuses to emit in that
#   case. Hand-typed copies of these figures drifted twice in earlier
#   revisions; the assertion is what ends that.
evt_path <- "output/validation/extreme-value-results.csv"
if (!file.exists(evt_path) || file.size(evt_path) == 0) {
  stop("09-export: missing or empty ", evt_path,
       "; run `Rscript R/10-extreme-value-tests.R` before the canonical run",
       call. = FALSE)
}
evt <- read.csv(evt_path, stringsAsFactors = FALSE)

evt_cell <- function(scenario, arm_label, column) {
  row <- evt[evt$scenario_id == scenario & evt$arm == arm_label, ]
  if (nrow(row) != 1L) {
    stop("09-export: expected exactly one ", scenario, "/", arm_label,
         " row in ", evt_path, ", got ", nrow(row), call. = FALSE)
  }
  as.numeric(row[[column]])
}

# Caution: the appendix A.2.2 sentences and the AdViSHE C2/C3 cells assert sign
#   and equality relations, not just magnitudes. Each clause below guards one
#   such sentence, so no macro can render a number that makes its prose false.
stopifnot(
  # S0 reproduces the live deterministic base case, i.e. the CSV is current.
  isTRUE(all.equal(evt_cell("S0", "Incremental", "total_costs_nok"),
                   det$total_costs_nok[det$arm == "Incremental"])),
  isTRUE(all.equal(evt_cell("S0", "Incremental", "total_qalys"),
                   det$total_qalys[det$arm == "Incremental"])),
  isTRUE(all.equal(evt_cell("S0", "Incremental", "cost_per_ly"),
                   det$cost_per_ly[det$arm == "Incremental"])),
  # "identical outcomes in the two arms"
  evt_cell("S1", "Standard Care", "total_qalys") ==
    evt_cell("S1", "Exercise", "total_qalys"),
  # "produced dominance, with ... NOK saved"
  evt_cell("S2", "Incremental", "total_costs_nok") < 0,
  # "produced zero QALYs in both arms"
  evt_cell("S3", "Standard Care", "total_qalys") == 0,
  evt_cell("S3", "Exercise", "total_qalys") == 0,
  # "cost per life-year gained unchanged"
  evt_cell("S3", "Incremental", "cost_per_ly") ==
    evt_cell("S0", "Incremental", "cost_per_ly"),
  # "State occupancy = ... all cycles"
  vc$state_sum_range[1] == vc$state_sum_range[2],
  isTRUE(vc$state_sum_all_arms)
)


# --- Compute derived values --------------------------------------------------

# GUIDELINE: applicable WTP is the computed severity-informed reference from the
#   severity engine (DMP; Magnussen et al. 2015); feeds psa_prob_ce -> \psaProbCE.
# DECISION: the superseded value was hardcoded 495,000 with no DMP source.
wtp <- wtp_threshold

# PSA summary statistics
# GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
inb_values <- wtp * psa_results$inc_qalys - psa_results$inc_costs
psa_mean_inc_cost <- mean(psa_results$inc_costs)
psa_mean_inc_qaly <- mean(psa_results$inc_qalys)
# GUIDELINE: Estimate the expected ICER as the ratio of mean increments (Briggs et al. 2006)
psa_mean_icer     <- psa_mean_inc_cost / psa_mean_inc_qaly
# GUIDELINE: Estimate cost-effectiveness probability as the PSA share with positive INB (Alarid-Escudero et al. 2019)
psa_prob_ce       <- mean(inb_values >= 0) * 100

# CE plane quadrant analysis
# WHY (display-consistency rule): single-sourced from compute_quadrant_analysis()
# (R/06-psa.R) so the four quadrant macros use the SAME counts, divisor, and
# rounding call as the thesis-table-12 percent column; the previous local
# recomputation drifted on axis-boundary handling (<= versus strict <).
quadrant_pct <- compute_quadrant_analysis(psa_results)$percentages
psa_ne <- quadrant_pct[["NE"]]
psa_se <- quadrant_pct[["SE"]]
psa_nw <- quadrant_pct[["NW"]]
psa_sw <- quadrant_pct[["SW"]]

# Decision uncertainty
# GUIDELINE: Express two-strategy decision uncertainty as the complement of CEAF (Fenwick et al. 2001)
psa_decision_uncertainty <- 100 - psa_prob_ce

# P(CE) at all 6 severity categories
# SOURCE: Magnussen et al. 2015; current six-group DMP mapping.
severity_wtp <- c(275000, 385000, 495000, 605000, 715000, 825000)
prob_ce_cats <- sapply(severity_wtp, function(w) {
  mean(w * psa_results$inc_qalys - psa_results$inc_costs >= 0) * 100
})

# 50% crossover WTP (step 1,000 NOK)
wtp_test <- seq(1000, 200000, 1000)
# GUIDELINE: Recompute the CEAC across the WTP grid (Briggs et al. 2006)
prob_curve <- sapply(wtp_test, function(w) {
  # GUIDELINE: Estimate the CEAC as the share of PSA draws that are cost-effective (Briggs et al. 2006)
  mean(w * psa_results$inc_qalys - psa_results$inc_costs >= 0)
})
crossover_50 <- wtp_test[which(prob_curve >= 0.5)[1]]

# EVPI and EVPPI
evpi_per_patient <- attr(evppi_results, "evpi")
# GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
opportunity_cost_inmb <- opportunity_cost_nok * psa_results$inc_qalys -
  psa_results$inc_costs
# GUIDELINE: Compute two-strategy EVPI from expected maximum net benefit (Briggs et al. 2006)
evpi_opportunity_cost <- mean(pmax(0, opportunity_cost_inmb)) -
  max(0, mean(opportunity_cost_inmb))

# Population EVPI at horizons (discounted)
pop_evpi_5yr  <- pop_evpi$pop_evpi[pop_evpi$horizon_years == 5]
pop_evpi_10yr <- pop_evpi$pop_evpi[pop_evpi$horizon_years == 10]
pop_evpi_20yr <- pop_evpi$pop_evpi[pop_evpi$horizon_years == 20]

# Individual EVPPI values
evppi_hr_os  <- evppi_results$evppi[evppi_results$parameter == "HR_OS"]
evppi_hr_dfs <- evppi_results$evppi[evppi_results$parameter == "HR_DFS"]
evppi_joint  <- evppi_results$evppi[
  evppi_results$parameter == "HR_OS + HR_DFS (joint)"]
# WHY (display-consistency rule): the joint share is computed FROM THE ROUNDED
# DISPLAYED OPERANDS (the integers printed by \evppiJointHR and
# \evpiPerPatient), matching the thesis-table-07 percent cells, so a reader
# can reproduce it by hand from the printed numbers.
# Derive reader-facing percentages from rounded displayed operands.
evppi_joint_pct <- round(
  100 * round(evppi_joint) / round(evpi_per_patient), 1)

# Naive sum of individual EVPPIs
evppi_naive_sum <- evppi_hr_os + evppi_hr_dfs

# Perspective comparison
icer_extended <- pd$icer[pd$perspective == "Extended (base case)"]
icer_standard <- pd$icer[pd$perspective == "Standard (SA)"]
# WHY (display-consistency rule): the delta and percentage are computed FROM
# THE ROUNDED DISPLAYED OPERANDS (the integers printed by \detICER and
# \detICERstd), so a reader can reproduce them by hand from the printed
# numbers; unrounded operands would drift by 1 NOK from the printed values.
det_icer_displayed    <- round(det$icer_nok_qaly[det$arm == "Incremental"])
det_icerstd_displayed <- round(icer_standard)
perspective_delta <- det_icer_displayed - det_icerstd_displayed
perspective_delta_pct <- round(
  (perspective_delta / det_icerstd_displayed) * 100)

# Structural SA: extract specific scenarios
ssa_get <- function(scenario_name) {
  val <- ssa$icer[ssa$scenario == scenario_name]
  if (length(val) == 0 || is.na(val)) return(NA)
  val
}

# Convergence
convergence_first <- attr(inmb_conv, "first_converged_at")
convergence_pct   <- conv$pct_diff

# BIC values for model selection
# DFS: log-normal vs generalised gamma (control arm, primary)
bic_dfs_lnorm <- aic_bic$BIC[aic_bic$endpoint == "DFS" &
  aic_bic$arm == "Standard Care (PRIMARY)" &
  aic_bic$distribution == "lnorm"]
bic_dfs_gengamma <- aic_bic$BIC[aic_bic$endpoint == "DFS" &
  aic_bic$arm == "Standard Care (PRIMARY)" &
  aic_bic$distribution == "gengamma"]
bic_dfs_diff <- bic_dfs_lnorm - bic_dfs_gengamma

# OS: log-normal (control arm, primary)
bic_os_lnorm <- aic_bic$BIC[aic_bic$endpoint == "OS" &
  aic_bic$arm == "Standard Care (PRIMARY)" &
  aic_bic$distribution == "lnorm"]

# --- Crossing diagnostics (discussion chapter) --------------------------------
# Late-horizon S_DFS > S_OS crossing of the RAW (pre-mortality-cap) independent
# log-normal extrapolations, plus the share of discounted QALYs accrued inside
# the control-arm crossing window. Descriptive reporting statistics ONLY: they
# feed nothing back into the trace, costs/QALYs, or the ICER (ICER-neutral).
# DECISION: computed at EXPORT time from stored artifacts because the saved
#   trace is POST-cap -- apply_mortality_cap() (03-build-psm.R) removes the raw
#   crossing entirely, so it cannot be recovered from psm_trace.rds. Defined in
#   this file (not 03-build-psm.R) because sourcing 03 executes its top-level
#   "Placeholder execution" block (rebuilds and re-saves psm_trace.rds), which
#   an export-only regeneration must not trigger.
# SOURCE: crossing rule mirrors the model's own PSM constraint check
#   `which(s_dfs > s_os + 1e-8)` in build_psm_trace() / build_psm_trace_from_ctrl()
#   (03-build-psm.R); exercise transform mirrors S_int(t) = S_ctrl(t)^HR with the
#   same 1e-15 floor (build_psm_trace_from_ctrl(); NICE TSD 14 s4.4, Latimer 2013).
crossing_diagnostics <- function(fit_dfs_ctrl, fit_os_ctrl,
                                 hr_dfs, hr_os,
                                 n_cycles, cycle_length,
                                 costs_qalys) {
  if (!inherits(fit_dfs_ctrl, "flexsurvreg")) {
    stop("crossing_diagnostics: fit_dfs_ctrl must be a flexsurvreg object.")
  }
  if (!inherits(fit_os_ctrl, "flexsurvreg")) {
    stop("crossing_diagnostics: fit_os_ctrl must be a flexsurvreg object.")
  }
  if (hr_dfs <= 0) stop("crossing_diagnostics: hr_dfs must be positive.")
  if (hr_os  <= 0) stop("crossing_diagnostics: hr_os must be positive.")
  need_cols <- c("arm", "cycle", "qalys_disc")
  if (!all(need_cols %in% names(costs_qalys))) {
    stop("crossing_diagnostics: costs_qalys missing columns: ",
         paste(setdiff(need_cols, names(costs_qalys)), collapse = ", "))
  }

  # RAW (pre-cap) survival on the same cycle-endpoint grid as build_psm_trace()
  times <- seq(0, n_cycles * cycle_length, by = cycle_length)
  # Diagnose crossing on the selected log-normal DFS control fit.
  s_dfs <- summary(fit_dfs_ctrl, t = times, type = "survival", ci = FALSE)[[1]]$est
  # Diagnose crossing on the selected log-normal OS control fit.
  s_os  <- summary(fit_os_ctrl,  t = times, type = "survival", ci = FALSE)[[1]]$est

  # SOURCE: same 1e-8 tolerance as the PSM constraint warning in 03-build-psm.R;
  #   a different tolerance would change the reported cycle counts.
  cr_ctrl <- which(s_dfs > s_os + 1e-8)
  cr_ex   <- which(pmax(s_dfs, 1e-15)^hr_dfs > pmax(s_os, 1e-15)^hr_os + 1e-8)

  if (length(cr_ctrl) == 0 || length(cr_ex) == 0) {
    stop("crossing_diagnostics: no raw-fit crossing detected; the discussion ",
         "crossing paragraph no longer applies -- re-derive before exporting.")
  }

  # Caution: times/trace are 0-indexed (times[i] = (i-1)*cycle_length, trace cycle
  #   = i - 1; first control violation at index 329 = cycle 328 = year 27.3333)
  #   while costs_qalys$cycle runs 1..n_cycles. The window therefore maps to
  #   costs_qalys rows with cycle %in% (cr_ctrl - 1). An off-by-one start shifts
  #   the shares (start 327 -> 3.41/6.90, start 329 -> 3.28/6.64; only start 328
  #   reproduces 3.35/6.77 -- verified 2026-07-12).
  win_cycles <- cr_ctrl - 1

  ctrl <- costs_qalys[costs_qalys$arm == "Standard Care", ]
  exi  <- costs_qalys[costs_qalys$arm == "Exercise", ]
  ctrl <- ctrl[order(ctrl$cycle), ]
  exi  <- exi[order(exi$cycle), ]
  if (!identical(ctrl$cycle, exi$cycle)) {
    stop("crossing_diagnostics: arm cycle grids differ in costs_qalys.")
  }
  in_win <- ctrl$cycle %in% win_cycles

  # Both shares use the CONTROL-arm window: the discussion sentence reports the
  # 153 control-arm crossing cycles and states both percentages for that window.
  # GUIDELINE: Derive incremental outcomes as exercise minus standard care (Drummond et al. 2015)
  inc <- exi$qalys_disc - ctrl$qalys_disc
  list(
    ctrl_cycles         = length(cr_ctrl),
    ctrl_start_yr       = times[cr_ctrl[1]],
    ex_cycles           = length(cr_ex),
    ex_start_yr         = times[cr_ex[1]],
    # Caution: Use the control-arm crossing window for both reported QALY shares.
    ctrl_qaly_share_pct = 100 * sum(ctrl$qalys_disc[in_win]) / sum(ctrl$qalys_disc),
    incr_qaly_share_pct = 100 * sum(inc[in_win]) / sum(inc)
  )
}

# SOURCE: HR_DFS, HR_OS, n_cycles, cycle_length, best_dist_dfs, best_dist_os from
#   00-parameters.R -- the same parameter objects the base-case trace consumes
#   (03-build-psm.R "Placeholder execution" block); never re-hardcoded here.
crossing_diag <- crossing_diagnostics(
  fit_dfs_ctrl = fits_dfs_ctrl[[best_dist_dfs]],
  fit_os_ctrl  = fits_os_ctrl[[best_dist_os]],
  hr_dfs       = HR_DFS,
  hr_os        = HR_OS,
  n_cycles     = n_cycles,
  cycle_length = cycle_length,
  costs_qalys  = costs_qalys
)


# --- Build command registry --------------------------------------------------
# Each entry: command name, formatted value, category, description, source

commands <- data.frame(
  command = character(),
  value = character(),
  category = character(),
  description = character(),
  source_file = character(),
  stringsAsFactors = FALSE
)

add_cmd <- function(cmd, val, cat, desc, src) {
  commands[nrow(commands) + 1, ] <<- list(cmd, val, cat, desc, src)
}

# --- Deterministic results ---
add_cmd("detICER",
  fmt_icer(det$icer_nok_qaly[det$arm == "Incremental"]),
  "det", "Deterministic ICER (NOK/QALY)",
  "deterministic_results.csv")

add_cmd("detIncrCost",
  fmt_cost(det$total_costs_nok[det$arm == "Incremental"]),
  "det", "Deterministic incremental cost (NOK)",
  "deterministic_results.csv")

add_cmd("detIncrQALY",
  fmt_qaly(det$total_qalys[det$arm == "Incremental"]),
  "det", "Deterministic incremental QALYs",
  "deterministic_results.csv")

add_cmd("detIncrLY",
  fmt_ly(det$total_ly_disc[det$arm == "Incremental"]),
  "det", "Deterministic incremental life-years (discounted)",
  "deterministic_results.csv")

add_cmd("detIncrLYundisc",
  fmt_ly(det$total_ly_raw[det$arm == "Incremental"]),
  "det", "Deterministic incremental life-years (undiscounted)",
  "deterministic_results.csv")

add_cmd("detCostPerLY",
  fmt_icer(det$cost_per_ly[det$arm == "Incremental"]),
  "det", "Deterministic cost per life-year gained (NOK/LY)",
  "deterministic_results.csv")

add_cmd("detQALYexercise",
  fmt_qaly(det$total_qalys[det$arm == "Exercise"]),
  "det", "Exercise arm total discounted QALYs",
  "deterministic_results.csv")

add_cmd("detQALYcontrol",
  fmt_qaly(det$total_qalys[det$arm == "Standard Care"]),
  "det", "Control arm total discounted QALYs",
  "deterministic_results.csv")

add_cmd("detCostExercise",
  fmt_cost(det$total_costs_nok[det$arm == "Exercise"]),
  "det", "Exercise arm total cost (NOK)",
  "deterministic_results.csv")

add_cmd("detCostControl",
  fmt_cost(det$total_costs_nok[det$arm == "Standard Care"]),
  "det", "Control arm total cost (NOK)",
  "deterministic_results.csv")

add_cmd("detQALYctrlUndisc",
  fmt_ly(det$total_qalys_raw[det$arm == "Standard Care"]),
  "det", "Control arm undiscounted QALYs",
  "deterministic_results.csv")

add_cmd("detIncrQALYundisc",
  fmt_qaly(det$total_qalys_raw[det$arm == "Incremental"]),
  "det", "Deterministic incremental QALYs (undiscounted)",
  "deterministic_results.csv")

add_cmd("detIncrCostUndisc",
  fmt_cost(det$total_costs_raw[det$arm == "Incremental"]),
  "det", "Deterministic incremental cost (NOK, undiscounted)",
  "deterministic_results.csv")

# --- Perspective comparison ---
add_cmd("detICERstd",
  fmt_icer(icer_standard),
  "det", "Deterministic ICER healthcare-services-only perspective (NOK/QALY)",
  "perspective_delta.rds")

add_cmd("detPerspectiveDelta",
  fmt_cost(abs(perspective_delta)),
  "det", "Perspective ICER difference (NOK/QALY)",
  "perspective_delta.rds")

add_cmd("detPerspectiveDeltaPct",
  as.character(perspective_delta_pct),
  "det", "Perspective ICER difference (percentage)",
  "perspective_delta.rds")

# --- PSA results ---
add_cmd("psaICER",
  fmt_icer(psa_mean_icer),
  "psa", "PA mean ICER (NOK/QALY, ratio of means)",
  "psa_results.rds")

add_cmd("psaIncrCost",
  fmt_cost(psa_mean_inc_cost),
  "psa", "PA mean incremental cost (NOK)",
  "psa_results.rds")

add_cmd("psaIncrQALY",
  fmt_qaly(psa_mean_inc_qaly),
  "psa", "PA mean incremental QALYs",
  "psa_results.rds")

add_cmd("psaProbCE",
  fmt_pct(psa_prob_ce),
  "psa", "P(CE) at applicable cost-effectiveness threshold (%)",
  "psa_results.rds")

add_cmd("psaNEpct",
  fmt_pct(psa_ne),
  "psa", "CE plane north-east quadrant (%)",
  "psa_results.rds")

add_cmd("psaSEpct",
  fmt_pct(psa_se),
  "psa", "CE plane south-east quadrant (%)",
  "psa_results.rds")

add_cmd("psaNWpct",
  fmt_pct(psa_nw),
  "psa", "CE plane north-west quadrant (%)",
  "psa_results.rds")

add_cmd("psaSWpct",
  fmt_pct(psa_sw),
  "psa", "CE plane south-west quadrant (%)",
  "psa_results.rds")

add_cmd("psaDecisionUncertainty",
  fmt_pct(psa_decision_uncertainty),
  "psa", "Decision uncertainty (100% minus P(CE))",
  "psa_results.rds")

add_cmd("psaCrossoverWTP",
  fmt_cost(crossover_50),
  "psa", "50% crossover threshold (NOK)",
  "psa_results.rds")

add_cmd("psaProbCEcatOne",
  fmt_pct(prob_ce_cats[1]),
  "psa", "P(CE) at severity category 1 (275K) (%)",
  "psa_results.rds")

add_cmd("psaProbCEcatTwo",
  fmt_pct(prob_ce_cats[2]),
  "psa", "P(CE) at severity category 2 (385K) (%)",
  "psa_results.rds")

add_cmd("psaProbCEcatFour",
  fmt_pct(prob_ce_cats[4]),
  "psa", "P(CE) at severity category 4 (605K) (%)",
  "psa_results.rds")

add_cmd("psaProbCEcatFive",
  fmt_pct(prob_ce_cats[5]),
  "psa", "P(CE) at severity category 5 (715K) (%)",
  "psa_results.rds")

add_cmd("psaProbCEcatSix",
  fmt_pct(prob_ce_cats[6]),
  "psa", "P(CE) at severity category 6 (825K) (%)",
  "psa_results.rds")

# --- EVPI / EVPPI ---
add_cmd("evpiPerPatient",
  fmt_plain(evpi_per_patient),
  "evpi", "Per-patient EVPI at applicable threshold (NOK)",
  "evppi_results.rds")

add_cmd("evpiOpportunityCost",
  fmt_plain(evpi_opportunity_cost),
  "evpi", "Per-patient EVPI at the quantified opportunity-cost threshold (NOK)",
  "psa_results.rds")

add_cmd("evpiPopFiveYr",
  fmt_millions(pop_evpi_5yr),
  "evpi", "Population EVPI 5-year horizon (million NOK)",
  "pop_evpi_horizon.rds")

add_cmd("evpiPopTenYr",
  fmt_millions(pop_evpi_10yr),
  "evpi", "Population EVPI 10-year horizon (million NOK)",
  "pop_evpi_horizon.rds")

add_cmd("evpiPopTwentyYr",
  fmt_millions(pop_evpi_20yr),
  "evpi", "Population EVPI 20-year horizon (million NOK)",
  "pop_evpi_horizon.rds")

add_cmd("evppiHRos",
  fmt_plain(evppi_hr_os),
  "evppi", "Individual EVPPI HR OS (NOK)",
  "evppi_results.rds")

add_cmd("evppiHRdfs",
  fmt_plain(evppi_hr_dfs),
  "evppi", "Individual EVPPI HR DFS (NOK)",
  "evppi_results.rds")

add_cmd("evppiJointHR",
  fmt_plain(evppi_joint),
  "evppi", "Joint EVPPI HR OS + HR DFS (NOK)",
  "evppi_results.rds")

add_cmd("evppiJointHRpct",
  fmt_pct(evppi_joint_pct),
  "evppi", "Joint EVPPI as percentage of EVPI (%)",
  "evppi_results.rds")

add_cmd("evppiNaiveSum",
  fmt_plain(evppi_naive_sum),
  "evppi", "Naive sum of individual EVPPIs HR OS + HR DFS (NOK)",
  "evppi_results.rds")

# --- PSA-specification sensitivity ---
positive_rho <- psa_spec[psa_spec$scenario_id %in% c(
  "dependence_rho03", "dependence_rho05", "dependence_rho07",
  "dependence_rho09"), ]
mapping_row <- psa_spec[
  psa_spec$scenario_id == "mapping_rmse_quadrature_rho0", ]
if (nrow(positive_rho) != 4L || nrow(mapping_row) != 1L) {
  stop("09-export: invalid PSA-specification scenario registry")
}
add_range <- function(prefix, values, formatter, description) {
  if (any(!is.finite(values))) stop("09-export: non-finite ", prefix)
  add_cmd(paste0(prefix, "Min"), formatter(min(values)), "psa_spec",
          paste0(description, " minimum"),
          "psa_specification_sensitivity.rds")
  add_cmd(paste0(prefix, "Max"), formatter(max(values)), "psa_spec",
          paste0(description, " maximum"),
          "psa_specification_sensitivity.rds")
}
add_range("rhoSensitivityIcer", positive_rho$icer_ratio_of_means,
          fmt_icer, "Positive-rho sensitivity ICER")
add_range("rhoSensitivityPce",
          positive_rho$probability_cost_effective * 100,
          fmt_pct, "Positive-rho sensitivity P(CE), percent")
add_range("rhoSensitivityEvpi", positive_rho$evpi_per_patient,
          fmt_plain, "Positive-rho sensitivity EVPI")
add_range("rhoSensitivityJointEvppi", positive_rho$evppi_joint_hr,
          fmt_plain, "Positive-rho sensitivity joint-HR EVPPI")
add_range("rhoSensitivityCeCorr", positive_rho$cor_incremental_cost_qaly,
          function(x) formatC(x, format = "f", digits = 3),
          "Positive-rho sensitivity incremental cost-QALY correlation")
add_cmd("rhoSensitivityConvergenceMaxN",
  fmt_plain(max(positive_rho$primary_convergence_n)),
  "psa_spec", "Maximum primary convergence N across positive-rho rows",
  "psa_specification_sensitivity.rds")
add_cmd("mappingSensitivityIcer",
  fmt_icer(mapping_row$icer_ratio_of_means),
  "psa_spec", "Mapping-RMSE quadrature sensitivity ICER",
  "psa_specification_sensitivity.rds")
add_cmd("mappingSensitivityPce",
  fmt_pct(mapping_row$probability_cost_effective * 100),
  "psa_spec", "Mapping-RMSE quadrature sensitivity P(CE), percent",
  "psa_specification_sensitivity.rds")
add_cmd("mappingSensitivityEvpi",
  fmt_plain(mapping_row$evpi_per_patient),
  "psa_spec", "Mapping-RMSE quadrature sensitivity EVPI",
  "psa_specification_sensitivity.rds")

add_cmd("costSurveillanceAnnual",
  fmt_cost(c_surveillance_early),
  "parameter", "Annual surveillance cost, years 0-5",
  "R/00-parameters.R")
add_cmd("costSurveillanceFiveYear",
  formatC(c_surveillance_5yr_bundle, format = "f", digits = 2, big.mark = ","),
  "parameter", "Five-year surveillance bundle, undiscounted",
  "R/00-parameters.R")
add_cmd("costProgressedAnnual",
  fmt_cost(c_progressed_annual),
  "parameter", "Annual progressed-state cost",
  "R/00-parameters.R")

# SOURCE: the source-basis figure the methods footnote and appendix A.7.3 print.
#   Exported so those two sites render the model's own number instead of a
#   hand-typed literal that no gate can hold to the model.
add_cmd("costProgressedEURsource",
  fmt_cost(c_progressed_eur_2011),
  "parameter", "Progressed-state cost in the source price basis (EUR, 2011 prices)",
  "R/00-parameters.R")

add_cmd("paramCostTerminal",
  fmt_cost(c_terminal),
  "param", "Terminal care cost (NOK)",
  "R/00-parameters.R")

add_cmd("paramSessionsAdherence",
  fmt_ly(n_sessions_adherence),
  "param", "Adherence-adjusted supervised exercise contacts over programme",
  "R/00-parameters.R")

add_cmd("paramCostExerciseAnnual",
  fmt_cost(c_exercise_annual),
  "param", "Annual exercise programme cost, exercise visits plus behavioural support (NOK/year)",
  "R/00-parameters.R")

add_cmd("paramBehaviourContacts",
  fmt_ly(n_behaviour_adherence),
  "param", "Adherence-adjusted behavioural-support contacts over programme",
  "R/00-parameters.R")

add_cmd("paramCostBehaviourContact",
  fmt_cost(c_behaviour_contact),
  "param", "Behavioural-support contact unit cost, Helfo A3a + 1 x A3b (NOK)",
  "R/00-parameters.R")

add_cmd("paramCostBehaviourAnnual",
  fmt_cost(c_behaviour_annual),
  "param", "Annual behavioural-support cost (NOK/year)",
  "R/00-parameters.R")

# Caution: this figure was a hardcoded 959 in methods.tex twice, derived from the
#   annual programme cost. It is exported so it can never drift again.
add_cmd("paramCostProgrammeCycle",
  fmt_cost(c_exercise_annual / 12),
  "param", "Programme cost per active monthly cycle, before occupancy and discounting (NOK)",
  "R/00-parameters.R")

add_cmd("paramCostSessionExtended",
  fmt_cost(c_session_extended),
  "param", "Extended-perspective exercise cost per contact (NOK)",
  "R/00-parameters.R")

# Caution: the healthcare session figure was hardcoded in methods.tex, discussion.tex
#   and appendix.tex. Since it moved, it is exported so it can
#   never drift again, on the same pattern as paramCostProgrammeCycle above.
add_cmd("paramCostSessionHealthcare",
  fmt_cost(c_session_healthcare),
  "param", "Base-case healthcare session cost, DMP-adjusted tariff (NOK)",
  "R/00-parameters.R")

add_cmd("paramCostSessionTariffBare",
  fmt_cost(c_session_tariff_bare),
  "param", "Bare Helfo A3 session tariff, before the DMP adjustment (NOK)",
  "R/00-parameters.R")

# --- Structural SA ---
add_cmd("ssaICERstepWane",
  fmt_icer(ssa_get("waning_step_5yr")),
  "ssa", "SSA ICER: step waning at year 5",
  "structural_sa_results.csv")

add_cmd("ssaICERlinearWane",
  fmt_icer(ssa_get("waning_linear_3_10")),
  "ssa", "SSA ICER: linear waning years 3-10",
  "structural_sa_results.csv")

add_cmd("ssaICERtenYr",
  fmt_icer(ssa_get("horizon_10yr")),
  "ssa", "SSA ICER: 10-year time horizon",
  "structural_sa_results.csv")

add_cmd("ssaICERtwentyYr",
  fmt_icer(ssa_get("horizon_20yr")),
  "ssa", "SSA ICER: 20-year time horizon",
  "structural_sa_results.csv")

add_cmd("ssaICERpmin",
  fmt_icer(ssa_get("mortality_pmin")),
  "ssa", "SSA ICER: pmin mortality method",
  "structural_sa_results.csv")

add_cmd("ssaICERdrugDiscount",
  fmt_icer(ssa_get("drug_discount_40pct")),
  "ssa", "SSA ICER: 40% drug discount scenario",
  "structural_sa_results.csv")

add_cmd("ssaICERageSeventyFour",
  fmt_icer(ssa_get("age_74")),
  "ssa", "SSA ICER: age 74 subgroup",
  "structural_sa_results.csv")

add_cmd("ssaIncrQALYageSeventyFour",
  fmt_qaly(ssa$inc_qalys[ssa$scenario == "age_74"]),
  "ssa", "SSA incremental QALYs: age 74 subgroup",
  "structural_sa_results.csv")

add_cmd("ssaICERfullAdherence",
  fmt_icer(ssa_get("full_adherence")),
  "ssa", "SSA ICER: full adherence scenario",
  "structural_sa_results.csv")

# --- Structural SA: alternative disease-survival specification -----------------
# SOURCE: structural_sa_results.csv `icer` column, scenario dfs_gengamma
#   (06b-structural-sa.R; generalised-gamma DFS fit selected on BIC).
# GUIDELINE: ratio-of-means ICER, extended healthcare perspective, stepped 4%
#   discounting (Rundskriv R-109). Same ssa_get -> icer -> fmt_icer path as the
#   sibling \ssaICER* macros above; NON-HARDCODED.
add_cmd("ssaICERdfsGengamma",
  fmt_icer(ssa_get("dfs_gengamma")),
  "ssa", "SSA ICER: generalised-gamma DFS survival specification",
  "structural_sa_results.csv")

# --- Structural-SA scenario counts (derived from ssa; NEVER hardcoded) ---------
# SOURCE: structural_sa_results.csv (the `ssa` read.csv above) -- the SAME object that feeds
#   every \ssaICER* macro, so the manuscript counts can never drift from the model.
# "tested" EXCLUDES base_case (ssa row 1) to match the
#   manuscript framing "... scenarios ... alongside the base case".
# Caution: there is NO status column -- a FAILED scenario is an NA-`icer` row
#   (06b-structural-sa.R: run_structural_sa() initialises `scenarios$icer <- NA_real_`
#   and overwrites it only on a scenario that executes); completed vs failed derived
#   from is.na(icer).
ssa_scn <- ssa[ssa$scenario != "base_case", ]
add_cmd("ssaNtested",
  as.character(nrow(ssa_scn)),
  "ssa", "Structural SA: named scenarios tested (excl. base case)",
  "structural_sa_results.csv")
add_cmd("ssaNcompleted",
  as.character(sum(!is.na(ssa_scn$icer))),
  "ssa", "Structural SA: named scenarios completed (finite ICER)",
  "structural_sa_results.csv")
add_cmd("ssaNfailed",
  as.character(sum(is.na(ssa_scn$icer))),
  "ssa", "Structural SA: named scenarios failed (NA ICER)",
  "structural_sa_results.csv")
# DECISION fork F2: "cost-effective" judged vs EACH scenario's OWN severity-weighted
#   reference (sev_reference), not one flat threshold -- older-cohort scenarios sit in
#   Magnussen group 1 (275,000) and must be compared there.
# Caution: guard NA sev_reference loudly (NO na.rm) -- !is.na() dominates the & so a
#   missing reference yields FALSE, not NA; executed rows only (icer already finite).
ssa_exec <- ssa_scn[!is.na(ssa_scn$icer), ]
add_cmd("ssaNcostEffective",
  as.character(sum(!is.na(ssa_exec$sev_reference) &
                   ssa_exec$icer <= ssa_exec$sev_reference)),
  "ssa", "Structural SA: completed scenarios cost-effective vs own sev_reference (F2)",
  "structural_sa_results.csv")

# --- Structural-SA INMB macros (all computed, never literal) ------------------
# SOURCE: structural_sa_results.csv columns inc_costs, inc_qalys, sev_weight,
#   sev_reference (written by R/06b-structural-sa.R). INMB_s = sev_reference_s
#   x inc_qalys_s - inc_costs_s (severity-weighted threshold basis, F2 fork).
# Caution: executed rows only; the all-NA no_mortality_cap row must not poison
#   min(). The floor threshold is DERIVED as sev_reference / sev_weight and
#   must be a single value across rows, or the severity framework changed.
# GUIDELINE: Compute scenario INB at its own severity-informed reference (Briggs et al. 2006)
ssa_inmb <- ssa_exec$sev_reference * ssa_exec$inc_qalys - ssa_exec$inc_costs
ssa_base_row <- ssa[ssa$scenario == "base_case", ]
stopifnot(nrow(ssa_base_row) == 1L, !is.na(ssa_base_row$sev_reference))
# GUIDELINE: Derive the opportunity-cost floor from severity reference divided by severity weight (Magnussen et al. 2015)
ssa_floor_nok <- unique(round(ssa_exec$sev_reference / ssa_exec$sev_weight))
stopifnot(length(ssa_floor_nok) == 1L)
add_cmd("ssaINMBmin",
  fmt_cost(min(ssa_inmb)),
  "ssa", "Structural SA: minimum scenario INMB at its own severity-weighted threshold (NOK)",
  "structural_sa_results.csv")
add_cmd("ssaINMBbase",
  fmt_cost(ssa_base_row$sev_reference * ssa_base_row$inc_qalys -
           ssa_base_row$inc_costs),
  "ssa", "Structural SA: base-case INMB at the base-case severity-weighted threshold (NOK)",
  "structural_sa_results.csv")
add_cmd("ssaINMBminFloor",
  fmt_cost(min(ssa_floor_nok * ssa_exec$inc_qalys - ssa_exec$inc_costs)),
  "ssa", "Structural SA: minimum scenario INMB at the derived opportunity-cost floor threshold (NOK)",
  "structural_sa_results.csv")

# --- POWSA (tornado diagram) ---
# Caution: these lookups match on param_name, NOT on the display label. The label
# embeds the sensitivity bounds ("... (60%-140%)"), so it changes whenever a
# band is rewidened, and a label match then silently returns numeric(0). That is
# not a soft failure: add_cmd() assigns a 5-element row, so a zero-length value
# aborts the whole export with "replacement has length zero" and NO commands
# file is written at all. It cost a full pipeline run on 2026-07-28, when the
# surveillance band moved to 0.40/1.60. param_name is the stable identifier.
powsa_range <- function(param) {
  hit <- owpsa$range[owpsa$param_name == param]
  # Fail closed and name the parameter, rather than letting a zero-length value
  # surface as an opaque data.frame assignment error 200 lines away.
  if (length(hit) != 1L) {
    stop(sprintf(
      "POWSA lookup for '%s' matched %d rows, expected exactly 1. Known: %s",
      param, length(hit), paste(owpsa$param_name, collapse = ", ")))
  }
  hit
}

add_cmd("powsaINMBrangeHRos",
  fmt_plain(powsa_range("HR_OS")),
  "powsa", "POWSA INMB range: HR OS (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaINMBrangeHRdfs",
  fmt_plain(powsa_range("HR_DFS")),
  "powsa", "POWSA INMB range: HR DFS (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaINMBrangeProgressedCost",
  fmt_plain(powsa_range("c_progressed_annual")),
  "powsa", "POWSA INMB range: shared progressed-disease annual cost (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaINMBrangeSurveillanceCost",
  fmt_plain(powsa_range("c_surveillance_early")),
  "powsa", "POWSA INMB range: shared surveillance annual cost (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaINMBrangeExerciseCost",
  fmt_plain(powsa_range("c_exercise_annual")),
  "powsa", "POWSA INMB range: exercise-programme annual cost (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaINMBrangeDFSutil",
  fmt_plain(powsa_range("u_dfs")),
  "powsa", "POWSA INMB range: DFS utility (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaMinProbCE",
  fmt_pct(min(c(owpsa$prob_ce_low, owpsa$prob_ce_high)) * 100),
  "powsa", "POWSA minimum P(CE) across all parameter bounds (%)",
  "owpsa_v7_results.csv")

add_cmd("powsaMinimumExpectedINMB",
  fmt_plain(min(c(owpsa$inmb_low, owpsa$inmb_high))),
  "powsa", "POWSA minimum expected INMB across bounds (NOK)",
  "owpsa_v7_results.csv")

add_cmd("powsaMaximumExpectedINMB",
  fmt_plain(max(c(owpsa$inmb_low, owpsa$inmb_high))),
  "powsa", "POWSA maximum expected INMB across bounds (NOK)",
  "owpsa_v7_results.csv")

# --- Convergence ---
add_cmd("psaConvergenceIter",
  as.character(convergence_first),
  "psa", "PA convergence: first INMB CI excludes zero (iterations)",
  "inmb_convergence.rds")

add_cmd("psaConvergencePct",
  fmt_pct(convergence_pct),
  "psa", "PA ICER stability: 1K vs 10K difference (%)",
  "convergence_check.rds")

# --- Validation ---
add_cmd("valDFSctrlFiveYr",
  fmt_pct(vc$dfs_5yr_standard_care),
  "val", "Model 5-year DFS control arm (%)",
  "validation_checks.rds")

add_cmd("valDFSexFiveYr",
  fmt_pct(vc$dfs_5yr_exercise),
  "val", "Model 5-year DFS exercise arm (%)",
  "validation_checks.rds")

add_cmd("valDFSpubCtrl",
  fmt_pct(vc$dfs_5yr_published_standard_care),
  "val", "Published 5-year DFS control arm (%)",
  "validation_checks.rds")

add_cmd("valDFSpubEx",
  fmt_pct(vc$dfs_5yr_published_exercise),
  "val", "Published 5-year DFS exercise arm (%)",
  "validation_checks.rds")

add_cmd("valDFSctrlGap",
  fmt_pct(abs(vc$dfs_5yr_standard_care -
              vc$dfs_5yr_published_standard_care)),
  "val", "Validation gap: 5-year DFS control (percentage points)",
  "validation_checks.rds")

add_cmd("valDFSexGap",
  fmt_pct(abs(vc$dfs_5yr_exercise -
              vc$dfs_5yr_published_exercise)),
  "val", "Validation gap: 5-year DFS exercise (percentage points)",
  "validation_checks.rds")

# --- Extreme-value scenarios and state-occupancy validation ---
# SOURCE: extreme-value-results.csv and validation_checks.rds, both guarded by
#   the assertion block above.
# DECISION: 4-dp QALY and occupancy emission is deliberate and reproduces the
#   rendered appendix text; fmt_qaly is 3 dp and would move rendered bytes.
#   Inline formatC follows the valCrossingQALYshare precedent below.
add_cmd("valEVTidenticalQALY",
  formatC(evt_cell("S1", "Standard Care", "total_qalys"),
          format = "f", digits = 4),
  "val", "Extreme value, no treatment effect: QALYs per arm (arms identical)",
  "output/validation/extreme-value-results.csv")

add_cmd("valEVTnoEffectIncrCost",
  fmt_cost(evt_cell("S1", "Incremental", "total_costs_nok")),
  "val", "Extreme value, no treatment effect: incremental cost (NOK)",
  "output/validation/extreme-value-results.csv")

add_cmd("valEVTzeroCostSaving",
  fmt_cost(abs(evt_cell("S2", "Incremental", "total_costs_nok"))),
  "val", "Extreme value, zero intervention cost: magnitude of the saving (NOK)",
  "output/validation/extreme-value-results.csv")

add_cmd("valEVTzeroCostQALY",
  formatC(evt_cell("S2", "Incremental", "total_qalys"),
          format = "f", digits = 4),
  "val", "Extreme value, zero intervention cost: incremental QALYs",
  "output/validation/extreme-value-results.csv")

add_cmd("valEVTzeroUtilQALY",
  fmt_plain(evt_cell("S3", "Standard Care", "total_qalys")),
  "val", "Extreme value, zero utilities: QALYs per arm",
  "output/validation/extreme-value-results.csv")

add_cmd("valEVTzeroUtilCostPerLY",
  fmt_cost(evt_cell("S3", "Incremental", "cost_per_ly")),
  "val", "Extreme value, zero utilities: incremental cost per life-year (NOK)",
  "output/validation/extreme-value-results.csv")

add_cmd("valStateOccupancy",
  formatC(vc$state_sum_range[1], format = "f", digits = 4),
  "val", "Validation: state-occupancy sum, every cycle and both arms",
  "validation_checks.rds")

# --- BIC model selection ---
add_cmd("valBICdfsLognormal",
  fmt_bic(bic_dfs_lnorm),
  "val", "BIC: DFS log-normal (control arm)",
  "aic_bic_comparison.csv")

add_cmd("valBICdfsGenGamma",
  fmt_bic(bic_dfs_gengamma),
  "val", "BIC: DFS generalised gamma (control arm)",
  "aic_bic_comparison.csv")

add_cmd("valBICdfsDiff",
  fmt_bic(bic_dfs_diff),
  "val", "BIC difference: DFS log-normal minus gen. gamma",
  "aic_bic_comparison.csv")

add_cmd("valBICosLognormal",
  fmt_bic(bic_os_lnorm),
  "val", "BIC: OS log-normal (control arm)",
  "aic_bic_comparison.csv")

# --- Full AIC/BIC selection table (Methods Table 3.1) ---
# Every data cell of that table is a model output and is emitted here, so no
# AIC/BIC literal is hand-maintained in the chapter source. Keyed on
# distribution, never on CSV row order, which is ascending-AIC within endpoint.
aic_bic_primary <- aic_bic[aic_bic$arm == "Standard Care (PRIMARY)", ]
for (dist in list(c("exp", "Exponential"), c("weibull", "Weibull"),
                  c("lnorm", "Lognormal"), c("llogis", "Loglogistic"),
                  c("gompertz", "Gompertz"), c("gengamma", "GenGamma"))) {
  for (ep in c("DFS", "OS")) {
    row <- aic_bic_primary[aic_bic_primary$endpoint == ep &
                           aic_bic_primary$distribution == dist[1], ]
    if (nrow(row) != 1L) {
      stop("aic_bic_comparison.csv: expected exactly one ", ep, "/", dist[1],
           " PRIMARY row, got ", nrow(row), call. = FALSE)
    }
    for (stat in c("AIC", "BIC")) {
      nm <- paste0("val", stat, tolower(ep), dist[2])
      if (nm %in% commands$command) next   # the four legacy names already emitted
      add_cmd(nm, fmt_bic(row[[stat]]), "val",
              paste0(stat, ": ", ep, " ", dist[2], " (control arm)"),
              "aic_bic_comparison.csv")
    }
  }
}

# --- Selected-specification survival parameters (Methods Table 3.1 panel) ---
# SOURCE: control-arm log-normal fits, $res (natural scale), read from
#   survival_fits_dfs_ctrl.rds and survival_fits_os_ctrl.rds loaded above.
# DECISION: the body selection table reports the preferred specification on the
#   NATURAL scale, so a reader takes each cell at its row label; the
#   estimation-scale pair and its intervals stay in the appendix coefficient
#   table.
# Caution: $res.t carries the ESTIMATION scale, on which sdlog is log-transformed;
#   reading it here would print log(sdlog) under an sdlog label.
lnorm_natural <- function(fit, par) {
  res_n <- fit$res
  if (!is.matrix(res_n) ||
      !identical(rownames(res_n), c("meanlog", "sdlog")) ||
      !("est" %in% colnames(res_n)) ||
      !is.finite(res_n[par, "est"])) {
    stop("09-export: selected control-arm fit lacks a finite natural-scale ",
         par, call. = FALSE)
  }
  formatC(res_n[par, "est"], format = "f", digits = 4)
}

add_cmd("valLnormDfsMeanlog",
  lnorm_natural(fits_dfs_ctrl[[best_dist_dfs]], "meanlog"),
  "val", "Selected log-normal meanlog, natural scale (DFS control arm)",
  "survival_fits_dfs_ctrl.rds")

add_cmd("valLnormDfsSdlog",
  lnorm_natural(fits_dfs_ctrl[[best_dist_dfs]], "sdlog"),
  "val", "Selected log-normal sdlog, natural scale (DFS control arm)",
  "survival_fits_dfs_ctrl.rds")

add_cmd("valLnormOsMeanlog",
  lnorm_natural(fits_os_ctrl[[best_dist_os]], "meanlog"),
  "val", "Selected log-normal meanlog, natural scale (OS control arm)",
  "survival_fits_os_ctrl.rds")

add_cmd("valLnormOsSdlog",
  lnorm_natural(fits_os_ctrl[[best_dist_os]], "sdlog"),
  "val", "Selected log-normal sdlog, natural scale (OS control arm)",
  "survival_fits_os_ctrl.rds")

# --- Crossing diagnostics (raw-fit curve crossing; discussion chapter) ---
# SOURCE: crossing_diag list computed above by crossing_diagnostics().
# DECISION: 2-dp emission for start years and QALY-share percents (prose parity:
#   the discussion states 27.33/37.58 and 3.35/6.77 at 2 dp). fmt_pct is 1 dp so
#   it is NOT used; fmt_ly is this file's 2-dp formatter (start years ARE years);
#   shares use formatC(digits = 2) like the sevAbsoluteShortfall precedent.
#   Bare numbers by design: the prose carries the word "percent"; the macro
#   names carry the Pct/Yr semantics.
add_cmd("valCrossingCyclesControl",
  fmt_plain(crossing_diag$ctrl_cycles),
  "val", "Raw-fit crossing: control-arm monthly cycles with S_DFS > S_OS (pre-cap)",
  "survival_fits_dfs_ctrl.rds; survival_fits_os_ctrl.rds")

add_cmd("valCrossingStartYrControl",
  fmt_ly(crossing_diag$ctrl_start_yr),
  "val", "Raw-fit crossing: control-arm first crossing time (years)",
  "survival_fits_dfs_ctrl.rds; survival_fits_os_ctrl.rds")

add_cmd("valCrossingCyclesExercise",
  fmt_plain(crossing_diag$ex_cycles),
  "val", "Raw-fit crossing: exercise-arm monthly cycles with S_DFS > S_OS (pre-cap)",
  "survival_fits_dfs_ctrl.rds; survival_fits_os_ctrl.rds")

add_cmd("valCrossingStartYrExercise",
  fmt_ly(crossing_diag$ex_start_yr),
  "val", "Raw-fit crossing: exercise-arm first crossing time (years)",
  "survival_fits_dfs_ctrl.rds; survival_fits_os_ctrl.rds")

add_cmd("valCrossingQALYshareControlPct",
  formatC(crossing_diag$ctrl_qaly_share_pct, format = "f", digits = 2),
  "val", "Crossing-window share of control-arm total discounted QALYs (%)",
  "costs_qalys.rds")

add_cmd("valCrossingIncrQALYsharePct",
  formatC(crossing_diag$incr_qaly_share_pct, format = "f", digits = 2),
  "val", "Crossing-window share of discounted incremental QALYs (%)",
  "costs_qalys.rds")

# --- Severity ---
# SOURCE: severity engine (derive_severity in R/00-parameters.R + severity block in
#   R/04-costs-qalys.R), opportunity_cost_nok, and prob_ce_cats above. All computed.
# DECISION: removes stale Garratt-norm 19.0/18.98
#   (no DMP source) in favour of the current-DMP-norm absolute-shortfall values.
add_cmd("sevCohortAge",
  as.character(cohort_age),
  "sev", "Cohort age used for severity norm lookup (years)",
  "00-parameters.R")

add_cmd("sevGenPopQALY",
  formatC(severity$general_population_qalys, format = "f", digits = 1),
  "sev", "General population remaining QALYs at cohort age (DMP New norm)",
  "dmp-eq5d5l-norms-current.csv")

add_cmd("sevComparatorQALY",
  formatC(round(severity$comparator_qalys, 2), format = "f", digits = 2),
  "sev", "Comparator (Standard Care) undiscounted QALYs (PA)",
  "costs_qalys.rds")

add_cmd("sevAbsoluteShortfall",
  formatC(severity$absolute_shortfall, format = "f", digits = 2),
  "sev", "Absolute shortfall AS = QALYsA - PA (rounded 2 dp)",
  "derive_severity (00-parameters.R)")

add_cmd("sevMagnussenGroup",
  as.character(severity$group),
  "sev", "Magnussen severity group for the absolute shortfall",
  "magnussen_bands (00-parameters.R)")

add_cmd("sevMagnussenWeight",
  formatC(severity$weight, format = "f", digits = 1),
  "sev", "Magnussen severity weight for the group",
  "magnussen_bands (00-parameters.R)")

add_cmd("wtpOpportunityCost",
  fmt_cost(opportunity_cost_nok),
  "sev", "DMP opportunity-cost benchmark (NOK/QALY)",
  "00-parameters.R")

add_cmd("wtpSeverityReference",
  fmt_cost(severity$reference_nok),
  "sev", "Severity-weighted cost-effectiveness threshold (NOK/QALY)",
  "derive_severity (00-parameters.R)")

add_cmd("wtpThreshold",
  fmt_cost(wtp_threshold),
  "sev", "Applicable cost-effectiveness threshold = severity-informed reference (NOK/QALY)",
  "04-costs-qalys.R")

# --- Severity at Norwegian registry ages (external-validity sensitivity) ---
# SOURCE: 06b-structural-sa.R scenarios norwegian_registry_age (73) and
#   norwegian_registry_age_women_76 (76); DMP absolute-shortfall (Magnussen et al. 2015).
#   Reads structural_sa_results.csv (ssa, loaded above). All computed; no new input.
# GUARD: emit a "--" sentinel if the scenario row is missing or non-finite, so every
#   macro is ALWAYS defined (an undefined \newcommand breaks the thesis compile).
emit_scenario_severity <- function(suffix, scn_name) {
  row <- ssa[ssa$scenario == scn_name, ]
  ok  <- nrow(row) == 1L && "sev_as" %in% names(row) && is.finite(row$sev_as)
  # Caution: the ICER guard is INDEPENDENT of the severity guard -- a row can carry a
  #   finite ICER but non-finite severity (or vice-versa); each macro guards its own
  #   column so an undefined \newcommand can never reach the thesis compile.
  ok_icer <- nrow(row) == 1L && "icer" %in% names(row) && is.finite(row$icer)
  add_cmd(paste0("sevAbsoluteShortfall", suffix),
    if (ok) formatC(row$sev_as, format = "f", digits = 2) else "--",
    "sev", paste0("Absolute shortfall AS at scenario ", scn_name, " (rounded 2 dp)"),
    "structural_sa_results.csv")
  add_cmd(paste0("sevMagnussenGroup", suffix),
    if (ok) as.character(row$sev_group) else "--",
    "sev", paste0("Magnussen severity group at scenario ", scn_name),
    "structural_sa_results.csv")
  add_cmd(paste0("wtpSeverityReference", suffix),
    if (ok) fmt_cost(row$sev_reference) else "--",
    "sev", paste0("Severity-informed reference threshold at scenario ", scn_name, " (NOK/QALY)"),
    "structural_sa_results.csv")
  # SOURCE: structural_sa_results.csv `icer` column for scenario scn_name
  #   (written in 06b-structural-sa.R by run_structural_sa() as
  #   `scenarios$icer[s_idx] <- scenarios$inc_costs[s_idx] / scenarios$inc_qalys[s_idx]`).
  #   row$icer IS ssa$icer[ssa$scenario==scn_name] --
  #   the same lookup + fmt_icer path as the sibling \ssaICER* macros; NON-HARDCODED.
  # GUIDELINE: ratio-of-means ICER, extended healthcare perspective, stepped 4%
  #   discounting (Rundskriv R-109). Emitted for BOTH re-aged calls (Nor73, Nor76).
  add_cmd(paste0("ssaICER", suffix),
    if (ok_icer) fmt_icer(row$icer) else "--",
    "ssa", paste0("SSA ICER at Norwegian registry-age scenario ", scn_name, " (NOK/QALY)"),
    "structural_sa_results.csv")
}
# Caution: suffix spelled out (NOT "Nor73"/"Nor76") -- LaTeX \newcommand names are
#   letter-only; a digit parses as \...Nor + 73 and breaks the preamble \input
#   (Missing \begin{document}). Matches the \ssaICERageSeventyFour convention;
#   one suffix change renames ALL paste0()-built macros (severity trio + ICER).
emit_scenario_severity("NorSeventyThree", "norwegian_registry_age")
emit_scenario_severity("NorSeventySix", "norwegian_registry_age_women_76")

add_cmd("psaProbCEopportunityCost",
  fmt_pct(prob_ce_cats[1]),
  "sev", "P(CE) at opportunity-cost benchmark (275K) (%)",
  "psa_results.rds")

add_cmd("psaProbCEseverityReference",
  fmt_pct(prob_ce_cats[2]),
  "sev", "P(CE) at severity-informed reference (385K) (%)",
  "psa_results.rds")


# --- Write model-results.tex -------------------------------------------------

tex_lines <- c(
  "% ===========================================================================",
  "% model-results.tex",
  "% Auto-generated by R/09-export-latex-commands.R",
  "% Generated deterministically by the canonical model run.",
  "% DO NOT EDIT MANUALLY. Rerun Rscript main.R to regenerate.",
  "% ===========================================================================",
  ""
)

# Group commands by category
categories <- unique(commands$category)
cat_labels <- c(
  det   = "Deterministic Results",
  psa   = "Probabilistic Analysis",
  evpi  = "Expected Value of Perfect Information",
  evppi = "Expected Value of Partial Perfect Information",
  ssa   = "Structural Sensitivity Analysis",
  powsa = "Probabilistic One-Way Sensitivity Analysis",
  val   = "Model Validation and Fit Statistics",
  sev   = "Severity Framework",
  psa_spec = "PA Specification Sensitivity"
)

for (cat in categories) {
  cat_cmds <- commands[commands$category == cat, ]
  label <- cat_labels[cat]
  if (is.na(label)) label <- cat

  tex_lines <- c(tex_lines,
    paste0("% --- ", label, " ", paste(rep("-", 50), collapse = "")),
    "")

  for (i in seq_len(nrow(cat_cmds))) {
    row <- cat_cmds[i, ]
    tex_lines <- c(tex_lines,
      paste0("% ", row$description),
      paste0("\\newcommand{\\", row$command, "}{", row$value, "}"),
      "")
  }
}

writeLines(tex_lines, "output/model-results.tex")
message("Written: output/model-results.tex (",
        nrow(commands), " commands)")


# --- Write manifest -----------------------------------------------------------

write.csv(commands, "output/model-results-manifest.csv",
          row.names = FALSE)
message("Written: output/model-results-manifest.csv")


# --- Write result provenance registry ---------------------------------------
sha256_file <- function(path) {
  if (!file.exists(path)) stop("09-export: provenance source missing: ", path)
  out <- system2("/usr/bin/shasum", c("-a", "256", path), stdout = TRUE)
  sub("[[:space:]].*$", "", out[[1]])
}

resolve_source_paths <- function(source_file) {
  tokens <- trimws(strsplit(source_file, ";", fixed = TRUE)[[1]])
  resolved <- vapply(tokens, function(token) {
    candidate <- if (file.exists(token)) {
      token
    } else if (file.exists(file.path("data", "processed", token))) {
      file.path("data", "processed", token)
    } else if (file.exists(file.path("output", "tables", token))) {
      file.path("output", "tables", token)
    } else if (file.exists(file.path("data", "raw", token))) {
      file.path("data", "raw", token)
    } else if (grepl("00-parameters\\.R", token)) {
      file.path("R", "00-parameters.R")
    } else if (grepl("04-costs-qalys\\.R", token)) {
      file.path("R", "04-costs-qalys.R")
    } else {
      stop("09-export: no provenance source path for: ", token)
    }
    normalizePath(candidate, winslash = "/", mustWork = TRUE)
  }, character(1))
  unname(resolved)
}

hash_source_set <- function(paths) {
  entries <- paste(basename(paths), vapply(paths, sha256_file, character(1)),
                   sep = "=")
  tmp <- tempfile("model-results-source-set-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(entries, tmp, useBytes = TRUE)
  sha256_file(tmp)
}

source_sets <- lapply(commands$source_file, resolve_source_paths)
provenance <- data.frame(
  concept_id = commands$command,
  owner_kind = "latex_macro",
  owner_path = "output/model-results.tex",
  owner_key = commands$command,
  current_rendered_value = commands$value,
  parameter_identity = commands$description,
  source_object = vapply(source_sets, function(x) {
    paste(sub(paste0("^", normalizePath(".", winslash = "/"), "/"), "", x),
          collapse = ";")
  }, character(1)),
  source_object_sha256 = vapply(source_sets, hash_source_set, character(1)),
  check.names = FALSE, stringsAsFactors = FALSE)
write.csv(provenance, "output/model-results-provenance.csv", row.names = FALSE)
message("Written: output/model-results-provenance.csv")


# --- Export function for main.R -----------------------------------------------

export_latex_commands <- function() {
  message("LaTeX commands exported: output/model-results.tex (",
          nrow(commands), " commands)")
}
