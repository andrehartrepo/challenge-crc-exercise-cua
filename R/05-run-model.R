# =============================================================================
# 05-run-model.R
# Deterministic Supplementary Analysis
#
# NOTE: PSA is the base case analysis.
# This deterministic analysis is supplementary  --  reported for transparency.
# The deterministic ICER is a special case of PSA with N=1 (point estimates).
#
# Input:  data/processed/costs_qalys.rds
#
# Output: output/tables/deterministic_results.csv
#
# Packages: dplyr
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   Deterministic ICER as a special case of PSA: Briggs, Claxton, Sculpher
#     (2006). Decision Modelling for Health Economic Evaluation, Oxford
#     University Press, Chapter 5. The point-estimate ICER is reported as
#     a transparency anchor; PSA is the base-case analysis method.
#   ICER calculation pattern (incremental costs / incremental QALYs):
#     standard ISPOR-SMDM Modeling Good Research Practices (Briggs et al.
#     2012, Value in Health 15(6):835-842).
#   Aggregation by arm and net benefit calculation: standard CEA pattern
#     in textbook implementations; no specific published source cloned.
#   This file is the only original-code file in the model. The function
#     run_base_case() is a bespoke wrapper around dplyr aggregation calls;
#     no published implementation precedent.
#
# Adaptations:
#   1. Hard NA pre-flight check prevents silent under-counting
#      via na.rm = TRUE. Assertion functions in 00-parameters.R catch NAs
#      upstream; this layer is a defence in depth.
#
# Academic citations:
#   Briggs A, Claxton K, Sculpher MJ (2006). Decision Modelling for
#     Health Economic Evaluation, Oxford University Press. ISBN 9780198526629.
#   Briggs AH et al. (2012). Value in Health 15(6):835-842.
#     DOI 10.1016/j.jval.2012.04.014. PMID 22999133.
# =============================================================================

run_base_case <- function(costs_qalys,
                           arm_int = "Exercise",
                           arm_ctrl = "Standard Care") {
  # na.rm = TRUE is not used. NAs in costs/QALYs indicate upstream
  # computation failure and must NOT be silently dropped. The assertion
  # functions (assert_costs_qalys) catch NAs before this point.
  # If NAs reach here, sum() returns NA, which propagates to the ICER
  # and surfaces the problem visibly.
  # Caution: na.rm = TRUE can silently produce understated totals, potentially
  # flipping ICER direction.

  # Pre-flight NA check: fail fast with informative error
  if (any(is.na(costs_qalys$qalys_disc)) || any(is.na(costs_qalys$costs_disc))) {
    stop("run_base_case: NA values detected in costs_qalys. ",
         "NA qalys_disc: ", sum(is.na(costs_qalys$qalys_disc)),
         ", NA costs_disc: ", sum(is.na(costs_qalys$costs_disc)),
         ". Fix upstream computation before running base case.")
  }

  summary_tbl <- costs_qalys |>
    dplyr::group_by(arm) |>
    dplyr::summarise(
      total_qalys = sum(qalys_disc),
      total_costs = sum(costs_disc),
      total_ly_disc = sum(ly_disc),
      total_ly_raw = sum(ly_raw),
      total_qalys_raw = sum(qalys_raw),
      total_costs_raw = sum(costs_raw),
      .groups = "drop"
    )

  # Arm names are configurable via parameters
  int_row  <- summary_tbl[summary_tbl$arm == arm_int, ]
  ctrl_row <- summary_tbl[summary_tbl$arm == arm_ctrl, ]

  if (nrow(int_row) != 1 || nrow(ctrl_row) != 1) {
    stop("run_base_case: expected exactly one row per arm. ",
         "Arms found: ", paste(summary_tbl$arm, collapse = ", "),
         ". Intervention rows: ", nrow(int_row),
         ", Control rows: ", nrow(ctrl_row))
  }

  # GUIDELINE: Calculate incremental cost as exercise minus standard care (Drummond et al. 2015)
  inc_costs  <- int_row$total_costs  - ctrl_row$total_costs
  # GUIDELINE: Calculate incremental QALYs as exercise minus standard care (Drummond et al. 2015)
  inc_qalys  <- int_row$total_qalys  - ctrl_row$total_qalys
  # GUIDELINE: Calculate incremental discounted life-years as exercise minus standard care (Drummond et al. 2015)
  inc_ly_disc <- int_row$total_ly_disc - ctrl_row$total_ly_disc
  # GUIDELINE: Calculate incremental undiscounted life-years as exercise minus standard care (Drummond et al. 2015)
  inc_ly_raw  <- int_row$total_ly_raw  - ctrl_row$total_ly_raw
  # GUIDELINE: Calculate incremental undiscounted QALYs as exercise minus standard care (Drummond et al. 2015)
  inc_qalys_raw <- int_row$total_qalys_raw - ctrl_row$total_qalys_raw
  # GUIDELINE: Calculate incremental undiscounted cost as exercise minus standard care (Drummond et al. 2015)
  inc_costs_raw <- int_row$total_costs_raw - ctrl_row$total_costs_raw

  # Guard against division by zero when incremental QALYs are negligible
  if (abs(inc_qalys) < 1e-10) {
    warning("Deterministic ICER undefined: incremental QALYs near zero (",
            inc_qalys, "). Setting ICER to NA.")
    icer <- NA_real_
  } else {
    # GUIDELINE: Calculate the ICER as incremental cost divided by incremental QALYs (DMP 2026; Drummond et al. 2015)
    icer <- inc_costs / inc_qalys
  }

  # GUIDELINE: Cost per LYG (DMP Section 12.3 requires reporting alongside ICER)
  # GUIDELINE: DMP retningslinjer Section 12.3
  cost_per_ly <- if (abs(inc_ly_disc) < 1e-10) NA_real_ else inc_costs / inc_ly_disc

  results <- data.frame(
    arm             = c("Standard Care", "Exercise", "Incremental"),
    total_qalys     = c(ctrl_row$total_qalys, int_row$total_qalys, inc_qalys),
    total_qalys_raw = c(ctrl_row$total_qalys_raw, int_row$total_qalys_raw, inc_qalys_raw),
    total_ly_disc   = c(ctrl_row$total_ly_disc, int_row$total_ly_disc, inc_ly_disc),
    total_ly_raw    = c(ctrl_row$total_ly_raw,  int_row$total_ly_raw,  inc_ly_raw),
    total_costs_nok = c(ctrl_row$total_costs,   int_row$total_costs,   inc_costs),
    total_costs_raw = c(ctrl_row$total_costs_raw, int_row$total_costs_raw, inc_costs_raw),
    icer_nok_qaly   = c(NA, NA, icer),
    cost_per_ly     = c(NA, NA, cost_per_ly)
  )

  results
}

# --- Placeholder execution ---------------------------------------------------
# Caution: this block consumes wtp_threshold and severity, which are assigned
# only inside 04-costs-qalys.R's TESTTHAT-guarded block. It must carry the
# same guard (codebase idiom: 03-build-psm.R, 04-costs-qalys.R, 06-psa.R),
# else test suites that source this file fail at source time.
if (!identical(Sys.getenv("TESTTHAT"), "true") &&
    file.exists("data/processed/costs_qalys.rds")) {
  costs_qalys <- readRDS("data/processed/costs_qalys.rds")

  base_case_results <- run_base_case(costs_qalys)

  write.csv(base_case_results,
            "output/tables/deterministic_results.csv",
            row.names = FALSE)

  message("\n=== DETERMINISTIC RESULTS (Supplementary) ===")
  icer_val <- base_case_results$icer_nok_qaly[3]
  inc_q    <- base_case_results$total_qalys[3]
  inc_c    <- base_case_results$total_costs_nok[3]

  # Guard sprintf against NA ICER
  message(sprintf("Incremental QALYs:  %.4f", inc_q))
  message(sprintf("Incremental costs:  %.0f NOK", inc_c))
  # GUIDELINE: applicable WTP is the computed severity-informed reference (DMP
  #   Magnussen group), not a fixed constant; also report the opportunity-cost
  #   benchmark (DMP; Magnussen et al. 2015).
  if (is.na(icer_val)) {
    message("Deterministic ICER: UNDEFINED (incremental QALYs near zero)")
    message(sprintf("Severity-informed threshold: %s NOK/QALY (DMP Magnussen group %d)",
                    format(wtp_threshold, big.mark = ","), severity$group))
    message(sprintf("Opportunity-cost benchmark: %s NOK/QALY",
                    format(opportunity_cost_nok, big.mark = ",")))
    message("Cost-effective at threshold: INDETERMINATE (ICER undefined)")
  } else {
    message(sprintf("Deterministic ICER: %.0f NOK/QALY", icer_val))
    message(sprintf("Severity-informed threshold: %s NOK/QALY (DMP Magnussen group %d)",
                    format(wtp_threshold, big.mark = ","), severity$group))
    message(sprintf("Cost-effective at severity reference: %s",
                    ifelse(icer_val <= wtp_threshold, "YES", "NO")))
    message(sprintf("Cost-effective at opportunity cost (%s NOK/QALY): %s",
                    format(opportunity_cost_nok, big.mark = ","),
                    ifelse(icer_val <= opportunity_cost_nok, "YES", "NO")))
  }
  message("NOTE: PSA results (06-psa.R) are the primary base case.")

} else {
  message("TODO: Run 04-costs-qalys.R first.")
}
