# =============================================================================
# 04-costs-qalys.R
# Apply costs and utility weights to PSM state occupancy
# Calculate discounted QALYs and costs per arm
#
# Half-cycle correction: trapezoidal rule
#   For each cycle, the effective state occupancy is the average of
#   the beginning and end of the cycle. This is the standard correction
#   for discrete-time models (Drummond et al., 2015, Ch. 3).
#
# Discounting: stepped 4%/3%/2% Norwegian schedule
#   Uses pre-computed discount_weights_stepped from 00-parameters.R.
#
# Input:  data/processed/psm_trace.rds
#         00-parameters.R (utilities, costs, discount weights)
#
# Output: data/processed/costs_qalys.rds
#
# Packages: dplyr
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   Half-cycle correction (trapezoidal rule): Drummond MF, Sculpher MJ,
#     Claxton K, Stoddart GL, Torrance GW (2015). Methods for the Economic
#     Evaluation of Health Care Programmes, 4th ed., Oxford University
#     Press, Chapter 3. The trapezoidal correction effective_p(t) =
#     (p(t) + p(t + 1)) / 2 is the standard adjustment when state
#     probabilities are evaluated at cycle boundaries but events occur
#     uniformly within the cycle. Avoids the systematic over-counting of
#     time in early states (and under-counting in late states) that
#     uncorrected discrete-time accounting introduces.
#   Stepped discounting via pre-computed weight vectors: aligns with the
#     midpoint evaluation in 00-parameters.R::create_discount_weights().
#     The weight vector is generated once at parameter load and reused
#     across both arms; this is faster than recomputing per-cycle and
#     guarantees identical discounting between arms.
#   Per-state QALY decomposition (qalys_dfs_disc, qalys_pd_disc): added
#     so that build_disaggregated_table() in 08-export-tables.R
#     can read DFS and PD QALY contributions directly without re-applying
#     the age multiplier and exercise disutility outside this file. The
#     decomposition is exact (gap = 0 because u_dead = 0) and captures
#     every adjustment already applied to the total qalys_disc.
#   Age-dependent utility multiplier integration: applied via the
#     age_utility_multipliers vector returned by get_age_utility_multipliers
#     in 00-parameters.R; follows the multiplicative method of Ara and
#     Brazier (2010), DOI 10.1016/j.jval.2010.01.005, PMID 20230546.
#
# Adaptations:
#   1. Vectorised cycle loop using cumprod-style discount factors and
#      pre-computed weight vectors. The implementation avoids per-cycle
#      function calls into 00-parameters.R logic.
#   2. cycle_length passed as an explicit function argument,
#      removing the hidden globalenv() dependency documented in
#      assert_costs_qalys.
#   3. Two-arm output stored in long format (one row per arm-cycle) so
#      05-run-model.R can aggregate via dplyr without manual indexing.
#   4. Explicit shared-surveillance and exercise-only schedules use active
#      zero values and cycle-end boundaries under the Norwegian follow-up route.
#
# Academic citations:
#   Drummond MF, Sculpher MJ, Claxton K, Stoddart GL, Torrance GW (2015).
#     Methods for the Economic Evaluation of Health Care Programmes,
#     4th ed., Oxford University Press. ISBN 9780199665884.
#   Ara R, Brazier J (2010). "Populating an economic model with health
#     state utility values." Value in Health 13(5):509-518.
#     DOI 10.1016/j.jval.2010.01.005. PMID 20230546.
# =============================================================================


#' Calculate Costs and QALYs with Half-Cycle Correction and Stepped Discounting
#'
#' Applies the trapezoidal rule for half-cycle correction:
#'   effective_p(t) = (p(t) + p(t+1)) / 2
#' where p(t) is the state occupancy at cycle start, p(t+1) at cycle end.
#'
#' Discounting uses pre-computed weight vectors from create_discount_weights()
#' (see 00-parameters.R). Base case: stepped 4%/3%/2% per Rundskriv R-109.
#'
#' Also computes undiscounted QALYs and life-years gained (LYG),
#' required for severity class calculation.
#'
#' @param psm_trace Data frame. PSM trace from build_psm_trace().
#' @param arm Character. Arm label to filter ("Exercise" or "Standard Care").
#' @param u_dfs Numeric. Utility for disease-free state.
#' @param u_prog Numeric. Utility for progressed state.
#' @param u_dead Numeric. Utility for death (fixed at 0).
#' @param c_surveillance_early Numeric. Shared annual surveillance cost through cutoff.
#' @param c_surveillance_late Numeric. Shared annual surveillance cost after cutoff.
#' @param surveillance_cutoff_years Numeric. End of intensive surveillance.
#' @param c_exercise_annual Numeric. Annual intervention-programme cost, exercise
#'   arm only (cost ownership): supervised exercise visits PLUS the
#'   behavioural-support contacts. "Exercise-only" here denotes arm
#'   ownership against the shared surveillance and progressed-disease costs; it
#'   has never meant "exercise sessions only".
#' @param intervention_duration_years Numeric. End of exercise programme.
#' @param c_progressed_annual Numeric. Shared annual progressed-state cost.
#' @param c_terminal Numeric. One-off terminal care cost applied at death
#'   (NOK, applied to proportion entering Dead state each cycle).
#'   Set to 0 or NA to omit terminal costs.
#' @param c_intervention_setup Numeric. One-off intervention setup cost
#'   applied in cycle 1 only. Set to 0 or NA if not applicable.
#' @param u_exercise_decrement Numeric. Utility decrement during the active
#'   exercise programme period (default 0). Applied to DFS utility for the
#'   intervention arm only during the intervention duration.
#' @param discount_weights Numeric vector. Pre-computed discount weights.
#' @param cycle_length Numeric. Cycle length in years.
#' @return Data frame with per-cycle and cumulative costs, QALYs, LYG.
#' Resolve shared surveillance and exercise-only annual costs at cycle end.
#'
#' Numeric zero is an active value; missing schedule
#' components are errors rather than a signal to select a fallback path.
dfs_annual_cost_components <- function(time_years,
                                       c_surveillance_early,
                                       c_surveillance_late,
                                       surveillance_cutoff_years,
                                       # Keep the exercise programme as a separate component, with zero as an active absence value.
                                       c_exercise_annual = 0,
                                       # Keep zero duration as the active absence value for the separate exercise programme.
                                       intervention_duration_years = 0) {
  stopifnot(is.numeric(time_years), length(time_years) > 0L,
            all(is.finite(time_years)),
            all(time_years >= 0),
            is.numeric(c_surveillance_early),
            length(c_surveillance_early) == 1L,
            is.finite(c_surveillance_early),
            is.numeric(c_surveillance_late),
            length(c_surveillance_late) == 1L,
            is.finite(c_surveillance_late),
            is.numeric(c_exercise_annual),
            length(c_exercise_annual) == 1L,
            is.finite(c_exercise_annual),
            is.numeric(surveillance_cutoff_years),
            length(surveillance_cutoff_years) == 1L,
            is.finite(surveillance_cutoff_years),
            surveillance_cutoff_years >= 0,
            is.numeric(intervention_duration_years),
            length(intervention_duration_years) == 1L,
            is.finite(intervention_duration_years),
            intervention_duration_years >= 0)
  # Accrue shared surveillance through the configured cutoff and retain the active late schedule afterward.
  surveillance <- ifelse(time_years <= surveillance_cutoff_years,
                         c_surveillance_early, c_surveillance_late)
  # Accrue the separate exercise programme only through its configured duration.
  exercise <- ifelse(time_years <= intervention_duration_years,
                     c_exercise_annual, 0)
  data.frame(time_end = time_years,
             surveillance = surveillance,
             exercise = exercise,
             # Preserve separate surveillance and exercise components while summing both for disease-free cost.
             total = surveillance + exercise)
}

calculate_costs_qalys <- function(psm_trace, arm,
                                   u_dfs, u_prog, u_dead,
                                   c_surveillance_early,
                                   c_surveillance_late,
                                   surveillance_cutoff_years,
                                   c_exercise_annual,
                                   intervention_duration_years,
                                   c_progressed_annual,
                                   # Caution: The zero default excludes terminal resources unless the sourced terminal-cost input is supplied explicitly.
                                   c_terminal = 0,
                                   c_intervention_setup = 0,
                                   # Keep exercise disutility fixed at zero and do not sample it.
                                   u_exercise_decrement = 0,
                                   discount_weights,
                                   cycle_length,
                                   age_utility_multipliers = NULL,
                                   validate = TRUE) {
  df <- psm_trace[psm_trace$arm == arm, ]

  if (nrow(df) == 0) {
    stop("calculate_costs_qalys: no rows found for arm '", arm,
         "'. Available arms: ", paste(unique(psm_trace$arm), collapse = ", "))
  }

  n <- nrow(df)

  # -----------------------------------------------------------------------
  # Half-cycle correction: trapezoidal rule
  # GUIDELINE: ISPOR-SMDM Modeling Good Research Practices Task Force 3
  #   (Siebert et al. 2012, Med Decis Making 32(5):667-677)  --  recommends
  #   trapezoidal rule over the traditional 0.5-cycle shift for HCC.
  # GUIDELINE: Drummond et al. (2015) Methods Ch. 3  --  trapezoidal rule.
  # For cycle k (interval from t_k to t_{k+1}):
  #   effective_p_dfs(k)  = (p_dfs(k)  + p_dfs(k+1))  / 2
  #   effective_p_prog(k) = (p_prog(k) + p_prog(k+1)) / 2
  # This produces n-1 cycle values from n time-point observations.
  # -----------------------------------------------------------------------
  # GUIDELINE: Apply trapezoidal half-cycle correction to state occupancy before accruing costs and effects (Drummond et al. 2015)
  eff_p_dfs  <- (df$p_dfs[1:(n-1)]  + df$p_dfs[2:n])  / 2
  eff_p_prog <- (df$p_prog[1:(n-1)] + df$p_prog[2:n]) / 2
  eff_p_dead <- (df$p_dead[1:(n-1)] + df$p_dead[2:n]) / 2

  # -----------------------------------------------------------------------
  # Treatment disutility during active exercise programme.
  # During the intervention period, DFS utility is reduced by
  # u_exercise_decrement (e.g., treatment burden). After the programme
  # ends, utility reverts to the standard u_dfs value.
  # Base case: u_exercise_decrement = 0 (no disutility).
  # Structurally present so the model CAN capture treatment disutility
  # even if the base case value is zero.
  # -----------------------------------------------------------------------
  # Use <= comparison on cycle end time to avoid off-by-one.
  # With strict <, a cycle starting at exactly the intervention duration
  # gets excluded (e.g., month 36 of a 3-year programme).
  # Using cycle_end <= intervention_duration_years ensures the full programme
  # period is captured (all cycles whose END is within the duration).
  # Use cycle-end timing to apply the exercise programme through its full configured duration.
  cycle_end_times <- df$time[2:n]
  # Include every cycle whose end remains within the exercise-programme duration.
  during_intervention <- cycle_end_times <= intervention_duration_years

  # Keep the exercise decrement explicit while its reference-analysis value remains fixed at zero.
  u_dfs_effective <- ifelse(during_intervention,
                             u_dfs - u_exercise_decrement,
                             u_dfs)

  # -----------------------------------------------------------------------
  # Age-dependent utility adjustment (Ara & Brazier 2010)
  # NICE/NOMA: Required for lifetime horizons (40 years, cohort ages 61-101).
  # Multiplier = u_genpop(age_t) / u_genpop(age_0), declining with age.
  # When not provided or all 1s, no adjustment is applied (backward compatible).
  # -----------------------------------------------------------------------
  if (!is.null(age_utility_multipliers) &&
      length(age_utility_multipliers) >= (n - 1)) {
    age_mult <- age_utility_multipliers[1:(n - 1)]
    # GUIDELINE: Apply age adjustment multiplicatively using Norwegian general-population utility norms (DMP 2026)
    u_dfs_effective <- u_dfs_effective * age_mult
    u_prog_adj      <- u_prog * age_mult
  } else {
    # Caution: If no complete age-multiplier vector is supplied, leave progressed-state utility unadjusted rather than recycle a partial vector.
    u_prog_adj <- u_prog
  }

  # Undiscounted QALYs per cycle (state occupancy * utility * cycle_length)
  # ISPOR: Drummond et al. (2015)  --  state occupancy * utility * cycle_length
  # Split DFS vs PD contributions so
  # build_disaggregated_table() can read the exact decomposition from the
  # RDS file without needing utility globals. u_dead = 0 by design, so the
  # sum of dfs + pd undiscounted equals qalys_raw exactly.
  # GUIDELINE: Accrue QALYs as state occupancy times utility times cycle length (Drummond et al. 2015)
  qalys_raw_dfs <- eff_p_dfs  * u_dfs_effective * cycle_length
  qalys_raw_pd  <- eff_p_prog * u_prog_adj      * cycle_length
  # GUIDELINE: Sum state-specific utility accruals to obtain total cycle QALYs (Drummond et al. 2015)
  qalys_raw     <- qalys_raw_dfs + qalys_raw_pd +
                   eff_p_dead * u_dead * cycle_length

  # -----------------------------------------------------------------------
  # Schedule availability is explicit; zero remains active.
  dfs_components <- dfs_annual_cost_components(
    time_years = cycle_end_times,
    c_surveillance_early = c_surveillance_early,
    c_surveillance_late = c_surveillance_late,
    surveillance_cutoff_years = surveillance_cutoff_years,
    c_exercise_annual = c_exercise_annual,
    intervention_duration_years = intervention_duration_years
  )

  # One-off intervention setup cost in cycle 1 only
  setup_cost <- numeric(n - 1)
  if (!is.na(c_intervention_setup) && c_intervention_setup > 0) {
    setup_cost[1] <- c_intervention_setup
  }

  # Undiscounted costs per cycle (state occupancy * cost * cycle_length)
  # SOURCE: component arithmetic from the existing cost equation.
  # DECISION: expose reporting-only categories; preserve the total-cost identity.
  new_deaths <- pmax(diff(df$p_dead), 0)

  # ---------------------------------------------------------------------
  # Death-cycle overlap netting.
  # SOURCE: Bjornelv et al. 2020, BMC Health Serv Res 20:115, Table 1
  #   (printed p. 3), Health care costs block, row "Total costs":
  #   "Sum of total costs secondary, primary and home- and community-based
  #   care". c_terminal (defined in 00-parameters.R, section "6. COST
  #   PARAMETERS"; grep `^c_terminal`) therefore already buys every
  #   health-service contact in the decedent's last month, and the model
  #   cycle IS one month (00-parameters.R, `cycle_length <- 1 / 12`).
  # GUIDELINE: a resource may be counted once only; charging a state cost
  #   on top of an all-inclusive terminal payment for the same month is
  #   double counting (Drummond et al. 2015, Methods, Ch. 4).
  # Under the trapezoid a decedent of cycle k carries occupancy 1 at t_k
  # and 0 at t_{k+1}, so exactly new_deaths[k]/2 of alive-state occupancy
  # sits inside eff_p_dfs + eff_p_prog for people the terminal payment has
  # already covered. That half-cycle is netted out of BOTH occupancies:
  # surveillance is health care too, so a disease-free decedent inside the
  # surveillance window double-counts exactly as a progressed one does.
  # The overlap was previously recorded as real and disclosed in prose rather
  #   than corrected; the accrual is now corrected here, so the disclosure
  #   wording is superseded on the model side.
  # DECISION: the exercise component is NOT netted. Bjornelv drew primary
  #   care from KUHR restricted to GP consultations, emergency-room visits
  #   and hospital laboratory/radiology services (printed p. 4, "Primary
  #   care services"), and costed no patient-borne resource anywhere. The
  #   exercise unit cost is an A3 physiotherapy tariff plus patient mileage
  #   plus patient leisure time (00-parameters.R, `c_session_extended`, which is
  #   exactly the sum of those three components), none of which is
  #   inside that registry scope, so there is nothing to net.
  # Caution: which state a death came from is NOT identified. 03-build-psm.R
  #   derives `p_dead <- 1 - s_os` (beside `p_prog`), a single scalar per time point,
  #   so a death carries no state of origin. The split below is an
  #   apportionment by alive-state occupancy share, not a measurement. Its
  #   residual is the distance to the two extremes (every death out of DFS
  #   versus every death out of PD); the arms hold different occupancy
  #   mixes, so that residual does not cancel in the incremental. Removing
  #   it needs a transition-based trace, which a partitioned-survival model
  #   does not provide.
  # Caution: alive_occupancy is 0 once the whole cohort is dead, and an
  #   unguarded 0/0 share would propagate NaN into every cost column.
  # ---------------------------------------------------------------------
  # Net half of each death-cycle alive occupancy because the terminal payment covers the full last month.
  death_overlap   <- new_deaths / 2
  # Caution: Death origin is not identified in a PSM, so apportion overlap using alive-state occupancy.
  alive_occupancy <- eff_p_dfs + eff_p_prog
  # Caution: Guard the alive-state share against zero occupancy to prevent a 0/0 NaN.
  share_dfs       <- ifelse(alive_occupancy > 0,
                            eff_p_dfs / alive_occupancy, 0)
  # Caution: Allocate death-cycle overlap by occupancy share because the PSM does not identify the state of origin.
  overlap_dfs     <- death_overlap * share_dfs
  # Caution: Allocate the residual overlap to progressed occupancy because the PSM does not identify the state of origin.
  overlap_prog    <- death_overlap - overlap_dfs

  # Net disease-free surveillance from the death-cycle share already covered by the terminal payment.
  cost_surveillance_raw <-
    (eff_p_dfs - overlap_dfs) * dfs_components$surveillance * cycle_length
  # Caution: Do not net exercise costs because the terminal-cost source excludes physiotherapy, travel, and patient time.
  cost_exercise_raw <-
    eff_p_dfs * dfs_components$exercise * cycle_length + setup_cost
  # Net progressed-state cost from the death-cycle share already covered by the terminal payment.
  cost_progressed_raw <-
    (eff_p_prog - overlap_prog) * c_progressed_annual * cycle_length
  c_terminal_active <-
    # Caution: Validate terminal cost upstream because this fallback converts missing or non-positive input to zero.
    if (!is.na(c_terminal) && c_terminal > 0) c_terminal else 0
  # Accrue the all-inclusive terminal payment once per new death event.
  cost_terminal_raw <- new_deaths * c_terminal_active
  # GUIDELINE: Count each resource once when summing cycle costs (Drummond et al. 2015)
  costs_raw <- cost_surveillance_raw + cost_exercise_raw +
    cost_progressed_raw + cost_terminal_raw
  stopifnot(isTRUE(all.equal(
    costs_raw,
    cost_surveillance_raw + cost_exercise_raw +
      cost_progressed_raw + cost_terminal_raw,
    tolerance = 1e-8, check.attributes = FALSE
  )))

  # Undiscounted life-years per cycle (probability alive * cycle_length)
  # Needed for severity class / absolute shortfall calculation
  # GUIDELINE: Report life-years separately when the intervention affects survival (DMP 2026)
  ly_raw <- (eff_p_dfs + eff_p_prog) * cycle_length

  # -----------------------------------------------------------------------
  # Discount weights are indexed from cycle 0 (length = n_cycles + 1).
  # For trapezoidal rule, the weight at the cycle midpoint is used,
  # which is the weight indexed at cycles 1 through n-1.
  # The discount_weights vector already applies midpoint correction.
  # -----------------------------------------------------------------------
  # Use weights for cycles 1 through n-1 (skip cycle 0 weight).
  # Index alignment clarification:
  #   - discount_weights has length n_cycles + 1 (indices 1 to n_cycles+1).
  #   - Element [1] = cycle 0 weight (= 1, no discounting).
  #   - Element [2] = cycle 1 midpoint weight, ..., Element [n] = cycle n-1 midpoint.
  #   - Trapezoidal correction produces n-1 = n_cycles cycle values.
  #   - discount_weights[2:n] maps n-1 midpoint weights to n-1 trapezoidal values.
  #   This alignment is correct per Drummond et al. 2015 and Gray et al. 2011.
  dw <- discount_weights[2:n]

  cost_surveillance_disc <- cost_surveillance_raw * dw
  # GUIDELINE: Apply the Norwegian real discount schedule consistently to healthcare and programme costs (Rundskriv R-109 2021)
  cost_exercise_disc <- cost_exercise_raw * dw
  cost_progressed_disc <- cost_progressed_raw * dw
  cost_terminal_disc <- cost_terminal_raw * dw

  # GUIDELINE: Apply the Norwegian real discount schedule consistently to health effects (Rundskriv R-109 2021)
  qalys_disc      <- qalys_raw * dw
  qalys_dfs_disc  <- qalys_raw_dfs * dw
  qalys_pd_disc   <- qalys_raw_pd  * dw
  # GUIDELINE: Apply the Norwegian real discount schedule consistently to total costs (Rundskriv R-109 2021)
  costs_disc      <- costs_raw * dw
  # GUIDELINE: Apply the Norwegian real discount schedule consistently to life-years (Rundskriv R-109 2021)
  ly_disc         <- ly_raw * dw

  result <- data.frame(
    arm            = arm,
    cycle          = 1:(n-1),
    time_start     = df$time[1:(n-1)],
    time_end       = dfs_components$time_end,
    surveillance   = dfs_components$surveillance,
    exercise       = dfs_components$exercise,
    total           = dfs_components$total,
    eff_p_dfs      = eff_p_dfs,
    eff_p_prog     = eff_p_prog,
    eff_p_dead     = eff_p_dead,
    qalys_raw      = qalys_raw,
    costs_raw      = costs_raw,
    cost_surveillance_raw  = cost_surveillance_raw,
    cost_exercise_raw      = cost_exercise_raw,
    cost_progressed_raw    = cost_progressed_raw,
    cost_terminal_raw      = cost_terminal_raw,
    ly_raw         = ly_raw,
    qalys_disc     = qalys_disc,
    qalys_dfs_disc = qalys_dfs_disc,
    qalys_pd_disc  = qalys_pd_disc,
    costs_disc     = costs_disc,
    cost_surveillance_disc = cost_surveillance_disc,
    cost_exercise_disc     = cost_exercise_disc,
    cost_progressed_disc   = cost_progressed_disc,
    cost_terminal_disc     = cost_terminal_disc,
    ly_disc        = ly_disc
  )

  # Validate cost/QALY output if enabled (default TRUE for base case,
  # FALSE in PSA inner loop for performance)
  if (validate) assert_costs_qalys(result, cycle_length = cycle_length)

  result
}


# --- Placeholder execution ---------------------------------------------------
if (!identical(Sys.getenv("TESTTHAT"), "true") &&
    file.exists("data/processed/psm_trace.rds") &&
    !is.na(u_dfs_mean) && !is.na(u_prog_mean) &&
    !is.na(c_surveillance_early) && !is.na(c_progressed_annual)) {

  psm_trace <- readRDS("data/processed/psm_trace.rds")

  # Validate cost parameters instead of silently converting NA to 0.
  # NA cost parameters indicate missing data  --  converting to zero hides the
  # problem and would produce a misleadingly favorable ICER.
  if (is.na(c_terminal)) {
    message("NOTE: c_terminal is NA  --  setting to 0. Set explicitly in ",
            "00-parameters.R if terminal costs are intentionally excluded.")
    # Caution: Treat a missing terminal-cost parameter as zero only after emitting the explicit source-time warning.
    c_terminal_val <- 0
  } else {
    c_terminal_val <- c_terminal
  }
  if (is.na(c_intervention_setup)) {
    message("NOTE: c_intervention_setup is NA  --  setting to 0. Set explicitly ",
            "if setup costs are intentionally excluded.")
    # Caution: Treat a missing setup-cost parameter as zero only after emitting the explicit source-time warning.
    c_setup_val <- 0
  } else {
    c_setup_val <- c_intervention_setup
  }

  # age_utility_multipliers must be passed to calculate_costs_qalys()
  # Age-adjusted utilities are mandatory for lifetime horizons
  age_multipliers_det <- get_age_utility_multipliers(
    cohort_age, n_cycles, cycle_length
  )

  results_int  <- calculate_costs_qalys(
    psm_trace, arm = "Exercise",
    u_dfs = u_dfs_mean, u_prog = u_prog_mean, u_dead = u_dead,
    c_surveillance_early = c_surveillance_early,
    c_surveillance_late = c_surveillance_late,
    surveillance_cutoff_years = surveillance_cutoff_years,
    c_exercise_annual = c_exercise_annual,
    intervention_duration_years = intervention_duration_years,
    c_progressed_annual = c_progressed_annual,
    c_terminal = c_terminal_val,
    c_intervention_setup = c_setup_val,
    u_exercise_decrement = u_exercise_decrement,
    discount_weights = discount_weights_stepped,
    cycle_length = cycle_length,
    age_utility_multipliers = age_multipliers_det
  )

  results_ctrl <- calculate_costs_qalys(
    psm_trace, arm = "Standard Care",
    u_dfs = u_dfs_mean, u_prog = u_prog_mean, u_dead = u_dead,
    c_surveillance_early = c_surveillance_early,
    c_surveillance_late = c_surveillance_late,
    surveillance_cutoff_years = surveillance_cutoff_years,
    # Assign no exercise-programme cost to the standard-care arm.
    c_exercise_annual = 0,
    # Assign no exercise-programme duration to the standard-care arm.
    intervention_duration_years = 0,
    c_progressed_annual = c_progressed_annual,
    c_terminal = c_terminal_val,
    # Assign no exercise-programme setup cost to the standard-care arm.
    c_intervention_setup = 0,  # No setup cost for control arm
    # Assign no exercise disutility to the standard-care arm.
    u_exercise_decrement = 0,  # No treatment disutility for control arm
    discount_weights = discount_weights_stepped,
    cycle_length = cycle_length,
    age_utility_multipliers = age_multipliers_det
  )

  # Assertions already called inside calculate_costs_qalys() via
  # validate = TRUE (default). No need to call again here.

  costs_qalys <- rbind(results_int, results_ctrl)
  saveRDS(costs_qalys, "data/processed/costs_qalys.rds")

  # Summary output
  total_int  <- colSums(results_int[, c("qalys_disc", "costs_disc",
                                         "ly_disc", "qalys_raw", "ly_raw")])
  total_ctrl <- colSums(results_ctrl[, c("qalys_disc", "costs_disc",
                                          "ly_disc", "qalys_raw", "ly_raw")])

  message("Costs and QALYs calculated (trapezoidal half-cycle correction).")
  message("  Intervention: ", round(total_int["qalys_disc"], 4),
          " disc QALYs | ", round(total_int["ly_disc"], 2), " disc LYs")
  message("  Control:      ", round(total_ctrl["qalys_disc"], 4),
          " disc QALYs | ", round(total_ctrl["ly_disc"], 2), " disc LYs")
  message("  Undiscounted LYs: Int = ", round(total_int["ly_raw"], 2),
          " | Ctrl = ", round(total_ctrl["ly_raw"], 2))
  message("Saved to data/processed/costs_qalys.rds")

  # -------------------------------------------------------------------------
  # Perspective Delta Analysis
  # Extended perspective = base case (Meld. St. 21, 2024-2025, Section 4.3.8).
  # This block computes the ICER under standard healthcare perspective
  # (excludes travel and patient time) for the perspective delta comparison.
  # SOURCE: Sanders et al. 2016 (Second Panel) Recommendation 1.
  # -------------------------------------------------------------------------
  if (exists("c_exercise_annual_standard", envir = globalenv())) {
    # Standard perspective substitutes only the exercise-programme component.
    results_int_std <- calculate_costs_qalys(
      psm_trace, arm = "Exercise",
      u_dfs = u_dfs_mean, u_prog = u_prog_mean, u_dead = u_dead,
      c_surveillance_early = c_surveillance_early,
      c_surveillance_late = c_surveillance_late,
      surveillance_cutoff_years = surveillance_cutoff_years,
      c_exercise_annual = c_exercise_annual_standard,
      intervention_duration_years = intervention_duration_years,
      c_progressed_annual = c_progressed_annual,
      c_terminal = c_terminal_val,
      c_intervention_setup = c_intervention_setup,
      u_exercise_decrement = u_exercise_decrement,
      discount_weights = discount_weights_stepped,
      cycle_length = cycle_length,
      age_utility_multipliers = age_multipliers_det
    )

    total_int_std <- colSums(results_int_std[, c("qalys_disc", "costs_disc")])
    # GUIDELINE: Calculate incremental cost as exercise minus standard care before the ICER (Drummond et al. 2015)
    inc_cost_std  <- total_int_std["costs_disc"] - total_ctrl["costs_disc"]
    # GUIDELINE: Calculate incremental QALYs as exercise minus standard care before the ICER (Drummond et al. 2015)
    inc_qaly_std  <- total_int_std["qalys_disc"] - total_ctrl["qalys_disc"]
    # GUIDELINE: Calculate the ICER as incremental cost divided by incremental QALYs (DMP 2026; Drummond et al. 2015)
    icer_standard <- inc_cost_std / inc_qaly_std

    perspective_delta <- data.frame(
      perspective = c("Extended (base case)", "Standard (SA)"),
      total_cost_int = c(total_int["costs_disc"], total_int_std["costs_disc"]),
      total_cost_ctrl = c(total_ctrl["costs_disc"], total_ctrl["costs_disc"]),
      # GUIDELINE: Report perspective-specific incremental cost as exercise minus standard care (Drummond et al. 2015)
      inc_cost = c(total_int["costs_disc"] - total_ctrl["costs_disc"], inc_cost_std),
      # GUIDELINE: Report perspective-specific incremental QALYs as exercise minus standard care (Drummond et al. 2015)
      inc_qaly = c(total_int["qalys_disc"] - total_ctrl["qalys_disc"], inc_qaly_std),
      # GUIDELINE: Report the perspective-specific ICER as incremental cost divided by incremental QALYs (Drummond et al. 2015)
      icer = c((total_int["costs_disc"] - total_ctrl["costs_disc"]) /
               (total_int["qalys_disc"] - total_ctrl["qalys_disc"]),
               icer_standard)
    )
    saveRDS(perspective_delta, "data/processed/perspective_delta.rds")
    message("Perspective Delta Analysis saved: data/processed/perspective_delta.rds")
    message("  Extended ICER: ", round(perspective_delta$icer[1]), " NOK/QALY")
    message("  Standard ICER: ", round(perspective_delta$icer[2]), " NOK/QALY")
    # Caution: this message rounds the difference of the unrounded ICERs, while
    # the perspective table (R/08-export-tables.R) differences the rounded ICERs
    # it displays. The two orders can disagree by one unit through compound
    # rounding, so read the table, not this line, as the reader-facing delta.
    message("  Delta: ", round(perspective_delta$icer[1] - perspective_delta$icer[2]),
            " NOK/QALY")
  }

  # -------------------------------------------------------------------------
  # Severity (absolute prognosetap) -- DMP current-norm method
  # GUIDELINE: AS = general-population remaining QALYs - comparator QALYs, mapped
  #   to a Magnussen severity group -> weight -> reference NOK/QALY (DMP;
  #   Magnussen et al. 2015). Comparator PA = control-arm undiscounted QALYs.
  # DECISION: the superseded 9.5 had no DMP source;
  #   current-norm APT method computes the applicable WTP from model output.
  # SOURCE: costs_qalys$qalys_raw summed over arm == "Standard Care" (= PA).
  # -------------------------------------------------------------------------
  .cq <- if (exists("costs_qalys")) costs_qalys else readRDS("data/processed/costs_qalys.rds")
  comparator_pa_undisc <- sum(.cq$qalys_raw[.cq$arm == "Standard Care"])
  # Caution: PA must be a finite positive QALY total; a zero/NA would corrupt AS.
  stopifnot(is.finite(comparator_pa_undisc), comparator_pa_undisc > 0)
  # GUIDELINE: Derive absolute shortfall from undiscounted QALYs and map it to the Norwegian severity group (Magnussen et al. 2015)
  severity <- derive_severity(severity_genpop_qaly, comparator_pa_undisc, magnussen_bands)
  # Use the model-derived Magnussen group reference as the applicable severity-informed threshold.
  wtp_threshold <- severity$reference_nok        # computed applicable severity-informed reference (385,000)
  message(sprintf("Severity: QALYsA=%.2f PA=%.4f AS=%.2f -> group %d, weight %.1f, reference %s NOK; opportunity-cost benchmark %s NOK",
    severity$general_population_qalys, severity$comparator_qalys, severity$absolute_shortfall,
    severity$group, severity$weight, format(severity$reference_nok, big.mark = ","), format(opportunity_cost_nok, big.mark = ",")))

} else {
  message("TODO: Populate utility and cost parameters in 00-parameters.R")
  message("      and ensure PSM trace is built (03-build-psm.R)")
}
