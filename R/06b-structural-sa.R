# =============================================================================
# 06b-structural-sa.R
# Structural Sensitivity Analysis
#
# Tests the impact of changing model structure assumptions on results.
# Structural SA is required alongside parameter uncertainty.
# Specific structural assumptions A1-A5 are tested per model specification.
#
# Structural scenarios tested:
#   1. Treatment effect waning (assumption A2):
#      a) Constant HR (base case)
#      b) Step waning: HR → 1 at year 5
#      c) Linear waning: HR linearly approaches 1 over years 3-10
#   2. General population mortality cap (assumption A4):
#      a) With mortality cap (base case)
#      b) Without mortality cap
#   3. Alternative survival distributions (assumption A1):
#      a) Best-fit distribution (base case)
#      b) Each candidate distribution as alternative
#   4. Alternative utility sources (assumption A5):
#      a) Primary utility set (base case)
#      b) Alternative utility values from different sources
#   5. Discounting schedule:
#      a) Stepped 4%/3%/2% (base case)
#      b) Flat 4% (sensitivity)
#
# Methodology: Treatment waning is implemented via time-dependent hazard
# modification, NOT via S(t)^HR(t) shortcut. The waning factor is applied
# to the log hazard ratio, preserving the proportional hazards structure.
#
# Input:  Fitted survival models, PSM infrastructure, cost/QALY functions
# Output: output/tables/structural_sa_results.csv
#
# Packages: flexsurv, dplyr
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   Structural sensitivity analysis as a complement to parameter uncertainty:
#     Bilcke J, Beutels P, Brisson M, Jit M (2011). "Accounting for methodological,
#     structural, and parameter uncertainty in decision-analytic models: a
#     practical guide." Medical Decision Making 31(4):675-692.
#     DOI 10.1177/0272989X11409240. PMID 21653805. Establishes the standard
#     workflow: enumerate alternative model structures and assumptions,
#     run each as a complete scenario, report alongside the base case.
#   Treatment effect waning framework: Taylor M, Wailoo A, Dickson R (2024)
#     and the Taylor 6-step waning framework for cancer-treatment HRs;
#     reflected in NICE TSD 14 (Latimer 2013) Section 5.3 (treatment effect
#     duration assumptions).
#   General population mortality cap (NO_MORTALITY_CAP scenario):
#     NICE DSU TSD 21 (Rutherford et al. 2020), Section 4. Removing the
#     cap is expected to fail clinically plausible state probability sums
#     in late-horizon cycles when the parametric extrapolation is heavy
#     tailed (lnorm AFT case). The "FAILED" status of no_mortality_cap is
#     therefore a positive demonstration of why the cap is necessary,
#     not an incidental fix.
#   Subgroup scenarios (Dimensions A-E): standard CHEERS 2022 subgroup
#     reporting practice (Husereau et al. 2022). Implemented at the
#     parameter override level (cohort age, utility source, adherence)
#     so the same PSM infrastructure runs each subgroup.
#   Drug discount sensitivity (40 percent confidential): Fagereng (2024)
#     PMID 38420198 (Norwegian DMP setting); Ehlers et al. (2022) PMID
#     36273196 (Danish 44.1 percent comparator).
#
# Adaptations:
#   1. Treatment waning is applied to the LOG hazard ratio (multiplicative
#      waning factor on the log scale), then exponentiated. This preserves
#      the proportional-hazards structure across the time-varying HR rather
#      than naively interpolating the survival curves themselves, which
#      would distort the underlying hazard.
#   2. Single-scenario dispatcher (run_structural_sa) iterates a list of
#      named scenarios and writes results to a single tidy CSV. Failed
#      scenarios are reported with their error message (no silent skip).
#   3. Subgroup scenarios use parameter overrides instead of forking the
#      entire PSM build chain; this guarantees the same construction logic
#      is applied across base case and subgroup runs.
#
# Academic citations:
#   Bilcke J et al. (2011). DOI 10.1177/0272989X11409240. PMID 21653805.
#   Latimer NR (2013). NICE DSU Technical Support Document 14, Section 5.3.
#   Rutherford MJ et al. (2020). NICE DSU Technical Support Document 21.
#   Husereau D et al. (2022). "Consolidated Health Economic Evaluation
#     Reporting Standards 2022 (CHEERS 2022) Statement." Value in Health
#     25(1):3-9. DOI 10.1016/j.jval.2021.11.1351.
# =============================================================================


#' Apply Treatment Effect Waning to Hazard Ratio
#'
#' Returns a time-dependent HR based on the waning scenario.
#' Waning is applied to the LOG hazard ratio and exponentiated,
#' preserving the proportional hazards structure.
#'
#' Scenarios per structural assumption A2:
#'   "constant"  --  HR constant over full horizon (base case)
#'   "step_5yr"  --  HR jumps to 1.0 at year 5 (no treatment effect after 5yr)
#'   "linear_3_10"  --  HR linearly approaches 1.0 from year 3 to year 10
#'
#' Implementation: waning factor w(t) in [0, 1] where:
#'   HR_effective(t) = exp(w(t) * log(HR_original))
#'   w(t) = 1 means full treatment effect
#'   w(t) = 0 means no treatment effect (HR = 1)
#'
#' @details
#' Caution: hr_original is the MARGINAL HR from CHALLENGE (HR_DFS = 0.72,
#' HR_OS = 0.63 per Courneya et al. 2025). For scenarios "step_5yr" and
#' "linear_3_10" the waning factor w(t) drives HR_effective toward 1.0,
#' i.e. the marginal HR is constrained to 1. Jennings et al. (2024,
#' DOI 10.1016/j.jval.2023.12.008, p. 348) warn that "treatment effect
#' waning happens at an individual level, analyses exploring it should
#' be based on conditional, rather than marginal, HRs"; constraining the
#' marginal HR to 1 will overstate (p. 354) long-term benefit relative to
#' loss-of-individual-effect (RMST inflation up to 0.8 years / 57% in
#' the strong-treatment / strong-prognostic case, Table 1, p. 353).
#' Marginal-HR waning retained because CHALLENGE IPD is
#' not available; conditional-HR waning per Jennings 2024 is infeasible
#' without IPD. Disclosed in extended-introduction.tex, section
#' \label{structural-uncertainty-and-treatment-effect-waning}, and
#' discussion.tex Limitations (DC-3 patch).
#' SOURCE: Jennings J, Davies C, Latimer NR, et al. (2024). Should
#' marginal hazard ratios be used for cost-effectiveness model inputs?
#' Implications of treatment effect waning. Value in Health 27(3):
#' 347-355. DOI 10.1016/j.jval.2023.12.008.
#'
#' @param hr_original Numeric. Original MARGINAL HR from CHALLENGE trial
#'   (Caution: marginal not conditional; see @details).
#' @param time Numeric vector. Times in years.
#' @param scenario Character. Waning scenario name.
#' @return Numeric vector of time-dependent HRs.
# Keep constant treatment effect as base case and test step and linear waning.
apply_treatment_waning <- function(hr_original, time, scenario = "constant") {
  # Implement waning on the hazard-ratio scale rather than by a survivor shortcut.
  log_hr <- log(hr_original)
  n <- length(time)
  waning_factor <- rep(1, n)   # Default: full effect


  if (scenario == "constant") {
    # Base case: no waning
    waning_factor <- rep(1, n)

  } else if (scenario == "step_5yr") {
    # Step waning: full effect until cutoff, then zero
    # Test a step-waning scenario that removes effect at year 5.
    waning_factor <- ifelse(time < waning_step_year, 1, 0)

  } else if (scenario == "linear_3_10") {
    # Linear waning: full effect until start, linear decay to end
    # Test linear treatment-effect waning from years 3 to 10.
    waning_factor <- ifelse(time <= waning_linear_start, 1,
                     ifelse(time >= waning_linear_end, 0,
                            1 - (time - waning_linear_start) /
                              (waning_linear_end - waning_linear_start)))

  } else {
    stop("Unknown waning scenario: '", scenario, "'. ",
         "Valid options: 'constant', 'step_5yr', 'linear_3_10'.")
  }

  # Apply waning on the log-hazard-ratio scale and return to the HR scale.
  exp(waning_factor * log_hr)
}


#' Run Structural Sensitivity Analysis
#'
#' Executes the model under each structural scenario and collects
#' key outputs (ICER, incremental QALYs, incremental costs).
#'
#' Each scenario modifies one structural assumption while keeping
#' all other assumptions at base case values. This is analogous to
#' one-way SA but for structural (non-parameterizable) assumptions.
#'
#' @param base_case_results Data frame. Base case results for reference.
#' @return Data frame with columns: scenario, description, inc_costs,
#'   inc_qalys, icer, diff_from_base_case.
run_structural_sa <- function(base_case_results = NULL) {
  # Time horizon scenarios (10yr, 20yr) included per ISPOR and NICE guidance.
  # ISPOR and NICE guidelines recommend testing the impact of time horizon
  # as a structural SA, particularly for a 40-year horizon where most QALY
  # gains come from extrapolation beyond the trial follow-up.
  # 9 subgroup scenarios (Dimensions A-E) for standard subgroup analysis
  scenarios <- data.frame(
    # Constant treatment effect is the reference scenario.
    scenario    = c("base_case",
                    # Include year-5 step waning as a structural sensitivity.
                    "waning_step_5yr",
                    # Include years-3-to-10 linear waning as a structural sensitivity.
                    "waning_linear_3_10",
                    # Test removal of the general-population mortality cap as structural uncertainty.
                    "no_mortality_cap",
                    # Keep stepped Norwegian discounting as base case and test flat 4%.
                    "flat_discount_4pct",
                    # Test time-horizon structure through deterministic scenarios.
                    "horizon_10yr",
                    "horizon_20yr",
                    # Test alternative mortality-cap implementations as structural uncertainty.
                    "mortality_pmin",
                    # Include an imposed year-10 statistical-cure-point scenario only.
                    "cure_point_10yr",
                    # Test a 40% discount to public drug prices.
                    "drug_discount_40pct",
                    # the bare tariff is now a tested alternative
                    "session_cost_bare_tariff",
                    # the amortised-subsidy anchor is now tested
                    "session_cost_driftstilskudd",
                    # SOURCE: Thorsen OUS Kostnadsoverslag Excel Ark1 B4+B5, 504 NOK/hour times 1.5 hours
                    "session_cost_thorsen_salary",
                    # SOURCE: DMP v1.6 rows 19 and 24; SSB employment share at age 61
                    "patient_time_working_age_weighted",
                    # Dimension A: Disease stage
                    # SOURCE: Mulder et al. 2022, Table 3, stage-II utility scenario
                    "stage_ii_only",
                    # SOURCE: Mulder et al. 2022, Table 3, stage-III utility scenario
                    "stage_iii_only",
                    # Dimension B: Cohort age
                    # SOURCE: Garratt et al. 2022, Table 6, age-50 subgroup utility norm
                    "age_50",
                    "age_74",
                    # SOURCE: Kreftregisteret Årsrapport 2024, Figure 2.3, p. 28, median age 73 for men
                    "norwegian_registry_age",
                    # SOURCE: Kreftregisteret Årsrapport 2024, Figure 2.3, p. 28, median age 76 for women
                    "norwegian_registry_age_women_76",
                    # Dimension C: Sex
                    # Use sex-specific mortality only in subgroup analyses.
                    "male_only",
                    "female_only",
                    # Dimension D: Adherence
                    # SOURCE: Courneya 2008 protocol Section 7.1.6, full supervised-session schedule
                    "full_adherence",
                    # SOURCE: Courneya 2025, Table 2, phase-3 exercise adherence 38%
                    "low_adherence",
                    # Dimension E: Alternative utility source
                    # SOURCE: Mulder et al. 2022, p. 1060, UK Devlin value-set sensitivity analysis
                    "utility_devlin",
                    # Dimension F: Alternative DFS distribution
                    # Keep log-normal DFS as base case and test generalized gamma.
                    "dfs_gengamma"),
    description = c("Constant HR, mortality cap, stepped discount (base)",
                    "HR waning: step to 1.0 at year 5 (A2)",
                    "HR waning: linear decay years 3-10 (A2)",
                    "Without general population mortality cap (A4)",
                    "Flat 4% discount rate (sensitivity test)",
                    "10-year time horizon (extrapolation test)",
                    "20-year time horizon (extrapolation test)",
                    "Mortality cap via pmin on S (vs the maximum-hazard floor base case)",
                    "Imposed year-10 statistical cure point (structural boundary test)",
                    "40% drug discount on PD costs",
                    paste0("Bare Helfo A3 tariff: ", format(sa_session_a3_base, big.mark = ","),
                           " NOK, without the DMP subsidy adjustment; travel and patient time unchanged"),
                    paste0("Amortised municipal driftstilskudd anchor: ",
                           format(sa_session_driftstilskudd, big.mark = ","),
                           " NOK; travel and patient time unchanged"),
                    "Salary-based session cost: OUS labour anchor, 756 NOK (L. Thorsen, personal communication, April 11, 2026); travel and patient time unchanged",
                    paste0("Working-age patient time: ",
                           round(patient_time_working_share * 100, 1),
                           "% gross-wage rate; remainder leisure rate"),
                    # Dimension A
                    "Stage II only: disease-free utility 0.82 (Mulder 2022 Table 3); cost inputs remain at their base values",
                    "Stage III only: disease-free utility 0.84 (Mulder 2022 Table 3); cost inputs remain at their base values",
                    # Dimension B
                    "Younger cohort age 50: SSB qx from 50, Garratt norms from 50-59",
                    "Older cohort age 74: SSB qx and age norms from 74",
                    "Norwegian registry age 73: colon median men (Årsrapport 2024 Fig 2.3 p.28), SSB qx from 73 [severity sensitivity]",
                    "Norwegian registry age 76: colon median women (Årsrapport 2024 Fig 2.3 p.28), SSB qx from 76 [severity sensitivity]",
                    # Dimension C
                    "Male only: SSB male qx, Garratt 2022 male norms",
                    "Female only: SSB female qx, Garratt 2022 female norms",
                    # Dimension D
                    # SOURCE: sessions_per_year = 20 (00-parameters.R) over
                    #   intervention_duration_years = 3 = 60 supervised sessions;
                    #   0.38 x 20 x 3 = 22.8, displayed as 23.
                    # Caution: these labels are the only prose statement of the
                    #   session counts this branch costs. If sessions_per_year
                    #   moves, they lie. Keep them and Methods Table 3.5 in step.
                    "Full adherence: 60 sessions (20/yr x 3yr), max cost",
                    "Low adherence: 38% rate (Phase 3 min), 23 sessions, reduced cost",
                    # Dimension E
                    "England EQ-5D-5L value set: disease-free utility 0.85 (Mulder 2022 sensitivity analysis)",
                    "DFS generalised gamma (best BIC 992.7 vs lognormal 994.8)"),
    stringsAsFactors = FALSE
  )

  scenarios$inc_costs  <- NA_real_
  scenarios$inc_qalys  <- NA_real_
  scenarios$icer       <- NA_real_
  # Severity (absolute-shortfall) columns -- additive; base-case macros unaffected.
  scenarios$sev_as        <- NA_real_
  scenarios$sev_group     <- NA_integer_
  scenarios$sev_weight    <- NA_real_
  scenarios$sev_reference <- NA_real_

  # Gated implementation: runs scenarios when prerequisites are available.
  # Execution requires: (1) fitted survival models, (2) populated cost/utility
  # parameters. When prerequisites are missing, returns NA results with message.
  # When prerequisites are met, executes each scenario.
  #
  # Waning scenarios pass mortality_method = "hazard_max"
  # to build_psm_trace_waning() to avoid confounding the waning analysis
  # with a different mortality adjustment method than the base case.
  #
  # Implementation pattern per scenario:
  #   1. Modify the relevant structural assumption
  #   2. Rebuild PSM trace (with/without mortality cap, with/without waning)
  #   3. Calculate costs and QALYs (with appropriate discount weights)
  #   4. Compute ICER
  #   5. Store results
  #
  # Structural SA required
  # GUIDELINE: NICE TSD 19  --  structural uncertainty should be explored

  can_execute <- file.exists("data/processed/survival_fits_dfs_ctrl.rds") &&
    file.exists("data/processed/survival_fits_os_ctrl.rds") &&
    !is.null(best_dist_dfs) && !is.null(best_dist_os) &&
    !is.na(u_dfs_mean) && !is.na(c_surveillance_early)

  if (!can_execute) {
    message("Structural SA framework loaded. ", nrow(scenarios),
            " scenarios defined.")
    message("Cannot execute: prerequisites missing (fit objects or parameters).")
    message("Populate parameters and fit survival models, then re-run.")
  } else {
    message("Structural SA: executing ", nrow(scenarios), " scenarios...")
    ssa_fits_dfs <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
    ssa_fits_os  <- readRDS("data/processed/survival_fits_os_ctrl.rds")
    ssa_fit_dfs_ctrl <- ssa_fits_dfs[[best_dist_dfs]]
    ssa_fit_os_ctrl  <- ssa_fits_os[[best_dist_os]]
    ssa_s_genpop <- load_genpop_survival(life_table_path, cohort_age,
                                          n_cycles, cycle_length)
    ssa_age_mult <- get_age_utility_multipliers(cohort_age, n_cycles,
                                                 cycle_length)

    for (s_idx in seq_len(nrow(scenarios))) {
      scn <- scenarios$scenario[s_idx]
      tryCatch({
        # Constant treatment effect is the base case.
        waning_scn <- "constant"
        # GUIDELINE: Floor modeled mortality at matched general-population mortality (Rutherford et al. 2020)
        mort_method <- "hazard_max"
        # NULL for every existing scenario and the base row.
        cure_t_star_scn <- NULL
        dw <- discount_weights_stepped
        n_cyc_scn <- n_cycles
        cl_scn <- cycle_length
        # Use age-specific both-sex background mortality in the base case.
        s_genpop_scn <- ssa_s_genpop
        # Shared progressed cost, and a programme cost owned by the
        #   exercise arm alone. That ownership claim is about which ARM pays, not
        #   about which contacts are inside it: the programme cost carries both
        #   supervised exercise visits and behavioural-support contacts.
        c_progressed_scn <- c_progressed_annual
        c_exercise_scn <- c_exercise_annual
        # Scenario-specific subgroup variables (default: base case)
        u_dfs_scn       <- u_dfs_mean
        u_prog_scn      <- u_prog_mean
        cohort_age_scn  <- cohort_age
        norms_scn       <- age_utility_norms

        # Test year-5 step waning outside the constant-effect base case.
        if (scn == "waning_step_5yr") waning_scn <- "step_5yr"
        # Test linear waning from years 3 to 10 outside the constant-effect base case.
        if (scn == "waning_linear_3_10") waning_scn <- "linear_3_10"
        # Test removal of the mortality cap as structural uncertainty.
        if (scn == "no_mortality_cap") s_genpop_scn <- NULL
        # Test flat 4% discounting against the stepped Norwegian base case.
        if (scn == "flat_discount_4pct") dw <- discount_weights_flat
        # Test time horizon as structural uncertainty.
        if (scn == "horizon_10yr") {
          # Recompute cycle count when the structural time horizon changes.
          n_cyc_scn <- round(10 / cycle_length)
          dw <- create_discount_weights(n_cyc_scn, cl_scn)
          ssa_s_genpop_short <- load_genpop_survival(life_table_path,
            cohort_age, n_cyc_scn, cl_scn)
          s_genpop_scn <- ssa_s_genpop_short
        }
        # Test time horizon as structural uncertainty.
        if (scn == "horizon_20yr") {
          # Recompute cycle count when the structural time horizon changes.
          n_cyc_scn <- round(20 / cycle_length)
          dw <- create_discount_weights(n_cyc_scn, cl_scn)
          ssa_s_genpop_short <- load_genpop_survival(life_table_path,
            cohort_age, n_cyc_scn, cl_scn)
          s_genpop_scn <- ssa_s_genpop_short
        }
        # Test an alternative mortality-cap implementation as structural uncertainty.
        if (scn == "mortality_pmin") mort_method <- "pmin"
        # Apply the imposed year-10 statistical cure point only in its scenario.
        if (scn == "cure_point_10yr") cure_t_star_scn <- cure_t_star_years
        # 40% drug discount on PD costs.
        # Drugs are ~50% of PD costs; 40% discount on drugs = 20% total
        # reduction. The one shared progressed input feeds both arms.
        # SOURCE: Fagereng 2024 (PMID 38420198), Ehlers 2022 (PMID 36273196).
        if (scn == "drug_discount_40pct") {
          c_progressed_scn <- c_progressed_annual * drug_discount_factor
        }
        # the base case carries the DMP x2 adjustment, so the
        #   bare tariff is now the LOWER tested alternative rather than the base.
        if (scn == "session_cost_bare_tariff") {
          # GUIDELINE: retain the extended healthcare perspective components.
          # Caution: substitute the healthcare session fee only; travel and patient
          #   time are separate components and are unchanged.
          c_exercise_scn <- c_exercise_annual +
            n_sessions_adherence * (sa_session_a3_base - c_session_healthcare) /
              intervention_duration_years
        }
        # the amortised-subsidy anchor is tested rather than only
        #   documented, because it is the middle point of the three-point range.
        if (scn == "session_cost_driftstilskudd") {
          # Caution: this anchor is an ASSUMPTION; it is tested, never a base.
          c_exercise_scn <- c_exercise_annual +
            n_sessions_adherence * (sa_session_driftstilskudd - c_session_healthcare) /
              intervention_duration_years
        }
        # SOURCE: Thorsen OUS Kostnadsoverslag Excel Ark1 B4+B5, 504 NOK/hour times 1.5 hours
        if (scn == "session_cost_thorsen_salary") {
          # SOURCE: L. Thorsen (personal communication, April 11, 2026), OUS
          #   labour cost 504 NOK/hr x 1.5 hr = 756 NOK (sa_session_thorsen_ous).
          # DECISION: the displayed salary anchor is EXECUTED rather than only
          #   documented: the OUS estimate is run as a structural scenario here.
          # Caution: substitute only the healthcare session fee; travel and patient
          #   time are unchanged. The subtracted term is
          #   c_session_healthcare, the DMP-adjusted base the programme cost is
          #   built from, exactly as the two branches above subtract it. It is
          #   NOT sa_session_a3_base: that alias is the bare Helfo A3 tariff
          #   and subtracting it here would price the scenario at 1,389 NOK.
          c_exercise_scn <- c_exercise_annual +
            n_sessions_adherence * (sa_session_thorsen_ous - c_session_healthcare) /
              intervention_duration_years
        }
        # SOURCE: DMP v1.6 rows 19 and 24; SSB employment share at age 61
        if (scn == "patient_time_working_age_weighted") {
          # SOURCE: DMP v1.6 rows 19 and 24; SSB employment share at age 61.
          # DECISION: working-age weighting is a deterministic scenario only.
          # GUIDELINE: retain the DMP common leisure-rate base case.
          # Caution: modify patient time only; do not alter travel reimbursement.
          weighted_rate_scn <- weighted_patient_time_rate(
            patient_time_working_share,
            c_patient_time_rate_gross,
            c_patient_time_rate)
          # Caution: this branch REBUILDS the programme cost, so the behavioural
          #   component must be re-added or the scenario silently drops it. It is
          #   added flat: a behavioural contact carries no travel, no patient time
          #   and no exercise hour, so it is invariant to the patient-time
          #   rate this scenario varies.
          c_exercise_scn <- n_sessions_adherence *
            (c_session_healthcare + c_travel_per_session +
               weighted_rate_scn * (exercise_time_hr + travel_time_min / 60)) /
            intervention_duration_years + c_behaviour_annual
        }

        # Subgroup scenario overrides (Dimensions A-E)
        # Dimension A: Disease stage (utility only; same survival curves)
        # SOURCE: Mulder et al. 2022, Table 3, stage-II disease-free utility
        if (scn == "stage_ii_only") {
          u_dfs_scn <- u_dfs_stage_ii   # Mulder 2022 Table 3, SD=0.21, n=52
        }
        if (scn == "stage_iii_only") {
          u_dfs_scn <- u_dfs_stage_iii  # Mulder 2022 Table 3, SD=0.17, n=49
        }
        # Dimension B: Cohort age (mortality + utility norms change)
        if (scn == "age_50") {
          # SOURCE: Garratt et al. 2022, Table 6, age-50 subgroup utility norm
          cohort_age_scn <- 50
          s_genpop_scn <- load_genpop_survival(life_table_path,
            cohort_age_scn, n_cyc_scn, cl_scn)
          norms_scn <- age_utility_norms  # weighted norms, age starts at 50
        }
        if (scn == "age_74") {
          cohort_age_scn <- 74
          s_genpop_scn <- load_genpop_survival(life_table_path,
            cohort_age_scn, n_cyc_scn, cl_scn)
          norms_scn <- age_utility_norms  # weighted norms, age starts at 74
        }
        # SOURCE: Kreftregisteret Årsrapport 2024, Figur 2.3 p.28 -- colon median age
        #   men 73, women 76 ("Median alder menn 73 ar"). PROXY (median / all-stage /
        #   at diagnosis); exact DMP mean-treatment-initiation age unavailable.
        #   Both-sex SSB qx (pure AGE sensitivity, matches age_74; sex tested separately).
        if (scn == "norwegian_registry_age") {
          cohort_age_scn <- 73
          s_genpop_scn <- load_genpop_survival(life_table_path,
            cohort_age_scn, n_cyc_scn, cl_scn)
          norms_scn <- age_utility_norms  # weighted norms, age starts at 73
        }
        # SOURCE: Kreftregisteret Årsrapport 2024, Figure 2.3, p. 28, median age 76 for women
        if (scn == "norwegian_registry_age_women_76") {
          cohort_age_scn <- 76
          s_genpop_scn <- load_genpop_survival(life_table_path,
            cohort_age_scn, n_cyc_scn, cl_scn)
          norms_scn <- age_utility_norms  # weighted norms, age starts at 76
        }
        # Dimension C: Sex (mortality + utility norms change)
        # Use male mortality only in the male subgroup analysis.
        if (scn == "male_only") {
          s_genpop_scn <- load_genpop_survival(life_table_sex_path,
            cohort_age_scn, n_cyc_scn, cl_scn, sex = "male")
          norms_scn <- age_utility_norms_male
        }
        # Use female mortality only in the female subgroup analysis.
        if (scn == "female_only") {
          s_genpop_scn <- load_genpop_survival(life_table_sex_path,
            cohort_age_scn, n_cyc_scn, cl_scn, sex = "female")
          norms_scn <- age_utility_norms_female
        }
        # Dimension D: Adherence (cost only; HRs are ITT)
        # SOURCE: Courneya 2008 protocol Section 7.1.6, full supervised-session schedule
        if (scn == "full_adherence") {
          # Maximum programme cost: all sessions at full attendance (extended perspective)
          # Caution: this branch REBUILDS the programme cost, so the behavioural
          #   component must be re-added or the scenario silently drops it.
          #   sessions_per_year is an EXERCISE-session constant; the lever here is
          #   exercise attendance, so behavioural contacts stay at their own
          #   printed Table 2 rates.
          c_exercise_scn <- sessions_per_year * c_session_extended +
            c_behaviour_annual
        }
        # SOURCE: Courneya 2025, Table 2, phase-3 exercise adherence 38%
        if (scn == "low_adherence") {
          # Minimum adherence: Phase 3 rate applied across all 3 years
          # SOURCE: Courneya 2025 Table 2, Phase 3 adherence 38%
          # Caution: same rebuild as full_adherence. adherence_phase3_min is the
          #   printed phase-3 EXERCISE rate, so the behavioural component is held
          #   at its own printed rates rather than scaled by an exercise rate.
          # SOURCE: Courneya 2025, Table 2, phase-3 exercise adherence 38%
          c_exercise_scn <- adherence_phase3_min * sessions_per_year *
            c_session_extended + c_behaviour_annual
        }
        # Dimension E: Alternative utility source
        # SOURCE: Mulder et al. 2022, p. 1060, UK Devlin value-set sensitivity analysis
        if (scn == "utility_devlin") {
          u_dfs_scn <- u_dfs_devlin  # Mulder 2022 SA (p. 1060), England value set
        }

        # Dimension F: Alternative DFS distribution
        # GUIDELINE: NICE DSU TSD 14 (Latimer, 2013): distribution choice
        #   should be justified by statistical fit AND clinical plausibility.
        #   Generalised gamma had the best BIC for DFS (992.7 vs lognormal
        #   994.8). This scenario tests whether the cost-effectiveness
        #   conclusion and the DFS/OS curve crossing are sensitive to this
        #   distribution choice.
        fit_dfs_scn <- ssa_fit_dfs_ctrl  # default: base case distribution
        # Test generalized gamma against the log-normal DFS base case.
        if (scn == "dfs_gengamma") {
          if ("gengamma" %in% names(ssa_fits_dfs)) {
            fit_dfs_scn <- ssa_fits_dfs[["gengamma"]]
            message("  Using generalised gamma for DFS (BIC=992.7)")
          } else {
            stop("Generalised gamma DFS fit not available in survival_fits_dfs_ctrl.rds")
          }
        }

        trace_ctrl_scn <- build_psm_trace(
          fit_dfs = fit_dfs_scn, fit_os = ssa_fit_os_ctrl,
          n_cycles = n_cyc_scn, cycle_length = cl_scn,
          s_genpop = s_genpop_scn, arm_label = "Standard Care",
          mortality_method = mort_method,
          cure_t_star_years = cure_t_star_scn
        )

        # Keep waning outside the constant-effect base case.
        if (waning_scn != "constant") {
          trace_int_scn <- build_psm_trace_waning(
            fit_dfs_ctrl = fit_dfs_scn,
            fit_os_ctrl = ssa_fit_os_ctrl,
            hr_dfs_original = HR_DFS, hr_os_original = HR_OS,
            waning_scenario = waning_scn,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn,
            mortality_method = mort_method,
            # no combined waning-plus-cure scenario exists.
            cure_t_star_years = NULL
          )
        } else {
          trace_int_scn <- build_psm_trace_from_ctrl(
            fit_dfs_ctrl = fit_dfs_scn,
            fit_os_ctrl = ssa_fit_os_ctrl,
            hr_dfs = HR_DFS, hr_os = HR_OS,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn, arm_label = "Exercise",
            mortality_method = mort_method,
            cure_t_star_years = cure_t_star_scn
          )
        }

        # The structural-SA dispatcher owns the validation
        #   artifact write (Phase A Section 12.1), so main.R stays outside the
        #   Item 03 write set and the existing single-dispatcher idiom holds.
        # Caution: every *_pass value below is computed from the traces this run
        #   produced. A literal TRUE would make the manifest evidence of
        #   nothing.
        # Emit cure diagnostics only for the imposed year-10 scenario.
        if (scn == "cure_point_10yr") {
          cure_arm_diagnostics <- function(trace_df, s_genpop_vec, t_star) {
            h_genpop_t <- -diff(log(pmax(s_genpop_vec, 1e-15)))
            post_t <- trace_df$time[-nrow(trace_df)] >= (t_star - 1e-8)
            per_endpoint <- function(s_vec) {
              h_adj <- -diff(log(pmax(s_vec, 1e-15)))
              excess <- h_adj[post_t] - h_genpop_t[post_t]
              list(
                post_tstar_intervals = sum(post_t),
                max_abs_post_tstar_excess_hazard =
                  if (length(excess) > 0) max(abs(excess)) else 0,
                monotone_survival_pass = all(diff(s_vec) <= 1e-8),
                background_interaction_pass = all(h_adj >= h_genpop_t - 1e-8),
                post_tstar_excess_hazard = excess
              )
            }
            list(
              DFS = per_endpoint(trace_df$p_dfs),
              OS  = per_endpoint(trace_df$p_dfs + trace_df$p_prog),
              max_state_sum_error = max(abs(trace_df$p_dfs + trace_df$p_prog +
                                              trace_df$p_dead - 1)),
              min_state_probability = min(c(trace_df$p_dfs, trace_df$p_prog,
                                            trace_df$p_dead))
            )
          }

          # Caution: base_case_identity_pass comes from an executed comparison.
          #   Omitting the argument must equal passing NULL (the default is
          #   inert), and the imposed scenario must actually differ from it.
          cure_ctrl_default <- build_psm_trace(
            fit_dfs = fit_dfs_scn, fit_os = ssa_fit_os_ctrl,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn, arm_label = "Standard Care",
            mortality_method = mort_method
          )
          cure_ctrl_null <- build_psm_trace(
            fit_dfs = fit_dfs_scn, fit_os = ssa_fit_os_ctrl,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn, arm_label = "Standard Care",
            mortality_method = mort_method, cure_t_star_years = NULL
          )
          cure_int_default <- build_psm_trace_from_ctrl(
            fit_dfs_ctrl = fit_dfs_scn, fit_os_ctrl = ssa_fit_os_ctrl,
            hr_dfs = HR_DFS, hr_os = HR_OS,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn, arm_label = "Exercise",
            mortality_method = mort_method
          )
          cure_int_null <- build_psm_trace_from_ctrl(
            fit_dfs_ctrl = fit_dfs_scn, fit_os_ctrl = ssa_fit_os_ctrl,
            hr_dfs = HR_DFS, hr_os = HR_OS,
            n_cycles = n_cyc_scn, cycle_length = cl_scn,
            s_genpop = s_genpop_scn, arm_label = "Exercise",
            mortality_method = mort_method, cure_t_star_years = NULL
          )
          cure_identity_pass <-
            identical(cure_ctrl_default, cure_ctrl_null) &&
            identical(cure_int_default, cure_int_null) &&
            !identical(cure_ctrl_default, trace_ctrl_scn) &&
            !identical(cure_int_default, trace_int_scn)

          cure_diag <- list(
            `Standard Care` = cure_arm_diagnostics(trace_ctrl_scn,
                                                   s_genpop_scn,
                                                   cure_t_star_scn),
            Exercise = cure_arm_diagnostics(trace_int_scn, s_genpop_scn,
                                            cure_t_star_scn)
          )
          cure_checks <- list(
            scenario = "cure_point_10yr",
            t_star_years = cure_t_star_scn,
            anchor = "Joranger 2020 Figure 1; Joranger 2015 pp. 258, 260",
            base_case_identity_pass = cure_identity_pass,
            arms = cure_diag
          )
          saveRDS(cure_checks, "data/processed/structural_cure_point_checks.rds")

          cure_manifest <- do.call(rbind, lapply(
            c("Exercise", "Standard Care"), function(a) {
              do.call(rbind, lapply(c("DFS", "OS"), function(ep) {
                d <- cure_diag[[a]]
                e <- d[[ep]]
                data.frame(
                  scenario = "cure_point_10yr",
                  arm = a,
                  endpoint = ep,
                  t_star_years = cure_t_star_scn,
                  post_tstar_intervals = e$post_tstar_intervals,
                  max_abs_post_tstar_excess_hazard =
                    e$max_abs_post_tstar_excess_hazard,
                  max_state_sum_error = d$max_state_sum_error,
                  min_state_probability = d$min_state_probability,
                  monotone_survival_pass = e$monotone_survival_pass,
                  background_interaction_pass = e$background_interaction_pass,
                  base_case_identity_pass = cure_identity_pass,
                  anchor =
                    "Joranger 2020 Figure 1; Joranger 2015 pp. 258, 260",
                  stringsAsFactors = FALSE
                )
              }))
            }))
          write.csv(cure_manifest,
                    "output/tables/structural_cure_point_manifest.csv",
                    row.names = FALSE)
          message("Structural cure point: diagnostics written (4 rows).")
        }

        ssa_trace <- rbind(trace_int_scn, trace_ctrl_scn)
        age_mult_scn <- get_age_utility_multipliers(cohort_age_scn, n_cyc_scn,
                                                     cl_scn, norms = norms_scn)

        # Shared components are modified once per scenario and consumed by
        # both strategies; only the programme value differs by strategy.
        cq_int_scn <- calculate_costs_qalys(
          ssa_trace, arm = "Exercise",
          u_dfs = u_dfs_scn, u_prog = u_prog_scn, u_dead = u_dead,
          c_surveillance_early = c_surveillance_early,
          c_surveillance_late = c_surveillance_late,
          surveillance_cutoff_years = surveillance_cutoff_years,
          c_exercise_annual = c_exercise_scn,
          intervention_duration_years = intervention_duration_years,
          c_progressed_annual = c_progressed_scn,
          c_terminal = if (is.na(c_terminal)) 0 else c_terminal,
          c_intervention_setup = if (is.na(c_intervention_setup)) 0
                                  else c_intervention_setup,
          u_exercise_decrement = u_exercise_decrement,
          discount_weights = dw, cycle_length = cl_scn,
          age_utility_multipliers = age_mult_scn
        )
        cq_ctrl_scn <- calculate_costs_qalys(
          ssa_trace, arm = "Standard Care",
          u_dfs = u_dfs_scn, u_prog = u_prog_scn, u_dead = u_dead,
          c_surveillance_early = c_surveillance_early,
          c_surveillance_late = c_surveillance_late,
          surveillance_cutoff_years = surveillance_cutoff_years,
          c_exercise_annual = 0,
          intervention_duration_years = 0,
          c_progressed_annual = c_progressed_scn,
          c_terminal = if (is.na(c_terminal)) 0 else c_terminal,
          c_intervention_setup = 0,
          u_exercise_decrement = 0,
          discount_weights = dw, cycle_length = cl_scn,
          age_utility_multipliers = age_mult_scn
        )

        # GUIDELINE: Derive incremental outcomes as exercise minus standard care (Drummond et al. 2015)
        scenarios$inc_costs[s_idx] <- sum(cq_int_scn$costs_disc) -
                                       sum(cq_ctrl_scn$costs_disc)
        scenarios$inc_qalys[s_idx] <- sum(cq_int_scn$qalys_disc) -
                                       sum(cq_ctrl_scn$qalys_disc)
        if (abs(scenarios$inc_qalys[s_idx]) < 1e-10) {
          scenarios$icer[s_idx] <- NA_real_
        } else {
          # GUIDELINE: Compute the ICER as incremental cost divided by incremental effect (Drummond et al. 2015)
          scenarios$icer[s_idx] <- scenarios$inc_costs[s_idx] /
                                    scenarios$inc_qalys[s_idx]
        }

        # --- Severity (absolute shortfall) at the scenario's re-aged cohort age ---
        # GUIDELINE: DMP absolute-shortfall method -- AS = genpop remaining QALYs
        #   minus patient (control-arm, UNDISCOUNTED) QALYs at the SAME cohort age,
        #   mapped to a Magnussen band (Magnussen et al. 2015; DMP 2026).
        # DECISION: severity age external-validity sensitivity.
        # Caution: reference-age-only (swap genpop age, hold base PA = 15.89) yields a
        #   NEGATIVE, invalid AS; severity MUST be recomputed on the fully re-aged
        #   control arm (this loop already re-ages SSB mortality + EQ-5D utilities).
        # ASSUMPTION: constant treatment effect across age -- CHALLENGE HR_DFS/HR_OS
        #   and the trial-fitted disease survival are held FIXED; only background
        #   mortality + age-utilities re-age (ISPOR; NICE TSD 14 subgroup practice).
        # PA kept UNDISCOUNTED (qalys_raw) to match base convention: 04-costs-qalys.R
        #   builds `qalys_raw` inside calculate_costs_qalys() and only then applies the
        #   discount weights to form `qalys_disc`.
        # Compute absolute shortfall from undiscounted standard-care QALYs.
        pa_undisc_scn   <- sum(cq_ctrl_scn$qalys_raw)
        # Compare patient QALYs with the current DMP remaining-QALY norm at the same age.
        genpop_qaly_scn <- .dmp_norms$remaining_qalys_newnorm[
                             .dmp_norms$age == cohort_age_scn]
        if (length(genpop_qaly_scn) == 1L && is.finite(genpop_qaly_scn) &&
            is.finite(pa_undisc_scn) && pa_undisc_scn > 0) {
          tryCatch({
            # Derive severity programmatically from absolute shortfall and Magnussen bands.
            sev_scn <- derive_severity(genpop_qaly_scn, pa_undisc_scn, magnussen_bands)
            scenarios$sev_as[s_idx]        <- sev_scn$absolute_shortfall
            scenarios$sev_group[s_idx]     <- sev_scn$group
            scenarios$sev_weight[s_idx]    <- sev_scn$weight
            scenarios$sev_reference[s_idx] <- sev_scn$reference_nok
          }, error = function(e)
            warning("Severity not derivable for scenario '", scn, "': ",
                    e$message, call. = FALSE))
        }
      }, error = function(e) {
        warning("Structural SA scenario '", scn, "' failed: ",
                e$message, call. = FALSE)
      })
    }
    message("Structural SA complete. ", sum(!is.na(scenarios$icer)),
            " of ", nrow(scenarios), " scenarios executed.")

    # --- Programmatic caption for structural SA table ---
    # SOURCE: scenarios data frame computed in this function
    # Structural SA required
    # GUIDELINE: NICE TSD 19, ISPOR Good Practices for structural uncertainty
    n_executed <- sum(!is.na(scenarios$icer))
    base_icer <- scenarios$icer[scenarios$scenario == "base_case"]
    icer_range <- range(scenarios$icer[!is.na(scenarios$icer) &
                                        scenarios$scenario != "base_case"])
    structural_sa_caption <- paste0(
      "Structural Sensitivity Analysis (",
      n_executed, " of ", nrow(scenarios), " scenarios executed, ",
      "extended healthcare perspective, effective discount rate 4% per Rundskriv R-109 stepped schedule). ",
      "Base-case ICER: NOK ", format(round(base_icer), big.mark = ","), "/QALY. ",
      "Range across scenarios: NOK ",
      format(round(icer_range[1]), big.mark = ","), " to NOK ",
      format(round(icer_range[2]), big.mark = ","), "/QALY."
    )
    cat("PROGRAMMATIC CAPTION:\n")
    cat(structural_sa_caption, "\n\n")
    attr(scenarios, "caption") <- structural_sa_caption
  }

  scenarios
}


#' List Available Structural Scenarios
#'
#' Utility function for documentation and interactive use.
#' @return Character vector of scenario names.
list_structural_scenarios <- function() {
  c("base_case",
    "waning_step_5yr",      # Assumption A2: step waning at year 5
    "waning_linear_3_10",   # Assumption A2: linear waning years 3-10
    "no_mortality_cap",     # Assumption A4: without genpop mortality cap
    "flat_discount_4pct",   # Flat 4% discount rate sensitivity test
    "horizon_10yr",         # Time horizon: 10 years (extrapolation test)
    "horizon_20yr",         # Time horizon: 20 years (extrapolation test)
    "mortality_pmin",       # M2: pmin mortality cap (vs hazard_max base case)
    "cure_point_10yr",       # Imposed year-10 structural cure point
    "drug_discount_40pct",  # 40% drug discount on PD costs
    "session_cost_bare_tariff", # bare Helfo A3 tariff, without the DMP x2 adjustment
    "session_cost_driftstilskudd", # amortised municipal driftstilskudd anchor
    "session_cost_thorsen_salary", # salary-based session-cost anchor (Thorsen OUS)
    "patient_time_working_age_weighted", # weighted gross/leisure patient time
    # Subgroup scenarios (thesis research question 3)
    "stage_ii_only",        # Dimension A: Stage II utility
    "stage_iii_only",       # Dimension A: Stage III utility
    "age_50",               # Dimension B: younger cohort
    "age_74",               # Dimension B: older cohort (no registry basis)
    "norwegian_registry_age", # Dimension B: Norwegian registry median age, men
    "norwegian_registry_age_women_76", # Dimension B: registry median age, women
    "male_only",            # Dimension C: male mortality + utility norms
    "female_only",          # Dimension C: female mortality + utility norms
    "full_adherence",       # Dimension D: maximum programme cost
    "low_adherence",        # Dimension D: minimum programme cost
    "utility_devlin",       # Dimension E: England EQ-5D-5L value set
    "dfs_gengamma"          # Dimension F: alternative DFS distribution
    # Future additions:
    # "alt_dist_weibull",   # Assumption A1: alternative distribution
    # "alt_dist_llogis",    # Assumption A1: alternative distribution
    # "alt_utility_source", # Assumption A5: alternative utility source
    # Endpoint-dependence sensitivities remain in the separate six-row PSA
    # specification registry and are never added to this deterministic count.
    # Sensitivity on rho should be tested via separate PSA runs, not structural SA.
  )
}


# --- Script loaded message ----------------------------------------------------
message("Structural SA script loaded. ", length(list_structural_scenarios()),
        " scenarios defined.")
message("Execution handled by main.R after sourcing.")
