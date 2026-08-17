# =============================================================================
# 00-parameters.R
# All model parameters for the colon cancer exercise intervention CUA
# 3-state Partitioned Survival Model  --  CHALLENGE Trial
#
# Author: André Álcega Hartmann
# Last updated: 2026-02
#
# Input:  Published literature (see SOURCE: tags inline), SSB life tables (data/ssb/)
# Output: All model parameters loaded into global environment
#
# PARAMETER LOCK STATUS
# Parameters from the CHALLENGE trial are locked (marked in-line).
# Parameters requiring elicitation or sourcing are noted in-line.
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   Stepped discount weight construction (create_discount_weights):
#     adapted from the standard "year-bracketed cumulative discount factor"
#     pattern in Briggs, Claxton, Sculpher (2006), Decision Modelling for
#     Health Economic Evaluation, Oxford University Press, Ch. 6 (annuity
#     and discounting); the bracketed schedule with midpoint evaluation
#     follows Norwegian Ministry of Finance Rundskriv R-109 (2021) and
#     DMP submission guidelines (DMP, 2026) Section 12.8.
#   Beta and gamma method-of-moments parameterisations (beta_params,
#     gamma_params): Briggs, Claxton, Sculpher (2006), Ch. 4, Box 4.5 and
#     Box 4.6. Standard ISPOR-SMDM Task Force 6 PSA practice (Briggs et al.
#     2012, Value in Health 15(6):835-842).
#   Endpoint-specific MVN sampling and PSA dependence sensitivity: Briggs,
#     Claxton, Sculpher (2006), Ch. 4, Section 4.5; MASS::mvrnorm() calls in
#     06-psa.R follow Venables and Ripley (2002), 4th ed.
#   Age-dependent utility multiplier (get_age_utility_multipliers):
#     Ara R, Brazier J (2010). "Populating an economic model with health
#     state utility values." Value in Health 13(5):509-518.
#     DOI 10.1016/j.jval.2010.01.005. PMID: 20230546. The multiplicative
#     adjustment u(t) = u_state * (u_genpop(age + t) / u_genpop(age)) is
#     the standard NICE / NOMA method for lifetime horizons.
#   PSM trace and cost/QALY assertion functions (assert_psm_trace,
#     assert_costs_qalys): adapted from the assertHE R package validation
#     pattern (Smith et al., 2024 Wellcome Open Research), implemented inline to
#     avoid the assertHE dependency until that package stabilises.
#
# Adaptations made:
#   1. Vectorised O(n) discount-weight loop replaces a naive O(n^2) double
#      loop; same numerical result, ~30x faster on the 480-cycle horizon
#      (see comments in create_discount_weights body).
#   2. Bracket boundaries are passed as a configurable data frame so the
#      flat 4 percent sensitivity scenario reuses the same function
#      Default brackets encode the Rundskriv R-109 stepped
#      schedule.
#   3. beta_params and gamma_params include defensive checks (NA handling,
#      domain validation, variance feasibility) not present in standard
#      textbook implementations.
#
# Academic citations:
#   Briggs A, Claxton K, Sculpher MJ (2006). Decision Modelling for Health
#     Economic Evaluation. Oxford University Press. ISBN 9780198526629.
#   Briggs AH, Weinstein MC, Fenwick EAL, Karnon J, Sculpher MJ, Paltiel AD
#     (2012). "Model parameter estimation and uncertainty analysis: a report
#     of the ISPOR-SMDM Modeling Good Research Practices Task Force-6."
#     Value in Health 15(6):835-842. DOI 10.1016/j.jval.2012.04.014.
#     PMID: 22999133.
#   Ara R, Brazier J (2010). DOI 10.1016/j.jval.2010.01.005. PMID: 20230546.
#   Norwegian Ministry of Finance (2021). Rundskriv R-109, "Prinsipper og krav
#     ved utarbeidelse av samfunnsokonomiske analyser". Var ref 21/2720-8,
#     dated 25.06.2021, in force from 1 January 2022. It replaces R-109/2014.
#     SOURCE: reference-materials/originals/Finansdepartementet-2021-Rundskriv-R-109.pdf,
#     printed p.1 (masthead) and printed p.9 (commencement clause). This is the
#     only R-109 edition held locally; no 2014 and no 2020 edition exists on disk.
#   DMP (2026). Submission guidelines for HTA of pharmaceuticals,
#     updated 06.07.2026, Section 12.8.
# =============================================================================


# =============================================================================
# 1. STRUCTURAL PARAMETERS
# =============================================================================

# Model time horizon and cycle structure
# Use a lifetime horizon long enough to capture all relevant survival and cost differences.
time_horizon   <- 40          # Years (lifetime horizon)
# Use monthly cycles because the intervention has no shorter treatment-cycle driver.
cycle_length   <- 1 / 12     # Years per cycle (monthly cycles)
# Derive the cycle count from horizon and cycle length to keep the time grid internally consistent.
n_cycles       <- time_horizon / cycle_length  # = 480 cycles

# Structural statistical-cure point (scenario only; base case is unaffected)
# SOURCE: Joranger et al. 2020 Figure 1 (p. 323) disease-free tunnel completes
#   at year 10; Joranger et al. 2015 pp. 258, 260 ("From cycle 11 ... no
#   recurrence"). Imposed from the thesis reference model, not fitted.
# imposed deterministic year-10 cure point, both arms,
#   DFS and OS, excess hazard 0 after t*, Norwegian background mortality
#   thereafter. A fitted cure model is not identifiable from CHALLENGE.
# Caution: this constant is consumed ONLY by the cure_point_10yr structural
#   scenario. The default argument everywhere is NULL; any code path that
#   reads it unconditionally would silently alter the base case.
# SOURCE: Joranger et al. 2020 Figure 1 (p. 323) and Joranger et al. 2015 pp. 258, 260.
cure_t_star_years <- 10       # Years from randomization

# Cohort entry age
# SOURCE: CHALLENGE trial median age at enrolment (Courneya et al. 2025)
cohort_age     <- 61          # Years  --  used for life table lookup, severity calc

# PSA iterations
# SOURCE: Briggs et al. 2006 p. 149 uses 10,000 PSA replications.
n_psa          <- 10000       # Number of PSA samples (10,000 for thesis convergence)

# Annual operated stage-mix cohort size (population EVPI base)
# SOURCE: printed "Anbefalt referanse" on the report's inside cover: Årsrapport
#   2024 med resultater og forbedringstiltak fra Nasjonalt kvalitetsregister for
#   tykk- og endetarmskreft. Oslo: Folkehelseinstituttet, Kreftregisteret, 2025.
#   Figur 2.1, printed p.12: all 762 stage III colon cancer cases operated in
#   Norway in 2024.
# Caution: the publisher is NOT Helsedirektoratet. That name appears nowhere on the
#   cover or the inside cover of this report; the earlier attribution was wrong.
# Proportional scaling via CHALLENGE trial stage mix (90.2 percent stage III):
#   762 / 0.902 = 845 as a modelled annual cohort size.
# No adjuvant-treatment filter is applied. The cohort-matched analogue based
# on recorded receipt or start of adjuvant chemotherapy is approximately 472,
# a receipt-based upper-bound proxy affected by the case-to-patient limitation;
# the completion count is unknown because the registry does not report it.
# Population EVPI base case T = 10 years.
# Defined here as the primary reference; 06-psa.R and
# 08-export-tables.R reference this constant.
# Caution (site scope): this registry files rectosigmoid junction C19 WITH
#   colon, not with rectum. Printed p.40: "tilfeller med kreft i overgangen
#   mellom tykk- og endetarm (Rektosigmoideum C19) i denne rapporten er
#   klassifisert sammen med tykktarmskreft"; Appendix C.1, printed p.121: "I
#   årsrapporten er C18 og C19 slått sammen under tykktarmskreft". So 762 is
#   C18 + C19, EXCLUDING C20. That is colon in the colon-versus-rectum sense,
#   NOT C18-only in the strict ICD-10 sense. The report prints no C18/C19 split
#   of the 762 or of anything above it in that chain. This is a transferability
#   assumption to state against CHALLENGE's enrolled population, not a defect,
#   and the cohort size takes NO change from it.
ANNUAL_OPERATED_STAGE_MIX_COHORT <- 845  # Modelled annual cohort size


# =============================================================================
# 2. DISCOUNT RATES  --  Stepped Schedule (Norwegian Standard)
# =============================================================================
# Source: Norwegian Ministry of Finance, Rundskriv R-109 (25.06.2021 edition)
#         DMP Guidelines (July 2026), Section 12.8
# Public schedule: 4% in years 0-39, 3% in years 40-74, 2% from year 75 onward.
# SOURCE: Stepped 4%/3%/2% discount schedule is the base case (Rundskriv R-109).
# Flat 4% retained as a sensitivity analysis scenario.

discount_rate_flat <- 0.04   # Sensitivity analysis: flat 4% rate

#' Create Discount Weight Vector (Stepped Norwegian Schedule)
#'
#' Generates a vector of discount weights for each model cycle,
#' implementing the stepped discount schedule mandated by the
#' Norwegian Ministry of Finance (Rundskriv R-109, 25.06.2021).
#'
#' Schedule (Rundskriv R-109, NOU 2012:16):
#'   First 40 years (years 0-39):   4% per annum
#'   Next 35 years (years 40-74):   3% per annum
#'   Year 75 onwards:               2% per annum
#'
#' Methodology: weight_t = 1 / prod(1 + r_i for i = 1..t)
#' where r_i is the annual rate for the bracket containing year i.
#' Cycle 0 always has weight 1 (no discounting).
#'
#' Midpoint evaluation: discount weights are evaluated at the cycle
#' midpoint (t - 0.5 cycles). This is a complementary adjustment to
#' the trapezoidal half-cycle correction applied to state occupancy
#' in 04-costs-qalys.R. Together, they correctly discount the
#' average state occupancy within each cycle. This is NOT the same
#' as the half-cycle correction itself (which is the trapezoidal rule
#' on state probabilities).
#'
#' Example (monthly cycles, flat 4%):
#'   Cycle 1 midpoint = 0.5 * (1/12) = 0.04167 years
#'   Weight_1 = 1 / (1.04)^0.04167 = 0.99837
#'   Cycle 2 midpoint = 1.5 * (1/12) = 0.125 years
#'   Weight_2 = 1 / (1.04)^0.125 = 0.99511
#' The midpoints index where discounting is applied; the trapezoidal
#' correction indexes where state occupancy is averaged.
#'
#' @param n_cycles   Integer. Total number of model cycles.
#' @param cycle_length Numeric. Length of each cycle in years (e.g., 1/12).
#' @param schedule   Character. "stepped" (base case) or "flat" (sensitivity).
#' @param flat_rate  Numeric. Rate for flat discounting (default 0.04).
#' @return Numeric vector of length n_cycles + 1 (includes cycle 0).
#' @note This function assumes cycles are in chronological order (monotonic
#'   time grid). The internal accumulator (prev_year_floor, cumulative_factor)
#'   only advances forward. Non-monotonic time queries are not supported and
#'   will produce incorrect weights.
create_discount_weights <- function(n_cycles, cycle_length,
                                     # Make the stepped Norwegian discount schedule the function default.
                                     schedule = "stepped",
                                     # Limit the declared flat-rate sensitivity to 4 percent.
                                     flat_rate = 0.04,
                                     # Bracket boundaries per DMP (2026)
                                     # Section 12.8: "4% annually for the first
                                     # 40 years (years 0-39), 3% annually for
                                     # the next 35 years (years 40-74), and
                                     # thereafter 2% annually (year 75 and
                                     # onwards)" (Rundskriv R-109).
                                     # INTERNAL convention: the accumulator
                                     # indexes complete elapsed-year intervals
                                     # as y = 1, 2, ...; y = 40 is [39, 40),
                                     # so the first rate change is y = 41.
                                     # Internal endpoints 40/75 must therefore
                                     # stay separate from the public year labels
                                     # SOURCE: 0-39/40-74/75+.
                                     brackets = data.frame(
                                       year_end = c(40, 75, Inf),
                                       rate = c(0.04, 0.03, 0.02),
                                       display_label = c(
                                         "Years 0-39",
                                         "Years 40-74",
                                         "Years 75+"
                                       ),
                                       stringsAsFactors = FALSE
                                     )) {
  # O(n) vectorized implementation replacing O(n^2) nested loops.
  # Bracket boundaries are configurable via the `brackets` parameter.
  # Default brackets encode Norwegian Rundskriv R-109 schedule.
  #
  # Total cycles including cycle 0
  n_total <- n_cycles + 1
  weights <- numeric(n_total)
  # GUIDELINE: Apply no discounting before any time has elapsed (DMP 2026).
  weights[1] <- 1  # Cycle 0: no discounting

  if (schedule == "flat") {
    # Flat rate: standard geometric discounting at midpoint
    # Fully vectorized  --  no loop needed
    # GUIDELINE: Evaluate discount weights at cycle midpoints (DMP 2026).
    midpoints <- ((2:n_total) - 1 - 0.5) * cycle_length
    # GUIDELINE: Apply geometric annual discounting at the selected midpoint time (Briggs et al. 2006).
    weights[2:n_total] <- 1 / (1 + flat_rate)^midpoints
  } else {
    # Stepped schedule: O(n) single-pass implementation.
    #
    # Approach: for each cycle midpoint, compute the cumulative discount
    # factor by accumulating through year brackets in a single forward pass.
    # The function maintains a running cumulative factor and the last year processed.
    prev_year_floor <- 0
    cumulative_factor <- 1

    for (t in 2:n_total) {
      # GUIDELINE: Preserve midpoint timing inside the stepped schedule (DMP 2026).
      year_midpoint <- (t - 1 - 0.5) * cycle_length
      year_floor <- floor(year_midpoint)

      # Catch up full years from prev_year_floor to year_floor
      if (year_floor > prev_year_floor) {
        for (y in (prev_year_floor + 1):year_floor) {
          # Select each completed-year rate using internal endpoints 40 and 75.
          r_y <- brackets$rate[which(y <= brackets$year_end)[1]]
          # GUIDELINE: Compound each completed-year rate into the cumulative discount factor (Briggs et al. 2006).
          cumulative_factor <- cumulative_factor * (1 + r_y)
        }
        prev_year_floor <- year_floor
      }

      # Fractional year remaining
      # GUIDELINE: Preserve the partial year at the cycle midpoint instead of imposing whole-year discounting (DMP 2026).
      frac <- year_midpoint - year_floor
      if (frac > 0) {
        y_next <- year_floor + 1
        # Select the partial-year rate from the next elapsed-year interval.
        r_frac <- brackets$rate[which(y_next <= brackets$year_end)[1]]
        # GUIDELINE: Combine complete-year compounding with the remaining fractional year (Briggs et al. 2006).
        weights[t] <- 1 / (cumulative_factor * (1 + r_frac)^frac)
      } else {
        # GUIDELINE: Use the completed-year cumulative factor when no fractional year remains (Briggs et al. 2006).
        weights[t] <- 1 / cumulative_factor
      }
    }
  }

  weights
}

# These vectors are used by 04-costs-qalys.R for discounting costs and effects
discount_weights_stepped <- create_discount_weights(n_cycles, cycle_length,
                                                     # Pre-compute the stepped schedule for base-case discounting.
                                                     schedule = "stepped")
discount_weights_flat    <- create_discount_weights(n_cycles, cycle_length,
                                                     # Pre-compute flat discounting only for the retained sensitivity scenario.
                                                     schedule = "flat",
                                                     # Keep the flat sensitivity at the first-bracket 4 percent rate.
                                                     flat_rate = 0.04)


# =============================================================================
# 3. CLINICAL PARAMETERS  --  CHALLENGE TRIAL
# =============================================================================
# Source: Courneya et al. CHALLENGE trial
# Intervention: Structured exercise programme vs. standard care
# Population: stage III or high-risk stage II COLON cancer survivors
# SOURCE: Courneya et al. 2025 enrolled "stage III or high-risk stage II colon
#   cancer". Caution: not colorectal. Where a comment below says CRC it is
#   describing THAT SOURCE's own broader population, not this model's.

# Hazard ratios (intervention vs. control)
HR_DFS <- 0.72   # SOURCE: HR for Disease-Free Survival (Courneya et al. 2025)
HR_OS  <- 0.63   # SOURCE: HR for Overall Survival (Courneya et al. 2025)

# Log hazard ratios (for PSA  --  log-normal distribution)
log_HR_DFS <- log(HR_DFS)   # = -0.329
# GUIDELINE: Represent hazard-ratio uncertainty on the log scale for PSA (Briggs et al. 2012).
log_HR_OS  <- log(HR_OS)    # = -0.462

# Standard errors for log HRs  --  derived from 95% CI (CHALLENGE trial)
# Formula: SE(log HR) = (log(HR_upper) - log(HR_lower)) / (2 * 1.96)
# SOURCE: CHALLENGE trial (Courneya et al., NEJM 2025;393:13-25)
#   Abstract, Results, Fig 2A/2B, Supplementary Fig S3A/S3B  --  all 6 locations
#   report identical CI values.
# Caution: CI values must be taken from the primary NEJM publication, not
#   secondary sources which may contain transcription errors.
HR_DFS_lower <- 0.55   # SOURCE: DFS HR lower 95% CI (NEJM: 0.55)
HR_DFS_upper <- 0.94   # SOURCE: DFS HR upper 95% CI (NEJM: 0.94)
HR_OS_lower  <- 0.43   # SOURCE: OS HR lower 95% CI (NEJM: 0.43)
HR_OS_upper  <- 0.94   # SOURCE: OS HR upper 95% CI (NEJM: 0.94)

se_log_HR_DFS <- (log(HR_DFS_upper) - log(HR_DFS_lower)) / (2 * 1.96)
se_log_HR_OS  <- (log(HR_OS_upper)  - log(HR_OS_lower))  / (2 * 1.96)

# Marginal pseudo-IPD do not identify joint
# DFS-OS covariance, so zero cross-endpoint dependence is the transparent
# reference assumption rather than a data estimate. Complete endpoint rows are
# rank-coupled at positive rho values only in PSA-specification sensitivity.
# Use zero cross-endpoint dependence as the transparent reference assumption.
rho_endpoint_latent_reference <- 0
# Test positive rank coupling at rho 0.3, 0.5, 0.7, and 0.9 only as PSA-specification sensitivity.
rho_endpoint_latent_sensitivity <- c(0.3, 0.5, 0.7, 0.9)
# Keep the reference PSA seed fixed for clean-room reproducibility.
psa_seed_reference <- 42L
# Keep a distinct fixed coupling seed so dependence sensitivities remain reproducible.
dependence_coupling_seed <- 20260713L


# =============================================================================
# 4. SURVIVAL MODEL PARAMETERS
# =============================================================================
# These are populated after running 02-fit-survival.R
# Placeholder objects defined here for parameter file completeness

# Best-fit distribution for DFS
# Log-normal selected per NICE TSD 14 criteria.
#   Best BIC among 2-parameter distributions (dBIC 2.2 from gengamma).
#   Hazard rises briefly then declines (plausible for post-adjuvant DFS).
#   Gen pop mortality cap (assumption A4) corrects long-term extrapolation.
#   SA: gengamma (best AIC), run as the registered dfs_gengamma scenario.
# Select log-normal DFS using fit, parsimony, and long-term plausibility together.
best_dist_dfs <- "lnorm"

# Best-fit distribution for OS
# Log-normal selected per NICE TSD 14 criteria.
#   Best BIC (dBIC 0.0). Near-best AIC (dAIC 0.2 from gengamma, trivial).
#   More parsimonious than gengamma (2 vs 3 parameters).
#   Gen pop mortality cap corrects implausible long-term tail.
#   SA: none registered. The structural SA varies the DFS distribution only.
# Select log-normal OS using fit, parsimony, and the mortality-floor safeguard together.
best_dist_os  <- "lnorm"


# =============================================================================
# 5. UTILITY VALUES (Health-Related Quality of Life)
# =============================================================================
# All utilities on 0-1 scale (1 = perfect health, 0 = death)
# EQ-5D CRC utilities (EQ-5D preferred over HUI; )
#   (EQ-5D preferred over HUI; )

# Disease-Free state
# SOURCE: Mulder et al. (2022), J Cancer Survivorship 16:1055-1064
#   DOI: 10.1007/s11764-021-01096-6
#   EQ-5D-5L, Dutch tariff (Versteegh et al.), n=151 CRC survivors
#   Stage I-III, 2-10 years post-diagnosis, Netherlands
#   Table 2: mean 0.82 (SD 0.20, range -0.1 to 1.0)
#   By stage: I=0.78, II=0.82, III=0.84
#   Verified against original PDF.
# Note: source reports SD, not SE. PSA requires SE of the mean.
#   SE = SD/sqrt(n) = 0.20/sqrt(151) = 0.01628.
#   Briggs, Claxton & Sculpher (2006), Ch. 4: method of moments for Beta
#   requires SE (parameter uncertainty), not SD (patient variability).
n_mulder_2022 <- 151  # SOURCE: Mulder 2022, Table 2, CRC survivorship sample size
u_dfs_mean <- 0.82
u_dfs_sd   <- 0.20    # SOURCE: SD from Mulder 2022, Table 2
u_dfs_se   <- u_dfs_sd / sqrt(n_mulder_2022)  # SE = SD/sqrt(n) = 0.01628

# Progressed/Recurrence state
# SOURCE (original): Färkkilä et al. (2014), Qual Life Res 23:1387-1394
#   DOI: 10.1007/s11136-013-0562-y
#   EQ-5D-3L, UK TTO value set, n=57 CRC palliative patients, Finland
#   Table 2: CRC mean 0.662 (SD 0.298, 95% CI 0.585-0.745)
#   Verified against original publication.
# MAPPING: Torkilseng et al. (2025), PharmacoEcon Open 9(3)
#   DOI: 10.1007/s41669-025-00562-6
#   Linear algorithm mapping UK EQ-5D-3L means to Danish EQ-5D-5L means.
#   Validated on 11 oncology trials (n=52,342). Input 0.662 is within the
#   external validation range (0.63-0.84). Danish 5L is the closest Nordic
#   proxy for Norway (Garratt et al., 2025, noted similar preference
#   structures across Scandinavian countries).
#   Mapped value: 0.662 (UK 3L) -> 0.743 (Danish 5L)
# VALIDATION: Flyum et al. (2021), BMC Palliat Care 20(1):144
#   Norwegian systematic review, OsloMet. Pooled CRC palliative EQ-5D =
#   0.67 (95% CI 0.62-0.73). Applying the same Torkilseng mapping to 0.67
#   yields 0.749, consistent with our mapped 0.743.
# NOTE: This represents end-stage/palliative CRC. Using the mapped 5L value
#   (0.743) is conservative: it narrows the DFS-PD utility gap (0.82-0.743
#   = 0.077 vs 0.82-0.662 = 0.158), reducing incremental QALYs from
#   delaying progression and biasing the ICER upward.
# Preserve source-scale facts and transform both the mean
# and sampling uncertainty with the published linear mapping.
u_prog_3l_mean <- 0.662
u_prog_3l_sd <- 0.298
# SOURCE: Färkkilä et al. 2014 Table 2.
u_prog_3l_n <- 57
u_prog_3l_ci <- c(0.585, 0.745)
# SOURCE: Torkilseng et al. 2025 linear mapping algorithm.
u_prog_mapping_intercept <- 0.231
u_prog_mapping_slope <- 0.773
# SOURCE: Torkilseng et al. 2025 mapping-model RMSE.
u_prog_mapping_rmse <- 0.00764
# GUIDELINE: Apply the published linear mapping on the source utility scale (Torkilseng et al. 2025).
u_prog_mean <- u_prog_mapping_intercept +
  u_prog_mapping_slope * u_prog_3l_mean
# Transform source-scale sampling uncertainty with the mapping slope.
u_prog_sampling_se <- u_prog_mapping_slope *
  (u_prog_3l_sd / sqrt(u_prog_3l_n))
# Keep sampling SE alone in the reference PSA.
u_prog_se <- u_prog_sampling_se
# Map source-scale POWSA bounds with the same published transformation.
u_prog_powsa_bounds <- u_prog_mapping_intercept +
  u_prog_mapping_slope * u_prog_3l_ci
# Add mapping RMSE by quadrature only in the separate PSA-specification sensitivity.
u_prog_se_mapping_sensitivity <- sqrt(
  u_prog_sampling_se^2 + u_prog_mapping_rmse^2)

# Death (fixed at 0)
# Fix utility at zero in the absorbing death state.
u_dead <- 0

# --- Subgroup and SA utility values ---
# Named parameters for structural SA subgroup scenarios
# SOURCE: Mulder et al. (2022), Table 3, stage-specific values
u_dfs_stage_ii  <- 0.82   # Stage II DFS utility (SD=0.21, n=52), Mulder 2022 Table 3
u_dfs_stage_iii <- 0.84   # Stage III DFS utility (SD=0.17, n=49), Mulder 2022 Table 3
# SOURCE: Mulder et al. (2022) p. 1060, sensitivity analysis: the England
#   EQ-5D-5L value set yields a whole-sample mean utility of 0.85 (n = 151,
#   stage I-III survivors 2-10 years post-diagnosis) against 0.82 under the
#   Dutch value set. Mapping that mean to the disease-free state is the
#   thesis's own convention, identical to the base-case 0.82.
# Caution: the object name keeps the historical `devlin` token because the
#   scenario ID `utility_devlin` is a registered structural-SA key; renaming
#   it would move structural_sa_results.csv rows and the guardrail count pin.
#   Reader-facing text names the value set, never the value-set author alone.
# Caution: u_dfs_powsa_high below is a SEPARATE 0.85 (Beta quantile), unrelated.
u_dfs_devlin    <- 0.85   # SOURCE: England EQ-5D-5L value set, Mulder 2022 SA (p. 1060)
# --- POWSA utility bounds (derived from Beta distribution) ---
# Beta(~456, ~100) for u_dfs (using SE, not SD, per method of moments)
#   qbeta(0.025, 456, 100) = 0.787 → rounded 0.79
#   qbeta(0.975, 456, 100) = 0.851 → rounded 0.85
# Caution: This lower bound is a derived Beta quantile, not a source-reported utility value.
u_dfs_powsa_low  <- 0.79  # Beta 2.5th percentile
# Caution: This upper bound is a derived Beta quantile, not a source-reported utility value.
u_dfs_powsa_high <- 0.85  # Beta 97.5th percentile
# Progressed-state bounds are mapped to the target tariff scale.
# Keep the progressed-state lower bound on the mapped target scale.
u_prog_powsa_low <- u_prog_powsa_bounds[1]
# Keep the progressed-state upper bound on the mapped target scale.
u_prog_powsa_high <- u_prog_powsa_bounds[2]

# =============================================================================
# 5b. AGE-DEPENDENT UTILITY ADJUSTMENT
# =============================================================================
# SOURCE: Garratt et al. (2022), PMID 34272631, Table 6
#   "Norwegian population norms for the EQ-5D-5L"
#   Quality of Life Research 31(2):517-526
#   Scoring: EQ-5D-3L crosswalk value set (UK)
#   Garratt 2022 is the selected population norm source.
#
# NOTE: These norms use the crosswalk (3L) scoring, not the D5 Norwegian
# tariff (Garratt 2025, PMID 39565555). Garratt 2025 provides the D5
# VALUE SET (scoring coefficients) but does NOT publish age-specific
# population norms scored with D5. Until D5-scored population norms are
# published, crosswalk norms are the best available Norwegian data.
# This instrument inconsistency (crosswalk norms + D5 tariff for disease
# states) must be acknowledged in the Discussion as a limitation.
#
# NICE/NOMA: age-adjustment required for lifetime horizons (40 years).
# Without age-adjustment, constant utilities overestimate
# QALYs at older ages, biasing the ICER downward.
#
# Method: multiplicative adjustment per Ara & Brazier (2010):
#   u(t) = u_health_state * (u_genpop(age_0 + t) / u_genpop(age_0))
# where u_genpop(age) is the general population EQ-5D norm at that age.
#
# Sex weighting: 51.4% female, 48.6% male (CHALLENGE Table 1, N=889).

age_utility_enabled <- TRUE  # Set FALSE to disable age-adjustment (SA)

# Norwegian EQ-5D population norms by age group
# SOURCE: Garratt et al. (2022), Table 6, sex-weighted (51.4% F / 48.6% M)
# SOURCE: Garratt et al. (2022), Quality of Life Research 31(2):517-526
age_utility_norms <- data.frame(
  age_lower = c(18, 30, 40, 50, 60, 70, 80),
  age_upper = c(29, 39, 49, 59, 69, 79, 100),
  # SOURCE: Garratt et al. 2022 Table 6, sex-weighted by Courneya et al. 2025 Table 1.
  eq5d_mean = c(0.8197, 0.8387, 0.7879, 0.7999, 0.8163, 0.7933, 0.7222)
  # Garratt 2022 Table 6 (sex-specific columns, weighted):
  #   18-29: 0.514*0.825 + 0.486*0.814 = 0.8197
  #   30-39: 0.514*0.828 + 0.486*0.850 = 0.8387
  #   40-49: 0.514*0.786 + 0.486*0.790 = 0.7879
  #   50-59: 0.514*0.831 + 0.486*0.767 = 0.7999
  #   60-69: 0.514*0.826 + 0.486*0.806 = 0.8163
  #   70-79: 0.514*0.805 + 0.486*0.781 = 0.7933
  #     80+: 0.514*0.746 + 0.486*0.697 = 0.7222
)

# Sex-specific Norwegian EQ-5D population norms
# SOURCE: Garratt et al. (2022), Table 6, female and male columns
# Used by sex-specific subgroup scenarios (Dimension C)
age_utility_norms_female <- data.frame(
  age_lower = c(18, 30, 40, 50, 60, 70, 80),
  # SOURCE: Garratt et al. 2022 Table 6, female age bands.
  age_upper = c(29, 39, 49, 59, 69, 79, 100),
  # SOURCE: Garratt et al. 2022 Table 6, female column.
  eq5d_mean = c(0.825, 0.828, 0.786, 0.831, 0.826, 0.805, 0.746)
  # Garratt 2022 Table 6, female column
)

age_utility_norms_male <- data.frame(
  # SOURCE: Garratt et al. 2022 Table 6, male age bands.
  age_lower = c(18, 30, 40, 50, 60, 70, 80),
  age_upper = c(29, 39, 49, 59, 69, 79, 100),
  # SOURCE: Garratt et al. 2022 Table 6, male column.
  eq5d_mean = c(0.814, 0.850, 0.790, 0.767, 0.806, 0.781, 0.697)
  # Garratt 2022 Table 6, male column
)

# Sex-specific life table path (SSB Table 07902, male/female qx)
# SOURCE: SSB Table 07902. Columns: age, qx_male, qx_female.
life_table_sex_path <- "data/raw/norway_life_table_sex_specific.csv"

# No evidence identifies an exercise disutility; zero is active
# and fixed, retained as an explicit applied-model column rather than sampled.
u_exercise_decrement <- 0


# =============================================================================
# 6. COST PARAMETERS (Norwegian Krone  --  NOK)
# =============================================================================
# All costs in 2024 NOK (price year = 2024).
# Unit costs: DMP Enhetskostnader v1.6 (2024 prices).
# Kenseth 2024 costs (2021 NOK) CPI-adjusted using SSB Table 08981 ratio 1.1507.
# SEs: SD = 0.20 x mean where no published SE (Kenseth 2024 approach).
# PSA distribution: Gamma (positive-valued costs). Kenseth Table 1 confirmed.
# All values match their original source publications.

# Treatment costs  --  Disease-Free state
# GUIDELINE: Helsedirektoratet CRC follow-up schedule.
# SOURCE: hospital consultation = DRG 906A cost weight 0.050 from the 2026
#   official DRG master list x DMP v1.6 2024 DRG unit price 52,248 NOK
#   = 2,612.40 NOK. Other unit costs remain DMP/Kenseth inputs.
# DECISION: two DRG-valued hospital consultations; surveillance remains one
#   shared component.
# Caution: use Kostnadsvekt 0.050, not an unrelated specialist-practice tariff.
#   Five-year total = 29,008.80 NOK; annual rounded input = 5,802 NOK.
# Caution (consequential, 2026-07-28): this literal is the tie-out of the
#   component bundle guard-checked further down this file. It moved 5,766 ->
#   5,802 solely because c_colonoscopy_unit moved 3,636 -> 3,814 (Kenseth-
#   derived to ISF DRG 710O). Nothing else in the bundle changed. Leaving 5,766
#   in place would have made that stopifnot abort at source time.
c_surveillance_early <- 5802
c_surveillance_early_se <- round(0.20 * c_surveillance_early)
# Keep late surveillance as an active fixed zero after the five-year window.
c_surveillance_late <- 0
# End the shared surveillance component after the national five-year follow-up window.
surveillance_cutoff_years <- 5
# --- Exercise intervention cost components (extended healthcare perspective) ---
# Extended healthcare perspective as base case (Meld. St. 21, 2024-2025, Section 4.3.8).
# SOURCE: Meld. St. 21 (2024-2025) p.43, Section 4.3.8.
# Includes: healthcare costs + patient travel + patient time at leisure rate.

# Healthcare component: Helfo physiotherapy tariff, DMP-adjusted
# SOURCE: Helfo A3a (225 NOK) + 4 x A3b (4 x 102 = 408 NOK) = 633 NOK.
#   FOR-2024-06-17-1184 as amended by FOR-2025-06-23-1196; rates in force from 2025-07-01.
#   Verified twice. A fail-closed guard further down this file ties this
#   literal to c_helfo_a3a and c_helfo_a3b, which are defined after it.
c_session_tariff_bare <- 633  # Bare Helfo A3 tariff; lower tested alternative
# DMP: the unit-cost database's own assumptions sheet documents that a tariff
#   understates the cost of providing a contact, because the fee covers only part
#   of the provider's cost and the remainder arrives as public operating subsidy
#   (basistilskudd for GPs, driftstilskudd for contract specialists), and it
#   gives multiplication by two as a rule-of-thumb rough approximation
#   (DMP enhetskostnader v1.6, sheet "1.2 Forutsetninger", row 27
#   "Justering av takst" = 2, source SLV 2018 s. 27).
dmp_takst_adjustment  <- 2    # SOURCE: DMP v1.6, sheet 1.2 Forutsetninger, row 27
# the base case takes the DMP-doubled tariff. This is the
#   fallback branch, which fires because no defensible
#   per-session driftstilskudd allocation is derivable. The
#   1,695-hour divisor behind the 955 NOK anchor has no primary source and is
#   contradicted by DMP's own derivation, and a base case may not rest on an
#   unreproducible number.
# Caution: the doubling applies to the healthcare fee ONLY. Travel and patient time
#   are separate components and are NOT doubled.
c_session_healthcare <- dmp_takst_adjustment * c_session_tariff_bare  # = 1,266 NOK

# Travel component: Pasientreiser regulated mileage
# SOURCE: grue2021, TOI rapport 1835/2021, Tabell 10.2, p. 73: representative one-way
#   road distance 9.1 km and round-trip travel time 32 min.
# GUIDELINE: pasientreiseforskriften (FOR-2015-06-25-793) Section 21 supplies
# the mileage rate only. The 3.20 NOK/km standardsats is the rate in force from
# 1 January 2026, set by FOR-2025-12-15-2590 Section 21 first paragraph, and is
# entered at face value in the 2024 price-year analysis (the same
# face-value convention as the July-2025 Helfo tariffs).
# DECISION: locally delivered physiotherapy proxy; sourced replacement of 15 km.
# Caution: do not use emergency-department distance as a physiotherapy proxy.
travel_distance_km   <- 9.1
mileage_rate_nok     <- 3.20
# Apply the sourced one-way trip distance in both directions at the regulated mileage rate.
c_travel_per_session <- travel_distance_km * mileage_rate_nok * 2

# Patient time component: leisure-time wage rate x (exercise + travel time)
# SOURCE: DMP Enhetskostnader v1.6 Sheet 1.1, Fritid = 328 NOK/hr.
#   Exercise time: 1 hr/session. Travel time: 32 min round trip (16 min one-way x 2; grue2021 Tabell 10.2).
#   Total patient time per session: 328 x (1 + 32/60) = 502.93 NOK.
# Rate applies to both exercise and travel time.
c_patient_time_rate    <- 328   # SOURCE: NOK/hr, DMP v1.6 Fritid (NOK 2024)
# Caution: this is an ASSUMPTION, not a trial-reported quantity. Two independent
#   checks searched the CHALLENGE supplementary appendix and the
#   250-page protocol and found NO stated supervised-session duration anywhere;
#   appendix section 1.5.3 "Supervised PA" describes the sessions in full and
#   gives no length. It must never be cited as sourced, and it must never be used
#   to argue that behavioural-support time is already inside the priced hour.
exercise_time_hr       <- 1     # ASSUMPTION: exercise session duration (hours), not stated in source
travel_time_min <- 32  # SOURCE: grue2021, TOI rapport 1835/2021, Tabell 10.2, p. 73
c_patient_time_per_session <- c_patient_time_rate * (exercise_time_hr + travel_time_min / 60)
# = 328 x (1 + 0.5333) = 502.93 NOK

# Total per-session cost: extended healthcare perspective (base case)
# Base-case session cost includes healthcare, travel, and patient time.
c_session_extended <- c_session_healthcare + c_travel_per_session +
                      c_patient_time_per_session
# = 1,266 + 58.24 + 502.93 = 1,827.17 NOK
# Caution: the healthcare term carries the DMP adjustment; travel
#   and patient time do not. Only the first term moved.

# Standard healthcare perspective per-session cost (for SA-perspective-1)
# Excludes travel and patient time. Used in perspective delta analysis.
# The standard-healthcare sensitivity excludes travel and patient time.
c_session_standard <- c_session_healthcare
# = 1,266 NOK, the DMP-adjusted healthcare session cost

# -----------------------------------------------------------------------------
# Patient-time labour/leisure SENSITIVITY SCENARIO (scaffold; base case UNCHANGED)
# -----------------------------------------------------------------------------
# GUIDELINE: DMP submission guidelines (July 2026) section 12.7.1 - patient time is
#   valued at a COMMON leisure rate regardless of employment; productivity/work-time
#   changes must NOT be included. The base case (c_patient_time_rate = 328, above) is
#   guideline-compliant and is NOT modified here.
# The weighted labour/leisure valuation of patient time is GUIDELINE-DISCOURAGED
#   for the base case, so it is provided ONLY as a deterministic sensitivity
#   scenario.
# GUIDELINE: Rundskriv R-109 (Finansdepartementet 2021) section 6.1.4 - general
#   socioeconomic appraisal splits arbeidstid (gross real wage) from fritid (net real
#   wage); this is the basis for the weighted scenario rate.
# SOURCE: DMP enhetskostnader v1.6, sheet 1.1, row 24: gross hourly wage
#   including charges and social costs = 620.4822067386087 NOK/hour.
# SOURCE: ssbEmployment61, SSB Table 06161, 2024, exact age 61, register-based: employment share at age 61
#   = 74.0 per cent (0.740).
# GUIDELINE: DMP common leisure rate remains the base case.
# DECISION: working-age weighting is scenario-only.
# Caution: do not use the unverified 60-64 range or interpret this as productivity loss.
patient_time_working_share <- 0.740
c_patient_time_rate_gross  <- 620.4822067386087

#' Weighted patient-time rate (sensitivity scenario helper)
#'
#' GUIDELINE: Rundskriv R-109 section 6.1.4 - working time valued at gross real wage,
#'   leisure time at net real wage (the base-case leisure rate, 328 NOK/hr).
#' @param working_share Numeric in [0, 1]. Labour-force participation share (age ~61).
#' @param gross_rate Numeric > 0. Gross real-wage rate (NOK/hr) for working time.
#' @param leisure_rate Numeric > 0. Net real-wage leisure rate (NOK/hr); base case 328.
#' @return Numeric. working_share * gross_rate + (1 - working_share) * leisure_rate.
weighted_patient_time_rate <- function(working_share, gross_rate, leisure_rate) {
  # Caution: scaffold must fail loudly on missing external data, never return a
  #   plausible number. NA inputs (PENDING download-queue) stop() here.
  if (anyNA(c(working_share, gross_rate, leisure_rate))) {
    stop("weighted_patient_time_rate: inputs must not be NA (weighted ",
         "patient-time scenario blocked pending the missing source data; do not estimate).")
  }
  stopifnot(
    is.numeric(working_share), working_share >= 0, working_share <= 1,
    is.numeric(gross_rate), gross_rate > 0,
    is.numeric(leisure_rate), leisure_rate > 0
  )
  # GUIDELINE: Weight working time at gross wage and leisure time at the leisure rate only in this sensitivity scenario (Finansdepartementet 2021).
  working_share * gross_rate + (1 - working_share) * leisure_rate
}

# Adherence-adjusted SUPERVISED EXERCISE sessions over the 3-year programme
# SOURCE: Courneya et al. 2025 (NEJM 393:13-25), printed p.18. This count is
#   CONSTRUCTED, never transcribed. Table 2 prints NO session counts at all:
#   only eligible-patient counts (445/414/388/373/354/343) and attendance
#   PERCENTAGES as mean +/- SD. Every scheduled contact count lives in the
#   Table 2 footnotes (dagger, double dagger, section sign) and in the Results
#   narrative on the same page. Citing "Table 2" as the source of any session
#   count is inaccurate; cite the printed rates plus the footnoted schedule.
# SOURCE (construction, exercise sessions only):
#   Phase 1, footnote dagger, 12 mandatory + 12 recommended supervised exercise
#     sessions; Table 2 rates 79% and 20%:
#     (12 x 0.79) + (12 x 0.20) = 9.48 + 2.40 = 11.88
#   Phase 2, footnote double dagger, 12 in-person/remote behavioural-support
#     sessions with a supervised exercise session strongly recommended when the
#     support session is in person; Table 2 recommended-exercise rate 54%:
#     12 x 0.54 = 6.48
#   Phase 3, footnote section sign, 24 monthly support sessions with the same
#     co-located exercise recommendation, Table 2 reporting four 6-month blocks
#     at 52/46/40/38 per cent:
#     6 x 0.52 + 6 x 0.46 + 6 x 0.40 + 6 x 0.38 = 10.56
#   Total = 11.88 + 6.48 + 10.56 = 28.92
#   Cross-check, independent of the block split: the Results narrative prints a
#   pooled phase-3 rate of 44%, and 24 x 0.44 = 10.56 reproduces exactly because
#   (52+46+40+38)/4 = 44.0 per cent exactly.
# DECISION: count SUPERVISED EXERCISE SESSIONS ONLY. The superseded
#   62.16 was exercise sessions PLUS behavioural-support contacts:
#     P1 (12 x 0.83) + 11.88 = 21.84; P2 (12 x 0.68) + 6.48 = 14.64;
#     P3 (24 x 0.63) + 10.56 = 25.68; total 62.16, excess 33.24 = the
#     behavioural component (9.96 + 8.16 + 15.12).
#   Caution: 33.24 is the HISTORICAL excess of the superseded 62.16, which built
#     phase 3 on the narrative-only pooled 63 per cent. The LIVE behavioural
#     count is 33.30, built block-wise from the four printed Table 2 cells.
#     See n_behaviour_adherence below. Do not read 33.24 as a current value.
# Caution: adding the two Table 2 rows is a DOUBLE COUNT, not a scope preference.
#   The unit cost this multiplies is a per-IN-PERSON-VISIT quantity in all three
#   of its components (c_session_extended at the definition above = 1,266
#   DMP-adjusted Helfo A3 session cost + 58.24 round-trip mileage + 502.93
#   patient time, the
#   latter being one hour of exercise plus 32 minutes of travel). Behavioural
#   support may be delivered remotely by telephone or video per the Table 2
#   footnotes, incurring none of mileage, travel time or the exercise hour; and
#   where it IS in person the exercise session is CO-LOCATED with it. Summing
#   the two rows therefore prices one clinic visit twice.
# SOURCE: Courneya et al. 2025 p. 18, printed attendance rates plus the footnoted exercise schedule.
n_sessions_adherence  <- 28.92  # Adherence-adjusted supervised exercise sessions
# SOURCE: full-attendance supervised exercise sessions per year, programme mean.
#   Printed schedule, exercise sessions only: Phase 1 = 24 over 6 months (48/yr),
#   Phase 2 = 12 over 6 months (24/yr), Phase 3 = 24 over 24 months (12/yr);
#   60 sessions over 3 years = 20 per year.
# Caution: the superseded 52 rested on "1/week x 52 weeks". NOTHING in CHALLENGE is
#   weekly: phase 1 and 2 are every 2 weeks and phase 3 is MONTHLY per the
#   Table 2 footnotes. 52 also exceeded the programme's own scheduled maximum,
#   so the full-adherence scenario that consumes this constant was priced above
#   100 per cent of a schedule that does not exist.
sessions_per_year     <- 20     # Full-attendance exercise sessions/yr (60 / 3)
adherence_phase3_min  <- 0.38   # SOURCE: Phase 3 minimum adherence (Courneya 2025 Table 2)

# Treatment costs  --  Progressed state
# SOURCE: Joranger et al. 2020 Online Resource 1, Table 1, stage-IV column,
#   rows 15-18 and 20, in 2011 prices. The table's caption defines the
#   frequency as "how many times an average patient with a certain diagnosis
#   receives the listed treatment", so frequency x unit cost is the source's
#   own expected cost per average stage-IV patient.
# SOURCE: Joranger et al. 2020, Data and data sources, printed p. 325:
#   source conversion rate = 7.79 NOK/EUR.
# SOURCE: SSB Table 08981 annual averages: CPI 2011 = 93.3;
#   CPI 2024 = 133.6; ratio = 133.6 / 93.3.
# The progressed-state cost is the frequency-weighted sum of
#   ALL metastatic-treatment components the source prices, not palliative
#   chemotherapy alone. Metastasis-directed surgery is priced separately in
#   rows 15-16 and non-surgical metastasis care in rows 17-18; none of it is
#   contained in row 20.
# GUIDELINE: a health-state cost is the expected cost of an average patient
#   occupying that state (DMP enhetskostnad practice; ISPOR good practice on
#   state-cost construction).
# Caution: the superseded input took row 20 at frequency 1.000 while omitting
#   rows 15-18. Row 20's frequency is .610, so applying its unit cost to every
#   progressed patient mixes a conditional unit cost with an omission.
# Caution: Do not substitute Joranger's EUR 10,920 lifetime population average,
#   EUR 40,850 conditional total treatment cost, or Kenseth's NOK 146,763 sum.
cpi_2011_to_2024 <- 133.6 / 93.3
stage4_components <- data.frame(
  row_label          = c("Liver metastasis resection", "Lung metastasis resection",
                         "Liver metastasis, non-surgical care",
                         "Lung metastasis, non-surgical care", "Palliative chemotherapy"),
  # SOURCE: Joranger et al. 2020 Online Resource 1, Table 1, rows 15-18 and 20.
  joranger_row       = c(15L, 16L, 17L, 18L, 20L),
  # SOURCE: Joranger et al. 2020 Online Resource 1, Table 1, stage-IV frequencies.
  frequency_stage4   = c(0.125, 0.019, 0.188, 0.075, 0.610),
  # SOURCE: Joranger et al. 2020 Online Resource 1, Table 1, 2011 EUR unit costs.
  unit_cost_eur_2011 = c(26528, 18968, 6468, 7664, 20183),
  stringsAsFactors = FALSE
)
# Caution: this intermediate is deliberately NOT rounded before conversion. Rounding
#   here moves c_progressed_annual off 198,319; the exported macro rounds a copy
#   for display only. The multiplication order is unchanged, so the product is
#   bit-identical to the superseded single expression.
# Frequency-weight all five priced stage-IV components before summing.
c_progressed_eur_2011 <- sum(stage4_components$frequency_stage4 *
                             stage4_components$unit_cost_eur_2011)
# Convert the completed EUR sum at the source exchange rate and then index it to 2024 NOK.
c_progressed_annual <- round(c_progressed_eur_2011 * 7.79 * cpi_2011_to_2024)
# SOURCE: Kenseth et al. 2024 Table 1 cost-uncertainty convention.
c_progressed_annual_se <- round(0.20 * c_progressed_annual)

# One-off costs
# SOURCE: Helfo A1d = 516 NOK, one-time at t=0, intervention arm only.
#   FOR-2024-06-17-1184 as amended by FOR-2025-06-23-1196; rates in force from 2025-07-01.
# 1x A1d initial assessment.
c_intervention_setup <- 516   # Initial assessment A1d, one-time (rate in force from 2025-07-01)

# Intervention programme duration (years)
# The CHALLENGE trial exercise programme runs for a fixed period.
# After this duration, intervention-specific DFS costs stop and
# revert to standard care DFS costs.
# SOURCE: CHALLENGE trial protocol (ClinicalTrials.gov NCT00819208).
#   The structured exercise programme lasts 3 years from randomisation.
#   Courneya et al. NEJM 2025;393:13-25, Table 1 confirms 36-month
#   intervention period. Median follow-up was 7.9 years, so >4 years
#   of post-intervention follow-up data are available.
# SOURCE: CHALLENGE trial (Courneya et al. 2025).
intervention_duration_years <- 3

# --- Behavioural-support contacts (the second component of the programme) ------
# SOURCE (count): Courneya et al. 2025 (NEJM 393:13-25), printed p.18. Scheduled
#   behavioural-support contacts live in the Table 2 footnotes; the attendance
#   rates are the printed cells of the Table 2 row "Attendance at mandatory
#   behavioral-support sessions - %":
#     Phase 1  12 x 0.83 = 9.96
#     Phase 2  12 x 0.68 = 8.16
#     Phase 3  Table 2 prints FOUR 6-month block cells, 71 / 64 / 59 / 59, and NO
#              pooled cell: 6 x (0.71 + 0.64 + 0.59 + 0.59) = 15.18
#     Total = 9.96 + 8.16 + 15.18 = 33.30
# Caution: do NOT build phase 3 as 24 x 0.63. The 63 per cent is printed ONLY in the
#   Results narrative, never in Table 2, and it is 63.25 rounded down, so the
#   pooled route returns 15.12 and understates by 0.06. The exercise row above
#   can use its pooled 44 per cent only because (52+46+40+38)/4 is exactly 44.0;
#   the behavioural rates do not have that property. Attributing "63%" to Table 2
#   is the same inaccurate-attribution class. Confirmed twice
#   against the original PDF by two independent checks.
# SOURCE (duration): CHALLENGE supplementary appendix PDF p.14 (internal p.13),
#   section 1.5.2 "Content and Delivery of Behavior Support Sessions", sub-heading
#   "Sessions 1-12": "Each topic will take approximately 30-60 minutes".
#   Independently transcribed twice, same locator, same wording.
# SOURCE (tariff): FOR-2024-06-17-1184 as amended by FOR-2025-06-23-1196, rates in
#   force from 2025-07-01. A3a = 225 (inntil 20 min) + 1 x A3b = 102 (per pabegynte
#   10 min) prices the 30-minute LOWER bound of the printed range.
# SOURCE (deliverer): trial protocol p39, the PAC is "trained in exercise
#   physiology, behavior change and support, and cancer ... for example,
#   physiotherapist or occupational therapist"; p85 "qualified exercise
#   specialist". A physiotherapy tariff basis is therefore the same basis already
#   used for the supervised exercise sessions.
# DECISION: price the LOWER bound of the printed 30-60 minute range. An
#   omitted-cost correction that took the top of a printed range would invite the
#   charge that the correction manufactured cost; taking the bottom makes the
#   corrected ICER an explicit LOWER bound on the corrected ICER. The 60-minute
#   upper bound is A3a + 4 x A3b = 633 = c_session_tariff_bare, a +16.0% move in
#   c_exercise_annual, already inside the +/-50% POWSA band at powsa_c_exercise_*.
# DECISION: c_helfo_a3a alone (225) was rejected as the unit. It prices "inntil
#   20 min", below the source's own printed 30-minute minimum.
# Caution: this is NOT c_session_extended (1,827.17). A behavioural
#   contact must not carry travel, travel time or the exercise hour: the Table 2
#   footnotes permit it to be delivered remotely by telephone, and where it is in
#   person the exercise session is CO-LOCATED and already priced on its own row.
# Caution: the 33.30 contacts are NOT netted against the 26.52 co-located exercise
#   visits. exercise_time_hr <- 1 above is a model assumption with no source
#   counterpart (the trial states no supervised-session duration anywhere), so
#   behavioural time is not already inside the priced exercise hour. The source
#   prints that the two "may occur separately and/or in combination" and are
#   "combined with ... as well as occurring independently".
# Caution: the printed behavioural attendance rates (0.83; 0.68; and the phase-3
#   block cells 0.71/0.64/0.59/0.59) EXCEED the
#   printed exercise rates in every phase. Counting behavioural contacts on the
#   exercise row would price them at the lower rate. They are counted here on
#   their own rates; the exercise row keeps its own.
# Caution: c_helfo_a3a and c_helfo_a3b are defined further down this file, so they
#   cannot be referenced here. The literal follows the same idiom as
#   c_session_tariff_bare above (633 written out, derivation in the comment). A
#   fail-closed guard immediately after those definitions ties the two together.
# SOURCE: Courneya et al. 2025 p. 18, 12 phase-1 contacts at 83 percent attendance.
adj_behaviour_phase1  <- 9.96    # 12 x 0.83
# SOURCE: Courneya et al. 2025 p. 18, 12 phase-2 contacts at 68 percent attendance.
adj_behaviour_phase2  <- 8.16    # 12 x 0.68
# SOURCE: Courneya et al. 2025 p. 18, four phase-3 six-month attendance blocks.
adj_behaviour_phase3  <- 15.18   # 6 x (0.71 + 0.64 + 0.59 + 0.59)
# SOURCE: Courneya et al. 2025 p. 18 phase-specific behavioural attendance schedule.
n_behaviour_adherence <- adj_behaviour_phase1 + adj_behaviour_phase2 +
  adj_behaviour_phase3          # = 33.30 adherence-adjusted behavioural contacts
c_behaviour_contact   <- 327    # SOURCE: 30 min: A3a (225) + 1 x A3b (102); rate in force from 2025-07-01
c_behaviour_annual    <- n_behaviour_adherence * c_behaviour_contact /
  intervention_duration_years   # 33.30 x 327 / 3 = 3,629.70 NOK/year
# Caution: fail closed if the phase decomposition is ever edited without the total.
stopifnot(isTRUE(all.equal(n_behaviour_adherence, 33.30)))

# --- Computed annual exercise programme costs ---
# Annual exercise PROGRAMME cost (extended perspective, base case). Both
# components of the CHALLENGE intervention: supervised exercise visits AND the
# behavioural-support contacts scheduled alongside them.
# = (n_sessions x c_session_extended) / intervention_duration_years
#   + c_behaviour_annual
# = 28.92 x 1,827.173333 / 3 = 17,613.950933  (exercise visits)
#   + 33.30 x 327 / 3        =  3,629.70      (behavioural contacts)
#   = 21,243.650933 NOK/year (exact float, not rounded)
# Caution: the name is historical and is deliberately NOT changed. It is the key of
#   the PSA parameter vector, the POWSA row and the exported LaTeX macro; renaming
#   it would drift every one of those against files that match it by string.
# SOURCE: Courneya et al. 2025 p. 18 programme schedule plus the sourced unit-cost blocks above.
c_exercise_annual <- n_sessions_adherence * c_session_extended /
  intervention_duration_years + c_behaviour_annual
# SOURCE: Kenseth et al. 2024 Table 1 cost-uncertainty convention.
c_exercise_annual_se <- 0.20 * c_exercise_annual

# Standard perspective DFS intervention cost (for perspective delta SA)
# Caution: this REBUILDS the programme cost from the healthcare-only unit, so the
#   behavioural component must be re-added or the healthcare-only perspective
#   silently drops it. It is added UNCHANGED: c_behaviour_contact is already a
#   pure Helfo tariff carrying no travel and no patient time, so it is identical
#   under both perspectives and cancels exactly in the perspective delta. That is
#   why the delta remains travel plus patient time over the exercise sessions
#   alone, as the appendix states.
# Rebuild the programme under the standard-healthcare perspective without travel or patient time.
c_exercise_annual_standard <- n_sessions_adherence * c_session_standard /
                              intervention_duration_years + c_behaviour_annual

# Terminal care costs (death cycle)
# SOURCE: Bjornelv et al. 2020 (PMID 32054492) DIRECTLY, not via Kenseth 2024
#   Table 1. Bjornelv: all CRC decedents Norway 2009-2013, NPR+KUHR+IPLOS
#   registry data; all healthcare costs in the last month of life = 118,215 NOK.
# SOURCE (price basis, verbatim from Bjornelv Methods): "We estimated the costs
#   in 2013 Norwegian Krone (NOK)". The figure is therefore a 2013 NOK value.
# DECISION: index 2013 -> 2024, not 2021 -> 2024. SSB Table 08981 annual
#   averages: CPI 2013 = 95.9, CPI 2024 = 133.6; factor = 133.6 / 95.9
#   = 1.393118. 118,215 x 1.393118 = 164,687.42 -> 164,687 NOK (2024).
# Caution: the secondary route (Kenseth 2024 Table 1 read as a 2021 value, then
#   x cpi_2021_to_2024 = 1.1507, giving 136,030) silently loses eight years of
#   indexation. Kenseth's own uncertainty range for this row is exactly 0.8x
#   and 1.2x of 118,215, which proves the figure entered that paper as a raw
#   unindexed input carrying Bjornelv's 2013 basis, not as a 2021 value.
#   Do NOT apply cpi_2021_to_2024 to any Bjornelv-derived cost.
# SOURCE: Bjornelv is the base case; DMP 71,684 tested in sensitivity analysis.
# All uncertain cost parameters are sampled in PSA (base case).
c_terminal    <- 164687  # End-of-life cost, last month (NOK 2024)
# SOURCE: Bjornelv 2020 supplement Table S1 (Additional file 1; the local
#   DOCX of that supplement): last-month
#   all-healthcare costs, all Norwegian CRC decedents, mean 118,215,
#   SD 88,704, n 7,695, all in 2013 NOK per Bjornelv Methods. SE of the mean
#   = 88,704 / sqrt(7,695) = 1,011.2043 (2013 NOK); carried through the SAME
#   2013 -> 2024 chain as the mean: 1,011.2043 x 1.393118 = 1,408.7267 -> 1409
#   (relative SE preserved against the 164,687 base case: 1409 / 164,687
#   = 1011.2043 / 118,215 = 0.008554).
# DECISION: scope-matched published dispersion replaces the prior 20 percent SE
#   assumption. Price-basis correction applied 2026-07-28: the mean and the SE
#   now share one 2013-basis factor, so the two are internally consistent.
# Caution: gamma_params(mean, se) requires the SE of the MEAN, never the
#   patient-level SD; the SD-as-SE and direct-2024-SE-indexing routes were
#   rejected in the Item 01 decision brief and are guard-checked out.
# Caution: mean and SE must always be indexed by the same factor. The earlier
#   code labelled the SD "(2013 NOK)" while applying the 2021 factor 1.1507
#   to the mean; that mismatch is what this correction removes.
c_terminal_se <- 1409    # SOURCE: SE of mean, Bjornelv S1 SD/sqrt(n), 2013->2024 chain

# Intervention setup cost SE for PSA
c_intervention_setup_se <- round(0.20 * c_intervention_setup)  # SD = 0.20 x 516 = 103

# =============================================================================
# 6c. TREATMENT EFFECT WANING PARAMETERS
# =============================================================================
# SOURCE: Taylor 6-step waning framework.
# These define the waning scenarios tested in structural SA.
# Caution: HR_DFS=0.72 / HR_OS=0.63 (defined in this file, section "3. CLINICAL PARAMETERS  --  CHALLENGE TRIAL"; grep the symbol names, not a line number) are MARGINAL HRs from CHALLENGE; waning these toward 1.0 in structural SA is "less conservative than intended" under true individual-level effect loss because HR non-collapsibility means the marginal HR drifts toward 1.0 even when the conditional treatment effect persists (Jennings 2024, p.348).
# Treatment Waning: base case = constant HR (no waning); structural SA = step (yr 5) and linear (yr 3-10) waning. CHALLENGE published only marginal HRs; no IPD available, so conditional-HR waning per Jennings 2024 P5 workflow is not implementable in this thesis.
# SOURCE: Courneya 2025 (NEJM 2025;393:13-25) Fig 2A/2B (HR_DFS=0.72, HR_OS=0.63); Jennings 2024 (Value in Health 2024;27(3):347-355) p.348 (non-collapsibility, marginal-vs-conditional waning bias) and p.353 (P7: bias generalises to PSM regardless of model structure).
# Step waning: HR jumps to 1.0 (no effect) at this year
waning_step_year    <- 5   # Step waning cutoff (years)
# Linear waning: HR linearly approaches 1.0 from start to end year
# Begin the linear waning sensitivity at year 3.
waning_linear_start <- 3   # Linear waning onset (years)
# Complete the linear waning sensitivity at year 10.
waning_linear_end   <- 10  # Linear waning completion (years)

# =============================================================================
# 6d. DRUG DISCOUNT PARAMETERS
# =============================================================================
# ASSUMPTION: the 40 per cent manufacturer discount on the official list price
#   is an assumption, not an observed value. Norwegian confidential net prices
#   cannot be shared (Fagereng 2024), so no primary source states the magnitude
#   for this drug. For comparison, Danish competitive analogue tenders achieved
#   a mean saving of 44.1 per cent against official list prices (Ehlers 2022).
# Caution: neither cited paper prints 0.40 as a drug-discount magnitude, and the
#   earlier "SOURCE: 40% drug discount applied. Fagereng 2024, Ehlers 2022"
#   comment read as if they did. Fagereng 2024 prints no discount magnitude at
#   all; its only "40%" (p.2) is the share of member countries adhering to a
#   practice, and p.7 states "the confidential net-prices cannot be shared".
#   Ehlers 2022 prints a mean saving of 44.1 per cent for Danish competitive
#   analogue tenders against official list prices; the literal "0.40" in its
#   Table 1 is a MINIMUM expressed in PER CENT, not the fraction
#   0.40. Do not let that coincidence read as corroboration. The two papers are
#   used ONLY for confidentiality (Fagereng) and as the Danish comparator
#   (Ehlers).
# Caution: re-derivation from held sources is IMPOSSIBLE. Meld. St. 21 pp.70/92
#   name a flat percentage rebate as the standard form but print no number, and
#   the pembrolizumab metodevurdering redacts the magnitude in the source itself
#   at side 80/97. Retrieval, not analysis, is the only upgrade path.
# NOTE: 40 per cent is CONSERVATIVE against both held Danish figures, 44.1 per
#   cent (Ehlers 2022) and 48 per cent in 2023 (Morgante 2026).
drug_discount_rate     <- 0.40  # Assumption; see the cautions above
# Caution: this share is UNSOURCED. Evidence label: UNKNOWN. It appears in no
#   local primary source and no cited paper; the code's
#   own original comment already conceded "(approx)". It is retained because
#   base-case consumers = 0 and no better figure exists locally, and it is
#   disclosed here rather than left looking sourced.
drug_cost_share_pd     <- 0.50  # Drug share of palliative PD costs, UNSOURCED
# Declared sensitivity range for the unsourced share, widened from the implicit
# point estimate to a stated 0.30-0.70 band because an UNKNOWN-labelled input
# must not enter analysis as if it were a point value.
# Caution: no script consumes these two bounds yet. They are the greppable, single
#   source for the share's range; the sensitivity and limitations write-up must
#   read them rather than restate 0.30/0.70 as literals.
drug_cost_share_pd_low  <- 0.30
drug_cost_share_pd_high <- 0.70
# Caution: This sensitivity-only factor inherits the UNKNOWN drug-cost share and must not be presented as sourced evidence.
drug_discount_factor   <- 1 - (drug_cost_share_pd * drug_discount_rate)  # = 0.80
# Caution (materiality): base-case consumers
#   of these constants = 0, established by negative control across scripts 03,
#   04, 05, 06-psa, 07, 09 and 10. The only reads are sensitivity-only:
#   06b-structural-sa.R, inside `if (scn == "drug_discount_40pct")`, writing a
#   scenario-local c_progressed_scn; and build_pd_costs_table() in
#   08-export-tables.R (a table footnote). Anchored on the guard condition and the
#   function name, never on line numbers: this Caution is a negative-control claim
#   that must stay auditable after any edit above the reader.
#   This block is citation integrity, not results.

# =============================================================================
# 6e. POWSA COST BOUNDS
# =============================================================================
# Per Kenseth approach: costs varied as percentage of base case.
# Parameters defined here so the POWSA script does not carry literal bounds.
# DECISION (2026-07-28): surveillance is the ONE component in this set whose
#   largest constituent has unestablished provenance below its secondary
#   source. c_ct_session_unit (6,270) is 64.8 per cent of the 29,008.80 NOK
#   five-year bundle, and its Kenseth-level provenance is not established from
#   what is held locally, with no defensible Norwegian substitute available
#   (no imaging DRG; poliklinikkforskriften rates are partial refunds, not
#   costs). The Kenseth-convention +/-40 per cent band assumes an established
#   unit cost, so it understates the real uncertainty here. Widened to +/-60
#   per cent as the compensating control. Every other powsa bound in this
#   section keeps the Kenseth convention.
# Caution: this widening is a disclosure device on a DETERMINISTIC bound only.
#   It must NOT be mirrored into c_surveillance_early_se, which sets the PSA
#   gamma draw for the base case, nor into the base-case value itself.
# Caution: Keep the widened surveillance bound deterministic; do not mirror it into PSA SE or the base case.
powsa_c_surveillance_low <- c_surveillance_early * 0.40
powsa_c_surveillance_high <- c_surveillance_early * 1.60
# SOURCE: Kenseth et al. 2024 deterministic cost-bound convention.
powsa_c_exercise_low <- c_exercise_annual * 0.50
powsa_c_exercise_high <- c_exercise_annual * 1.50
powsa_c_progressed_low <- c_progressed_annual * 0.60
# SOURCE: Kenseth et al. 2024 deterministic cost-bound convention.
powsa_c_progressed_high <- c_progressed_annual * 1.40
powsa_c_terminal_low  <- c_terminal * 0.60            # 60% of base case
powsa_c_terminal_high <- c_terminal * 1.40            # 140% of base case


# =============================================================================
# 6f. TABLE-SPECIFIC CONSTANTS (appendix tables, not used in model calculations)
# =============================================================================
# These constants support the R-generated appendix tables (Tables 16-25).
# They are DISPLAY-ONLY: they do not enter any model calculation.
# The model uses the composite values defined above (c_session_healthcare,
# n_sessions_adherence, etc.). These constants decompose those composites
# for transparent reporting in the thesis appendix.

# --- Helfo A3 tariff components (Table 16: Exercise Intervention Costs) ---
# SOURCE: Forskrift FOR-2024-06-17-1184, amended by FOR-2025-06-23-1196.
#   Lovdata Kapittel II Takster, fradato 2025-07-01.
#   A3a: behandling hos fysioterapeut, inntil 20 min = 225 NOK (honorar).
#   A3b: tillegg per pabegynte 10 min = 102 NOK (honorar), repeatable 6x.
#   60-min session: A3a + 4 x A3b = 225 + 408 = 633 NOK.
# Caution: A3b is per 10 minutes, not per 15 minutes.
c_helfo_a3a   <- 225    # SOURCE: Helfo A3a start tariff, individual PT (rate in force from 2025-07-01)
c_helfo_a3b   <- 102    # SOURCE: Helfo A3b time tariff per 10 min (rate in force from 2025-07-01)
n_a3b_units   <- 4L     # Number of A3b units per 60-min session
# Caution: c_behaviour_contact is written as a literal above because it is needed
#   before these tariff parameters exist. Fail closed if the two ever drift.
stopifnot(identical(c_behaviour_contact, c_helfo_a3a + 1L * c_helfo_a3b))
# Validation: c_helfo_a3a + n_a3b_units * c_helfo_a3b must equal c_session_tariff_bare
# Caution: this guard binds the BARE tariff, not c_session_healthcare. Since
#   the base case is dmp_takst_adjustment x the bare tariff, so
#   comparing the tariff components against c_session_healthcare would fail.
stopifnot(c_helfo_a3a + n_a3b_units * c_helfo_a3b == c_session_tariff_bare)

# --- Adherence phase breakdown (Table 17: Adherence Schedule) ---
# SOURCE: Courneya et al. 2025 (NEJM 393:13-25), printed p.18. As above, the
#   session COUNTS are CONSTRUCTED from the Table 2 footnoted schedule and the
#   Results narrative; Table 2 itself prints no session count. Attendance rates
#   are the printed Table 2 percentages for SUPERVISED EXERCISE sessions.
# SOURCE (scheduled exercise sessions, footnotes dagger / double dagger /
#   section sign): Phase 1 = 12 mandatory + 12 recommended = 24; Phase 2 = 12;
#   Phase 3 = 24 monthly. Total 60 over 36 months.
# DECISION: this block is now EXERCISE-ONLY and reconciles exactly to
#   n_sessions_adherence = 28.92. The single derivation replaces the former
#   split in which the model used 62.16 while this table displayed 62.88.
# Caution: the superseded 26/26/52 gross counts reproduce nothing printed. The
#   printed schedule is 36/24/48 counting ALL contacts, or 24/12/24 counting
#   exercise sessions only; neither yields 26/26/52. The superseded phase-3
#   comment also called the schedule fortnightly, where the footnote prints
#   MONTHLY. The superseded rates 0.84 / 0.5631 / 0.5077 / 0.6046 and the
#   displayed 84%/56%/51% appear NOWHERE in Courneya: they were artefacts of
#   dividing correct numerators by those wrong denominators.
adherence_phase1_months   <- 6L      # Phase 1 duration (months)
adherence_phase2_months   <- 6L      # Phase 2 duration (months)
# SOURCE: Courneya et al. 2025 p. 18, phase-3 monthly schedule.
adherence_phase3_months   <- 24L     # Phase 3 duration (months)
gross_sessions_phase1     <- 24L     # SOURCE: 12 mandatory + 12 recommended (footnote dagger)
gross_sessions_phase2     <- 12L     # SOURCE: 12 co-located with support sessions (double dagger)
gross_sessions_phase3     <- 24L     # SOURCE: 24 monthly, co-located (section sign)
gross_sessions_total      <- gross_sessions_phase1 + gross_sessions_phase2 + gross_sessions_phase3  # = 60
# Adjusted exercise sessions per phase: printed rates x scheduled sessions.
adj_sessions_phase1       <- 11.88   # (12 x 0.79) + (12 x 0.20)
# SOURCE: Courneya et al. 2025 p. 18, 12 phase-2 exercise sessions at 54 percent attendance.
adj_sessions_phase2       <- 6.48    # 12 x 0.54
# SOURCE: Courneya et al. 2025 p. 18, four phase-3 six-month exercise-attendance blocks.
adj_sessions_phase3       <- 10.56   # 6 x (0.52 + 0.46 + 0.40 + 0.38)
# SOURCE: Courneya et al. 2025 p. 18 phase-specific exercise schedule and attendance rates.
adj_sessions_total_gross  <- adj_sessions_phase1 + adj_sessions_phase2 + adj_sessions_phase3  # = 28.92
# Implied adherence rates (for display; derived from adjusted sessions / gross)
# Caution: only phase 2 and phase 3 are single printed rates (54% and the pooled
#   44%). Phase 1 blends two printed rates over one schedule, so 0.495 is the
#   mean of 79% and 20%, not a figure Courneya prints.
adherence_phase1_rate     <- adj_sessions_phase1 / gross_sessions_phase1  # = 0.495
adherence_phase2_rate     <- adj_sessions_phase2 / gross_sessions_phase2  # = 0.54
adherence_phase3_rate     <- adj_sessions_phase3 / gross_sessions_phase3  # = 0.44
# Caution: This weighted adherence rate is model-derived and must not be attributed as a printed CHALLENGE value.
adherence_weighted_rate   <- adj_sessions_total_gross / gross_sessions_total  # = 0.482
# Caution: this block must stay tied to the model. Guard-check it.
stopifnot(isTRUE(all.equal(adj_sessions_total_gross, n_sessions_adherence)))

# --- SA session cost anchors (Table 18: SA Anchors) ---
# SOURCE: Structural SA grid (see R/06b-structural-sa.R)
#   against FOR-2024-06-17-1184 as amended by FOR-2025-06-23-1196,
#   plus separately dated primary Norwegian sources.
sa_session_c22a_group       <- 142    # SOURCE: Helfo C22a group PT, N=10 (lower bound)
# Caution: the name says "base" for historical reasons. This
#   is the BARE tariff and it is a TESTED ALTERNATIVE, not the base case. The
#   base case is c_session_healthcare (the DMP-doubled tariff).
sa_session_a3_base          <- c_session_tariff_bare  # SOURCE: 633, bare Helfo A3
sa_session_a7_specialist    <- 711    # SOURCE: Helfo A7a (275) + 4 x A7b (4 x 109 = 436)
sa_session_thorsen_ous      <- 756    # SOURCE: Thorsen OUS labour: 504 NOK/hr x 1.5 hr
sa_session_a8_manual        <- 904    # SOURCE: Helfo A8a (340) + 4 x A8b (4 x 141 = 564)
# Caution: the annual driftstilskudd is sourced, but the DIVISOR that amortises it
#   per session is an ASSUMPTION with no primary source. Its
#   unsupported precision is deliberately not restated here.
sa_session_driftstilskudd   <- 955    # SOURCE: A3 (633) + driftstilskudd amortised per session; 545,916 NOK/yr from 1 July 2024, FOR-2024-06-17-1184 section 9
# SOURCE: DMP v1.6, sheet 1.2 Forutsetninger, row 27, documents an x2
#   consultation-honorarium rule.
# the x2 rule is the BASE CASE, not a stress test. The name
#   "extreme" is historical; this constant now equals c_session_healthcare and is
#   retained so the anchor table can print the base case in its anchor range.
sa_session_extreme <- c_session_healthcare  # 1,266 NOK/session (base case)

# --- DFS surveillance schedule + unit costs (Table 19: DF State Costs) ---------
# GUIDELINE: Norwegian national CRC follow-up programme (Helsedirektoratet
#   handlingsprogram, ch.16 Oppfolgingsskjema). Guideline-faithful 5-year
#   post-curative-resection surveillance schedule.
# SOURCE: schedule counts -- Helsedir handlingsprogram pp.115-116;
#   unit costs -- DMP enhetskostnader v1.6 (CEA and 2024 DRG unit price) +
#   official 2026 DRG master list (906A cost weight 0.050 for the consultation,
#   710O cost weight 0.073 for the colonoscopy) + Kenseth et al. (2024) Table 1
#   CPI-adjusted 2021->2024 x1.1507 (CT only).
# Caution: 6,270 (CT session = 3 anatomical CTs) is a CPI-adjusted Kenseth value,
#   not a raw value, and its provenance below Kenseth is NOT established from
#   what is held locally (see its own block). The 145 NOK CEA input is DMP's
#   generic blood-draw tariff. The hospital consultation is DRG 906A, not the
#   846 NOK private-specialist tariff formerly used. Colonoscopy is
#   no longer Kenseth-derived: it is ISF DRG 710O.
# These components amortise to the shared surveillance input of 5,802 NOK/yr.
# A shared unit cost does not algebraically cancel when state occupancy differs.
n_cea_5yr          <- 8      # SOURCE: CEA blood tests over 5 yr: 1/6/12/18/24/30/36/60 mnd
n_ct_5yr           <- 3      # SOURCE: CT lungs/liver/abdomen: yr 1-3 (12/24/36 mnd)
n_colonoscopy_5yr  <- 1      # SOURCE: Colonoscopy: yr 5 only (60 mnd)
n_specialist_5yr   <- 2      # SOURCE: Specialist/clinical consultations over 5 yr
c_cea_unit         <- 145    # SOURCE: DMP v1.6 blood-draw ("Blodprove") tariff, 2024 NOK
# SOURCE: Kenseth et al. (2024) Table 1, three anatomical CT rows (raw 2021 NOK
#   1,617 / 1,617 / 2,214 = 5,448), CPI-adjusted x1.1507 -> 1,861 / 1,861 /
#   2,548 = 6,270 NOK (2024). That is the full extent of the traceable chain.
# Caution, stated plainly rather than implied: Kenseth's OWN unit-cost provenance
#   for this row is NOT ESTABLISHED from what is held locally. Joranger 2020,
#   its four held Online Resources and the 2015 predecessor contain no CT unit
#   cost in any currency, tested across three conversion routes with a
#   numerical scan. Joranger Online Resource 5 is not held locally, so the
#   honest finding is "absent from what we hold", not "does not exist".
# DECISION: RETAIN the value; do not substitute. Unlike the colonoscopy row
#   there is nothing defensible to move to. Norway has NO imaging DRG, and the
#   poliklinikkforskriften CT rates (46 to 484 NOK) are partial state refunds,
#   not costs, so they cannot serve as a unit-cost anchor. Inventing a
#   substitute would be worse than carrying a disclosed weak input.
# Caution: the compensating control for the unestablished provenance is the WIDER
#   deterministic sensitivity band on the surveillance component that carries
#   this cost (powsa_c_surveillance_low/high, above), not a changed base case.
#   The limitations disclosure belongs in the LaTeX text and is not coded here.
c_ct_session_unit  <- 6270   # SOURCE: CT session (3 anatomical CTs), Kenseth T1 x1.1507, 2024 NOK
# SOURCE: Norwegian ISF DRG 710O. helsedir-2026-isf-masterliste.xlsx, sheet
#   DRG-Masterliste, row 711: DRG code '710O', Norwegian description
#   'Koloskopi', English 'Colonoscopy, short therapy', kostnadsvekt 0.073.
#   Verified identical under openpyxl data_only TRUE and FALSE, so no formula
#   is involved. 0.073 x 52,248 = 3,814.10 -> 3,814 NOK.
# DECISION: this line combines two different documents, and that is stated
#   rather than left implicit. The WEIGHT 0.073 comes from the 2026 ISF
#   masterlist; the UNIT PRICE 52,248 comes from the DMP 2025 workbook
#   (DMP-2025-enhetskostnader-v1.6.xlsx, sheet '1.1 Enhetskostnader',
#   B46 = 2024, C46 = D46 = 52248). Mixing a 2026 weight with a 2024 unit
#   price is defensible (the weight is a relative resource measure, the price
#   is the price-year anchor) but it is not a single-source figure.
# DECISION: this replaces the Kenseth-derived 3,636. Kenseth's unit costs for
#   colonoscopy, CT, CEA and consultation are NOT TRACEABLE: Joranger 2020,
#   its four held Online Resources and the 2015 predecessor contain no such
#   unit cost in any currency, tested across three conversion routes with a
#   numerical scan. Honest limit: Joranger Online Resource 5 is not held
#   locally, so the finding is "absent from what we hold", not "does not exist".
# Caution: do NOT use DRG 712O (row 713, kostnadsvekt 0.064, 0.064 x 52,248
#   = 3,343.87). Its Norwegian description is literally 'Poliklinisk
#   sigmoidoskopi', a sigmoidoscopy, not a full colonoscopy. An earlier 3,344
#   candidate was wrong for exactly this reason.
c_colonoscopy_unit <- 3814   # SOURCE: Colonoscopy, ISF DRG 710O 0.073 x 52,248, 2024 NOK
drg_906a_cost_weight <- 0.050  # SOURCE: official 2026 DRG master list, row 906A
drg_unit_price_2024  <- 52248  # SOURCE: DMP v1.6, 1.1 Enhetskostnader row 46
c_specialist_unit <- drg_906a_cost_weight * drg_unit_price_2024  # 2,612.40 NOK
# Validation: guideline-faithful 5-year bundle, amortised over the intensive
#   follow-up window, must tie out to c_surveillance_early (5,802 NOK/yr).
#   8*145 + 3*6,270 + 1*3,814 + 2*2,612.40 = 29,008.80 / 5 -> 5,802.
# NOTE: named here so the assertion below and the \costSurveillanceFiveYear
#   macro read the same value; the chapter no longer hardcodes the bundle.
# Annualise one shared five-year surveillance bundle across both arms.
c_surveillance_5yr_bundle <- n_cea_5yr * c_cea_unit +
  n_ct_5yr * c_ct_session_unit +
  n_colonoscopy_5yr * c_colonoscopy_unit +
  n_specialist_5yr * c_specialist_unit
stopifnot(
  round(c_surveillance_5yr_bundle / surveillance_cutoff_years) ==
    c_surveillance_early
)

# --- CPI adjustment factors (Table 23: Deflator Methodology) ---
# SOURCE: SSB Table 08981 (Konsumprisindeks, arsgjennomsnitt, 2015=100).
#   2011: 93.3, 2013: 95.9, 2018: 108.4, 2021: 116.1, 2024: 133.6.
#   Local pin: reference-materials/originals/ssb/ssb-08981-cpi-annual-average-2011-2024-fetched-2026-07-27.json
# SOURCE: ISSUER: both ratios are SSB general CPI ratios, not DMP-specific factors.
# The audited values remain unchanged.
cpi_2021_to_2024 <- 1.1507   # SOURCE: SSB CPI ratio: 133.6 / 116.1
cpi_2018_to_2024 <- 1.2325   # SOURCE: SSB CPI ratio: 133.6 / 108.4
# NOTE: All Kenseth 2024 costs are in 2021 NOK, adjusted via cpi_2021_to_2024.
# Caution: Bjornelv 2020 costs do NOT flow through Kenseth's price year. Bjornelv
#   states 2013 NOK in its own Methods, and Kenseth reproduced 118,215 raw
#   (its 0.8x/1.2x range is exactly on that raw figure). The operative factor
#   for every Bjornelv-derived cost is therefore 133.6 / 95.9 = 1.393118,
#   NOT cpi_2021_to_2024. See the c_terminal block above.
#   cpi_2018_to_2024 is an audited transparency constant; no live model or
#   Table-23 expression currently consumes it.


# =============================================================================
# 7. SEVERITY (ABSOLUTE PROGNOSETAP) AND WILLINGNESS-TO-PAY
# =============================================================================
# GUIDELINE: Norwegian severity criterion via absolute shortfall (APT/AS):
#   AS = general-population remaining QALYs - patient (comparator) QALYs, mapped
#   to a Magnussen severity group -> weight -> reference NOK/QALY (DMP; Magnussen
#   et al. 2015). The severity-informed reference is COMPUTED, not a constant.
# DECISION: the superseded 9.5 had no DMP source; it was an
#   internal estimate. The current-norm APT method derives AS from the
#   model's own control-arm undiscounted QALYs against the current DMP norm.

# DMP quantified opportunity cost (no severity weighting).
# SOURCE: DMP prioritisation-criteria page (opportunity-cost benchmark, NOK/QALY).
opportunity_cost_nok <- 275000

# GUIDELINE: Magnussen et al. (2015) severity band ladder adopted by DMP --
#   AS range -> group -> weight -> severity-informed reference NOK/QALY.
# Caution: bands are closed intervals [lower, upper]; upper bounds carry a .9999
#   tail (and Inf at the top) so no AS value falls between two bands and every
#   AS maps to exactly one group. Lookup runs on the ROUNDED AS (2 dp).
magnussen_bands <- data.frame(
  # SOURCE: Magnussen et al. 2015 Table 3, p. 48, six severity groups.
  group         = 1:6,
  # SOURCE: Magnussen et al. 2015 Table 3, p. 48, absolute-shortfall bands.
  lower         = c(0, 4, 8, 12, 16, 20),
  # Caution: Use closed upper tails so each rounded absolute-shortfall value matches exactly one band.
  upper         = c(3.9999, 7.9999, 11.9999, 15.9999, 19.9999, Inf),
  # SOURCE: Magnussen et al. 2015 Table 3, p. 48, severity weights.
  weight        = c(1.0, 1.4, 1.8, 2.2, 2.6, 3.0),
  # SOURCE: Magnussen et al. 2015 Table 3, p. 48; 275,000 NOK multiplied by each severity weight.
  reference_nok = c(275000, 385000, 495000, 605000, 715000, 825000)
)

# General-population remaining (undiscounted) QALYs at cohort age, DMP New norm.
# SOURCE: DMP-2025-tools-for-severity-calculation-and-age-adjustment-july.xlsx,
#   sheet "Expected remaining QALYs", New norm, age 61 = 20.8 (snapshot in
#   data/raw/dmp-eq5d5l-norms-current.csv; workbook SHA-256
#   10b136df1a8f783f99fe5eef2e672b23842a5408467829b07e300519b45104cb).
.dmp_norms <- read.csv("data/raw/dmp-eq5d5l-norms-current.csv")
severity_genpop_qaly <- .dmp_norms$remaining_qalys_newnorm[.dmp_norms$age == cohort_age]
# Caution: cohort_age must resolve to exactly one finite norm row; a missing or
#   duplicated age would silently drop or double the lookup.
stopifnot(length(severity_genpop_qaly) == 1, is.finite(severity_genpop_qaly))

#' Derive DMP severity (absolute shortfall) class from QALYs
#' GUIDELINE: workbook method ROUND(QALYsA - PA, 2) -> Magnussen band (DMP;
#'   Magnussen et al. 2015). NEVER derives severity from a WTP value.
derive_severity <- function(qalys_a, comparator_pa, bands) {
  absolute_shortfall <- round(qalys_a - comparator_pa, 2)
  # Caution: boundary-safe closed-interval match; error (never guess) if no band.
  hit <- bands[absolute_shortfall >= bands$lower & absolute_shortfall <= bands$upper, ]
  if (nrow(hit) != 1L) {
    stop(sprintf("derive_severity: AS = %.2f matched %d Magnussen bands (expected 1)",
                 absolute_shortfall, nrow(hit)))
  }
  list(
    general_population_qalys = qalys_a,
    comparator_qalys         = comparator_pa,
    absolute_shortfall       = absolute_shortfall,
    group                    = hit$group,
    weight                   = hit$weight,
    reference_nok            = hit$reference_nok
  )
}

# wtp_threshold is COMPUTED after step 5 (see 04-costs-qalys.R severity block);
#   it is the severity-informed reference, not a fixed constant. Do NOT set it here.
wtp_low  <- opportunity_cost_nok   # SOURCE: NOK/QALY  --  DMP opportunity-cost benchmark (no severity weight)
wtp_high <- 825000                 # SOURCE: NOK/QALY  --  Magnussen ladder top (group 6), scenario only

# For CEAC: range of WTP thresholds to plot
wtp_range <- seq(0, 2000000, by = 1000)   # NOK per QALY (1K steps for precise crossover WTP)


# =============================================================================
# 8. LIFE TABLE PARAMETERS
# =============================================================================
# Norwegian general population mortality (Statistics Norway / SSB)
# File: data/raw/norway_life_table.csv
# SOURCE: SSB Table 07902 (2025 data).

life_table_path <- "data/raw/norway_life_table.csv"   # SSB 2025, populated


# =============================================================================
# 9. PSA PARAMETER DISTRIBUTIONS
# =============================================================================
# Defined here for reference; sampling happens in 06-psa.R
#
# Convention:
#   HRs          -> log-normal (parameter: log HR, SE of log HR)
#   Utilities    -> beta distribution (mean, SE)
#   Costs        -> gamma distribution (mean, SE)
#   Probabilities -> beta distribution (mean, SE)

#' Derive Beta Distribution Parameters from Mean and SE
#'
#' Converts mean and standard error to alpha/beta parameters for the
#' beta distribution using the method of moments.
#' Requires: 0 < mean < 1, se > 0, and var < mean*(1-mean).
#'
#' @param mean Numeric. Expected value in (0, 1).
#' @param se   Numeric. Standard error (positive).
#' @return Named list with alpha and beta parameters.
beta_params <- function(mean, se) {
  if (is.na(mean) || is.na(se)) {
    stop("beta_params: mean and se must not be NA. ",
         "Got mean=", mean, ", se=", se)
  }
  if (mean <= 0 || mean >= 1) {
    stop("beta_params: mean must be in (0, 1). Got mean=", mean)
  }
  if (se <= 0) {
    stop("beta_params: se must be positive. Got se=", se)
  }
  # GUIDELINE: Use variance equal to squared SE in Beta method-of-moments parameterisation (Briggs et al. 2006).
  var <- se^2
  # GUIDELINE: Enforce the Bernoulli upper variance bound before fitting a Beta distribution (Briggs et al. 2006).
  max_var <- mean * (1 - mean)
  if (var >= max_var) {
    stop("beta_params: variance (", round(var, 6),
         ") >= mean*(1-mean) (", round(max_var, 6),
         "). SE too large for method of moments. ",
         "Reduce SE or use a different parameterisation.")
  }
  # GUIDELINE: Derive the shared Beta concentration term by method of moments (Briggs et al. 2006).
  common <- (max_var / var) - 1
  # GUIDELINE: Derive Beta alpha from the mean and shared concentration term (Briggs et al. 2006).
  alpha <- mean * common
  # GUIDELINE: Derive Beta beta from the complementary mean and shared concentration term (Briggs et al. 2006).
  beta_val  <- (1 - mean) * common
  list(alpha = alpha, beta = beta_val)
}

#' Derive Gamma Distribution Parameters from Mean and SE
#'
#' Converts mean and standard error to shape/rate parameters for the
#' gamma distribution using the method of moments.
#' Requires: mean > 0, se > 0.
#'
#' @param mean Numeric. Expected value (positive).
#' @param se   Numeric. Standard error (positive).
#' @return Named list with shape and rate parameters.
gamma_params <- function(mean, se) {
  if (is.na(mean) || is.na(se)) {
    stop("gamma_params: mean and se must not be NA. ",
         "Got mean=", mean, ", se=", se)
  }
  if (mean <= 0) {
    stop("gamma_params: mean must be positive. Got mean=", mean)
  }
  if (se <= 0) {
    stop("gamma_params: se must be positive. Got se=", se)
  }
  # GUIDELINE: Use variance equal to squared SE in Gamma method-of-moments parameterisation (Briggs et al. 2006).
  var   <- se^2
  # GUIDELINE: Derive Gamma shape as mean squared over variance (Briggs et al. 2006).
  shape <- mean^2 / var
  # GUIDELINE: Express the Gamma method-of-moments scale as R rate equal to mean over variance (Briggs et al. 2006).
  rate  <- mean / var
  list(shape = shape, rate = rate)
}


# =============================================================================
# 10. MODEL ASSERTIONS (assertHE-style checks)
# =============================================================================
# Basic model validation checks. These run after model calculations to
# verify internal consistency. When assertHE is installed, these can be
# replaced with assertHE::assert_* functions.
#
# Purpose: catch errors early (negative costs, probabilities > 1, etc.)
# before they propagate through the model and produce misleading results.

#' Validate PSM Trace Probabilities
#'
#' Checks that state occupancy probabilities are valid:
#'   - All probabilities in [0, 1]
#'   - Probabilities sum to 1 (within tolerance) at each cycle
#'   - No negative values in progressed state
#'
#' @param trace Data frame. PSM trace from build_psm_trace().
#' @param tolerance Numeric. Tolerance for sum-to-1 check (default 1e-6).
#' @return TRUE if all checks pass. Stops with error otherwise.
assert_psm_trace <- function(trace, tolerance = 1e-6) {
  required_cols <- c("p_dfs", "p_prog", "p_dead")
  stopifnot(all(required_cols %in% names(trace)))

  # Check for NAs first  --  NAs in a PSM trace indicate computation failure
  # and must not be silently ignored via na.rm = TRUE
  for (col in required_cols) {
    if (any(is.na(trace[[col]]))) {
      stop("Assertion failed: NA values in ", col,
           ". Count = ", sum(is.na(trace[[col]])),
           ". NAs in a PSM trace indicate a computation failure upstream.")
    }
  }

  # All probabilities in [0, 1]
  # na.rm is intentionally omitted. The NA check above guarantees no NAs
  # reach this point. Using na.rm = TRUE would contradict the intent of
  # failing on NA values and could silently pass corrupt data if the
  # function is later refactored.
  for (col in required_cols) {
    if (any(trace[[col]] < -tolerance)) {
      stop("Assertion failed: ", col, " contains negative values. ",
           "Min = ", min(trace[[col]]))
    }
    if (any(trace[[col]] > 1 + tolerance)) {
      stop("Assertion failed: ", col, " exceeds 1. ",
           "Max = ", max(trace[[col]]))
    }
  }

  # Sum to 1
  row_sums <- trace$p_dfs + trace$p_prog + trace$p_dead
  if (any(abs(row_sums - 1) > tolerance)) {
    worst <- which.max(abs(row_sums - 1))
    stop("Assertion failed: state probabilities do not sum to 1. ",
         "Worst deviation at cycle ", trace$cycle[worst],
         ": sum = ", row_sums[worst])
  }

  TRUE
}

#' Validate Cost and QALY Outputs
#'
#' Checks that calculated costs and QALYs are sensible:
#'   - Costs are non-negative
#'   - QALYs per cycle in [0, cycle_length]
#'   - No NA values in computed columns
#'
#' cycle_length is an explicit parameter (not accessed from globalenv())
#' to make the function testable in isolation and remove hidden global
#' state dependency.
#'
#' @param results Data frame. Output from calculate_costs_qalys().
#' @param cycle_length Numeric. Cycle length in years. If NULL, the
#'   QALY-per-cycle upper bound check is skipped (with a warning).
#' @return TRUE if all checks pass. Stops with error otherwise.
assert_costs_qalys <- function(results, cycle_length = NULL) {
  # No NAs in key columns  --  check FIRST (same pattern as assert_psm_trace)
  key_cols <- c("qalys_disc", "costs_disc", "ly_disc")
  for (col in key_cols) {
    if (col %in% names(results) && any(is.na(results[[col]]))) {
      stop("Assertion failed: NA values in ", col,
           ". Count = ", sum(is.na(results[[col]])))
    }
  }

  # Costs must be non-negative
  if (any(results$costs_disc < 0)) {
    stop("Assertion failed: negative discounted costs detected. ",
         "Min = ", min(results$costs_disc))
  }

  # QALYs per cycle must be non-negative
  if (any(results$qalys_disc < 0)) {
    stop("Assertion failed: negative discounted QALYs detected. ",
         "Min = ", min(results$qalys_disc))
  }

  # QALYs per cycle cannot exceed theoretical maximum (cycle_length * 1.0)
  # Utility <= 1 and discount weight <= 1, so max QALY per cycle = cycle_length
  # Uses explicit cycle_length parameter (not globalenv())
  if (!is.null(cycle_length)) {
    if (any(results$qalys_disc > cycle_length + 1e-6)) {
      stop("Assertion failed: discounted QALYs per cycle exceed theoretical ",
           "maximum (", cycle_length, "). ",
           "Max = ", max(results$qalys_disc))
    }
  } else {
    warning("assert_costs_qalys: cycle_length not provided. ",
            "Skipping QALY upper bound check.")
  }

  TRUE
}


# =============================================================================
# 11. AGE-DEPENDENT UTILITY MULTIPLIER FUNCTION
# =============================================================================
# NICE/NOMA: Mandatory for lifetime horizons. Ara & Brazier (2010).
# Age-adjustment prevents overestimation of QALYs at
# older ages where general population utility is lower.
#
# Returns a vector of multipliers for each model cycle:
#   multiplier(t) = u_genpop(age_0 + t) / u_genpop(age_0)
# where u_genpop is interpolated from age_utility_norms.

#' Compute Age-Dependent Utility Multipliers
#'
#' Generates a per-cycle vector of multiplicative utility adjustments
#' based on general population EQ-5D norms declining with age.
#' Applied as: u_adjusted(t) = u_health_state * multiplier(t)
#'
#' Uses linear interpolation between age-band midpoints from the
#' norms table. If norms are NA, returns a vector of 1s (no adjustment).
#'
#' @param entry_age Numeric. Cohort entry age.
#' @param n_cycles Integer. Number of model cycles.
#' @param cycle_length Numeric. Cycle length in years.
#' @param norms Data frame. Age-utility norms with columns: age_lower,
#'   age_upper, eq5d_mean.
#' @param enabled Logical. If FALSE, returns all 1s (no adjustment).
#' @return Numeric vector of length n_cycles (one per cycle, NOT n_cycles + 1).
#'   Covers cycles 1 through n_cycles (excludes cycle 0). This aligns with
#'   the trapezoidal rule in calculate_costs_qalys(), which produces n-1 = n_cycles
#'   cycle values from n_cycles + 1 time-point observations. The slicing
#'   age_utility_multipliers[1:(n-1)] in calculate_costs_qalys() maps exactly
#'   to this vector.
get_age_utility_multipliers <- function(entry_age, n_cycles, cycle_length,
                                         norms = age_utility_norms,
                                         enabled = age_utility_enabled) {
  # Caution: When norms are not yet populated (all NA), return no adjustment
  if (!enabled || all(is.na(norms$eq5d_mean))) {
    return(rep(1.0, n_cycles))
  }

  # Midpoint ages for interpolation
  # Interpolate the selected Garratt age-band norms at band midpoints.
  norms$age_mid <- (norms$age_lower + norms$age_upper) / 2

  # Remove NA rows for interpolation
  valid <- norms[!is.na(norms$eq5d_mean), ]
  if (nrow(valid) < 2) {
    warning("get_age_utility_multipliers: fewer than 2 valid norm entries. ",
            "Returning no adjustment.")
    return(rep(1.0, n_cycles))
  }

  # Interpolation function (linear, clamped to range)
  # Caution: Clamp interpolation outside the observed age-band range instead of extrapolating utilities.
  interp_fn <- approxfun(valid$age_mid, valid$eq5d_mean, rule = 2)

  # Baseline utility at cohort entry age
  u_baseline <- interp_fn(entry_age)

  # GUIDELINE: ISPOR: Ara & Brazier (2010) multiplicative adjustment
  cycle_ages <- entry_age + (1:n_cycles) * cycle_length
  u_at_age <- interp_fn(cycle_ages)

  # GUIDELINE: Apply multiplicative age adjustment using the general-population utility ratio (DMP 2026).
  multipliers <- u_at_age / u_baseline

  # NOTE: The Garratt et al. (2022) Norwegian EQ-5D-3L crosswalk population
  # norms (Table 6, p.521-522) are NOT strictly monotonic with age. The
  # 18-29 group has the highest mean (0.820), the 30-39 group rises to
  # 0.839, the 40-49 group drops to 0.788, and the 50-59 and 60-69 groups
  # rise again before declining at 70-plus. Garratt et al. note in the
  # Discussion (p.523): "EQ-5D index and EQ VAS scores did not consistently
  # decrease with age, rather there was a slight increase for the second
  # age group and two age groups from 50 to 69 years." This non-monotonic
  # plateau is therefore a documented feature of the Norwegian source
  # data, NOT a coding error or transcription error in this model.
  # As a result, the multiplicative adjustment can produce small (>1.0)
  # multipliers when the cohort entry age sits in a relatively low band
  # (e.g., 40-49) and the cycle age moves into a relatively high band
  # (e.g., 50-69). These overshoots are bounded, expected, and should
  # NOT be flagged as anomalies. The threshold below tolerates the
  # documented Garratt 2022 plateau behaviour while still warning on
  # large unexpected overshoots that would indicate a data error.
  if (any(multipliers > 1.10, na.rm = TRUE)) {
    warning("get_age_utility_multipliers: some multipliers exceed 1.10. ",
            "Garratt et al. (2022) Table 6 (p.521-522) report a documented ",
            "non-monotonic age plateau between 50 and 69 years, so values ",
            "slightly above 1.0 are expected and acceptable. Multipliers ",
            "above 1.10 are unusually large and warrant inspection of the ",
            "age_utility_norms data frame for transcription errors.")
  }

  multipliers
}


# =============================================================================
# END OF PARAMETERS
# =============================================================================
message("Parameters loaded. Time horizon: ", time_horizon, " years | ",
        n_cycles, " monthly cycles | n_psa = ", n_psa)
message("Cohort age: ", cohort_age, " | HR_DFS = ", HR_DFS,
        " (SE_log = ", round(se_log_HR_DFS, 4), ")",
        " | HR_OS = ", HR_OS,
        " (SE_log = ", round(se_log_HR_OS, 4), ")")
# Monotonicity tolerance for PSM trace internal validation.
# Used by R/03-build-psm.R to guard against floating-point noise when
# testing that state proportions are non-increasing (DFS) or
# non-decreasing (cumulative death probability) across cycles.
# Also read by R/08-export-tables.R via validation_checks.rds so the
# monotonicity table caption and footnote cite the same value the
# upstream test used (defined once here; no duplicate literal values).
monotonicity_tolerance <- 1e-10

message("Discount: stepped 4%/3%/2% (Rundskriv R-109) | Opportunity-cost benchmark: ",
        format(opportunity_cost_nok, big.mark = ","),
        " NOK/QALY (severity-informed reference computed after the model runs)")
message("All parameters populated (utilities, costs, life table, survival).")
