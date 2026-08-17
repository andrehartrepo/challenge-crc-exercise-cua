# =============================================================================
# 03-build-psm.R
# Build Partitioned Survival Model (PSM)
# Construct state occupancy over time from survival functions
#
# States:
#   S1: Disease-Free Survival (DFS)
#   S2: Progressed / Recurred
#   S3: Dead (absorbing)
#
# PSM state probabilities at each cycle t:
#   P(DFS, t)  = S_DFS(t)
#   P(Prog, t) = S_OS(t) - S_DFS(t)   [must be >= 0]
#   P(Dead, t) = 1 - S_OS(t)
#
# General population mortality cap (assumption A4):
#   S_OS(t) <= S_genpop(t) at all t. CRC patients cannot have better
#   survival than the Norwegian general population.
#   Source: SSB Table 07902 (confirmed)
#
# Input:  data/processed/survival_fits_dfs.rds
#         data/processed/survival_fits_os.rds
#         00-parameters.R (best_dist_dfs, best_dist_os, cohort_age)
#
# Output: data/processed/psm_trace.rds
#         output/figures/psm_state_occupancy.pdf
#
# Packages: flexsurv
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   PSM construction methodology: Woods BS, Sideris E, Palmer S, Latimer N,
#     Soares M (2020). "Partitioned survival and state transition models
#     for healthcare decision making in oncology: where are we now?"
#     Value in Health 23(12):1613-1621. DOI 10.1016/j.jval.2020.08.2094.
#     PMID: 33248517. Defines the canonical PSM construction:
#     P_DF(t) = S_DFS(t); P_PD(t) = S_OS(t) - S_DFS(t); P_D(t) = 1 - S_OS(t).
#     Section "Partitioned Survival Models" provides the constraint
#     S_OS(t) >= S_DFS(t) and the recommendation to enforce a max(0, .)
#     floor when independent extrapolation crosses the boundary.
#   NICE DSU TSD 19 (Woods et al. 2017). "Partitioned survival analysis
#     for decision modelling in health care: a critical review." NICE
#     Decision Support Unit Technical Support Document 19. Section 3.2:
#     PSM constraint enforcement and curve crossing diagnostics.
#   General population mortality cap: Beca J, Husereau D, Chan KKW,
#     Mittmann N, Hoch JS (2025). "Lifetime extrapolation of clinical
#     trial survival data: best practice and pitfalls." Value in Health
#     28(1):37-46. DOI 10.1016/j.jval.2024.09.012. The "hazard_max"
#     approach (per-cycle hazard floor) and the "pmin" approach (per-cycle
#     survival ceiling) are both NICE TSD 21 (Rutherford et al. 2020)
#     practice; both implementations are exposed via the mortality_method
#     parameter for sensitivity testing.
#   Treatment effect application S_int(t) = S_ctrl(t)^HR (constant Cox HR
#     applied to a parametric baseline survival): NICE DSU TSD 14 (Latimer
#     2013), Section 4.4. Exact for PH families (exp, weibull, gompertz),
#     approximation for AFT families (lnorm, llogis, gengamma); the AFT
#     approximation caveat is disclosed in 02-fit-survival.R and the
#     thesis Methods chapter.
#
# Adaptations:
#   1. Two arms via build_psm_trace_from_ctrl(): control arm fitted from
#      reconstructed IPD, intervention arm derived by S_int(t) = S_ctrl(t)^HR
#      with endpoint-specific marginal HR draws in PSA.
#      This decouples baseline distribution selection from
#      treatment effect mechanism.
#   2. Configurable mortality_method ("hazard_max" or "pmin") parameter
#      enables structural SA scenario testing alternative implementations
#      of the general population mortality cap.
#   3. Inline assertions via assert_psm_trace() (sum to 1, monotonicity,
#      non-negativity) catch silent computation failures before they
#      propagate to costs/QALYs.
#   4. PSM constraint violation warnings expose count and first violation
#      time for transparency in the appendix; the max(0, S_OS - S_DFS)
#      floor ensures the model remains internally consistent even when
#      independent extrapolation produces curve crossing in the deep tail.
#
# Academic citations:
#   Woods BS, Sideris E, Palmer S, Latimer N, Soares M (2020).
#     DOI 10.1016/j.jval.2020.08.2094. PMID: 33248517.
#   Woods B, Sideris E, Palmer S, Latimer N, Soares M (2017). NICE DSU
#     Technical Support Document 19.
#   Latimer NR (2013). NICE DSU Technical Support Document 14.
#   Beca J et al. (2025). DOI 10.1016/j.jval.2024.09.012.
# =============================================================================


#' Load Norwegian General Population Survival
#'
#' Base case: age-specific, both-sex SSB qx. Male/female columns are used
#' only when a sex-specific subgroup is explicitly requested.
#' The life table provides annual mortality probabilities by single year
#' of age. The function interpolates to monthly cycle resolution.
#'
#' Assumption A4: Norwegian population mortality as OS floor.
#' Source: Statistics Norway (SSB), Table 07902
#'
#' @param life_table_path Character. Path to life table CSV.
#' @param entry_age Integer. Cohort entry age. No default  --  must be
#'   passed explicitly by the caller to avoid hidden global dependencies.
#' @param n_cycles Integer. Number of model cycles.
#' @param cycle_length Numeric. Cycle length in years.
#' @param sex Character. "both" (default, reads 'qx' column),
#'   "male" (reads 'qx_male'), or "female" (reads 'qx_female').
#   Enables sex-specific subgroup scenarios (Dimension C).
#' @return Numeric vector of general population survival probabilities,
#'   length n_cycles + 1 (including time 0).
load_genpop_survival <- function(life_table_path, entry_age,
                                  n_cycles, cycle_length,
                                  # Use age-specific both-sex background mortality in the base case; reserve sex-specific mortality for subgroup analyses.
                                  sex = "both") {
  # Evaluate the PSM on the configured cycle grid over the lifetime horizon.
  times <- seq(0, n_cycles * cycle_length, by = cycle_length)
  n_total <- length(times)

  if (!file.exists(life_table_path)) {
    # Use warning() instead of message() so it appears in warnings()
    # after model execution. This is a methodological degradation (no mortality
    # cap = assumption A4 violated), not just an informational message.
    warning("Life table not found at: ", life_table_path, ". ",
            "Returning uncapped survival (S_genpop = 1 for all t). ",
            "TODO: Download SSB Table 07902. ",
            "Model runs WITHOUT general population mortality cap (assumption A4).",
            call. = FALSE)
    return(rep(1, n_total))
  }

  lt <- read.csv(life_table_path, stringsAsFactors = FALSE)

  # Use the both-sex qx column for the base case and sex-specific columns only for subgroup analyses.
  qx_col <- switch(sex,
    "both"   = "qx",
    "male"   = "qx_male",
    "female" = "qx_female",
    stop("load_genpop_survival: sex must be 'both', 'male', or 'female'. Got: ", sex)
  )

  if (!"age" %in% names(lt)) {
    stop("Life table must have column 'age'. Found: ",
         paste(names(lt), collapse = ", "))
  }
  if (!qx_col %in% names(lt)) {
    stop("Life table must have column '", qx_col, "' for sex='", sex,
         "'. Found: ", paste(names(lt), collapse = ", "))
  }

  s_genpop <- numeric(n_total)
  s_genpop[1] <- 1

  for (i in 2:n_total) {
    # GUIDELINE: Apply age-specific Norwegian background mortality over the extrapolation horizon (Siebert et al. 2012)
    age_at_t <- entry_age + times[i]
    # Caution: Ages above the life-table maximum reuse the final qx row; they do not trigger the missing-age fallback below.
    age_floor <- min(floor(age_at_t), max(lt$age))
    qx_annual <- lt[[qx_col]][lt$age == age_floor]

    if (length(qx_annual) == 0) {
      # Age beyond table: assume death
      # Caution: A missing age row forces survival to zero, so life-table gaps become terminal events.
      qx_annual <- 1
    }

    # p_cycle = 1 - (1 - qx)^cycle_length
    qx_cycle <- 1 - (1 - qx_annual)^cycle_length
    s_genpop[i] <- s_genpop[i - 1] * (1 - qx_cycle)
  }

  s_genpop
}


#' Apply General Population Mortality Cap to Survival Curves
#'
#' Adjusts DFS and OS survival curves so that disease hazards
#' are at least as high as general population hazards at every
#' time point. Extracted to eliminate triplication across
#' build_psm_trace(), build_psm_trace_from_ctrl(), and
#' build_psm_trace_waning().
#'
#' Two methods available:
#'   "hazard_max"  --  ensures h_disease >= h_genpop at every t
#'     (NICE DSU TSD 21, Rutherford et al. 2020  --  supplementary;
#'      DMP/NOMA has no equivalent guidance on mortality cap method).
#'   "pmin"  --  simple cap: S(t) = min(S_model(t), S_genpop(t))
#'
#' Assumption A4: Norwegian population mortality as OS floor.
#'
#' @param s_dfs Numeric vector. DFS survival probabilities.
#' @param s_os Numeric vector. OS survival probabilities.
#' @param s_genpop Numeric vector. General population survival.
#' @param method Character. "hazard_max" (default) or "pmin".
#' @return Named list with adjusted s_dfs and s_os.
#' Apply an imposed structural statistical-cure point
#'
#' Structural sensitivity scenario only. Before t*, hazards behave exactly as
#' the current hazard_max floor. At and after t*, excess hazard is 0 and the
#' Norwegian background hazard operates alone, for DFS and OS in both arms.
#'
#' # GUIDELINE: Norwegian reference-model practice (Joranger 2020 Fig. 1;
#' #   Kenseth 2024 life-table floor). ISPOR structural-uncertainty workflow
#' #   (Bilcke 2011): run each alternative structure as a complete scenario.
#' # Imposed, not fitted; excess multiplier exactly 1.0.
#' # Caution: callers choose EITHER this helper OR apply_mortality_cap(), never
#' #   both. Applying both would impose the background floor twice.
#' @return Named list with adjusted s_dfs, s_os, and diagnostics.
apply_structural_cure_point <- function(s_dfs, s_os, s_genpop, times,
                                        t_star_years) {
  # Caution: fail closed on every input defect. A silent coercion or default here
  #   would corrupt a scenario whose base-case twin is compared byte-for-byte.
  if (is.null(s_genpop)) {
    stop("apply_structural_cure_point: s_genpop is required (the imposed ",
         "cure point is defined only against background mortality).")
  }
  if (!is.numeric(s_dfs) || !is.numeric(s_os) || !is.numeric(s_genpop) ||
      !is.numeric(times)) {
    stop("apply_structural_cure_point: s_dfs, s_os, s_genpop and times must ",
         "all be numeric vectors.")
  }
  n_t <- length(times)
  if (n_t < 2L || length(s_dfs) != n_t || length(s_os) != n_t ||
      length(s_genpop) != n_t) {
    stop("apply_structural_cure_point: s_dfs, s_os and s_genpop must each ",
         "have length(times) = ", n_t, ". Got ", length(s_dfs), ", ",
         length(s_os), ", ", length(s_genpop), ".")
  }
  if (!all(is.finite(s_dfs)) || !all(is.finite(s_os)) ||
      !all(is.finite(s_genpop)) || !all(is.finite(times))) {
    stop("apply_structural_cure_point: non-finite value in the survival ",
         "inputs or the time grid.")
  }
  tol <- 1e-8
  if (any(diff(times) <= 0)) {
    stop("apply_structural_cure_point: times must be strictly increasing.")
  }
  if (any(diff(s_dfs) > tol) || any(diff(s_os) > tol) ||
      any(diff(s_genpop) > tol)) {
    stop("apply_structural_cure_point: survival inputs must be ",
         "non-increasing in time.")
  }
  if (!is.numeric(t_star_years) || length(t_star_years) != 1L ||
      !is.finite(t_star_years)) {
    stop("apply_structural_cure_point: t_star_years must be a single finite ",
         "numeric value.")
  }
  if (!any(abs(times - t_star_years) < tol)) {
    stop("apply_structural_cure_point: t_star_years = ", t_star_years,
         " is not a point on the model time grid.")
  }

  # Interval hazards, computed once. Interval i runs from times[i] to
  # times[i + 1], so its LEFT endpoint decides which regime applies.
  # GUIDELINE: Convert interval survival to hazards before imposing the background-mortality floor (Rutherford et al. 2020)
  h_dfs_model <- -diff(log(pmax(s_dfs,    1e-15)))
  h_os_model  <- -diff(log(pmax(s_os,     1e-15)))
  # GUIDELINE: Express general-population mortality on the same interval-hazard scale as model survival (Rutherford et al. 2020)
  h_genpop    <- -diff(log(pmax(s_genpop, 1e-15)))
  # Apply the imposed cure regime from the selected cycle-grid point onward.
  post_tstar  <- times[-n_t] >= (t_star_years - tol)

  # before t* this reproduces the hazard_max floor exactly;
  #   at and after t* the excess hazard is 0, so the background hazard stands
  #   alone (post-t* excess multiplier exactly 1.0, both endpoints, both arms).
  h_dfs_adj <- ifelse(post_tstar, h_genpop, pmax(h_dfs_model, h_genpop))
  # Use background mortality alone at and after the imposed cure point; retain the larger hazard before it.
  h_os_adj  <- ifelse(post_tstar, h_genpop, pmax(h_os_model,  h_genpop))

  # GUIDELINE: Reconstruct adjusted survival from the cumulative interval-hazard path (Rutherford et al. 2020)
  s_dfs_adj <- c(1, cumprod(exp(-h_dfs_adj)))
  s_os_adj  <- c(1, cumprod(exp(-h_os_adj)))
  # Caution: enforce S_DFS <= S_OS ONCE, after reconstruction, exactly as
  #   apply_mortality_cap() does. Enforcing it per interval would re-impose
  #   the background floor a second time.
  # GUIDELINE: Enforce DFS no greater than OS after independent survival adjustment (Woods et al. 2020)
  s_dfs_adj <- pmin(s_dfs_adj, s_os_adj)

  max_excess <- if (any(post_tstar)) {
    max(abs(c(h_dfs_adj[post_tstar] - h_genpop[post_tstar],
              h_os_adj[post_tstar]  - h_genpop[post_tstar])))
  } else {
    0
  }

  list(
    s_dfs = s_dfs_adj,
    s_os  = s_os_adj,
    post_tstar_intervals = sum(post_tstar),
    max_abs_post_tstar_excess_hazard = max_excess,
    monotone_survival_pass = all(diff(s_dfs_adj) <= tol) &&
      all(diff(s_os_adj) <= tol) &&
      all(s_dfs_adj <= s_os_adj + tol),
    background_interaction_pass = all(h_dfs_adj >= h_genpop - tol) &&
      all(h_os_adj >= h_genpop - tol)
  )
}

# GUIDELINE: Incorporate general-population mortality into standard parametric extrapolation (Rutherford et al. 2020)
apply_mortality_cap <- function(s_dfs, s_os, s_genpop, method = "hazard_max") {
  # GUIDELINE: Use a per-cycle hazard floor so model mortality cannot fall below general-population mortality (Rutherford et al. 2020)
  if (method == "hazard_max") {
    # GUIDELINE: NICE DSU TSD 21: hazard_max ensures disease hazard >= genpop hazard
    h_os_model <- -diff(log(pmax(s_os, 1e-15)))
    h_genpop   <- -diff(log(pmax(s_genpop, 1e-15)))
    # GUIDELINE: Set each OS interval hazard to at least the general-population hazard (Rutherford et al. 2020)
    h_os_adj <- pmax(h_os_model, h_genpop)
    # GUIDELINE: Reconstruct capped OS from the floored interval hazards (Rutherford et al. 2020)
    s_os <- c(1, cumprod(exp(-h_os_adj)))

    # GUIDELINE: Convert DFS survival to interval hazards before applying the mortality floor (Rutherford et al. 2020)
    h_dfs_model <- -diff(log(pmax(s_dfs, 1e-15)))
    # GUIDELINE: Set each DFS interval hazard to at least the general-population hazard (Rutherford et al. 2020)
    h_dfs_adj   <- pmax(h_dfs_model, h_genpop)
    # GUIDELINE: Reconstruct capped DFS from the floored interval hazards (Rutherford et al. 2020)
    s_dfs <- c(1, cumprod(exp(-h_dfs_adj)))
    # DFS cannot exceed OS
    # GUIDELINE: Enforce DFS no greater than OS after independent extrapolation (Woods et al. 2020)
    s_dfs <- pmin(s_dfs, s_os)
  } else {
    # pmin approach (retained as structural SA option)
    # GUIDELINE: Test direct survival capping as the alternative mortality-floor structure (Rutherford et al. 2020)
    s_os  <- pmin(s_os,  s_genpop)
    # GUIDELINE: Preserve DFS no greater than OS under the direct survival-cap alternative (Woods et al. 2020)
    s_dfs <- pmin(s_dfs, s_os)
  }

  list(s_dfs = s_dfs, s_os = s_os)
}


#' Build PSM Trace with General Population Mortality Cap
#'
#' Constructs state occupancy proportions over the model time horizon.
#' Applies the general population mortality cap (assumption A4):
#' S_OS(t) = min(S_model(t), S_genpop(t)).
#' S_DFS(t) is also capped since DFS <= OS <= S_genpop.
#'
#' @param fit_dfs flexsurvreg object for DFS.
#' @param fit_os flexsurvreg object for OS.
#' @param n_cycles Integer. Number of model cycles.
#' @param cycle_length Numeric. Cycle length in years.
#' @param s_genpop Numeric vector. General population survival (from
#'   load_genpop_survival()). If NULL, no cap applied.
#' @param arm_label Character. Arm identifier ("Exercise"/"Standard Care").
#' @return Data frame with columns: arm, cycle, time, p_dfs, p_prog, p_dead.
  # Assumption A4: mortality cap using general population survival.
  # hazard_max is chosen as default because it
  # ensures h_disease >= h_genpop at every t (NICE DSU TSD 21, supplementary;
  # DMP silent on method). pmin retained as structural SA alternative.
  # arm_label defaults to "Standard Care"
  # to match the standard usage convention in the model scripts.
build_psm_trace <- function(fit_dfs, fit_os,
                             n_cycles, cycle_length,
                             s_genpop = NULL,
                             arm_label = "Standard Care",
                             # GUIDELINE: Default to the per-cycle hazard floor and retain direct survival capping for structural sensitivity analysis (Rutherford et al. 2020)
                             mortality_method = "hazard_max",
                             # Keep the imposed cure point scenario-only; NULL preserves the base case.
                             cure_t_star_years = NULL,
                             validate = TRUE) {
  if (!inherits(fit_dfs, "flexsurvreg")) {
    stop("build_psm_trace: fit_dfs must be a flexsurvreg object. ",
         "Got class: ", paste(class(fit_dfs), collapse = ", "))
  }
  if (!inherits(fit_os, "flexsurvreg")) {
    stop("build_psm_trace: fit_os must be a flexsurvreg object. ",
         "Got class: ", paste(class(fit_os), collapse = ", "))
  }

  # Evaluate state occupancy on the configured monthly cycle grid over the lifetime horizon.
  times <- seq(0, n_cycles * cycle_length, by = cycle_length)

  # GUIDELINE: Evaluate fitted DFS survival at every model cycle endpoint (Jackson 2016)
  summary_dfs <- summary(fit_dfs, t = times, type = "survival", ci = FALSE)
  # GUIDELINE: Evaluate fitted OS survival at every model cycle endpoint (Jackson 2016)
  summary_os  <- summary(fit_os,  t = times, type = "survival", ci = FALSE)

  if (!is.list(summary_dfs) || length(summary_dfs) < 1 ||
      is.null(summary_dfs[[1]]$est)) {
    stop("build_psm_trace: flexsurv summary for DFS returned unexpected ",
         "structure. Expected list with $est component.")
  }
  if (!is.list(summary_os) || length(summary_os) < 1 ||
      is.null(summary_os[[1]]$est)) {
    stop("build_psm_trace: flexsurv summary for OS returned unexpected ",
         "structure. Expected list with $est component.")
  }

  s_dfs <- summary_dfs[[1]]$est
  s_os  <- summary_os[[1]]$est

  if (length(s_dfs) != length(times)) {
    stop("build_psm_trace: DFS summary length (", length(s_dfs),
         ") != expected time points (", length(times), ").")
  }
  if (length(s_os) != length(times)) {
    stop("build_psm_trace: OS summary length (", length(s_os),
         ") != expected time points (", length(times), ").")
  }

  # Apply general population mortality adjustment (assumption A4)
  # Uses shared apply_mortality_cap() helper.
  # See apply_mortality_cap() docstring for method details.
  if (!is.null(s_genpop)) {
    # exactly one hazard construction. NULL keeps the
    # current path byte-identical; non-NULL replaces the cap, never follows it.
    if (is.null(cure_t_star_years)) {
      capped <- apply_mortality_cap(s_dfs, s_os, s_genpop, method = mortality_method)
    } else {
      # Caution: the imposed scenario is defined only on the hazard_max floor.
      if (!identical(mortality_method, "hazard_max")) {
        stop("cure_t_star_years requires mortality_method = 'hazard_max'")
      }
      capped <- apply_structural_cure_point(s_dfs, s_os, s_genpop,
                                            times = times,
                                            t_star_years = cure_t_star_years)
    }
    s_dfs <- capped$s_dfs
    s_os  <- capped$s_os
  }

  # Warn when S_DFS > S_OS (PSM constraint violation)
  crossing <- which(s_dfs > s_os + 1e-8)
  if (length(crossing) > 0) {
    warning("PSM constraint violation: S_DFS > S_OS at ",
            length(crossing), " time points (arm: ", arm_label, "). ",
            "First violation at t = ", round(times[crossing[1]], 4), " years. ",
            "P(Prog) set to 0 at these points. ",
            "This may indicate parametric model misspecification.",
            call. = FALSE)
  }

  # Enforce PSM constraint: S_OS(t) >= S_DFS(t) at all t
  # GUIDELINE: Set disease-free occupancy equal to the DFS survival curve (Woods et al. 2020)
  p_dfs  <- s_dfs
  # GUIDELINE: Derive progressed occupancy as OS minus DFS and floor curve crossings at zero (Woods et al. 2020)
  p_prog <- pmax(s_os - s_dfs, 0)   # Progressed state
  # GUIDELINE: Derive dead occupancy as one minus OS (Woods et al. 2020)
  p_dead <- 1 - s_os

  result <- data.frame(
    arm    = arm_label,
    cycle  = seq_along(times) - 1,
    time   = times,
    p_dfs  = p_dfs,
    p_prog = p_prog,
    p_dead = p_dead
  )

  # Validate trace if enabled (default TRUE for base case,
  # set FALSE in PSA inner loop for performance)
  if (validate) assert_psm_trace(result)

  result
}

#' Build Intervention Arm PSM Trace from Control Arm + HR
#'
#' Standard PSM approach for trials reporting HRs.
#' Fits the control arm parametric models, then derives the intervention
#' arm by applying HR to the control arm's cumulative hazard:
#'   H_int(t) = H_ctrl(t) * HR   (constant HR)
#'   S_int(t) = S_ctrl(t)^HR     (equivalent under proportional hazards)
#'
#' This ensures a single, consistent treatment effect mechanism across
#' deterministic and PSA analyses.
#'
#' @param fit_dfs_ctrl flexsurvreg object for control arm DFS.
#' @param fit_os_ctrl  flexsurvreg object for control arm OS.
#' @param hr_dfs Numeric. HR for DFS (intervention vs control).
#' @param hr_os  Numeric. HR for OS (intervention vs control).
#' @param n_cycles Integer. Number of model cycles.
#' @param cycle_length Numeric. Cycle length in years.
#' @param s_genpop Numeric vector. General population survival.
#' @param arm_label Character. Arm identifier.
#' @param mortality_method Character. "hazard_max" or "pmin".
#' @return Data frame with columns: arm, cycle, time, p_dfs, p_prog, p_dead.
build_psm_trace_from_ctrl <- function(fit_dfs_ctrl, fit_os_ctrl,
                                       hr_dfs, hr_os,
                                       n_cycles, cycle_length,
                                       s_genpop = NULL,
                                       arm_label = "Exercise",
                                       # GUIDELINE: Default the intervention trace to the same per-cycle mortality floor as the control trace (Rutherford et al. 2020)
                                       mortality_method = "hazard_max",
                                       # Keep the intervention cure-point path scenario-only; NULL preserves the base case.
                                       cure_t_star_years = NULL,
                                       validate = TRUE) {
  if (!inherits(fit_dfs_ctrl, "flexsurvreg")) {
    stop("build_psm_trace_from_ctrl: fit_dfs_ctrl must be a flexsurvreg object.")
  }
  if (!inherits(fit_os_ctrl, "flexsurvreg")) {
    stop("build_psm_trace_from_ctrl: fit_os_ctrl must be a flexsurvreg object.")
  }
  if (hr_dfs <= 0) stop("build_psm_trace_from_ctrl: hr_dfs must be positive.")
  if (hr_os  <= 0) stop("build_psm_trace_from_ctrl: hr_os must be positive.")

  # Evaluate the HR-derived intervention trace on the same cycle grid as the control trace.
  times <- seq(0, n_cycles * cycle_length, by = cycle_length)

  # GUIDELINE: Evaluate control-arm DFS survival before applying the reported treatment hazard ratio (Latimer 2013)
  s_dfs_ctrl <- summary(fit_dfs_ctrl, t = times, type = "survival",
                         ci = FALSE)[[1]]$est
  # GUIDELINE: Evaluate control-arm OS survival before applying the reported treatment hazard ratio (Latimer 2013)
  s_os_ctrl  <- summary(fit_os_ctrl,  t = times, type = "survival",
                         ci = FALSE)[[1]]$est

  # Derive intervention arm: S_int(t) = S_ctrl(t)^HR
  # NICE TSD 14: This is equivalent to multiplying the cumulative hazard by HR
  # under proportional hazards: H_int(t) = HR * H_ctrl(t)
  # NOTE: Exact for PH distributions (exp, weibull, gompertz).
  # For non-PH distributions (lnorm, llogis, gengamma), this is an approximation.
  # GUIDELINE: The CHALLENGE trial HRs are from a Cox PH model, so this is standard practice
  # GUIDELINE: (Latimer 2013, NICE TSD 14 Section 4.4). See 02-fit-survival.R for full note.
  s_dfs <- pmax(s_dfs_ctrl, 1e-15)^hr_dfs
  s_os  <- pmax(s_os_ctrl,  1e-15)^hr_os

  # Apply general population mortality adjustment (assumption A4)
  # Uses shared apply_mortality_cap() helper.
  if (!is.null(s_genpop)) {
    # exactly one hazard construction. NULL keeps the
    # current path byte-identical; non-NULL replaces the cap, never follows it.
    if (is.null(cure_t_star_years)) {
      capped <- apply_mortality_cap(s_dfs, s_os, s_genpop, method = mortality_method)
    } else {
      # Caution: the imposed scenario is defined only on the hazard_max floor.
      if (!identical(mortality_method, "hazard_max")) {
        stop("cure_t_star_years requires mortality_method = 'hazard_max'")
      }
      capped <- apply_structural_cure_point(s_dfs, s_os, s_genpop,
                                            times = times,
                                            t_star_years = cure_t_star_years)
    }
    s_dfs <- capped$s_dfs
    s_os  <- capped$s_os
  }

  # Warn on PSM constraint violation
  crossing <- which(s_dfs > s_os + 1e-8)
  if (length(crossing) > 0) {
    warning("PSM constraint violation: S_DFS > S_OS at ",
            length(crossing), " time points (arm: ", arm_label, "). ",
            "First violation at t = ", round(times[crossing[1]], 4), " years.",
            call. = FALSE)
  }

  # GUIDELINE: Set disease-free occupancy equal to the DFS survival curve (Woods et al. 2020)
  p_dfs  <- s_dfs
  # GUIDELINE: Derive progressed occupancy as OS minus DFS and floor curve crossings at zero (Woods et al. 2020)
  p_prog <- pmax(s_os - s_dfs, 0)
  # GUIDELINE: Derive dead occupancy as one minus OS (Woods et al. 2020)
  p_dead <- 1 - s_os

  result <- data.frame(
    arm    = arm_label,
    cycle  = seq_along(times) - 1,
    time   = times,
    p_dfs  = p_dfs,
    p_prog = p_prog,
    p_dead = p_dead
  )

  if (validate) assert_psm_trace(result)

  result
}


#' Build PSM Trace with Treatment Effect Waning
#'
#' Constructs state occupancy under a time-varying hazard ratio.
#' Used for structural sensitivity analysis (assumption A2).
#'
#' When treatment effect wanes, the intervention arm survival is computed
#' by applying a time-dependent HR to the control arm's cumulative hazard:
#'   H_int(t) = integral_0^t [h_ctrl(s) * HR(s)] ds
#'   S_int(t) = exp(-H_int(t))
#'
#' This preserves the proportional hazards structure while allowing
#' the HR to change over time per the waning schedule.
#'
#' @param fit_dfs_ctrl flexsurvreg object for control arm DFS.
#' @param fit_os_ctrl  flexsurvreg object for control arm OS.
#' @param hr_dfs_original Numeric. Original DFS HR from CHALLENGE trial.
#' @param hr_os_original  Numeric. Original OS HR from CHALLENGE trial.
#' @param waning_scenario Character. Waning scenario for
#'   apply_treatment_waning().
#' @param n_cycles Integer. Number of model cycles.
#' @param cycle_length Numeric. Cycle length in years.
#' @param s_genpop Numeric vector. General population survival.
#' @param arm_label Character. Arm identifier.
#' @return Data frame with columns: arm, cycle, time, p_dfs, p_prog, p_dead.
build_psm_trace_waning <- function(fit_dfs_ctrl, fit_os_ctrl,
                                    hr_dfs_original, hr_os_original,
                                    # Use a constant hazard ratio in the base case and reserve waning for structural scenarios.
                                    waning_scenario = "constant",
                                    n_cycles, cycle_length,
                                    s_genpop = NULL,
                                    arm_label = "Exercise",
                                    # GUIDELINE: Keep the mortality-floor method constant when testing treatment-effect waning (Rutherford et al. 2020)
                                    mortality_method = "hazard_max",
                                    # Keep the cure-point structure off by default when evaluating waning scenarios.
                                    cure_t_star_years = NULL,
                                    validate = TRUE) {
  # Treatment waning scenarios for structural SA
  # GUIDELINE: ISPOR-SMDM TF7  --  validation must cover all model paths
  times <- seq(0, n_cycles * cycle_length, by = cycle_length)

  # Control arm survival (baseline)
  # GUIDELINE: Evaluate control-arm DFS survival before applying time-varying treatment effects (Latimer 2013)
  s_dfs_ctrl <- summary(fit_dfs_ctrl, t = times, type = "survival",
                         ci = FALSE)[[1]]$est
  # GUIDELINE: Evaluate control-arm OS survival before applying time-varying treatment effects (Latimer 2013)
  s_os_ctrl  <- summary(fit_os_ctrl,  t = times, type = "survival",
                         ci = FALSE)[[1]]$est

  # Time-dependent HRs from waning function (defined in 06b-structural-sa.R)
  # Apply the configured DFS waning only in structural sensitivity analysis.
  hr_dfs_t <- apply_treatment_waning(hr_dfs_original, times, waning_scenario)
  # Apply the configured OS waning only in structural sensitivity analysis.
  hr_os_t  <- apply_treatment_waning(hr_os_original,  times, waning_scenario)

  # Implement treatment-effect waning on the hazard scale rather than exponentiating survival by a time-varying hazard ratio.
  h_dfs_ctrl <- -log(pmax(s_dfs_ctrl, 1e-15))
  h_os_ctrl  <- -log(pmax(s_os_ctrl,  1e-15))

  # Numerical integration of h_ctrl(s) * HR(s) via trapezoidal rule
  # on the incremental hazard between time points.
  n_t <- length(times)
  h_dfs_int <- numeric(n_t)
  h_os_int  <- numeric(n_t)
  h_dfs_int[1] <- 0
  h_os_int[1]  <- 0

  for (i in 2:n_t) {
    # Incremental control hazard for this interval
    # Apply waning to interval hazard increments so treatment-effect durability is tested on the hazard scale.
    delta_h_dfs <- h_dfs_ctrl[i] - h_dfs_ctrl[i - 1]
    delta_h_os  <- h_os_ctrl[i]  - h_os_ctrl[i - 1]

    avg_hr_dfs <- (hr_dfs_t[i - 1] + hr_dfs_t[i]) / 2
    avg_hr_os  <- (hr_os_t[i - 1]  + hr_os_t[i])  / 2

    # Accumulate waned DFS effects through the interval-hazard path.
    h_dfs_int[i] <- h_dfs_int[i - 1] + delta_h_dfs * avg_hr_dfs
    # Accumulate waned OS effects through the interval-hazard path.
    h_os_int[i]  <- h_os_int[i - 1]  + delta_h_os  * avg_hr_os
  }

  # GUIDELINE: Apply treatment-effect waning on the hazard scale before reconstructing survival (Trigg et al. 2024)
  s_dfs <- exp(-h_dfs_int)
  s_os  <- exp(-h_os_int)

  # Apply general population mortality adjustment (assumption A4)
  # Uses shared apply_mortality_cap() helper.
  # Waning function uses the same mortality_method
  # as build_psm_trace() and build_psm_trace_from_ctrl(), preventing
  # confounding in structural SA (waning vs mortality method).
  # Apply the Norwegian general-population mortality floor in every model path when life-table survival is available.
  if (!is.null(s_genpop)) {
    # exactly one hazard construction. NULL keeps the
    # current path byte-identical; non-NULL replaces the cap, never follows it.
    if (is.null(cure_t_star_years)) {
      capped <- apply_mortality_cap(s_dfs, s_os, s_genpop, method = mortality_method)
    } else {
      # Caution: the imposed scenario is defined only on the hazard_max floor.
      if (!identical(mortality_method, "hazard_max")) {
        stop("cure_t_star_years requires mortality_method = 'hazard_max'")
      }
      capped <- apply_structural_cure_point(s_dfs, s_os, s_genpop,
                                            times = times,
                                            t_star_years = cure_t_star_years)
    }
    s_dfs <- capped$s_dfs
    s_os  <- capped$s_os
  }

  # PSM state probabilities
  # GUIDELINE: Set disease-free occupancy equal to the DFS survival curve (Woods et al. 2020)
  p_dfs  <- s_dfs
  # GUIDELINE: Derive progressed occupancy as OS minus DFS and floor curve crossings at zero (Woods et al. 2020)
  p_prog <- pmax(s_os - s_dfs, 0)
  # GUIDELINE: Derive dead occupancy as one minus OS (Woods et al. 2020)
  p_dead <- 1 - s_os

  result <- data.frame(
    arm    = arm_label,
    cycle  = seq_along(times) - 1,
    time   = times,
    p_dfs  = p_dfs,
    p_prog = p_prog,
    p_dead = p_dead
  )

  # Validate trace if enabled,
  # consistent with build_psm_trace() and build_psm_trace_from_ctrl().
  # Waning scenarios involve numerical integration of time-varying hazards
  # and are MORE susceptible to numerical errors  --  validation is critical.
  # ISPOR-SMDM TF7  --  internal validity checks on all model paths
  if (validate) assert_psm_trace(result)

  result
}


# --- Placeholder execution ---------------------------------------------------
# Uses control-arm-fit + HR approach consistently.
# Reads control arm fits, derives intervention arm via HR.
if (!identical(Sys.getenv("TESTTHAT"), "true") &&
    file.exists("data/processed/survival_fits_dfs_ctrl.rds") &&
    file.exists("data/processed/survival_fits_os_ctrl.rds") &&
    !is.null(best_dist_dfs) && !is.null(best_dist_os)) {

  fits_dfs_ctrl <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
  fits_os_ctrl  <- readRDS("data/processed/survival_fits_os_ctrl.rds")

  # Load general population survival for mortality adjustment (assumption A4)
  s_genpop <- load_genpop_survival(
    life_table_path = life_table_path,
    entry_age       = cohort_age,
    n_cycles        = n_cycles,
    cycle_length    = cycle_length
  )

  # Control arm: fit directly from parametric model
  trace_ctrl <- build_psm_trace(
    fit_dfs      = fits_dfs_ctrl[[best_dist_dfs]],
    fit_os       = fits_os_ctrl[[best_dist_os]],
    n_cycles     = n_cycles,
    cycle_length = cycle_length,
    s_genpop     = s_genpop,
    arm_label    = "Standard Care"
  )

  # Exercise arm derived from Standard Care arm + HR
  trace_int  <- build_psm_trace_from_ctrl(
    fit_dfs_ctrl = fits_dfs_ctrl[[best_dist_dfs]],
    fit_os_ctrl  = fits_os_ctrl[[best_dist_os]],
    hr_dfs       = HR_DFS,
    hr_os        = HR_OS,
    n_cycles     = n_cycles,
    cycle_length = cycle_length,
    s_genpop     = s_genpop,
    arm_label    = "Exercise"
  )

  # Redundant assert_psm_trace() calls are not needed here.
  # build_psm_trace() and build_psm_trace_from_ctrl() both call
  # assert_psm_trace() internally when validate=TRUE (default).
  # Double-asserting wastes cycles and obscures the validation contract.

  psm_trace <- rbind(trace_int, trace_ctrl)
  saveRDS(psm_trace, "data/processed/psm_trace.rds")

  # Validation checks  --  BOTH arms (intervention arm
  # has HR-modified survival and mortality cap, making monotonicity violations
  # more likely). Saves to RDS for reproducible verification.
  validation_checks <- list()
  monotonicity_arms <- c("Standard Care", "Exercise")
  for (arm_name in monotonicity_arms) {
    arm_data <- psm_trace[psm_trace$arm == arm_name, ]
    arm_key <- gsub(" ", "_", tolower(arm_name))
    s_alive <- 1 - arm_data$p_dead  # S_OS(t) = p_dfs + p_prog = 1 - p_dead
    validation_checks[[paste0("dfs_monotonic_", arm_key)]] <-
      all(diff(arm_data$p_dfs) <= monotonicity_tolerance)
    validation_checks[[paste0("os_monotonic_", arm_key)]] <-
      all(diff(s_alive) <= monotonicity_tolerance)
    validation_checks[[paste0("dead_monotonic_", arm_key)]] <-
      all(diff(arm_data$p_dead) >= -monotonicity_tolerance)
  }
  validation_checks$monotonicity_tolerance <- monotonicity_tolerance
  validation_checks$monotonicity_arms <- monotonicity_arms
  validation_checks$state_sum_range <- range(
    psm_trace$p_dfs + psm_trace$p_prog + psm_trace$p_dead
  )
  validation_checks$min_pd <- min(psm_trace$p_prog)
  # Aggregate pass/fail verdicts for internal validation table (Table A.1).
  # state_sum_all_arms: state occupancy sums to 1 at every cycle in every arm.
  # non_negative_all_arms: p_prog >= 0 at every cycle in every arm
  # (p_dfs and p_dead are non-negative by construction; p_prog = 1 - p_dfs - p_dead
  # can incur floating-point noise of magnitude ~tolerance).
  # Both reuse monotonicity_tolerance (defined once in 00-parameters.R).
  validation_checks$state_sum_all_arms <-
    all(sapply(monotonicity_arms, function(arm_name) {
      arm_data <- psm_trace[psm_trace$arm == arm_name, ]
      state_sum <- arm_data$p_dfs + arm_data$p_prog + arm_data$p_dead
      all(abs(state_sum - 1) <= monotonicity_tolerance)
    }))
  validation_checks$non_negative_all_arms <-
    all(sapply(monotonicity_arms, function(arm_name) {
      arm_data <- psm_trace[psm_trace$arm == arm_name, ]
      all(arm_data$p_prog >= -monotonicity_tolerance)
    }))

  # 5-year DFS endpoint reproduction (internal validation)
  # Compare fitted parametric model prediction at t=5yr against published trial endpoints
  # Published: DFS 73.9% control, 80.3% exercise (Courneya et al. 2025, NEJM)
  # Caution: psm_trace row 1 = cycle 0 = time 0. Row N = cycle (N-1) = time (N-1)*cycle_length.
  # For t=5yr with monthly cycles: cycle 60 = row 61 (not row 60).
  cycle_at_5yr <- round(5 / cycle_length)  # cycle number for t=5 years
  row_at_5yr <- cycle_at_5yr + 1           # R is 1-indexed; row 1 = cycle 0
  for (arm_name in c("Standard Care", "Exercise")) {
    arm_data <- psm_trace[psm_trace$arm == arm_name, ]
    arm_key <- gsub(" ", "_", tolower(arm_name))
    if (nrow(arm_data) >= row_at_5yr) {
      dfs_5yr <- arm_data$p_dfs[row_at_5yr]
      validation_checks[[paste0("dfs_5yr_", arm_key)]] <-
        round(dfs_5yr * 100, 1)
    }
  }
  # Published reference values for gap calculation
  # SOURCE: Courneya et al. 2025, Table 3, 5-year DFS estimate for standard care.
  validation_checks$dfs_5yr_published_standard_care <- 73.9
  # SOURCE: Courneya et al. 2025, Table 3, 5-year DFS estimate for exercise.
  validation_checks$dfs_5yr_published_exercise <- 80.3

  saveRDS(validation_checks, "data/processed/validation_checks.rds")
  message("Validation checks saved: data/processed/validation_checks.rds")

  message("PSM trace built. ", nrow(psm_trace), " rows saved.")
  message("Architecture: control-arm fit + HR -> intervention arm.")
  message("General population mortality: ",
          ifelse(all(s_genpop == 1), "NOT APPLIED (life table missing)",
                 "APPLIED via hazard_max method (SSB Table 07902)"))
  message("Check PSM constraint violation warnings above (if any).")

} else if (file.exists("data/processed/survival_fits_dfs.rds") &&
           file.exists("data/processed/survival_fits_os.rds") &&
           !is.null(best_dist_dfs) && !is.null(best_dist_os)) {
  # Legacy fallback: old format with both arms in one file
  message("NOTE: Using legacy survival fits format. Re-run 02-fit-survival.R",
          " for C1-compliant control-arm-only fits.")
  fits_dfs <- readRDS("data/processed/survival_fits_dfs.rds")
  fits_os  <- readRDS("data/processed/survival_fits_os.rds")

  s_genpop <- load_genpop_survival(
    life_table_path = life_table_path,
    entry_age       = cohort_age,
    n_cycles        = n_cycles,
    cycle_length    = cycle_length
  )

  trace_ctrl <- build_psm_trace(
    fit_dfs      = fits_dfs$control[[best_dist_dfs]],
    fit_os       = fits_os$control[[best_dist_os]],
    n_cycles     = n_cycles,
    cycle_length = cycle_length,
    s_genpop     = s_genpop,
    arm_label    = "Standard Care"
  )

  trace_int <- build_psm_trace_from_ctrl(
    fit_dfs_ctrl = fits_dfs$control[[best_dist_dfs]],
    fit_os_ctrl  = fits_os$control[[best_dist_os]],
    hr_dfs       = HR_DFS,
    hr_os        = HR_OS,
    n_cycles     = n_cycles,
    cycle_length = cycle_length,
    s_genpop     = s_genpop,
    arm_label    = "Exercise"
  )

  # Redundant assert_psm_trace() calls are not needed on
  # the legacy path. Internal validate=TRUE is sufficient.

  psm_trace <- rbind(trace_int, trace_ctrl)
  saveRDS(psm_trace, "data/processed/psm_trace.rds")
  message("PSM trace built (legacy format). ", nrow(psm_trace), " rows.")

} else {
  message("TODO: Complete survival fitting (02-fit-survival.R) and set",
          " best_dist_dfs / best_dist_os in 00-parameters.R")
}
