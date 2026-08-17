# =============================================================================
# 06-psa.R
# Probabilistic Sensitivity Analysis  --  BASE CASE ANALYSIS
#
# PSA is the base case analysis.
# Deterministic ICER (05-run-model.R) is supplementary.
#
# This script produces:
#   - Expected (mean) ICER from PSA iterations
#   - P(cost-effective) at the applicable severity-informed WTP reference
#   - Expected incremental net benefit (INB)
#   - One-way probabilistic SA (OWPSA)
#   - PSA convergence diagnostics (INMB CI method per Hatswell 2018; ICER stability check as secondary)
#   - Expected loss curves (via dampack)
#
# Input:  data/processed/survival_fits_dfs.rds
#         data/processed/survival_fits_os.rds
#         00-parameters.R (distribution parameters, n_psa)
#
# Output: data/processed/psa_results.rds
#         output/tables/psa_base_case_summary.csv
#         output/tables/owpsa_results.csv
#
# Packages: flexsurv, MASS, mgcv, ggplot2, dampack
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   PSA framework and parameter sampling: Briggs AH, Weinstein MC, Fenwick
#     EAL, Karnon J, Sculpher MJ, Paltiel AD (2012). "Model parameter
#     estimation and uncertainty analysis: a report of the ISPOR-SMDM
#     Modeling Good Research Practices Task Force-6." Value in Health
#     15(6):835-842. DOI 10.1016/j.jval.2012.04.014. PMID 22999133.
#     Defines the canonical PSA workflow: assign distributions to all
#     uncertain parameters, draw correlated samples, propagate through
#     the model, summarise as expected outputs and decision uncertainty.
#   Endpoint-specific normal/MVN sampling and dependence sensitivity: Briggs,
#     Claxton, Sculpher (2006), Decision Modelling for Health Economic
#     Evaluation, Chapter 4; Venables and Ripley (2002), 4th ed.
#   Survival uncertainty propagation via flexsurv vcov: standard
#     practice in flexsurvreg-based PSAs (Jackson 2016, JSS 70(8)). The
#     Full vcov-based draws replace an earlier ad-hoc parametric resampling approach
#     with full vcov-based draws to preserve parameter correlation.
#   PSA convergence diagnostics:
#     - PRIMARY (INMB CI method): Hatswell AJ, Bullement A, Briggs A,
#       Paulden M, Stevenson MD (2018). "Probabilistic sensitivity
#       analysis in cost-effectiveness models: determining model
#       convergence in cohort models." PharmacoEconomics 36(12):1421-1426.
#       DOI 10.1007/s40273-018-0697-3. PMID 30051221. Key Points (p.1421):
#       "the running of the model until the 95% confidence interval for
#       the incremental net monetary benefit does not include zero."
#       The accompanying critique of cumulative-mean ICER plots as
#       "subjective" appears at p.1424. The correct
#       attribution: ICER stability is a secondary diagnostic; INMB CI
#       is the primary Hatswell method.
#     - SECONDARY (ICER stability check): retained as a transparency
#       diagnostic per general practice; not endorsed by Hatswell.
#   EVPI computation (two-strategy formula): Briggs, Claxton, Sculpher
#     (2006), Chapter 6, Section 6.2.1.
#   EVPPI estimation via GAM regression: Strong M, Oakley JE, Brennan A
#     (2014). "Estimating multiparameter partial expected value of perfect
#     information from a probabilistic sensitivity analysis sample: a
#     nonparametric regression approach." Medical Decision Making
#     34(3):311-326. DOI 10.1177/0272989X13505910. PMID 24246566. Uses
#     mgcv::gam(); fitted to the PSA sample with input parameters as
#     predictors of the incremental net benefit.
#   Population EVPI with technology horizon discounting: Briggs, Claxton,
#     Sculpher (2006), p.176, Section 6.2.1 (annuity formulation);
#     Fenwick E, Steuten L, Knies S, Ghabri S, Basu A, Murray JF, Koffijberg
#     HE, Strong M, Sanders Schmidler GD, Rothery C (2020). "Value of
#     information analysis for research decisions: an introduction. Report
#     1 of the ISPOR Value of Information Analysis Emerging Good Practices
#     Task Force." Value in Health 23(2):139-150. p.144-145, Box 4.
#     The compute_pop_evpi_horizon() function implements the discounted
#     annuity formula at horizons 5, 10, 20 years (technology horizon decision: T=10 base case, sensitivity 5/20).
# SOURCE: dampack::calc_exp_loss() is provided by Alarid-Escudero F,
# Knowlton G, Easterly CW, Enns E (2024), dampack 1.0.2.1000,
# DOI 10.32614/CRAN.package.dampack.
# SOURCE: Expected-loss-curve interpretation follows Alarid-Escudero F,
# Enns EA, Kuntz KM, Michaud TL, Jalal H (2019), Value in Health
# 22(5):611-618, DOI 10.1016/j.jval.2019.02.008.
#
# Adaptations:
#   1. Common Random Numbers (CRN) pattern across OWPSA scenarios via
#      reusable seed. Each OWPSA run reuses the same random
#      stream so the only difference between runs is the fixed parameter.
#   2. assess_convergence_inmb() computes the
#      INMB CI at sample sizes 100, 500, 1000, 2000, 5000, 10000 using
#      qnorm(0.975) rather than the literal 1.96.
#   3. compute_pop_evpi_horizon() implements the Briggs annuity formula
#      explicitly with the discount rate exposed as an argument; default
#      4 percent matches Rundskriv R-109 first-bracket rate. Returns
#      both discounted and undiscounted variants for transparency.
#   4. EVPI curve regeneration block: the curve is
#      recomputed on every model run from the current psa_results using
#      the same two-strategy formula as compute_evppi(), then written to
#      data/processed/evpi_results.rds. Replaces the legacy stale file.
#
# Academic citations:
#   Briggs AH et al. (2012). DOI 10.1016/j.jval.2012.04.014. PMID 22999133.
#   Hatswell AJ, Bullement A, Briggs A, Paulden M, Stevenson MD (2018).
#     DOI 10.1007/s40273-018-0697-3. PMID 30051221.
#   Strong M, Oakley JE, Brennan A (2014).
#     DOI 10.1177/0272989X13505910. PMID 24246566.
#   Fenwick E et al. (2020). DOI 10.1016/j.jval.2020.01.001.
#   Briggs A, Claxton K, Sculpher MJ (2006). Decision Modelling for
#     Health Economic Evaluation, Oxford University Press.
#   Alarid-Escudero F, Enns EA, Kuntz KM, Michaud TL, Jalal H (2019).
#     Value in Health 22(5):611-618. DOI 10.1016/j.jval.2019.02.008.
# =============================================================================

if (!exists("canonical_hash_v1", mode = "function")) {
  source("R/functions/canonical-hash-v1.R")
}

# --- PSA parameter sampling --------------------------------------------------

PRIMARY_REFERENCE_PUBLIC_COLUMNS_V1 <- c(
  "HR_DFS", "HR_OS", "u_dfs", "u_prog", "c_surveillance_early",
  "c_exercise_annual", "c_progressed_annual", "c_terminal",
  "c_intervention_setup", "c_surveillance_late",
  "u_exercise_decrement"
)
PRIMARY_REFERENCE_PUBLIC_ATTRIBUTES_V1 <- c(
  "names", "class", "row.names", "surv_coef_dfs", "surv_coef_os",
  "endpoint_row_id_dfs", "endpoint_row_id_os", "dependence_diagnostics"
)
# Freeze one public draw order for the reference and dependence-sensitivity frames.
PSA_DRAW_ORDER_VERSION <- "primary-reference-v1"
PSA_SPECIFICATION_SCENARIO_IDS_V1 <- c(
  # Use zero cross-endpoint dependence as reference and positive copula values as sensitivities.
  "reference_rho0_sampling_only", "dependence_rho03",
  "dependence_rho05", "dependence_rho07", "dependence_rho09",
  # Test mapping RMSE as a separate quadrature-variance sensitivity.
  "mapping_rmse_quadrature_rho0")

draw_mvn_rows <- function(n, mu, Sigma) {
  # GUIDELINE: Sample fitted survival coefficients from their multivariate-normal approximation (Kearns et al. 2020)
  out <- MASS::mvrnorm(n, mu = mu, Sigma = Sigma)
  if (n == 1L) matrix(out, nrow = 1L) else out
}

#' Evaluate an expression without leaking or replacing the caller RNG state.
with_local_seed <- function(seed, expr) {
  stopifnot(typeof(seed) == "integer", length(seed) == 1L, !is.na(seed))
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  eval(substitute(expr), envir = parent.frame())
}

endpoint_block_hash <- function(block, endpoint) {
  endpoint <- match.arg(endpoint, c("dfs", "os"))
  expected <- if (endpoint == "dfs") {
    c("row_id", "log_HR_DFS", "dfs_meanlog", "dfs_sdlog")
  } else {
    c("row_id", "log_HR_OS", "os_meanlog", "os_sdlog")
  }
  stopifnot(identical(names(block), expected), typeof(block$row_id) == "integer",
            all(vapply(block[-1], typeof, character(1)) == "double"),
            !anyDuplicated(block$row_id), all(is.finite(as.matrix(block[-1]))))
  schema <- c(row_id = "integer", setNames(rep("double", 3L), expected[-1]))
  canonical_hash_v1(block[order(block$row_id, method = "radix"), ], schema)
}

#' Couple complete endpoint rows by a Gaussian-copula rank map.
rank_couple_endpoint_blocks <- function(dfs_block, os_block,
                                        z_dfs, epsilon_os, rho) {
  dfs_names <- c("row_id", "log_HR_DFS", "dfs_meanlog", "dfs_sdlog")
  os_names <- c("row_id", "log_HR_OS", "os_meanlog", "os_sdlog")
  stopifnot(is.data.frame(dfs_block), is.data.frame(os_block),
            identical(names(dfs_block), dfs_names),
            identical(names(os_block), os_names),
            nrow(dfs_block) == nrow(os_block), nrow(dfs_block) > 0L,
            typeof(dfs_block$row_id) == "integer",
            typeof(os_block$row_id) == "integer",
            !anyDuplicated(dfs_block$row_id), !anyDuplicated(os_block$row_id),
            all(is.finite(as.matrix(dfs_block[-1]))),
            all(is.finite(as.matrix(os_block[-1]))),
            is.double(z_dfs), is.double(epsilon_os),
            length(z_dfs) == nrow(dfs_block),
            length(epsilon_os) == nrow(os_block),
            all(is.finite(z_dfs)), all(is.finite(epsilon_os)),
            is.numeric(rho), length(rho) == 1L, is.finite(rho),
            rho >= 0, rho <= 1)
  # Couple complete endpoint rows only in positive-dependence sensitivity analyses.
  z_os <- rho * z_dfs + sqrt(1 - rho^2) * epsilon_os
  dfs_order <- order(z_dfs, seq_along(z_dfs), method = "radix")
  os_order <- order(z_os, seq_along(z_os), method = "radix")
  dfs_rows <- order(dfs_block$log_HR_DFS,
                    dfs_block$row_id, method = "radix")
  os_rows <- order(os_block$log_HR_OS,
                   os_block$row_id, method = "radix")
  out_dfs <- dfs_block
  out_os <- os_block
  out_dfs[dfs_order, ] <- dfs_block[dfs_rows, ]
  out_os[os_order, ] <- os_block[os_rows, ]
  list(dfs = out_dfs, os = out_os, z_dfs = z_dfs, z_os = z_os)
}

#' Sample PSA Parameter Sets
#'
#' Draws n parameter vectors from their joint uncertainty distributions.
#' Endpoint rows: marginal log-HR normal plus baseline-coefficient MVN;
#' utilities: beta; costs: gamma; fixed zeros remain explicit.
#' This is the core of the probabilistic analysis.
#'
#' @param n Integer. Number of PSA iterations.
#' @param params Named list. All parameter values needed for sampling.
#'   Required elements are validated against the canonical public-draw registry.
#' @param seed Integer. Random seed for reproducibility.
#' @param fixed_params Named list. Parameters to hold fixed (for OWPSA).
#'   Names must match column names in the output. Fixed parameters are
#'   set to their point estimate instead of being sampled.
#' @return Data frame with n rows and one column per parameter.
sample_psa_parameters <- function(n, params, seed = psa_seed_reference,
                                  fixed_params = list(),
                                  fit_dfs_ctrl = NULL,
                                  fit_os_ctrl = NULL,
                                  rho_endpoint_latent = rho_endpoint_latent_reference,
                                  u_prog_se_override = NULL,
                                  return_bundle = FALSE) {
  stopifnot(typeof(n) == "integer" || (is.numeric(n) && n == as.integer(n)),
            n > 0L)
  n <- as.integer(n)
  seed <- as.integer(seed)
  with_local_seed(seed, {

  # Exact stochastic inputs consumed by the model.
  required <- c("log_HR_DFS", "se_log_HR_DFS", "log_HR_OS", "se_log_HR_OS",
                "u_dfs_mean", "u_dfs_se", "u_prog_mean", "u_prog_se",
                "u_exercise_decrement",
                "c_surveillance_early", "c_surveillance_early_se",
                "c_surveillance_late",
                "c_exercise_annual", "c_exercise_annual_se",
                "c_progressed_annual", "c_progressed_annual_se",
                "c_terminal", "c_terminal_se",
                "c_intervention_setup", "c_intervention_setup_se")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("sample_psa_parameters: missing params: ",
         paste(missing, collapse = ", "))
  }
  na_params <- required[sapply(required, function(x) {
    is.na(params[[x]])
  })]
  if (length(na_params) > 0) {
    stop("PSA requires non-NA values for: ",
         paste(na_params, collapse = ", "),
         ". Populate in 00-parameters.R first.")
  }

  if (is.null(fit_dfs_ctrl) || is.null(fit_os_ctrl)) {
    stop("sample_psa_parameters: endpoint fit objects are required")
  }
  if (length(coef(fit_dfs_ctrl)) != 2L || length(coef(fit_os_ctrl)) != 2L) {
    stop("sample_psa_parameters: selected log-normal endpoint fits must each have two coefficients")
  }

  # Frozen draw order starts with complete endpoint rows.
  log_hr_dfs <- rnorm(n, params$log_HR_DFS, params$se_log_HR_DFS)
  surv_coefs_dfs <- draw_mvn_rows(n, mu = coef(fit_dfs_ctrl),
                                  Sigma = vcov(fit_dfs_ctrl))
  # GUIDELINE: Sample hazard ratios on the log scale with a normal draw (Briggs et al. 2006)
  log_hr_os <- rnorm(n, params$log_HR_OS, params$se_log_HR_OS)
  # GUIDELINE: Sample fitted survival coefficients from their multivariate-normal approximation (Kearns et al. 2020)
  surv_coefs_os <- draw_mvn_rows(n, mu = coef(fit_os_ctrl),
                                 Sigma = vcov(fit_os_ctrl))
  dfs_block <- data.frame(
    row_id = seq_len(n), log_HR_DFS = as.double(log_hr_dfs),
    dfs_meanlog = as.double(surv_coefs_dfs[, 1]),
    dfs_sdlog = as.double(surv_coefs_dfs[, 2]), check.names = FALSE)
  os_block <- data.frame(
    row_id = seq_len(n), log_HR_OS = as.double(log_hr_os),
    os_meanlog = as.double(surv_coefs_os[, 1]),
    os_sdlog = as.double(surv_coefs_os[, 2]), check.names = FALSE)

  latent <- with_local_seed(as.integer(dependence_coupling_seed), {
    # Use Gaussian-copula rank coupling for positive endpoint-dependence sensitivities.
    list(z_dfs = rnorm(n), epsilon_os = rnorm(n))
  })
  coupled <- rank_couple_endpoint_blocks(
    dfs_block, os_block, latent$z_dfs, latent$epsilon_os,
    rho_endpoint_latent)

  # Frozen public draw order continues after endpoint blocks.
  # GUIDELINE: Represent bounded utility uncertainty with a beta distribution (Briggs et al. 2006)
  u_dfs_draw <- with(beta_params(params$u_dfs_mean, params$u_dfs_se),
                     rbeta(n, alpha, beta))
  # Keep mapping-RMSE uncertainty separate from the tariff-scale utility transformation.
  u_prog_uniform <- runif(n)
  u_prog_se_used <- if (is.null(u_prog_se_override)) params$u_prog_se else
    u_prog_se_override
  # GUIDELINE: Derive beta shapes from the utility mean and variance (Briggs et al. 2006)
  u_prog_shape <- beta_params(params$u_prog_mean, u_prog_se_used)
  # GUIDELINE: Represent bounded utility uncertainty with a beta distribution (Briggs et al. 2006)
  u_prog_draw <- qbeta(u_prog_uniform, u_prog_shape$alpha, u_prog_shape$beta)
  # GUIDELINE: Represent non-negative right-skewed cost uncertainty with a gamma distribution (Briggs et al. 2012)
  c_surveillance_early_draw <- with(
    gamma_params(params$c_surveillance_early, params$c_surveillance_early_se),
    rgamma(n, shape, rate))
  # GUIDELINE: Represent non-negative right-skewed cost uncertainty with a gamma distribution (Briggs et al. 2012)
  c_exercise_annual_draw <- with(
    gamma_params(params$c_exercise_annual, params$c_exercise_annual_se),
    rgamma(n, shape, rate))
  # GUIDELINE: Represent non-negative right-skewed cost uncertainty with a gamma distribution (Briggs et al. 2012)
  c_progressed_annual_draw <- with(
    gamma_params(params$c_progressed_annual, params$c_progressed_annual_se),
    rgamma(n, shape, rate))
  # GUIDELINE: Represent non-negative right-skewed cost uncertainty with a gamma distribution (Briggs et al. 2012)
  c_terminal_draw <- with(
    gamma_params(params$c_terminal, params$c_terminal_se),
    rgamma(n, shape, rate))
  # GUIDELINE: Represent non-negative right-skewed cost uncertainty with a gamma distribution (Briggs et al. 2012)
  c_intervention_setup_draw <- with(
    gamma_params(params$c_intervention_setup, params$c_intervention_setup_se),
    rgamma(n, shape, rate))

  draws <- data.frame(
    # GUIDELINE: Exponentiate normally sampled log hazard ratios (Briggs et al. 2006)
    HR_DFS = exp(coupled$dfs$log_HR_DFS),
    HR_OS = exp(coupled$os$log_HR_OS),
    u_dfs = u_dfs_draw,
    u_prog = u_prog_draw,
    c_surveillance_early = c_surveillance_early_draw,
    c_exercise_annual = c_exercise_annual_draw,
    c_progressed_annual = c_progressed_annual_draw,

    # Terminal care and setup costs must be sampled.
    # Previously fixed scalars from globalenv()  --  violated PSA completeness.
    # Gamma distribution for all cost parameters.
    # SOURCE: Norwegian DRG / Helfo end-of-life care data
    c_terminal = c_terminal_draw,
    c_intervention_setup = c_intervention_setup_draw,
    c_surveillance_late = rep(as.double(params$c_surveillance_late), n),
    u_exercise_decrement = rep(as.double(params$u_exercise_decrement), n),
    check.names = FALSE
  )

  for (param_name in names(fixed_params)) {
    if (param_name %in% names(draws)) {
      draws[[param_name]] <- fixed_params[[param_name]]
    } else {
      # Caution: a fixed parameter absent from the draws frame is a silent
      # no-op that reports an unperturbed scenario as a perturbed one; fail
      # closed so the caller cannot mistake it for a completed OWPSA run.
      stop("OWPSA: parameter '", param_name, "' not found in PSA draws. ",
           "Available draw columns: ", paste(names(draws), collapse = ", "), ".")
    }
  }

  dfs_hash <- endpoint_block_hash(dfs_block, "dfs")
  os_hash <- endpoint_block_hash(os_block, "os")
  cross_corr <- cor(coupled$dfs[, c("dfs_meanlog", "dfs_sdlog")],
                    coupled$os[, c("os_meanlog", "os_sdlog")])
  diagnostics <- list(
    target_rho = as.double(rho_endpoint_latent),
    pearson_log_hr = cor(coupled$dfs$log_HR_DFS, coupled$os$log_HR_OS),
    spearman_log_hr_diagnostic = cor(coupled$dfs$log_HR_DFS,
                                     coupled$os$log_HR_OS,
                                     method = "spearman"),
    max_abs_cross_coefficient_correlation = max(abs(cross_corr)),
    sorted_endpoint_hash_dfs = dfs_hash,
    sorted_endpoint_hash_os = os_hash,
    draw_order_version = PSA_DRAW_ORDER_VERSION,
    rng_tuple = RNGkind()
  )
  base_row_names <- attr(draws, "row.names")
  attributes(draws) <- list(
    names = names(draws), class = "data.frame", row.names = base_row_names,
    surv_coef_dfs = as.matrix(coupled$dfs[, c("dfs_meanlog", "dfs_sdlog")]),
    surv_coef_os = as.matrix(coupled$os[, c("os_meanlog", "os_sdlog")]),
    endpoint_row_id_dfs = coupled$dfs$row_id,
    endpoint_row_id_os = coupled$os$row_id,
    dependence_diagnostics = diagnostics
  )
  stopifnot(identical(names(draws), PRIMARY_REFERENCE_PUBLIC_COLUMNS_V1),
            identical(names(attributes(draws)),
                      PRIMARY_REFERENCE_PUBLIC_ATTRIBUTES_V1))
  bundle <- list(
    frame = draws,
    u_prog_uniform = u_prog_uniform,
    dfs_block = dfs_block,
    os_block = os_block,
    z_dfs = latent$z_dfs,
    epsilon_os = latent$epsilon_os
  )
  if (isTRUE(return_bundle)) bundle else draws
  })
}


# --- Flexsurv helper (file scope) -----------------------------------------

#' Create Modified flexsurv Fit with Sampled Coefficients
#'
#' Replaces the point-estimate coefficients in a flexsurvreg object
#' with a sampled parameter vector from MASS::mvrnorm(). Used in the
#' PSA inner loop to reconstruct survival curves from sampled parameters.
#'
#' Defined at file scope for testability and reusability (rather than as a
#' closure inside run_psa()).
#'
#' @param fit flexsurvreg object. Original fitted model.
#' @param new_coefs Numeric vector. Sampled coefficient values.
#' @return Modified flexsurvreg object with new coefficients.
modify_flexsurv_fit <- function(fit, new_coefs) {
  # summary.flexsurvreg reads $res.t[,"est"] (internal/transformed
  # scale), NOT $res[,"est"] (natural scale). coef(fit) and vcov(fit) return
  # values on the internal scale, so MVN draws must go into $res.t.
  # The old code wrote to $res (natural scale), which had no effect on
  # survival predictions  --  PSA survival uncertainty was silently disabled.
  fit_mod <- fit
  fit_mod$res.t[, "est"] <- new_coefs
  fit_mod$coefficients <- new_coefs
  # Also update $res with back-transformed values for consistency
  for (i in seq_along(new_coefs)) {
    inv_fn <- fit$dlist$inv.transforms[[i]]
    fit_mod$res[i, "est"] <- inv_fn(new_coefs[i])
  }
  fit_mod
}


# --- PSA execution loop (BASE CASE) -----------------------------------------

#' Run Full Probabilistic Sensitivity Analysis
#'
#' This is the BASE CASE analysis.
#' For each PSA iteration: override parameters, rebuild PSM trace,
#' calculate costs and QALYs, store incremental results.
#'
#' Stores both total and incremental costs/QALYs per arm per iteration.
#' Total values are required by dampack::calc_exp_loss().
#'
#' @param n_sim Integer. Number of PSA iterations (default: n_psa from params).
#' @param params Named list. All model parameters for sampling. If NULL,
#'   constructs from global variables for backward compatibility.
#' @param fixed_params Named list. For OWPSA: parameters to hold fixed.
#' @param seed Integer. Random seed.
#' @return Data frame with columns: iteration, inc_costs, inc_qalys, icer, inb,
#'   total_costs_int, total_costs_ctrl, total_qalys_int, total_qalys_ctrl.
run_psa <- function(n_sim = NULL, params = NULL,
                     fixed_params = list(), seed = psa_seed_reference,
                     fit_dfs_ctrl = NULL, fit_os_ctrl = NULL,
                     s_genpop = NULL,
                     discount_weights = NULL,
                     wtp = NULL,
                     intervention_duration_years_arg = NULL,
                     surveillance_cutoff_years_arg = NULL,
                     n_cyc = NULL,
                     cyc_length = NULL,
                     hr_dfs_point = NULL,
                     hr_os_point = NULL,
                     psa_params = NULL) {
  # Default arguments are NULL instead of referencing globals.
  # Resolved lazily from globals only when NULL, with clear documentation.
  if (is.null(n_sim)) {
    if (exists("n_psa", envir = globalenv())) {
      n_sim <- get("n_psa", envir = globalenv())
    } else {
      stop("run_psa: n_sim must be provided or n_psa must exist in global env.")
    }
  }
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("run_psa: wtp must be provided or wtp_threshold must exist in global env.")
    }
  }
  if (is.null(intervention_duration_years_arg)) {
    if (exists("intervention_duration_years", envir = globalenv())) {
      intervention_duration_years_arg <- get(
        "intervention_duration_years", envir = globalenv())
    } else {
      stop("run_psa: intervention_duration_years_arg is required")
    }
  }
  if (is.null(surveillance_cutoff_years_arg)) {
    if (exists("surveillance_cutoff_years", envir = globalenv())) {
      surveillance_cutoff_years_arg <- get(
        "surveillance_cutoff_years", envir = globalenv())
    } else {
      stop("run_psa: surveillance_cutoff_years_arg is required")
    }
  }
  if (is.null(n_cyc)) {
    if (exists("n_cycles", envir = globalenv())) {
      n_cyc <- get("n_cycles", envir = globalenv())
    } else {
      stop("run_psa: n_cyc must be provided or n_cycles must exist in global env.")
    }
  }
  if (is.null(cyc_length)) {
    if (exists("cycle_length", envir = globalenv())) {
      cyc_length <- get("cycle_length", envir = globalenv())
    } else {
      stop("run_psa: cyc_length must be provided or cycle_length must exist in global env.")
    }
  }
  # HR point estimates are explicit parameters.
  # Previously, the fallback code path referenced global HR_DFS/HR_OS directly,
  # breaking encapsulation. Now resolved lazily like other parameters.
  if (is.null(hr_dfs_point)) {
    if (exists("HR_DFS", envir = globalenv())) {
      hr_dfs_point <- get("HR_DFS", envir = globalenv())
    } else {
      stop("run_psa: hr_dfs_point must be provided or HR_DFS must exist in global env.")
    }
  }
  if (is.null(hr_os_point)) {
    if (exists("HR_OS", envir = globalenv())) {
      hr_os_point <- get("HR_OS", envir = globalenv())
    } else {
      stop("run_psa: hr_os_point must be provided or HR_OS must exist in global env.")
    }
  }
  # Backward compatibility: if params not provided, construct from globals.
  # This ensures main.R runs without changes during the transition.
  # NOTE: global-state pattern is technical debt.
  # Full refactoring to parameter-list-only interface is deferred.
  if (is.null(params)) {
    # NOTE: constructing params from globals is technical debt.
    # Refactor callers to pass params explicitly (deferred).
    # This global fallback is retained for backward compatibility during transition.
    params <- list(
      log_HR_DFS           = log_HR_DFS,
      se_log_HR_DFS        = se_log_HR_DFS,
      log_HR_OS            = log_HR_OS,
      se_log_HR_OS         = se_log_HR_OS,
      u_dfs_mean           = u_dfs_mean,
      u_dfs_se             = u_dfs_se,
      u_prog_mean          = u_prog_mean,
      u_prog_se            = u_prog_se,
      u_exercise_decrement = u_exercise_decrement,
      c_surveillance_early = c_surveillance_early,
      c_surveillance_early_se = c_surveillance_early_se,
      c_surveillance_late = c_surveillance_late,
      c_exercise_annual = c_exercise_annual,
      c_exercise_annual_se = c_exercise_annual_se,
      c_progressed_annual = c_progressed_annual,
      c_progressed_annual_se = c_progressed_annual_se,
      # Terminal and setup costs are sampled in PSA
      # Standardized envir = globalenv() for consistency
      c_terminal             = if (exists("c_terminal", envir = globalenv()))
                                 get("c_terminal", envir = globalenv()) else 0,
      c_terminal_se          = if (exists("c_terminal_se", envir = globalenv()))
                                 get("c_terminal_se", envir = globalenv()) else 0,
      c_intervention_setup   = if (exists("c_intervention_setup", envir = globalenv()))
                                 get("c_intervention_setup", envir = globalenv()) else 0,
      c_intervention_setup_se = if (exists("c_intervention_setup_se", envir = globalenv()))
                                  get("c_intervention_setup_se", envir = globalenv()) else 0
    )
  }

  if (is.null(discount_weights)) {
    if (exists("discount_weights_stepped", envir = globalenv())) {
      discount_weights <- get("discount_weights_stepped",
                               envir = globalenv())
    } else {
      stop("run_psa: discount_weights must be provided or ",
           "discount_weights_stepped must exist in global env.")
    }
  }

  message("Running PSA with ", n_sim, " iterations...")
  n_sim <- as.integer(n_sim)
  if (is.null(psa_params)) {
    psa_params <- sample_psa_parameters(
      n_sim, params = params, seed = seed, fixed_params = fixed_params,
      fit_dfs_ctrl = fit_dfs_ctrl, fit_os_ctrl = fit_os_ctrl)
  } else {
    if (!is.data.frame(psa_params) || nrow(psa_params) != n_sim ||
        !identical(names(psa_params), PRIMARY_REFERENCE_PUBLIC_COLUMNS_V1) ||
        !identical(names(attributes(psa_params)),
                   PRIMARY_REFERENCE_PUBLIC_ATTRIBUTES_V1)) {
      stop("run_psa: injected psa_params violates the exact frame registry or row count")
    }
  }

  # Pre-allocate results matrix  --  includes total costs/QALYs per arm
  # for dampack::calc_exp_loss() which requires absolute values per strategy
  results <- data.frame(
    iteration        = 1:n_sim,
    inc_costs        = numeric(n_sim),
    inc_qalys        = numeric(n_sim),
    icer             = numeric(n_sim),
    inb              = numeric(n_sim),
    total_costs_int        = numeric(n_sim),
    total_costs_ctrl       = numeric(n_sim),
    total_qalys_int        = numeric(n_sim),
    total_qalys_ctrl       = numeric(n_sim),
    cost_surveillance_int  = numeric(n_sim),
    cost_surveillance_ctrl = numeric(n_sim),
    cost_exercise_int      = numeric(n_sim),
    cost_exercise_ctrl     = numeric(n_sim),
    cost_progressed_int    = numeric(n_sim),
    cost_progressed_ctrl   = numeric(n_sim),
    cost_terminal_int      = numeric(n_sim),
    cost_terminal_ctrl     = numeric(n_sim)
  )

  # PSA inner loop implementation.
  # For each iteration: sample params → build PSM traces → costs/QALYs → store.
  #
  # Uses control-arm-fit + HR approach consistently.
  # If survival coefficient draws are available (from
  #   sample_psa_parameters with fit objects), reconstructs survival curves
  #   from sampled parameters. Otherwise uses fixed flexsurv fit objects.
  #
  # Guard: if fit objects are not available, the PSA cannot run.
  # This replaces the old silent-zeros behavior with an informative error.
  if (is.null(fit_dfs_ctrl) || is.null(fit_os_ctrl)) {
    stop("run_psa() requires flexsurvreg fit objects (fit_dfs_ctrl, ",
         "fit_os_ctrl) to build PSM traces. ",
         "Complete 02-fit-survival.R first, then pass fit objects to run_psa().")
  }

  # Complete endpoint rows are consumed exactly in paired public-frame order.
  surv_coefs_dfs <- attr(psa_params, "surv_coef_dfs")
  surv_coefs_os <- attr(psa_params, "surv_coef_os")
  if (!is.matrix(surv_coefs_dfs) || !is.matrix(surv_coefs_os) ||
      nrow(surv_coefs_dfs) != n_sim || nrow(surv_coefs_os) != n_sim ||
      length(attr(psa_params, "endpoint_row_id_dfs")) != n_sim ||
      length(attr(psa_params, "endpoint_row_id_os")) != n_sim) {
    stop("run_psa: endpoint coefficient attributes are absent or misaligned")
  }

  # Time grid for survival computation
  times <- seq(0, n_cyc * cyc_length, by = cyc_length)

  # All loop parameters are explicit function arguments.
  cl <- cyc_length
  int_dur <- intervention_duration_years_arg

  # modify_flexsurv_fit() is defined at file scope
  # (see definition above run_psa). Was a closure inside run_psa, making it
  # untestable and inaccessible to other code.

  # Pre-compute age-utility multipliers ONCE (outside loop).
  # Age adjustment is a structural feature, not a parameter  --  it does not
  # need to be sampled in PSA. But it MUST be APPLIED during PSA iterations.
  # Age-adjusted utilities are mandatory for lifetime horizons.
  # GUIDELINE: Ara & Brazier (2010) multiplicative adjustment.
  age_multipliers <- get_age_utility_multipliers(
    entry_age    = if (exists("cohort_age", envir = globalenv()))
                     get("cohort_age", envir = globalenv())
                   else stop("cohort_age not found in global environment. ",
                             "Source 00-parameters.R before running PSA."),
    n_cycles     = n_cyc,
    cycle_length = cl
  )

  for (i in 1:n_sim) {
    # 1. Extract parameter set for this iteration
    p_i <- psa_params[i, ]

    # 2. Build control arm survival via shared functions
    # If survival coefficient draws available, reconstruct S(t)
    # from sampled parameters using modify_flexsurv_fit helper.
    if (!is.null(surv_coefs_dfs) && !is.null(surv_coefs_os)) {
      fit_dfs_i <- modify_flexsurv_fit(fit_dfs_ctrl, surv_coefs_dfs[i, ])
      fit_os_i  <- modify_flexsurv_fit(fit_os_ctrl,  surv_coefs_os[i, ])
    } else {
      fit_dfs_i <- fit_dfs_ctrl
      fit_os_i  <- fit_os_ctrl
    }

    # Use build_psm_trace() for control arm  --  same function as
    # the deterministic analysis. validate = FALSE for PSA performance.
    #
    # On control arm failure, the fallback uses
    # point-estimate fits for BOTH the control arm AND point-estimate HRs
    # for the intervention arm (via ctrl_fallback_used flag). This ensures
    # a failed iteration is fully deterministic rather than mixing sampled
    # HRs with unsampled baseline survival.
    # Caution: Fallback produces a deterministic-like iteration that biases
    # toward the point-estimate ICER. With well-behaved parametric fits,
    # failures are rare (<1% of iterations). If failures exceed 5%,
    # investigate the survival model specification.
    ctrl_fallback_used <- FALSE
    trace_ctrl <- tryCatch(
      build_psm_trace(
        fit_dfs      = fit_dfs_i,
        fit_os       = fit_os_i,
        n_cycles     = n_cyc,
        cycle_length = cl,
        s_genpop     = s_genpop,
        arm_label    = "Standard Care",
        # GUIDELINE: Floor modeled mortality at matched general-population mortality (Rutherford et al. 2020)
        mortality_method = "hazard_max",
        validate     = FALSE  # Skip validation in PSA inner loop (performance)
      ),
      error = function(e) {
        warning("PSA iteration ", i, ": control trace failed. ",
                "Using point estimates for BOTH arms. Error: ",
                e$message, call. = FALSE)
        ctrl_fallback_used <<- TRUE
        build_psm_trace(
          fit_dfs      = fit_dfs_ctrl,
          fit_os       = fit_os_ctrl,
          n_cycles     = n_cyc,
          cycle_length = cl,
          s_genpop     = s_genpop,
          arm_label    = "Standard Care",
          # GUIDELINE: Floor modeled mortality at matched general-population mortality (Rutherford et al. 2020)
          mortality_method = "hazard_max",
          validate     = FALSE
        )
      }
    )

    # Use build_psm_trace_from_ctrl() for intervention arm  --
    # SAME function as the deterministic analysis, defined once.
    # When control fallback is used, also use point-estimate
    # HRs for intervention arm to maintain consistency within the iteration.
    # Use hr_dfs_point/hr_os_point function parameters
    # instead of global HR_DFS/HR_OS. Fixes encapsulation violation.
    hr_dfs_i <- if (ctrl_fallback_used) hr_dfs_point else p_i$HR_DFS
    hr_os_i_val <- if (ctrl_fallback_used) hr_os_point else p_i$HR_OS
    fit_dfs_for_int <- if (ctrl_fallback_used) fit_dfs_ctrl else fit_dfs_i
    fit_os_for_int  <- if (ctrl_fallback_used) fit_os_ctrl  else fit_os_i

    trace_int <- tryCatch(
      build_psm_trace_from_ctrl(
        fit_dfs_ctrl = fit_dfs_for_int,
        fit_os_ctrl  = fit_os_for_int,
        hr_dfs       = hr_dfs_i,
        hr_os        = hr_os_i_val,
        n_cycles     = n_cyc,
        cycle_length = cl,
        s_genpop     = s_genpop,
        arm_label    = "Exercise",
        # GUIDELINE: Floor modeled mortality at matched general-population mortality (Rutherford et al. 2020)
        mortality_method = "hazard_max",
        validate     = FALSE  # Skip validation in PSA inner loop (performance)
      ),
      error = function(e) {
        warning("PSA iteration ", i, ": exercise trace failed. ",
                "Using point estimates for BOTH arms. Error: ",
                e$message, call. = FALSE)
        # Use hr_dfs_point/hr_os_point function params
        # instead of global HR_DFS/HR_OS. Consistent with control arm fallback.
        build_psm_trace_from_ctrl(
          fit_dfs_ctrl = fit_dfs_ctrl,
          fit_os_ctrl  = fit_os_ctrl,
          hr_dfs       = hr_dfs_point,  # Point-estimate HR (function param)
          hr_os        = hr_os_point,   # Point-estimate HR (function param)
          n_cycles     = n_cyc,
          cycle_length = cl,
          s_genpop     = s_genpop,
          arm_label    = "Exercise",
          # GUIDELINE: Floor modeled mortality at matched general-population mortality (Rutherford et al. 2020)
          mortality_method = "hazard_max",
          validate     = FALSE
        )
      }
    )

    psm_trace_i <- rbind(trace_int, trace_ctrl)

    # The same surveillance/progressed scalars feed both arms;
    # only the programme component is exercise-specific.
    cq_int <- calculate_costs_qalys(
      psm_trace_i, arm = "Exercise",
      # SOURCE: Mulder et al. 2022 Table 2 and Färkkilä et al. 2014 Table 2, disease-state utilities
      u_dfs = p_i$u_dfs, u_prog = p_i$u_prog, u_dead = 0,
      c_surveillance_early = p_i$c_surveillance_early,
      c_surveillance_late = p_i$c_surveillance_late,
      surveillance_cutoff_years = surveillance_cutoff_years_arg,
      c_exercise_annual = p_i$c_exercise_annual,
      intervention_duration_years = int_dur,
      c_progressed_annual = p_i$c_progressed_annual,
      c_terminal = p_i$c_terminal,
      c_intervention_setup = p_i$c_intervention_setup,
      u_exercise_decrement = p_i$u_exercise_decrement,
      discount_weights = discount_weights,
      cycle_length = cl,
      age_utility_multipliers = age_multipliers,
      validate = FALSE  # Skip validation in PSA inner loop (performance)
    )

    cq_ctrl <- calculate_costs_qalys(
      psm_trace_i, arm = "Standard Care",
      # SOURCE: Mulder et al. 2022 Table 2 and Färkkilä et al. 2014 Table 2, disease-state utilities
      u_dfs = p_i$u_dfs, u_prog = p_i$u_prog, u_dead = 0,
      c_surveillance_early = p_i$c_surveillance_early,
      c_surveillance_late = p_i$c_surveillance_late,
      surveillance_cutoff_years = surveillance_cutoff_years_arg,
      # The exercise programme cost belongs to the exercise arm only.
      c_exercise_annual = 0,
      # The exercise programme is absent from standard care.
      intervention_duration_years = 0,
      c_progressed_annual = p_i$c_progressed_annual,
      c_terminal = p_i$c_terminal,
      # Exercise-programme setup cost belongs to the exercise arm only.
      c_intervention_setup = 0,
      # Exercise disutility remains fixed at zero and is not sampled.
      u_exercise_decrement = 0,
      discount_weights = discount_weights,
      cycle_length = cl,
      age_utility_multipliers = age_multipliers,
      validate = FALSE  # Skip validation in PSA inner loop (performance)
    )

    # 7. Store total costs/QALYs per arm
    results$total_costs_int[i]  <- sum(cq_int$costs_disc)
    results$total_costs_ctrl[i] <- sum(cq_ctrl$costs_disc)
    results$total_qalys_int[i]  <- sum(cq_int$qalys_disc)
    results$total_qalys_ctrl[i] <- sum(cq_ctrl$qalys_disc)

    # SOURCE: reporting components returned by calculate_costs_qalys().
    # DECISION: store arm-level components without changing incremental analysis.
    results$cost_surveillance_int[i] <- sum(cq_int$cost_surveillance_disc)
    results$cost_surveillance_ctrl[i] <- sum(cq_ctrl$cost_surveillance_disc)
    results$cost_exercise_int[i] <- sum(cq_int$cost_exercise_disc)
    # The exercise programme cost is exercise-arm-specific.
    results$cost_exercise_ctrl[i] <- sum(cq_ctrl$cost_exercise_disc)
    # Use one shared progressed-disease unit cost with arm-specific occupancy.
    results$cost_progressed_int[i] <- sum(cq_int$cost_progressed_disc)
    results$cost_progressed_ctrl[i] <- sum(cq_ctrl$cost_progressed_disc)
    results$cost_terminal_int[i] <- sum(cq_int$cost_terminal_disc)
    results$cost_terminal_ctrl[i] <- sum(cq_ctrl$cost_terminal_disc)
    stopifnot(isTRUE(all.equal(
      results$total_costs_int[i],
      results$cost_surveillance_int[i] + results$cost_exercise_int[i] +
        results$cost_progressed_int[i] + results$cost_terminal_int[i],
      tolerance = 1e-8, check.attributes = FALSE
    )))
    stopifnot(isTRUE(all.equal(
      results$total_costs_ctrl[i],
      results$cost_surveillance_ctrl[i] + results$cost_exercise_ctrl[i] +
        results$cost_progressed_ctrl[i] + results$cost_terminal_ctrl[i],
      tolerance = 1e-8, check.attributes = FALSE
    )))

    # 8. Compute incremental values
    # GUIDELINE: Derive incremental outcomes as exercise minus standard care (Drummond et al. 2015)
    results$inc_costs[i]  <- results$total_costs_int[i] -
                              results$total_costs_ctrl[i]
    results$inc_qalys[i]  <- results$total_qalys_int[i] -
                              results$total_qalys_ctrl[i]
    if (abs(results$inc_qalys[i]) < 1e-10) {
      results$icer[i] <- NA_real_
    } else {
      # GUIDELINE: Compute the ICER as incremental cost divided by incremental effect (Drummond et al. 2015)
      results$icer[i] <- results$inc_costs[i] / results$inc_qalys[i]
    }
    # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
    results$inb[i] <- results$inc_qalys[i] * wtp - results$inc_costs[i]
  }

  message("PSA complete. ", n_sim, " iterations executed.")
  attr(results, "psa_params") <- psa_params
  if (n_sim == 1L) {
    attr(results, "component_traces") <- list(
      exercise = cq_int[, c("time_end", "surveillance", "exercise", "total")],
      control = cq_ctrl[, c("time_end", "surveillance", "exercise", "total")])
  }
  results
}


# --- PSA summary statistics --------------------------------------------------

#' Summarise PSA Results (Base Case Output)
#'
#' Produces the primary analysis outputs (PSA is the base case):
#'   - Expected (mean) ICER
#'   - P(cost-effective) at the applicable severity-informed WTP reference
#'   - Expected incremental net benefit
#'   - 95% credible interval for ICER
#'
#' @param psa_results Data frame from run_psa().
#' @param wtp Numeric. WTP threshold (default: wtp_threshold from params).
#' @return Named list of summary statistics.
summarise_psa <- function(psa_results, wtp = NULL) {
  # Default is NULL instead of wtp_threshold (global) for encapsulation
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("summarise_psa: wtp must be provided or wtp_threshold must exist.")
    }
  }
  valid <- psa_results[is.finite(psa_results$icer), ]

  # Expected ICER: ratio of means (Briggs, Claxton & Sculpher 2006, Ch. 4).
  # NOT mean of ratios  --  the mean of individual-iteration ICERs is an
  # unstable and biased estimator when some iterations have near-zero
  # or negative incremental QALYs.
  # GUIDELINE: Estimate the expected ICER as the ratio of mean incremental costs to mean incremental QALYs (Briggs et al. 2006)
  expected_icer <- mean(psa_results$inc_costs) /
                   mean(psa_results$inc_qalys)

  # ICER 95% CI from individual ratios is unreliable when
  # incremental QALYs cross zero (bimodal/heavy-tailed distribution).
  # Report it with a warning, and also report INB 95% CI which has a
  # GUIDELINE: well-defined distribution (Briggs et al. 2006).
  inb_values <- psa_results$inc_qalys * wtp - psa_results$inc_costs

  list(
    expected_icer     = expected_icer,
    median_icer       = median(valid$icer),
    # Caution: Individual-ratio ICER intervals are unstable when incremental QALYs cross zero.
    icer_95ci         = quantile(valid$icer, c(0.025, 0.975)),
    icer_95ci_warning = paste0(
      "ICER CI from individual ratios may be misleading when ",
      "incremental QALYs cross zero. Prefer INB CI or P(CE)."),
    # GUIDELINE: Prefer INB intervals when incremental QALYs can cross zero (Briggs et al. 2006)
    inb_95ci          = quantile(inb_values, c(0.025, 0.975)),
    # 95% CrIs for the headline PSA estimates: the 0.025/0.975 quantile pair,
    # same convention as inb_95ci.
    qalys_int_95ci    = quantile(psa_results$total_qalys_int,  c(0.025, 0.975)),
    qalys_ctrl_95ci   = quantile(psa_results$total_qalys_ctrl, c(0.025, 0.975)),
    # 0.025/0.975 quantile pair, the standard 95% interval convention
    inc_qalys_95ci    = quantile(psa_results$inc_qalys,        c(0.025, 0.975)),
    costs_int_95ci    = quantile(psa_results$total_costs_int,  c(0.025, 0.975)),
    costs_ctrl_95ci   = quantile(psa_results$total_costs_ctrl, c(0.025, 0.975)),
    # 0.025/0.975 quantile pair, the standard 95% interval convention
    inc_costs_95ci    = quantile(psa_results$inc_costs,        c(0.025, 0.975)),
    expected_inc_cost = mean(psa_results$inc_costs),
    expected_inc_qaly = mean(psa_results$inc_qalys),
    expected_inb      = mean(inb_values),
    expected_costs_int = mean(psa_results$total_costs_int),
    expected_costs_ctrl = mean(psa_results$total_costs_ctrl),
    expected_qalys_int = mean(psa_results$total_qalys_int),
    expected_qalys_ctrl = mean(psa_results$total_qalys_ctrl),
    expected_cost_surveillance_int = mean(psa_results$cost_surveillance_int),
    expected_cost_surveillance_ctrl = mean(psa_results$cost_surveillance_ctrl),
    expected_cost_exercise_int = mean(psa_results$cost_exercise_int),
    expected_cost_exercise_ctrl = mean(psa_results$cost_exercise_ctrl),
    expected_cost_progressed_int = mean(psa_results$cost_progressed_int),
    expected_cost_progressed_ctrl = mean(psa_results$cost_progressed_ctrl),
    expected_cost_terminal_int = mean(psa_results$cost_terminal_int),
    expected_cost_terminal_ctrl = mean(psa_results$cost_terminal_ctrl),
    # GUIDELINE: Compute strategy net monetary benefit as QALYs times WTP minus costs (Briggs et al. 2006)
    expected_nmb_int = mean(psa_results$total_qalys_int * wtp -
                            psa_results$total_costs_int),
    expected_nmb_ctrl = mean(psa_results$total_qalys_ctrl * wtp -
                             psa_results$total_costs_ctrl),
    # GUIDELINE: Estimate cost-effectiveness probability as the PSA share with positive INB (Alarid-Escudero et al. 2019)
    prob_ce           = mean(inb_values >= 0),
    n_iterations      = nrow(psa_results),
    n_valid           = nrow(valid),
    wtp_used          = wtp
  )
}


# --- CE Plane Quadrant Analysis ------------------------------------------------

#' CE Plane Quadrant Analysis
#'
#' Classifies each PSA iteration into one of four CE plane quadrants.
#' Optionally computes the 50% crossover WTP (where P(CE) = 50%) when
#' wtp_range is provided.
#' SOURCE: Drummond et al. (2015), Black (1990).
#' REFERENCE CODE: Krijkamp et al. (2018) HEVAL5200.
#' CHEERS-VOI Item 20 (characterizing uncertainty, modified).
#'
#' @param psa_results Data frame from run_psa() with inc_costs, inc_qalys.
#' @param wtp_range Numeric vector. WTP values for crossover computation
#'   (optional; if NULL, crossover WTP is not computed).
#' @return Named list: counts (named vector), percentages, n_iterations,
#'   n_on_axis (iterations with exactly zero incremental cost or QALY),
#'   dominance_pct (percentage in SE quadrant), crossover_wtp (NA if
#'   wtp_range not provided or no crossover found).
compute_quadrant_analysis <- function(psa_results, wtp_range = NULL) {
  n <- nrow(psa_results)
  dc <- psa_results$inc_costs
  dq <- psa_results$inc_qalys

  # Strict inequalities per standard CE plane convention (Black, 1990)
  # and reference implementation (Krijkamp et al. 2018). On-axis iterations (inc_costs = 0 or
  # inc_qalys = 0 exactly) are vanishingly rare with continuous PSA
  # distributions; counted separately for verification.
  counts <- c(
    # GUIDELINE: Classify cost-effectiveness-plane draws by the signs of incremental costs and effects (Black 1990)
    NE = sum(dc > 0 & dq > 0),   # More costly, more effective
    SE = sum(dc < 0 & dq > 0),   # Dominant
    NW = sum(dc > 0 & dq < 0),   # Dominated
    # GUIDELINE: Classify cost-effectiveness-plane draws by the signs of incremental costs and effects (Black 1990)
    SW = sum(dc < 0 & dq < 0)    # Less costly, less effective
  )

  # Caution: Strict quadrant inequalities leave zero-cost or zero-QALY draws on the axis.
  on_axis <- n - sum(counts)
  if (on_axis > 0) {
    warning("compute_quadrant_analysis: ", on_axis,
            " of ", n, " iterations on axis boundaries.",
            call. = FALSE)
  }

  # GUIDELINE: Report cost-effectiveness-plane quadrant shares over all PSA draws (Black 1990)
  pct <- round(100 * counts / n, 1)

  # 50% crossover WTP: where P(CE) = 50%
  # METHOD: minimum distance, wtp_range[which.min(abs(prob_ce - 0.5))].
  # Reference implementation uses sign-change detection (diff(sign(...))),
  # equivalent for 2-strategy monotonic CEACs with a single crossing.
  crossover_wtp <- NA_real_
  if (!is.null(wtp_range)) {
    prob_ce <- sapply(wtp_range, function(w) {
      # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
      inb <- dq * w - dc
      # GUIDELINE: Estimate the CEAC as the share of PSA draws that are cost-effective (Briggs et al. 2006)
      mean(inb[!is.na(inb)] >= 0)
    })
    crossover_idx <- which.min(abs(prob_ce - 0.5))
    # Guard: only report if P(CE) actually approaches 50% (within 5pp)
    # Caution: Do not report a crossover unless the closest CEAC point is within 5 percentage points of 50%.
    if (abs(prob_ce[crossover_idx] - 0.5) < 0.05) {
      crossover_wtp <- wtp_range[crossover_idx]
    }
  }

  list(
    counts        = counts,
    percentages   = pct,
    n_iterations  = n,
    n_on_axis     = on_axis,
    dominance_pct = unname(pct["SE"]),
    crossover_wtp = crossover_wtp
  )
}


#' Cost-effectiveness acceptability (CEAC) + frontier (CEAF) by WTP, tabular
#'
#' Consolidates the CEAC (probability a strategy is cost-effective) and the CEAF
#' (which strategy sits on the cost-effectiveness frontier, i.e. maximises expected
#' net monetary benefit) into one WTP-indexed table. Two-strategy case (Standard
#' Care vs Exercise): Exercise is on the frontier at a given WTP iff its expected
#' incremental net monetary benefit is >= 0.
#'
#' GUIDELINE: Fenwick, Claxton & Sculpher (2001); Briggs, Claxton & Sculpher (2006)
#'   - CEAC = P(strategy cost-effective); CEAF = frontier (max expected NMB).
#' GUIDELINE: INMB estimator kept consistent with summarise_psa() and
#'   build_ceac_table() (expected INMB / ratio-of-means; internal-consistency).
#' @param inc_costs Numeric vector. Incremental costs (Exercise - Standard) per PSA iteration.
#' @param inc_qalys Numeric vector. Incremental QALYs (Exercise - Standard) per PSA iteration.
#' @param wtps Numeric vector. WTP thresholds (NOK/QALY) to evaluate (all >= 0).
#' @return data.frame(wtp, prob_ce, expected_inmb, frontier). prob_ce is the CEAC
#'   probability the exercise strategy is cost-effective; frontier is the CEAF strategy.
compute_ceac_ceaf_by_wtp <- function(inc_costs, inc_qalys, wtps) {
  stopifnot(
    is.numeric(inc_costs), is.numeric(inc_qalys),
    length(inc_costs) == length(inc_qalys), length(inc_costs) > 0,
    is.numeric(wtps), length(wtps) > 0, all(wtps >= 0)
  )
  do.call(rbind, lapply(wtps, function(w) {
    # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
    inb <- inc_qalys * w - inc_costs
    # Drop NA explicitly (no na.rm inside CE computations).
    valid <- inb[!is.na(inb)]
    if (length(valid) == 0) {
      stop("compute_ceac_ceaf_by_wtp: all INB values NA at WTP ", w)
    }
    # GUIDELINE: Define the CEAC as the probability that a strategy is cost-effective (Fenwick et al. 2001)
    prob_ce  <- mean(valid >= 0)   # CEAC: P(Exercise cost-effective vs Standard Care)
    # GUIDELINE: Use expected INMB to identify the frontier strategy (Fenwick et al. 2001)
    mean_inb <- mean(valid)        # expected INMB
    data.frame(
      wtp           = w,
      prob_ce       = prob_ce,
      expected_inmb = mean_inb,
      # CEAF: 2-strategy frontier - Exercise optimal iff expected INMB >= 0.
      # GUIDELINE: Select the cost-effectiveness frontier by maximum expected NMB (Fenwick et al. 2001)
      frontier      = if (mean_inb >= 0) "Exercise" else "Standard Care",
      stringsAsFactors = FALSE
    )
  }))
}


# --- One-Way Probabilistic SA (OWPSA) ----------------------------------------

#' Run One-Way Probabilistic Sensitivity Analysis (OWPSA)
#'
#' "Fix one parameter at a specific value, then run PSA
#' on all remaining parameters." This is more sophisticated than standard
#' deterministic one-way SA because it preserves parameter correlations
#' and uncertainty in all non-fixed parameters.
#'
#' @param param_name Character. Name of parameter to fix.
#' @param param_values Numeric vector. Values to test (e.g., low/high).
#' @param n_sim Integer. PSA iterations per fixed value.
#' @param params Named list. Model parameters (passed to run_psa).
#' @return Data frame with columns: param_name, param_value,
#'   expected_icer, prob_ce, expected_inb.
run_owpsa <- function(param_name, param_values, n_sim = NULL,
                       params = NULL,
                       fit_dfs_ctrl = NULL, fit_os_ctrl = NULL,
                       s_genpop = NULL, discount_weights = NULL) {
  # n_sim default is NULL for lazy resolution (encapsulation)
  if (is.null(n_sim)) {
    if (exists("n_psa", envir = globalenv())) {
      n_sim <- get("n_psa", envir = globalenv())
    } else {
      stop("run_owpsa: n_sim must be provided or n_psa must exist in global env.")
    }
  }

  # CRN is achieved via set.seed(42) inside sample_psa_parameters().
  # Each run_psa() call uses the same seed, so all non-fixed parameters
  # have IDENTICAL draws across OWPSA runs. The pre-generated base_psa_params
  # was dead code (generated but never used by run_psa). Removed.
  #
  # Use lapply + do.call(rbind, ...) instead of growing
  # data frame via rbind() in a loop (O(n^2) memory anti-pattern).

  results_list <- lapply(param_values, function(val) {
    # Run PSA with fixed_params: seed=42 ensures CRN across runs.
    # The fixed_params mechanism in sample_psa_parameters() overrides
    # the target column AFTER all random draws complete, so all
    # non-fixed parameters use identical random streams.
    fixed <- setNames(list(val), param_name)
    psa_out <- run_psa(n_sim = n_sim, params = params,
                        # Caution: Keep seed 42 fixed across OWPSA runs so non-fixed draws remain common random numbers.
                        fixed_params = fixed, seed = 42,
                        fit_dfs_ctrl = fit_dfs_ctrl,
                        fit_os_ctrl = fit_os_ctrl,
                        s_genpop = s_genpop,
                        discount_weights = discount_weights)
    summ <- summarise_psa(psa_out)

    data.frame(
      param_name    = param_name,
      param_value   = val,
      expected_icer = summ$expected_icer,
      prob_ce       = summ$prob_ce,
      expected_inb  = summ$expected_inb
    )
  })

  do.call(rbind, results_list)
}


# --- EVPPI via GAM Regression -------------------------------------------------

#' Compute Expected Value of Partial Perfect Information (EVPPI)
#'
#' Uses the GAM regression method per Strong, Oakley, and Brennan (2014),
#' "Estimating Multiparameter Partial Expected Value of Perfect Information
#' from a Probabilistic Sensitivity Analysis Sample: A Nonparametric
#' Regression Approach," Medical Decision Making 34(3):311-326.
#'
#' Implementation follows the voi R package (Heath et al., 2024) formulation:
#' for each parameter phi, fit INB ~ s(phi) via mgcv::gam, then
#' EVPPI(phi) = mean(pmax(fitted, 0)) - max(mean(INB), 0).
#'
#' REFERENCE CODE: voi package (reference-code/github-repos/voi/R/evppi_npreg.R
#' lines 130-133); DARTH crcCEA (reference-code/github-repos/crcCEA/
#' analysis/05c_value_of_information.R).
#'
#' @param psa_results Data frame from run_psa() with inc_qalys, inc_costs.
#' @param psa_draws Data frame of PSA parameter draws (one column per parameter,
#'   same number of rows as psa_results). If NULL, read from the exact frame
#'   attached by run_psa(); outcomes and EVPPI must consume identical draws.
#' @param params_to_test Character vector. Column names in psa_draws to test.
#' @param wtp Numeric. WTP threshold for INB computation.
#' @return Data frame with columns: parameter, evppi, pct_evpi.
compute_evppi <- function(psa_results, psa_draws = NULL,
                          params_to_test = NULL, wtp = NULL) {
  # NULL default with lazy resolution for encapsulation
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("compute_evppi: wtp must be provided or wtp_threshold must exist.")
    }
  }

  if (is.null(psa_draws)) {
    psa_draws <- attr(psa_results, "psa_params")
    if (is.null(psa_draws)) {
      stop("compute_evppi: exact consumed psa_params attribute is required")
    }
  }

  # Default parameters to test: key uncertain parameters
  if (is.null(params_to_test)) {
    params_to_test <- intersect(
      # The EVPPI parameter set is pre-specified, not selected post hoc.
      c("HR_OS", "HR_DFS", "u_dfs", "u_prog", "c_surveillance_early",
        "c_exercise_annual", "c_progressed_annual", "c_terminal",
        "c_intervention_setup"),
      names(psa_draws)
    )
  }

  # INB at the reference WTP
  # GUIDELINE: INB = inc_qalys * WTP - inc_costs (Briggs, Claxton, Sculpher 2006)
  inb <- psa_results$inc_qalys * wtp - psa_results$inc_costs
  mean_inb <- mean(inb)

  # EVPI for reference (denominator for pct_evpi)
  # GUIDELINE: EVPI = E[max(INB, 0)] - max(E[INB], 0) for 2 strategies
  evpi <- mean(pmax(inb, 0)) - max(mean_inb, 0)

  # GAM regression EVPPI per parameter
  # SOURCE: Strong, Oakley, Brennan (2014), MDM 34(3):311-326
  # REFERENCE CODE: voi::calc_evppi (line 132): mean(apply(fit, 1, max)) - max(colMeans(fit))
  # For 2 strategies with INB: EVPPI = mean(pmax(fitted_inb, 0)) - max(mean_inb, 0)
  results <- lapply(params_to_test, function(par) {
    draws_par <- psa_draws[[par]]
    if (is.null(draws_par) || all(is.na(draws_par))) {
      return(data.frame(parameter = par, evppi = NA_real_, pct_evpi = NA_real_))
    }
    # mgcv::gam with default thin plate regression splines
    # GUIDELINE: Estimate EVPPI by nonparametric GAM regression of INB on the parameter (Strong et al. 2014)
    gam_fit <- mgcv::gam(inb ~ s(draws_par))
    fitted_inb <- gam_fit$fitted.values

    # GUIDELINE: EVPPI formula per Strong et al. (2014) and voi package
    # For 2 strategies: NMB matrix = [fitted_inb, 0] (exercise vs control)
    # EVPPI = mean(pmax(fitted_inb, 0)) - max(mean_inb, 0)
    evppi_val <- mean(pmax(fitted_inb, 0)) - max(mean_inb, 0)
    # Caution: Clamp regression noise below zero because EVPPI cannot be negative.
    evppi_val <- max(0, evppi_val)  # EVPPI cannot be negative

    data.frame(
      parameter = par,
      evppi = round(evppi_val),
      pct_evpi = round(evppi_val / evpi * 100, 1)
    )
  })

  result_df <- do.call(rbind, results)

  grouped_evppi <- function(matrix_draws, label, prefix) {
    if (!is.matrix(matrix_draws) || nrow(matrix_draws) != length(inb)) {
      stop("compute_evppi: grouped coefficient frame is absent or misaligned")
    }
    group_data <- data.frame(inb = inb, matrix_draws, check.names = FALSE)
    coef_names <- sprintf("%s__%03d", prefix, seq_len(ncol(matrix_draws)))
    names(group_data)[-1] <- coef_names
    smooth <- paste(coef_names, collapse = ",")
    # GUIDELINE: Estimate grouped EVPPI by nonparametric GAM regression (Strong et al. 2014)
    fit <- mgcv::gam(as.formula(paste0("inb ~ s(", smooth, ")")),
                     data = group_data)
    # GUIDELINE: Compute EVPPI from conditional expected net benefit (Strong et al. 2014)
    value <- max(0, mean(pmax(fit$fitted.values, 0)) - max(mean_inb, 0))
    data.frame(parameter = label, evppi = round(value),
               pct_evpi = round(value / evpi * 100, 1))
  }
  result_df <- rbind(
    result_df,
    grouped_evppi(attr(psa_draws, "surv_coef_dfs"),
                  "surv_coef_dfs (grouped)", "surv_coef_dfs"),
    grouped_evppi(attr(psa_draws, "surv_coef_os"),
                  "surv_coef_os (grouped)", "surv_coef_os")
  )

  # Reviewed secondary combination; both HR draws retain their primary rows.
  # GUIDELINE: Strong, Oakley, Brennan (2014), Section 3.2.
  # IMPLEMENTATION: 2D tensor product smooth te() via mgcv, appropriate for
  #   parameters on different scales (log-HR) with unknown correlation structure
  #   in the INB response surface.
  if (all(c("HR_OS", "HR_DFS") %in% names(psa_draws))) {
    hr_os_draws  <- psa_draws[["HR_OS"]]
    hr_dfs_draws <- psa_draws[["HR_DFS"]]
    if (!is.null(hr_os_draws) && !is.null(hr_dfs_draws) &&
        !all(is.na(hr_os_draws)) && !all(is.na(hr_dfs_draws))) {
      # GUIDELINE: Model multiparameter EVPPI with a joint smooth response surface (Strong et al. 2014)
      gam_joint <- mgcv::gam(inb ~ te(hr_os_draws, hr_dfs_draws))
      fitted_joint <- gam_joint$fitted.values
      # GUIDELINE: Compute joint EVPPI from conditional expected net benefit (Strong et al. 2014)
      evppi_joint <- mean(pmax(fitted_joint, 0)) - max(mean_inb, 0)
      # Caution: Clamp regression noise below zero because EVPPI cannot be negative.
      evppi_joint <- max(0, evppi_joint)
      joint_row <- data.frame(
        parameter = "HR_OS + HR_DFS (joint)",
        # Reader-facing derived values use the displayed rounding convention.
        evppi = round(evppi_joint),
        pct_evpi = round(evppi_joint / evpi * 100, 1)
      )
      result_df <- rbind(result_df, joint_row)
      message("  Joint EVPPI (HR_OS + HR_DFS): ", round(evppi_joint),
              " NOK (", round(evppi_joint / evpi * 100, 1), "% of EVPI)")
    }
  }

  expected_estimands <- c(
    "HR_OS", "HR_DFS", "u_dfs", "u_prog", "c_surveillance_early",
    "c_exercise_annual", "c_progressed_annual", "c_terminal",
    "c_intervention_setup", "surv_coef_dfs (grouped)",
    "surv_coef_os (grouped)", "HR_OS + HR_DFS (joint)")
  if (!identical(result_df$parameter, expected_estimands)) {
    stop("compute_evppi: EVPPI registry is not the exact 12-estimand contract")
  }
  attr(result_df, "evpi") <- round(evpi)
  attr(result_df, "wtp") <- wtp
  attr(result_df, "method") <- "GAM regression (Strong, Oakley, Brennan 2014)"
  result_df
}

make_specification_frame <- function(reference_bundle, rho,
                                     u_prog_se_value = NULL) {
  stopifnot(is.list(reference_bundle), is.data.frame(reference_bundle$frame))
  if (identical(as.double(rho), 0) && is.null(u_prog_se_value)) {
    return(reference_bundle$frame)
  }
  coupled <- rank_couple_endpoint_blocks(
    reference_bundle$dfs_block, reference_bundle$os_block,
    reference_bundle$z_dfs, reference_bundle$epsilon_os, rho)
  frame <- reference_bundle$frame
  # GUIDELINE: Exponentiate normally sampled log hazard ratios (Briggs et al. 2006)
  frame$HR_DFS <- exp(coupled$dfs$log_HR_DFS)
  frame$HR_OS <- exp(coupled$os$log_HR_OS)
  if (!is.null(u_prog_se_value)) {
    shape <- beta_params(u_prog_mean, u_prog_se_value)
    # GUIDELINE: Represent bounded utility uncertainty with a beta distribution (Briggs et al. 2006)
    frame$u_prog <- qbeta(reference_bundle$u_prog_uniform,
                          shape$alpha, shape$beta)
  }
  cross_corr <- cor(coupled$dfs[, c("dfs_meanlog", "dfs_sdlog")],
                    coupled$os[, c("os_meanlog", "os_sdlog")])
  diagnostics <- list(
    target_rho = as.double(rho),
    pearson_log_hr = cor(coupled$dfs$log_HR_DFS, coupled$os$log_HR_OS),
    spearman_log_hr_diagnostic = cor(coupled$dfs$log_HR_DFS,
                                     coupled$os$log_HR_OS,
                                     method = "spearman"),
    max_abs_cross_coefficient_correlation = max(abs(cross_corr)),
    sorted_endpoint_hash_dfs = endpoint_block_hash(
      reference_bundle$dfs_block, "dfs"),
    sorted_endpoint_hash_os = endpoint_block_hash(
      reference_bundle$os_block, "os"),
    draw_order_version = PSA_DRAW_ORDER_VERSION,
    rng_tuple = RNGkind())
  base_row_names <- attr(frame, "row.names")
  attributes(frame) <- list(
    names = names(frame), class = "data.frame", row.names = base_row_names,
    surv_coef_dfs = as.matrix(coupled$dfs[, c("dfs_meanlog", "dfs_sdlog")]),
    surv_coef_os = as.matrix(coupled$os[, c("os_meanlog", "os_sdlog")]),
    endpoint_row_id_dfs = coupled$dfs$row_id,
    endpoint_row_id_os = coupled$os$row_id,
    dependence_diagnostics = diagnostics)
  stopifnot(identical(names(frame), PRIMARY_REFERENCE_PUBLIC_COLUMNS_V1),
            identical(names(attributes(frame)),
                      PRIMARY_REFERENCE_PUBLIC_ATTRIBUTES_V1))
  frame
}

#' Execute the six-row PSA-specification sensitivity registry.
run_psa_specification_sensitivity <- function(
    reference_bundle, primary_psa_results, primary_evppi,
    fit_dfs_ctrl, fit_os_ctrl, s_genpop, discount_weights, wtp,
    n_cyc = n_cycles, cyc_length = cycle_length,
    tolerance = 1e-10) {
  if (nrow(reference_bundle$frame) != 10000L) {
    stop("run_psa_specification_sensitivity: exact N=10000 reference frame required")
  }
  registry <- data.frame(
    scenario_id = PSA_SPECIFICATION_SCENARIO_IDS_V1,
    scenario_label = c(
      "Reference: rho 0", "Dependence: rho 0.3", "Dependence: rho 0.5",
      "Dependence: rho 0.7", "Dependence: rho 0.9",
      "Mapping RMSE in quadrature: rho 0"),
    rho_endpoint_latent = c(0, 0.3, 0.5, 0.7, 0.9, 0),
    u_prog_uncertainty = c(rep("Sampling SE only", 5L),
                           "Sampling SE + mapping RMSE in quadrature"),
    stringsAsFactors = FALSE)
  evppi_columns <- c(
    HR_OS = "evppi_hr_os", HR_DFS = "evppi_hr_dfs",
    u_dfs = "evppi_u_dfs", u_prog = "evppi_u_prog",
    c_surveillance_early = "evppi_c_surveillance_early",
    c_exercise_annual = "evppi_c_exercise_annual",
    c_progressed_annual = "evppi_c_progressed_annual",
    c_terminal = "evppi_c_terminal",
    c_intervention_setup = "evppi_c_intervention_setup",
    `surv_coef_dfs (grouped)` = "evppi_surv_coef_dfs_grouped",
    `surv_coef_os (grouped)` = "evppi_surv_coef_os_grouped",
    `HR_OS + HR_DFS (joint)` = "evppi_joint_hr")
  rows <- lapply(seq_len(nrow(registry)), function(i) {
    mapping_row <- registry$scenario_id[i] == "mapping_rmse_quadrature_rho0"
    frame <- make_specification_frame(
      reference_bundle, registry$rho_endpoint_latent[i],
      if (mapping_row) u_prog_se_mapping_sensitivity else NULL)
    out <- run_psa(
      n_sim = 10000L, fit_dfs_ctrl = fit_dfs_ctrl,
      fit_os_ctrl = fit_os_ctrl, s_genpop = s_genpop,
      discount_weights = discount_weights, wtp = wtp,
      n_cyc = n_cyc, cyc_length = cyc_length,
      psa_params = frame)
    summary <- summarise_psa(out, wtp = wtp)
    convergence <- assess_convergence_inmb(out, wtp)
    convergence_n <- attr(convergence, "first_converged_at")
    if (!is.finite(convergence_n)) {
      stop("PSA specification row failed the primary convergence rule: ",
           registry$scenario_id[i])
    }
    evppi <- compute_evppi(out, psa_draws = frame, wtp = wtp)
    diag <- attr(frame, "dependence_diagnostics")
    row <- data.frame(
      scenario_id = registry$scenario_id[i],
      scenario_label = registry$scenario_label[i],
      rho_endpoint_latent = registry$rho_endpoint_latent[i],
      u_prog_uncertainty = registry$u_prog_uncertainty[i],
      mean_incremental_cost = mean(out$inc_costs),
      mean_incremental_qaly = mean(out$inc_qalys),
      icer_ratio_of_means = summary$expected_icer,
      probability_cost_effective = summary$prob_ce,
      sd_incremental_cost = sd(out$inc_costs),
      sd_incremental_qaly = sd(out$inc_qalys),
      cor_incremental_cost_qaly = cor(out$inc_costs, out$inc_qalys),
      primary_convergence_n = as.integer(convergence_n),
      evpi_per_patient = as.double(attr(evppi, "evpi")),
      pearson_log_hr = diag$pearson_log_hr,
      spearman_log_hr = diag$spearman_log_hr_diagnostic,
      max_abs_cross_coefficient_correlation =
        diag$max_abs_cross_coefficient_correlation,
      stringsAsFactors = FALSE)
    for (parameter in names(evppi_columns)) {
      row[[evppi_columns[[parameter]]]] <-
        evppi$evppi[match(parameter, evppi$parameter)]
    }
    row
  })
  result <- do.call(rbind, rows)
  exact_schema <- c(
    "scenario_id", "scenario_label", "rho_endpoint_latent",
    "u_prog_uncertainty", "mean_incremental_cost", "mean_incremental_qaly",
    "icer_ratio_of_means", "probability_cost_effective",
    "sd_incremental_cost", "sd_incremental_qaly",
    "cor_incremental_cost_qaly", "primary_convergence_n",
    "evpi_per_patient", "evppi_hr_os", "evppi_hr_dfs", "evppi_u_dfs",
    "evppi_u_prog", "evppi_c_surveillance_early",
    "evppi_c_exercise_annual", "evppi_c_progressed_annual",
    "evppi_c_terminal", "evppi_c_intervention_setup",
    "evppi_surv_coef_dfs_grouped", "evppi_surv_coef_os_grouped",
    "evppi_joint_hr", "pearson_log_hr", "spearman_log_hr",
    "max_abs_cross_coefficient_correlation")
  result <- result[, exact_schema]
  if (any(!is.finite(as.matrix(result[, vapply(result, is.numeric,
                                                logical(1))])))) {
    stop("run_psa_specification_sensitivity: non-finite summary field")
  }
  primary_summary <- summarise_psa(primary_psa_results, wtp = wtp)
  reference_values <- unlist(result[1, c(
    "mean_incremental_cost", "mean_incremental_qaly",
    "icer_ratio_of_means", "probability_cost_effective")], use.names = FALSE)
  primary_values <- c(
    primary_summary$expected_inc_cost, primary_summary$expected_inc_qaly,
    primary_summary$expected_icer, primary_summary$prob_ce)
  if (any(abs(reference_values - primary_values) > tolerance)) {
    stop("run_psa_specification_sensitivity: rho-0 reference did not reproduce primary PSA")
  }
  primary_evppi_values <- primary_evppi$evppi[match(
    names(evppi_columns), primary_evppi$parameter)]
  reference_evppi_values <- unlist(result[1, unname(evppi_columns)],
                                   use.names = FALSE)
  if (!identical(primary_evppi$parameter, names(evppi_columns)) ||
      any(abs(reference_evppi_values - primary_evppi_values) > tolerance)) {
    stop("run_psa_specification_sensitivity: rho-0 EVPPI did not reproduce primary EVPPI")
  }
  reference_hash <- canonical_object_hash_v1(
    reference_bundle$frame, "primary_reference_frame")
  uniform_rows <- data.frame(
    position = seq_along(reference_bundle$u_prog_uniform),
    u_prog_uniform = as.double(reference_bundle$u_prog_uniform),
    check.names = FALSE)
  uniform_hash <- canonical_hash_v1(
    uniform_rows, c(position = "integer", u_prog_uniform = "double"))
  provenance <- list(
    scenario_summaries = result,
    psa_seed_reference = as.integer(psa_seed_reference),
    dependence_coupling_seed = as.integer(dependence_coupling_seed),
    rng_tuple = RNGkind(), draw_order_version = PSA_DRAW_ORDER_VERSION,
    primary_reference_frame_hash = reference_hash,
    u_prog_uniform_hash = uniform_hash,
    sorted_endpoint_hash_dfs = endpoint_block_hash(
      reference_bundle$dfs_block, "dfs"),
    sorted_endpoint_hash_os = endpoint_block_hash(
      reference_bundle$os_block, "os"))
  saveRDS(provenance, "data/processed/psa_specification_sensitivity.rds")
  write.csv(result, "output/tables/psa-specification-sensitivity.csv",
            row.names = FALSE, na = "")
  result
}


# --- Population EVPI with Technology Horizon Discounting ---------------------

#' Compute Discounted Population EVPI over Technology Decision Horizon
#'
#' Implements the Briggs, Claxton, Sculpher (2006) population EVPI formula:
#'   EVPI_pop = EVPI x Sum_{t=1}^{T} (I_t / (1+r)^t)
#' where I_t is the modelled annual cohort, r is the discount rate, and T is
#' the technology decision horizon in years.
#'
#' Fenwick et al. (2020), p.145, Box 4: discounting is mandatory for multi-year
#' population EVPI. Fenwick et al. (2020), p.144, GPR 3: the technology horizon
#' must be justified and alternative horizons explored in scenario analyses.
#'
#' For constant annual incidence I, the summation reduces to:
#'   EVPI_pop = EVPI x I x a_T, where a_T = (1 - (1+r)^(-T)) / r
#'
#' @param evpi_per_patient Numeric. Per-patient EVPI in NOK.
#' @param annual_cohort Integer. Modelled annual operated stage-mix cohort size.
#' @param discount_rate Numeric. Annual discount rate (default 0.04 per R-109).
#' @param horizons Integer vector. Technology horizons in years to evaluate.
#' @return Data frame with columns: horizon_years, annuity_factor, pop_evpi,
#'   undiscounted. Attributes: evpi_per_patient, annual_cohort, discount_rate.
#'
#' GUIDELINE: Briggs, Claxton, Sculpher (2006), p.176, Section 6.2.1
#' GUIDELINE: Fenwick et al. (2020), p.144-145, GPR 3, Box 4
#' Technology horizon T=10 base case, sensitivity 5/20
compute_pop_evpi_horizon <- function(evpi_per_patient, annual_cohort,
    discount_rate = 0.04,
    # GUIDELINE: Justify the technology horizon and test alternatives (Fenwick et al. 2020)
    horizons = c(5, 10, 20)) {
  results <- lapply(horizons, function(T_horizon) {
    # GUIDELINE: Discount future eligible cohorts in population EVPI (Briggs et al. 2006)
    annuity_factor <- (1 - (1 + discount_rate)^(-T_horizon)) / discount_rate
    # GUIDELINE: Scale per-patient EVPI over the discounted eligible population (Briggs et al. 2006)
    pop_evpi <- evpi_per_patient * annual_cohort * annuity_factor
    data.frame(
      horizon_years = T_horizon,
      annuity_factor = annuity_factor,
      pop_evpi = pop_evpi,
      undiscounted = evpi_per_patient * annual_cohort * T_horizon
    )
  })
  result_df <- do.call(rbind, results)
  attr(result_df, "evpi_per_patient") <- evpi_per_patient
  attr(result_df, "annual_cohort") <- annual_cohort
  attr(result_df, "discount_rate") <- discount_rate
  result_df
}


# --- PSA Convergence Diagnostics (ADOPT A5) ----------------------------------

#' Assess PSA Convergence via INMB Confidence Interval Method
#'
#' Implements the preferred convergence assessment from Hatswell et al. (2018,
#' PharmacoEconomics 36(12):1421-1426, Key Points p.1421): "running of the
#' model until the 95% confidence interval for the incremental net monetary
#' benefit does not include zero." Hatswell critiques the broader category of
#' visual aids for convergence determination, not only cumulative mean ICER
#' plots; the wording at p.1424 is: "interpretation of visual aids
#' is subjective and may imply stability (or instability), dependent on the
#' scales of the plot chosen." This is the language quoted in the convergence
#' caption and the methods chapter. The correct
#' attribution ascribes not only the narrower cumulative-mean ICER critique
#' to Hatswell, but the broader category of visual aids for convergence.
#'
#' @param psa_results Data frame from run_psa() with inc_qalys and inc_costs.
#' @param wtp Willingness-to-pay threshold (NOK per QALY).
#' @param sample_sizes Vector of sample sizes to evaluate convergence at.
#' @return Data frame with columns: n, mean_inb, se, ci_lower, ci_upper,
#'   ci_excludes_zero. Attributes: first_converged_at, wtp, method.
#'
#' GUIDELINE: Hatswell et al. (2018), PharmacoEconomics 36(12):1421-1426
assess_convergence_inmb <- function(psa_results, wtp,
    sample_sizes = c(100, 500, 1000, 2000, 5000, 10000)) {
  # GUIDELINE: Hatswell et al. (2018), Key Points, p.1421
  # Preferred method: 95% CI of INMB excludes zero
  inb <- psa_results$inc_qalys * wtp - psa_results$inc_costs
  results <- lapply(sample_sizes, function(k) {
    inb_k <- inb[1:min(k, length(inb))]
    # GUIDELINE: Assess PSA convergence with the 95% confidence interval for INMB (Hatswell et al. 2018)
    mean_inb <- mean(inb_k)
    se_inb <- sd(inb_k) / sqrt(length(inb_k))
    ci_lower <- mean_inb - qnorm(0.975) * se_inb
    # GUIDELINE: Assess PSA convergence with the 95% confidence interval for INMB (Hatswell et al. 2018)
    ci_upper <- mean_inb + qnorm(0.975) * se_inb
    data.frame(n = k, mean_inb = mean_inb, se = se_inb,
               ci_lower = ci_lower, ci_upper = ci_upper,
               ci_excludes_zero = (ci_lower > 0) | (ci_upper < 0))
  })
  convergence_table <- do.call(rbind, results)
  first_converged <- min(convergence_table$n[convergence_table$ci_excludes_zero],
                         Inf)
  attr(convergence_table, "first_converged_at") <- first_converged
  attr(convergence_table, "wtp") <- wtp
  attr(convergence_table, "method") <- "Hatswell et al. (2018) INMB CI"
  convergence_table
}


#' Plot PSA Convergence (Cumulative Mean ICER)
#'
#' Visual diagnostic to assess whether PSA has converged.
#' Plots the cumulative mean ICER as a function of iteration number.
#' A stable line at high iteration counts indicates convergence.
#'
#' @param psa_results Data frame from run_psa().
#' @return ggplot object.
plot_psa_convergence <- function(psa_results) {
  # Both the cumulative convergence LINE and the reference LINE
  # now use the ratio-of-means estimator. The old code used:
  #   cumsum(valid$icer) / seq_len(nrow(valid))  [WRONG: mean-of-ratios]
  # for the line, and ratio-of-means for the reference. This inconsistency
  # made the line converge to a different value than the reference,
  # falsely suggesting non-convergence.
  #
  # Filter on finite inc_qalys (not finite icer) to avoid
  # dropping valid iterations where icer happens to be Inf/-Inf due to
  # near-zero denominators.
  valid <- psa_results[is.finite(psa_results$inc_qalys) &
                        psa_results$inc_qalys != 0, ]

  # Cumulative ratio-of-means: cumsum(costs) / cumsum(qalys)
  # GUIDELINE: Keep cumulative ICER diagnostics on the ratio-of-means estimator (Briggs et al. 2006)
  valid$cum_mean_icer <- cumsum(valid$inc_costs) /
                          cumsum(valid$inc_qalys)
  valid$valid_iteration <- seq_len(nrow(valid))

  # Reference line: ratio-of-means across ALL iterations
  # GUIDELINE: Estimate the expected ICER as the ratio of means (Briggs et al. 2006)
  expected_icer_rom <- mean(psa_results$inc_costs) /
                       mean(psa_results$inc_qalys)

  col_ref  <- if (exists("pal")) pal$highlight else "#D55E00"

  conv_ann_size <- if (exists("ann_size_standard")) ann_size_standard else 8.0
  conv_padding  <- if (exists("ann_label_padding")) ann_label_padding else
                     ggplot2::unit(0.15, "lines")

  p <- ggplot2::ggplot(valid, ggplot2::aes(x = valid_iteration,
                                       y = cum_mean_icer)) +
    # Convergence line: map BOTH colour and linetype for merged legend
    ggplot2::geom_line(
      ggplot2::aes(colour = "Cumulative mean ICER",
                   linetype = "Cumulative mean ICER"),
      linewidth = 0.6
    ) +
    # Expected ICER reference: map BOTH colour and linetype
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = expected_icer_rom,
                   colour = "Expected ICER (ratio of means)",
                   linetype = "Expected ICER (ratio of means)"),
      linewidth = 0.8
    ) +
    # Both aesthetics mapped to same names → single merged legend
    ggplot2::scale_colour_manual(
      values = c("Cumulative mean ICER" = "#333333",
                 "Expected ICER (ratio of means)" = col_ref),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Cumulative mean ICER" = "solid",
                 "Expected ICER (ratio of means)" = "dashed"),
      name = NULL
    ) +
    # SOURCE: the converged expected ICER is printed in the subtitle, so the
    # axis must carry labelled breaks around it; the default break algorithm
    # labelled only 0 and -50,000, leaving the converged region unlabelled
    # and the value unreadable off the panel.
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title    = "PA Convergence: Cumulative Mean ICER",
      subtitle = paste0("n = ", format(nrow(valid), big.mark = ","),
                        " valid iterations | Expected ICER (ratio of means) = ",
                        format(round(expected_icer_rom), big.mark = ",")),
      x        = "Valid PA Iteration",
      y        = "Cumulative Mean ICER (NOK/QALY)"
    ) +
    theme_thesis()

  conv_caption <- paste0(
    "PSA Convergence Diagnostic. Cumulative mean ICER (ratio of means) ",
    "across ", format(nrow(valid), big.mark = ","), " valid PSA iterations. ",
    "Expected ICER = NOK ", format(round(expected_icer_rom), big.mark = ","),
    "/QALY (dashed reference). ",
    "(Extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(conv_caption, "\n\n")
  attr(p, "caption") <- conv_caption
  p
}


# --- Expected Loss Curves (ADOPT A4) ----------------------------------------

#' Calculate Expected Loss
#'
#' Uses dampack::calc_exp_loss() to compute the expected loss
#' (opportunity cost of choosing the wrong strategy) across WTP values.
#' This is a complement to the CEAC, recommended by Fenwick et al. (2020).
#'
#' dampack::calc_exp_loss() expects TOTAL (absolute) costs and effects
#' per strategy per iteration, not incremental values. The PSA results
#' now store total values per arm (total_costs_int, total_costs_ctrl,
#' total_qalys_int, total_qalys_ctrl) to satisfy this API requirement.
#'
#' @param psa_results Data frame from run_psa(). Must contain columns:
#'   total_costs_int, total_costs_ctrl, total_qalys_int, total_qalys_ctrl.
#' @param wtp_range Numeric vector of WTP thresholds.
#' @return Result from dampack::calc_exp_loss().
calculate_expected_loss <- function(psa_results, wtp_range) {
  required_cols <- c("total_costs_int", "total_costs_ctrl",
                     "total_qalys_int", "total_qalys_ctrl")
  missing_cols <- setdiff(required_cols, names(psa_results))
  if (length(missing_cols) > 0) {
    stop("calculate_expected_loss: PSA results missing total-value columns: ",
         paste(missing_cols, collapse = ", "),
         ". Ensure run_psa() stores total costs/QALYs per arm.")
  }

  # dampack 1.0.2.1000 requires a psa object via make_psa_obj()
  # Rows = PSA iterations, Columns = strategies
  cost_matrix <- cbind(
    `Standard Care` = psa_results$total_costs_ctrl,
    Exercise        = psa_results$total_costs_int
  )
  effect_matrix <- cbind(
    `Standard Care` = psa_results$total_qalys_ctrl,
    Exercise        = psa_results$total_qalys_int
  )

  # GUIDELINE: Construct strategy-level PSA inputs for expected-loss analysis (Alarid-Escudero et al. 2019)
  psa_obj <- dampack::make_psa_obj(
    cost          = cost_matrix,
    effectiveness = effect_matrix,
    strategies    = c("Standard Care", "Exercise"),
    currency      = "NOK"
  )
  # GUIDELINE: Derive expected-loss curves from strategy-level PSA outcomes (Alarid-Escudero et al. 2019)
  dampack::calc_exp_loss(psa = psa_obj, wtp = wtp_range)
}


# --- Conditional execution ---------------------------------------------------
# Mirrors pattern from 03-build-psm.R and 04-costs-qalys.R: conditional
# execution when prerequisites are met, placeholder messages otherwise.
# Guard unconditional message block from test environment.
# When sourced by testthat.R, these messages are noise and the global
# variable references (n_psa, wtp_threshold) could error if init order changes.
if (!identical(Sys.getenv("TESTTHAT"), "true")) {
  message("=== PSA Script (Base Case Analysis) ===")
  message("PSA is the base case analysis. Deterministic is supplementary.")
  message("n_psa = ", n_psa, " iterations | Threshold = ",
          format(wtp_threshold, big.mark = ","), " NOK/QALY")
  message("Functions loaded: run_psa(), run_owpsa(), summarise_psa()")
  message("                  plot_psa_convergence(), calculate_expected_loss()")
  message("                  sample_psa_parameters()")
}

if (!identical(Sys.getenv("TESTTHAT"), "true") &&
    file.exists("data/processed/survival_fits_dfs_ctrl.rds") &&
    file.exists("data/processed/survival_fits_os_ctrl.rds") &&
    !is.null(best_dist_dfs) && !is.null(best_dist_os) &&
    !is.na(u_dfs_mean) && !is.na(c_surveillance_early)) {

  psa_fits_dfs <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
  psa_fits_os  <- readRDS("data/processed/survival_fits_os_ctrl.rds")

  psa_s_genpop <- load_genpop_survival(
    life_table_path = life_table_path,
    entry_age       = cohort_age,
    n_cycles        = n_cycles,
    cycle_length    = cycle_length
  )

  psa_parameter_values <- list(
    log_HR_DFS = log_HR_DFS, se_log_HR_DFS = se_log_HR_DFS,
    log_HR_OS = log_HR_OS, se_log_HR_OS = se_log_HR_OS,
    u_dfs_mean = u_dfs_mean, u_dfs_se = u_dfs_se,
    u_prog_mean = u_prog_mean, u_prog_se = u_prog_se,
    u_exercise_decrement = u_exercise_decrement,
    c_surveillance_early = c_surveillance_early,
    c_surveillance_early_se = c_surveillance_early_se,
    c_surveillance_late = c_surveillance_late,
    c_exercise_annual = c_exercise_annual,
    c_exercise_annual_se = c_exercise_annual_se,
    c_progressed_annual = c_progressed_annual,
    c_progressed_annual_se = c_progressed_annual_se,
    c_terminal = c_terminal, c_terminal_se = c_terminal_se,
    c_intervention_setup = c_intervention_setup,
    c_intervention_setup_se = c_intervention_setup_se)
  primary_psa_bundle <- sample_psa_parameters(
    n = as.integer(n_psa), params = psa_parameter_values,
    seed = as.integer(psa_seed_reference),
    fit_dfs_ctrl = psa_fits_dfs[[best_dist_dfs]],
    fit_os_ctrl = psa_fits_os[[best_dist_os]],
    rho_endpoint_latent = rho_endpoint_latent_reference,
    return_bundle = TRUE)

  # The rho-0 frame is the one and only primary PSA frame.
  psa_results <- run_psa(
    n_sim        = n_psa,
    fit_dfs_ctrl = psa_fits_dfs[[best_dist_dfs]],
    fit_os_ctrl  = psa_fits_os[[best_dist_os]],
    s_genpop     = psa_s_genpop,
    discount_weights = discount_weights_stepped,
    psa_params = primary_psa_bundle$frame
  )

  saveRDS(psa_results, "data/processed/psa_results.rds")

  # Convergence diagnostic: compare 1,000 vs full 10,000 ICER (secondary
  # check, general practice). Primary convergence: INMB CI method
  # GUIDELINE: (Hatswell et al. 2018) via assess_convergence_inmb().
  psa_1k <- psa_results[1:1000, ]
  icer_1k <- mean(psa_1k$inc_costs) / mean(psa_1k$inc_qalys)
  icer_10k <- mean(psa_results$inc_costs) / mean(psa_results$inc_qalys)
  convergence <- list(
    icer_1k = icer_1k,
    icer_10k = icer_10k,
    pct_diff = abs(icer_1k - icer_10k) / icer_10k * 100,
    within_1pct = abs(icer_1k - icer_10k) / icer_10k < 0.01
  )
  saveRDS(convergence, "data/processed/convergence_check.rds")
  message(sprintf("Convergence: 1,000-draw ICER=%.0f, 10,000-draw ICER=%.0f, diff=%.2f%%",
                  icer_1k, icer_10k, convergence$pct_diff))

  # --- Programmatic caption for convergence table ---
  # SOURCE: convergence list computed above (icer_1k, icer_10k, pct_diff)
  # PSA is the base-case analysis method
  convergence_caption <- paste0(
    "PSA Convergence Diagnostic (primary: INMB CI method per Hatswell et al. ",
    "2018; secondary: ICER stability check). ",
    "Extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule. ",
    "Secondary check: ICER at ", format(nrow(psa_1k), big.mark = ","),
    " iterations: NOK ", format(round(icer_1k), big.mark = ","),
    "/QALY. ICER at ", format(n_psa, big.mark = ","),
    " iterations: NOK ", format(round(icer_10k), big.mark = ","),
    "/QALY. Difference: ", round(convergence$pct_diff, 2), "%. ",
    if (convergence$within_1pct) "Within 1% threshold." else "Exceeds 1% threshold."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(convergence_caption, "\n\n")
  attr(convergence, "caption") <- convergence_caption

  # --- Primary convergence: INMB CI method (Hatswell et al. 2018) ---
  # GUIDELINE: Hatswell et al. (2018), Key Points p.1421
  # INMB CI is the primary convergence method (Hatswell et al. 2018)
  inmb_convergence <- assess_convergence_inmb(psa_results, wtp_threshold)
  saveRDS(inmb_convergence, "data/processed/inmb_convergence.rds")
  first_conv <- attr(inmb_convergence, "first_converged_at")
  message(sprintf("INMB CI convergence: 95%% CI excludes zero from n=%s onward",
                  if (is.infinite(first_conv)) "NOT CONVERGED" else format(first_conv, big.mark = ",")))
  convergence$inmb_table <- inmb_convergence
  convergence$inmb_first_converged <- first_conv
  saveRDS(convergence, "data/processed/convergence_check.rds")

  psa_summary <- summarise_psa(psa_results)
  message("PSA base case complete: ", n_psa, " iterations.")
  message("Expected ICER (ratio of means): ",
          format(round(psa_summary$expected_icer), big.mark = ","))
  message("P(CE) at threshold = ", format(wtp_threshold, big.mark = ","),
          ": ", round(psa_summary$prob_ce * 100, 1), "%")

  # --- Programmatic caption for PSA summary table ---
  # SOURCE: psa_summary list from summarise_psa(psa_results) above
  # PSA is the base-case analysis method
  # GUIDELINE: ISPOR Good Practices, Briggs Claxton Sculpher 2006 Ch. 4
  psa_summary_caption <- paste0(
    "Probabilistic Sensitivity Analysis Results (N = ",
    format(psa_summary$n_iterations, big.mark = ","),
    " iterations, extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule). ",
    "Expected incremental cost: NOK ",
    format(round(psa_summary$expected_inc_cost), big.mark = ","), ". ",
    "Expected incremental QALY: ",
    round(psa_summary$expected_inc_qaly, 4), ". ",
    "Expected ICER (ratio of means): NOK ",
    format(round(psa_summary$expected_icer), big.mark = ","), "/QALY. ",
    "Probability cost-effective at threshold = NOK ",
    format(psa_summary$wtp_used, big.mark = ","), ": ",
    round(psa_summary$prob_ce * 100, 1), "%."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(psa_summary_caption, "\n\n")
  attr(psa_summary, "caption") <- psa_summary_caption

  # --- Quadrant analysis ---
  # SOURCE: Drummond et al. (2015), Black (1990), Krijkamp et al. (2018)
  # CHEERS-VOI Item 20: characterizing uncertainty
  quadrant_results <- compute_quadrant_analysis(psa_results,
                                                 wtp_range = wtp_range)
  message("Quadrant analysis: NE ", quadrant_results$percentages["NE"], "%, ",
          "SE (dominant) ", quadrant_results$percentages["SE"], "%, ",
          "NW (dominated) ", quadrant_results$percentages["NW"], "%, ",
          "SW ", quadrant_results$percentages["SW"], "%")
  if (!is.na(quadrant_results$crossover_wtp)) {
    message("Crossover threshold (P(CE) = 50%): ",
            format(quadrant_results$crossover_wtp, big.mark = ","), " NOK/QALY")
  }

  # --- Decision uncertainty ---
  # 1 - max(P_exercise, P_control) at reference WTP
  # For 2 strategies: min(prob_ce, 1 - prob_ce)
  # SOURCE: complement of CEAF at reference WTP (Fenwick et al. 2001)
  decision_uncertainty <- min(psa_summary$prob_ce,
                              1 - psa_summary$prob_ce)
  message("Decision uncertainty at threshold = ",
          format(wtp_threshold, big.mark = ","), ": ",
          round(decision_uncertainty * 100, 1), "%")

  # --- EVPPI computation ---
  # SOURCE: Strong, Oakley, Brennan (2014), MDM 34(3):311-326
  # GUIDELINE: CHEERS-VOI Item S3 (parameters of interest for EVPPI)
  # Reconstructs parameter draws using same seed for deterministic reproduction
  message("Computing EVPPI via GAM regression...")
  evppi_results <- compute_evppi(psa_results)
  saveRDS(evppi_results, "data/processed/evppi_results.rds")
  message("EVPPI saved: data/processed/evppi_results.rds")
  message("  EVPI: ", attr(evppi_results, "evpi"), " NOK")
  for (i in seq_len(nrow(evppi_results))) {
    message("  ", evppi_results$parameter[i], ": ",
            evppi_results$evppi[i], " NOK (",
            evppi_results$pct_evpi[i], "% of EVPI)")
  }

  psa_specification_sensitivity <- run_psa_specification_sensitivity(
    reference_bundle = primary_psa_bundle,
    primary_psa_results = psa_results,
    primary_evppi = evppi_results,
    fit_dfs_ctrl = psa_fits_dfs[[best_dist_dfs]],
    fit_os_ctrl = psa_fits_os[[best_dist_os]],
    s_genpop = psa_s_genpop,
    discount_weights = discount_weights_stepped,
    wtp = wtp_threshold)

  # --- EVPI curve regeneration ---
  # Recomputes the full WTP curve from the current PSA draws
  # using the same two-strategy EVPI formula as compute_evppi() above.
  # Ensures downstream consumers (build_evpi_table() Panel A in
  # 08-export-tables.R) trace to the same PSA source as EVPPI.
  # GUIDELINE: Briggs, Claxton, Sculpher (2006), Ch. 6 - two-strategy EVPI
  # SOURCE: psa_results (current PSA run)
  # WTP grid: 50K-825K by 25K, plus the computed applicable WTP (severity-informed
  # reference) and the opportunity-cost benchmark, so EVPI is evaluated at both.
  # DECISION: applicable WTP is programmatic.
  evpi_wtps <- sort(unique(c(seq(50000, 825000, by = 25000), wtp_threshold, opportunity_cost_nok)))
  evpi_curve <- data.frame(
    wtp = evpi_wtps,
    evpi = vapply(evpi_wtps, function(lambda) {
      # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
      inb_lambda <- psa_results$inc_qalys * lambda - psa_results$inc_costs
      # GUIDELINE: Compute two-strategy EVPI from expected maximum net benefit (Briggs et al. 2006)
      mean(pmax(inb_lambda, 0)) - max(mean(inb_lambda), 0)
    }, numeric(1))
  )
  saveRDS(evpi_curve, "data/processed/evpi_results.rds")
  message(sprintf(
    "EVPI curve saved: data/processed/evpi_results.rds (%d thresholds). At threshold %s: %.2f NOK/patient",
    nrow(evpi_curve),
    format(wtp_threshold, big.mark = ","),
    evpi_curve$evpi[evpi_curve$wtp == wtp_threshold]))

  # --- Population EVPI with technology horizon discounting ---
  # GUIDELINE: Briggs, Claxton, Sculpher (2006), p.176, Section 6.2.1
  # GUIDELINE: Fenwick et al. (2020), p.144-145, GPR 3, Box 4
  # Technology horizon T=10 base case, sensitivity 5/20, r=4% per Rundskriv R-109
  # SOURCE: ANNUAL_OPERATED_STAGE_MIX_COHORT defined in R/00-parameters.R.
  # Uses the named constant (not a literal) so the
  # value is defined once. Both 06-psa.R and 08-export-tables.R read
  # ANNUAL_OPERATED_STAGE_MIX_COHORT from 00-parameters.R via the main.R source chain.
  evpi_val <- attr(evppi_results, "evpi")
  pop_evpi_horizon <- compute_pop_evpi_horizon(
    evpi_per_patient = evpi_val,
    annual_cohort = ANNUAL_OPERATED_STAGE_MIX_COHORT,
    # GUIDELINE: Use the Norwegian 4% annual discount rate for population EVPI (Rundskriv R-109)
    discount_rate = 0.04,
    # GUIDELINE: Justify the technology horizon and test alternatives (Fenwick et al. 2020)
    horizons = c(5, 10, 20)
  )
  saveRDS(pop_evpi_horizon, "data/processed/pop_evpi_horizon.rds")
  message("Population EVPI (discounted, Briggs 2006):")
  for (i in seq_len(nrow(pop_evpi_horizon))) {
    message(sprintf("  T=%d yr: %.1fM NOK (annuity factor %.4f; undiscounted: %.1fM)",
                    pop_evpi_horizon$horizon_years[i],
                    pop_evpi_horizon$pop_evpi[i] / 1e6,
                    pop_evpi_horizon$annuity_factor[i],
                    pop_evpi_horizon$undiscounted[i] / 1e6))
  }

} else {
  message("PSA inner loop implemented. Requires fit objects to execute.")
  message("[INFO] Populate parameters and fit survival models before running PSA.")
}
