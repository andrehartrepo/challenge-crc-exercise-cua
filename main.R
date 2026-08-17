# =============================================================================
# main.R
# Cost-Utility Analysis: Exercise Intervention for CRC (CHALLENGE Trial)
# 3-state Partitioned Survival Model
#
# Author: André Álcega Hartmann
# Programme: Eu-HEM (European Master in Health Economics and Management)
# Date: 2026
#
# Usage: source("main.R")  [run from the repository root]
# All outputs written to output/figures/ and output/tables/
#
# REFERENCE CODE PROVENANCE:
#   Partitioned survival model framework: Williams et al. (2017), Med Decis
#     Making 37(4):427-439. DOI 10.1177/0272989X16670464.
#   DARTH group teaching materials: Krijkamp et al. (2018), HEVAL5200.
#   PSA and value-of-information methods: Briggs, Claxton, Sculpher (2006).
#     Decision Modelling for Health Economic Evaluation, Oxford University Press.
#
# Working directory MUST be the repository root when sourcing this file.
# All file paths are relative to the repository root. To use from another
# directory:
#   setwd("path/to/this-repository"); source("main.R")
# =============================================================================

# --- 0. Environment setup ----------------------------------------------------
preflight_scripts <- c(
  "reproducibility/check-inputs.R",
  "reproducibility/check-runtime.R")
for (preflight in preflight_scripts) {
  status <- system2(file.path(R.home("bin"), "Rscript"), preflight)
  if (!identical(status, 0L)) stop("strict preflight failed: ", preflight)
}

# Load required packages
library(flexsurv)   # Parametric survival modelling
library(IPDfromKM)  # KM curve digitization and IPD reconstruction
library(dampack)    # Expected loss curve computation (calc_exp_loss)
library(MASS)       # Endpoint-specific baseline-coefficient MVN draws
                     # Loaded before dplyr so dplyr::select masks MASS::select
library(ggplot2)    # General plotting
library(scales)     # Scale formatting (percent_format, etc.)
library(dplyr)      # Data manipulation (masks MASS::select  --  intentional)
library(tidyr)      # Data reshaping (pivot_longer for PSM trace plots)
library(survival)   # Surv() for survival formulas (explicit load)
library(flextable)  # Table formatting for DOCX export
library(officer)    # Word document properties (fp_border, fp_text)
library(jsonlite)   # Manifest JSON output

# --- Seed governance -------------------------------------------------------
# Function-local seed is retained in R/06-psa.R sample_psa_parameters()
# (default seed = 42). No top-level set.seed() here. Rationale: OWPSA (main.R step 8b)
# relies on sample_psa_parameters() resetting RNG to seed 42 on each call
# for Common Random Numbers (CRN). A top-level seed would break CRN because
# the global RNG state drifts between OWPSA iterations.
#
# Stochastic operations governed by the function-local seed:
#   - PSA parameter draws: R/06-psa.R sample_psa_parameters() (seed=42)
#   - OWPSA iterations: main.R step 8b via run_owpsa() (CRN, seed=42 per call)
#
# No other set.seed() exists in R/00-parameters.R through R/08-export-tables.R.
# Audit confirmed: no other set.seed() exists in the parameter-to-export code.

# Ensure output directories exist (required for fresh clone)
dir.create("output/figures",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",   recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed",  recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw",        recursive = TRUE, showWarnings = FALSE)

# --- 1. Parameters -----------------------------------------------------------
# All model parameters: hazard ratios, utilities, costs, discount rates
message("Loading parameters...")
source("R/00-parameters.R")

# --- 2. Digitize KM curves ---------------------------------------------------
# Import and clean digitized KM data from the CHALLENGE trial
# Input:  data/raw/*.csv  (digitized from publication figures)
# Output: data/processed/ipd_*.rds  (reconstructed individual-level data)
message("Digitizing KM curves...")
source("R/01-digitize-km.R")

# --- 3. Fit parametric survival models ---------------------------------------
# Fit and select parametric distributions (Weibull, log-normal, log-logistic,
# Gompertz, generalized gamma) for DFS and OS endpoints
# Output: model fit objects saved to data/processed/
message("Fitting parametric survival models...")
source("R/02-fit-survival.R")

# --- 4. Build PSM -------------------------------------------------------------
# Construct state-transition probabilities from survival functions
# States: Disease-Free | Progressed/Recurrence | Death
# Output: PSM trace matrices for intervention and control arms
message("Building Partitioned Survival Model...")
source("R/03-build-psm.R")

# --- 5. Calculate costs and QALYs --------------------------------------------
# Apply utility weights and unit costs to state occupancy
# Discount at stepped 4%/3%/2% per DMP guidelines (Rundskriv R-109)
# NOTE: For this model's 40-year horizon (age 61-101), every cycle midpoint
# is below elapsed time 40, so only 4% applies. The 3% and 2% intervals begin
# at elapsed years 40 and 75 (public labels: years 40-74 and 75+).
# Output: discounted costs and QALYs per arm
message("Calculating costs and QALYs...")
source("R/04-costs-qalys.R")

# --- 6. Probabilistic sensitivity analysis (BASE CASE) ----------------------
# PSA is the base case analysis.
# PSA with n_psa iterations (default: 10,000)
# Draws from parameter distributions defined in 00-parameters.R
# Primary output: expected ICER, P(CE) at the applicable severity-informed WTP reference, expected INB
message("Running probabilistic sensitivity analysis (base case)...")
source("R/functions/canonical-hash-v1.R")
source("R/06-psa.R")

# --- 7. Deterministic analysis (supplementary) ------------------------------
# Deterministic ICER reported for transparency only.
# The deterministic run is a special case of PSA with N=1 (point estimates).
# Output: tables to output/tables/
message("Running deterministic supplementary analysis...")
source("R/05-run-model.R")

# --- 8. Structural sensitivity analysis --------------------------------------
# Structural sensitivity analysis tests assumptions on waning, mortality cap, and distributions.
# Output: tables to output/tables/
message("Running structural sensitivity analysis...")
source("R/06b-structural-sa.R")

# After sourcing 06b-structural-sa.R  --  run and save structural SA
# NOTE: run_structural_sa() handles prerequisites internally (the `can_execute`
# file.exists()/is.null() guard inside run_structural_sa() in R/06b-structural-sa.R).
# If prerequisites are missing, it returns a data frame with NA values.
# If prerequisites are met, it returns populated results.
if (exists("run_structural_sa")) {
  ssa_results <- run_structural_sa()
  if (any(!is.na(ssa_results$icer))) {
    write.csv(ssa_results, "output/tables/structural_sa_results.csv",
              row.names = FALSE)
    message("Structural SA saved: output/tables/structural_sa_results.csv")
  } else {
    message("Structural SA: prerequisites missing, results contain NAs. CSV not saved.")
  }
}

# --- 8a2. Parametric distribution selection (Table 26) ----------------------
# Table 26 reports AIC and BIC values for six candidate parametric
# distributions fitted to the standard-care arm DFS and OS endpoints
# (Latimer 2013, NICE DSU TSD 14). The comparison values are written to
# output/tables/aic_bic_comparison.csv by R/02-fit-survival.R and are
# rendered as Table 26 by build_distribution_comparison_table() in
# R/08-export-tables.R.

# --- 8b. Probabilistic One-Way Sensitivity Analysis (POWSA) -----------------
# Per McCabe et al. (2020): fix each parameter at low/high bounds,
# sample all others probabilistically (500 iterations per scenario).
# Report conditional expected INMB at the applicable severity-informed WTP reference.
# Utility bounds use corrected SE-based Beta distribution quantiles.
# Output: output/tables/owpsa_v7_results.csv
message("Running POWSA with a dynamically defined component-owned registry...")
message("This takes a few minutes...")

n_owpsa <- 500

# Load prerequisites
psa_fits_dfs <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
psa_fits_os  <- readRDS("data/processed/survival_fits_os_ctrl.rds")
psa_s_genpop <- load_genpop_survival(
  life_table_path = life_table_path,
  entry_age       = cohort_age,
  n_cycles        = n_cycles,
  cycle_length    = cycle_length
)

powsa_params <- data.frame(
  param = c("HR_DFS", "HR_OS",
            "u_dfs", "u_prog",
            "c_surveillance_early", "c_exercise_annual",
            "c_progressed_annual", "c_terminal"),
  low   = c(HR_DFS_lower, HR_OS_lower,
            u_dfs_powsa_low, u_prog_powsa_low,
            powsa_c_surveillance_low, powsa_c_exercise_low,
            powsa_c_progressed_low, powsa_c_terminal_low),
  high  = c(HR_DFS_upper, HR_OS_upper,
            u_dfs_powsa_high, u_prog_powsa_high,
            powsa_c_surveillance_high, powsa_c_exercise_high,
            powsa_c_progressed_high, powsa_c_terminal_high),
  label = c(paste0("HR DFS (", HR_DFS_lower, "-", HR_DFS_upper, ")"),
            paste0("HR OS (", HR_OS_lower, "-", HR_OS_upper, ")"),
            paste0("Utility DF (", u_dfs_powsa_low, "-", u_dfs_powsa_high, ")"),
            paste0("Utility PD (", u_prog_powsa_low, "-", u_prog_powsa_high, ")"),
            # Caution: this label is DERIVED from the bounds, not typed. A typed
            # label silently outlives the band it describes: the surveillance
            # band widened to 0.40/1.60 while a hardcoded "(60%-140%)" kept
            # printing into the tornado table and thesis-table-05. The three
            # labels below are still typed and still correct as of this run;
            # any change to their powsa_* bounds must change them by hand, or
            # be converted to the derived form used here.
            paste0("Shared surveillance annual cost (",
                   round(100 * powsa_c_surveillance_low / c_surveillance_early), "%-",
                   round(100 * powsa_c_surveillance_high / c_surveillance_early), "%)"),
            "Exercise-programme annual cost (50%-150%)",
            "Shared progressed-disease annual cost (60%-140%)",
            "EOL cost (60%-140%)"),
  stringsAsFactors = FALSE
)

message("Running POWSA for ", nrow(powsa_params), " parameters, ",
        n_owpsa, " PSA iterations each (",
        2L * nrow(powsa_params), " low/high runs).")

results_list <- lapply(seq_len(nrow(powsa_params)), function(i) {
  pname <- powsa_params$param[i]
  plow  <- powsa_params$low[i]
  phigh <- powsa_params$high[i]
  plabel <- powsa_params$label[i]

  message("  Parameter ", i, "/", nrow(powsa_params), ": ", plabel)

  res_low <- run_owpsa(
    param_name   = pname,
    param_values = plow,
    n_sim        = n_owpsa,
    fit_dfs_ctrl = psa_fits_dfs[[best_dist_dfs]],
    fit_os_ctrl  = psa_fits_os[[best_dist_os]],
    s_genpop     = psa_s_genpop,
    discount_weights = discount_weights_stepped
  )

  res_high <- run_owpsa(
    param_name   = pname,
    param_values = phigh,
    n_sim        = n_owpsa,
    fit_dfs_ctrl = psa_fits_dfs[[best_dist_dfs]],
    fit_os_ctrl  = psa_fits_os[[best_dist_os]],
    s_genpop     = psa_s_genpop,
    discount_weights = discount_weights_stepped
  )

  data.frame(
    param_name = pname,
    label      = plabel,
    value_low  = plow,
    value_high = phigh,
    inmb_low   = res_low$expected_inb,
    inmb_high  = res_high$expected_inb,
    range      = abs(res_high$expected_inb - res_low$expected_inb),
    prob_ce_low  = res_low$prob_ce,
    prob_ce_high = res_high$prob_ce
  )
})
owpsa_results <- do.call(rbind, results_list)
owpsa_results <- owpsa_results[order(-owpsa_results$range), ]
rownames(owpsa_results) <- NULL

write.csv(owpsa_results, "output/tables/owpsa_v7_results.csv", row.names = FALSE)

# Programmatic caption for POWSA results table
# SOURCE: owpsa_results data frame, sorted by range (most influential first)
top_param <- owpsa_results$label[1]
top_range <- owpsa_results$range[1]
powsa_caption <- paste0(
  "Probabilistic One-Way Sensitivity Analysis (",
  nrow(owpsa_results), " parameters, ",
  n_owpsa, " PSA iterations per scenario, ",
  "threshold = NOK ", format(wtp_threshold, big.mark = ","), "/QALY, ",
  "extended healthcare perspective, effective discount rate 4% per Rundskriv R-109 stepped schedule). ",
  "Most influential parameter: ", top_param,
  " (INMB range: NOK ", format(round(top_range), big.mark = ","), "). ",
  "Parameters ordered by INMB range (descending)."
)
message("PROGRAMMATIC CAPTION:\n", powsa_caption)
attr(owpsa_results, "caption") <- powsa_caption

# Assign to all_results for downstream compatibility with 08-export-tables.R
all_results <- owpsa_results

message("POWSA complete. Results saved: output/tables/owpsa_v7_results.csv")

# --- 8c. Expected Loss Computation --------------------------------------------
# Expected opportunity loss per strategy across WTP values
# SOURCE: Alarid-Escudero et al. (2019), Value in Health 22(5):611-618,
# DOI 10.1016/j.jval.2019.02.008 (expected-loss interpretation).
# GUIDELINE: Fenwick et al. (2020), GPR 4 - report expected loss alongside CEACs
# SOURCE: psa_results from step 6
psa_results_for_el <- readRDS("data/processed/psa_results.rds")
exp_loss <- calculate_expected_loss(psa_results_for_el, wtp_range)
message("Expected loss computed for ", length(wtp_range), " threshold values.")

# --- 9. Visualisation --------------------------------------------------------
# Generate: CE plane, CEAC, tornado diagram, expected loss, convergence
# Output: figures to output/figures/
message("Generating visualisations...")
source("R/07-visualization.R")

# --- 10. Table Export --------------------------------------------------------
# Generate: formatted DOCX tables for thesis insertion
# Output: .docx files to output/tables/
# PSA is the base case; structural SA supplements the primary analysis.
message("Generating thesis tables...")
source("R/08-export-tables.R")
build_all_tables()

# --- 11. Text Snippets -------------------------------------------------------
# Generate: programmatic interpretation strings from R objects
# Output: .txt files to output/text-snippets/
# SOURCE: dominance and model selection interpretation strings
message("Generating text snippets...")
build_all_snippets()

# --- 12. LaTeX Commands -------------------------------------------------------
# Generate: \newcommand definitions for thesis chapter prose
# Output: model-results.tex to output/, manifest CSV to output/
# SOURCE: all model outputs (deterministic, PSA, VOI, SSA, POWSA, validation)
message("Generating LaTeX commands...")
source("R/09-export-latex-commands.R")

# --- 13. Required-output verification ---------------------------------------
source("reproducibility/verify-required-outputs.R")

# --- Done --------------------------------------------------------------------
message("")
message("=== Model run complete ===")
message("Base case: PSA results")
message("Supplementary: Deterministic ICER")
message("Figures:  output/figures/")
message("Tables:   output/tables/")
message("Snippets: output/text-snippets/")
