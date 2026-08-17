# =============================================================================
# 10-extreme-value-tests.R
# Deterministic extreme-value scenario harness
#
# Standalone use from the repository root:
#   Rscript R/10-extreme-value-tests.R
# =============================================================================

options(digits = 17)

# GUIDELINE: deterministic validation evidence retains the model's returned
# numeric precision so later review can inspect the measured result directly.
# DECISION: each scenario is launched through a separate Rscript process so
# the parameter environment cannot carry scenario state into the next run.
# SOURCE: scenario-local argument overrides follow the established dispatcher
# idiom in R/06b-structural-sa.R to keep the model implementation unchanged.
# Caution: sourcing R/05-run-model.R would execute its writer; a
# private loader environment makes that placeholder branch unreachable.

get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) != 1L) {
    stop("Cannot identify the executing script path.")
  }
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
}

write_full_precision_csv <- function(data, path) {
  numeric_columns <- vapply(data, is.numeric, logical(1))
  data[numeric_columns] <- lapply(data[numeric_columns], function(values) {
    ifelse(is.na(values), NA_character_, sprintf("%.17g", as.double(values)))
  })
  write.csv(data, path, row.names = FALSE, na = "NA")
}

load_model_environment <- function() {
  library(flexsurv)
  library(dplyr)

  model_env <- new.env(parent = globalenv())
  sys.source("R/00-parameters.R", envir = model_env)

  old_testthat <- Sys.getenv("TESTTHAT", unset = NA_character_)
  Sys.setenv(TESTTHAT = "true")
  on.exit({
    if (is.na(old_testthat)) {
      Sys.unsetenv("TESTTHAT")
    } else {
      Sys.setenv(TESTTHAT = old_testthat)
    }
  }, add = TRUE)

  sys.source("R/03-build-psm.R", envir = model_env)
  sys.source("R/04-costs-qalys.R", envir = model_env)

  run_model_env <- new.env(parent = model_env)
  run_model_env$file.exists <- function(...) FALSE
  sys.source("R/05-run-model.R", envir = run_model_env)
  model_env$run_base_case <- run_model_env$run_base_case

  model_env
}

scenario_definition <- function(scenario_id) {
  definitions <- list(
    S0 = list(
      label = "base-case reproduction",
      override = "none"
    ),
    S1 = list(
      label = "no treatment effect",
      override = "hr_dfs_scn = 1.0; hr_os_scn = 1.0"
    ),
    S2 = list(
      label = "zero intervention cost",
      override = "c_exercise_annual = 0; c_intervention_setup = 0"
    ),
    S3 = list(
      label = "zero utilities",
      override = "u_dfs_scn = 0; u_prog_scn = 0"
    )
  )
  if (!scenario_id %in% names(definitions)) {
    stop("Unknown scenario_id: ", scenario_id)
  }
  definitions[[scenario_id]]
}

run_one_scenario <- function(scenario_id) {
  definition <- scenario_definition(scenario_id)
  warning_log <- new.env(parent = emptyenv())
  warning_log$messages <- character()

  results <- withCallingHandlers({
    model <- load_model_environment()

    hr_dfs_scn <- model$HR_DFS
    hr_os_scn <- model$HR_OS
    c_exercise_scn <- model$c_exercise_annual
    c_intervention_setup_scn <- model$c_intervention_setup
    u_dfs_scn <- model$u_dfs_mean
    u_prog_scn <- model$u_prog_mean

    if (scenario_id == "S1") {
      hr_dfs_scn <- 1.0
      hr_os_scn <- 1.0
    }
    if (scenario_id == "S2") {
      c_exercise_scn <- 0
      c_intervention_setup_scn <- 0
    }
    if (scenario_id == "S3") {
      u_dfs_scn <- 0
      u_prog_scn <- 0
    }

    fits_dfs <- readRDS("data/processed/survival_fits_dfs_ctrl.rds")
    fits_os <- readRDS("data/processed/survival_fits_os_ctrl.rds")
    fit_dfs_ctrl <- fits_dfs[[model$best_dist_dfs]]
    fit_os_ctrl <- fits_os[[model$best_dist_os]]

    s_genpop <- model$load_genpop_survival(
      life_table_path = model$life_table_path,
      entry_age = model$cohort_age,
      n_cycles = model$n_cycles,
      cycle_length = model$cycle_length
    )
    age_multipliers <- model$get_age_utility_multipliers(
      model$cohort_age,
      model$n_cycles,
      model$cycle_length
    )

    trace_ctrl <- model$build_psm_trace(
      fit_dfs = fit_dfs_ctrl,
      fit_os = fit_os_ctrl,
      n_cycles = model$n_cycles,
      cycle_length = model$cycle_length,
      s_genpop = s_genpop,
      arm_label = "Standard Care",
      mortality_method = "hazard_max"
    )
    trace_int <- model$build_psm_trace_from_ctrl(
      fit_dfs_ctrl = fit_dfs_ctrl,
      fit_os_ctrl = fit_os_ctrl,
      hr_dfs = hr_dfs_scn,
      hr_os = hr_os_scn,
      n_cycles = model$n_cycles,
      cycle_length = model$cycle_length,
      s_genpop = s_genpop,
      arm_label = "Exercise",
      mortality_method = "hazard_max"
    )
    scenario_trace <- rbind(trace_int, trace_ctrl)

    cq_int <- model$calculate_costs_qalys(
      scenario_trace,
      arm = "Exercise",
      u_dfs = u_dfs_scn,
      u_prog = u_prog_scn,
      u_dead = model$u_dead,
      c_surveillance_early = model$c_surveillance_early,
      c_surveillance_late = model$c_surveillance_late,
      surveillance_cutoff_years = model$surveillance_cutoff_years,
      c_exercise_annual = c_exercise_scn,
      intervention_duration_years = model$intervention_duration_years,
      c_progressed_annual = model$c_progressed_annual,
      c_terminal = if (is.na(model$c_terminal)) 0 else model$c_terminal,
      c_intervention_setup = if (is.na(c_intervention_setup_scn)) 0
                             else c_intervention_setup_scn,
      u_exercise_decrement = model$u_exercise_decrement,
      discount_weights = model$discount_weights_stepped,
      cycle_length = model$cycle_length,
      age_utility_multipliers = age_multipliers
    )
    cq_ctrl <- model$calculate_costs_qalys(
      scenario_trace,
      arm = "Standard Care",
      u_dfs = u_dfs_scn,
      u_prog = u_prog_scn,
      u_dead = model$u_dead,
      c_surveillance_early = model$c_surveillance_early,
      c_surveillance_late = model$c_surveillance_late,
      surveillance_cutoff_years = model$surveillance_cutoff_years,
      c_exercise_annual = 0,
      intervention_duration_years = 0,
      c_progressed_annual = model$c_progressed_annual,
      c_terminal = if (is.na(model$c_terminal)) 0 else model$c_terminal,
      c_intervention_setup = 0,
      u_exercise_decrement = 0,
      discount_weights = model$discount_weights_stepped,
      cycle_length = model$cycle_length,
      age_utility_multipliers = age_multipliers
    )

    model$run_base_case(rbind(cq_int, cq_ctrl))
  }, warning = function(w) {
    warning_log$messages <- c(warning_log$messages, conditionMessage(w))
  })

  # DECISION: an explicit sentinel survives per-scenario CSV type inference,
  # keeping observed absence distinct from an unknown warning state.
  warning_text <- if (length(warning_log$messages) == 0L) {
    "NONE"
  } else {
    paste(warning_log$messages, collapse = " || ")
  }
  data.frame(
    scenario_id = scenario_id,
    scenario_label = definition$label,
    override_applied = definition$override,
    results,
    warnings = rep(warning_text, nrow(results)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

compare_s0_to_base_case <- function(s0_results, reference_path) {
  reference <- read.csv(
    reference_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result_columns <- names(reference)
  if (!all(result_columns %in% names(s0_results))) {
    missing_columns <- setdiff(result_columns, names(s0_results))
    return(list(
      pass = FALSE,
      schema_equal = FALSE,
      arms_equal = FALSE,
      numeric_equal = FALSE,
      max_abs_diff = NA_real_,
      detail = paste("Missing S0 columns:", paste(missing_columns, collapse = ", "))
    ))
  }

  actual <- s0_results[, result_columns, drop = FALSE]
  rownames(actual) <- NULL
  rownames(reference) <- NULL
  schema_equal <- identical(names(actual), names(reference))
  arms_equal <- identical(actual$arm, reference$arm)
  numeric_columns <- setdiff(result_columns, "arm")
  field_equal <- vapply(numeric_columns, function(column_name) {
    isTRUE(all.equal(
      actual[[column_name]],
      reference[[column_name]],
      tolerance = 1e-12,
      check.attributes = FALSE
    ))
  }, logical(1))

  finite_differences <- unlist(lapply(numeric_columns, function(column_name) {
    actual_values <- actual[[column_name]]
    reference_values <- reference[[column_name]]
    finite <- is.finite(actual_values) & is.finite(reference_values)
    abs(actual_values[finite] - reference_values[finite])
  }), use.names = FALSE)
  max_abs_diff <- if (length(finite_differences) == 0L) {
    NA_real_
  } else {
    max(finite_differences)
  }

  list(
    pass = schema_equal && arms_equal && all(field_equal),
    schema_equal = schema_equal,
    arms_equal = arms_equal,
    numeric_equal = all(field_equal),
    max_abs_diff = max_abs_diff,
    detail = paste(
      paste(names(field_equal), ifelse(field_equal, "PASS", "FAIL"), sep = "="),
      collapse = "; "
    )
  )
}

run_child_process <- function(script_path, scenario_id) {
  result_path <- file.path("/tmp", paste0("evt-", scenario_id, "-results.csv"))
  console_path <- file.path("/tmp", paste0("evt-", scenario_id, "-console.txt"))
  completed <- FALSE
  on.exit({
    if (!completed) unlink(c(result_path, console_path), force = TRUE)
  }, add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  child_output <- system2(
    rscript,
    args = shQuote(c(
      script_path,
      "--scenario", scenario_id,
      "--out", result_path
    )),
    stdout = TRUE,
    stderr = TRUE
  )
  child_status <- attr(child_output, "status")
  if (is.null(child_status)) child_status <- 0L
  writeLines(child_output, console_path, useBytes = TRUE)
  if (length(child_output) > 0L) {
    message(paste(child_output, collapse = "\n"))
  }
  completed <- TRUE
  list(
    status = as.integer(child_status),
    result_path = result_path,
    console_path = console_path
  )
}

run_all_scenarios <- function(script_path) {
  scenario_ids <- c("S0", "S1", "S2", "S3")
  collected <- vector("list", length(scenario_ids))
  names(collected) <- scenario_ids
  child_artifacts <- character()
  # DECISION: child evidence is ephemeral and is removed when the parent exits,
  # including after a scenario failure.
  on.exit(unlink(child_artifacts, force = TRUE), add = TRUE)

  s0_run <- run_child_process(script_path, "S0")
  child_artifacts <- c(
    child_artifacts,
    s0_run$result_path,
    s0_run$console_path
  )
  if (s0_run$status != 0L) {
    stop("S0 scenario process failed with status ", s0_run$status, ".")
  }
  s0_results <- read.csv(
    s0_run$result_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  s0_comparison <- compare_s0_to_base_case(
    s0_results,
    "output/tables/deterministic_results.csv"
  )
  message("S0_CONTROL=", if (s0_comparison$pass) "PASS" else "FAIL")
  message("S0_SCHEMA=", if (s0_comparison$schema_equal) "PASS" else "FAIL")
  message("S0_ARM_ORDER=", if (s0_comparison$arms_equal) "PASS" else "FAIL")
  message("S0_NUMERIC_FIELDS=", s0_comparison$detail)
  message("S0_MAX_ABS_DIFF=", format(
    s0_comparison$max_abs_diff,
    digits = 17,
    scientific = TRUE
  ))
  if (!s0_comparison$pass) {
    stop("S0 did not reproduce output/tables/deterministic_results.csv.")
  }
  collected[["S0"]] <- s0_results

  for (scenario_id in scenario_ids[-1]) {
    scenario_run <- run_child_process(script_path, scenario_id)
    child_artifacts <- c(
      child_artifacts,
      scenario_run$result_path,
      scenario_run$console_path
    )
    if (scenario_run$status != 0L) {
      stop(scenario_id, " scenario process failed with status ",
           scenario_run$status, ".")
    }
    collected[[scenario_id]] <- read.csv(
      scenario_run$result_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  combined <- do.call(rbind, collected)
  rownames(combined) <- NULL
  dir.create("output/validation", recursive = TRUE, showWarnings = FALSE)
  write_full_precision_csv(
    combined,
    "output/validation/extreme-value-results.csv"
  )
  message("SCENARIOS_COMPLETED=S0,S1,S2,S3")
  message("RESULT_FILE=output/validation/extreme-value-results.csv")
}

main <- function() {
  script_path <- get_script_path()
  model_root <- dirname(dirname(script_path))
  setwd(model_root)
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0L) {
    run_all_scenarios(script_path)
    return(invisible(NULL))
  }

  scenario_position <- match("--scenario", args)
  output_position <- match("--out", args)
  if (is.na(scenario_position) || is.na(output_position) ||
      scenario_position == length(args) || output_position == length(args)) {
    stop("Child mode requires --scenario <ID> and --out <path>.")
  }

  scenario_id <- args[scenario_position + 1L]
  output_path <- args[output_position + 1L]
  message("PROCESS_PID=", Sys.getpid())
  message("SCENARIO_ID=", scenario_id)
  results <- run_one_scenario(scenario_id)
  write_full_precision_csv(results, output_path)
  message("SCENARIO_COMPLETED=", scenario_id)
  invisible(NULL)
}

exit_status <- tryCatch({
  main()
  0L
}, error = function(e) {
  message("ERROR: ", conditionMessage(e))
  1L
})

quit(save = "no", status = exit_status)
