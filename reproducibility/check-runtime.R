#!/usr/bin/env Rscript

# Caution: under a non-UTF-8 LC_CTYPE the generators do not fail, they silently
# escape every non-ASCII character on the way out: literal UTF-8 source bytes
# print as <c3><b8> and \uXXXX literals as <U+00F8>, so Norwegian and Finnish
# names reach the generated tables and the built PDF corrupted (VIZQ-R7
# 2026-08-02: 6 table sources, 8 lines, 9 PDF text-layer hits, no gate fired).
# The check sits in the runtime preflight main.R runs before it writes anything,
# so a wrong environment stops the run instead of publishing mojibake.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("non-UTF-8 runtime: LC_CTYPE=", Sys.getlocale("LC_CTYPE"),
       "; set LC_ALL or LANG to a UTF-8 locale before running the model")
}

source("R/functions/lock-writer-v1.R")
bootstrap_lib <- file.path("renv", "bootstrap")
if (dir.exists(bootstrap_lib)) {
  .libPaths(c(normalizePath(bootstrap_lib), .libPaths()))
}
runtime_path <- file.path("reproducibility", "r-runtime-spec.csv")
package_path <- file.path("reproducibility", "r-package-lock.csv")
if (!file.exists(runtime_path) || !file.exists(package_path)) {
  stop("runtime lock artifacts are missing")
}
runtime <- read.csv(runtime_path, stringsAsFactors = FALSE,
                    check.names = FALSE, colClasses = "character")
runtime$schema_version <- as.integer(runtime$schema_version)
lock_writer_validate_schema_v1(
  runtime, lock_writer_schema_v1("r_runtime"), "r-runtime-spec.csv")
if (nrow(runtime) != 1L) stop("r-runtime-spec.csv must contain one row")

blas_path <- normalizePath(unname(extSoftVersion()[["BLAS"]]),
                           winslash = "/", mustWork = TRUE)
lapack_path <- normalizePath(La_library(), winslash = "/", mustWork = TRUE)
rng <- RNGkind()
repos <- getOption("repos")
actual <- c(
  r_version = R.version.string, platform = R.version$platform,
  rng_kind = rng[[1L]], normal_kind = rng[[2L]], sample_kind = rng[[3L]],
  blas_path = blas_path, blas_sha256 = lock_writer_sha256_file_v1(blas_path),
  lapack_path = lapack_path,
  lapack_sha256 = lock_writer_sha256_file_v1(lapack_path),
  repository_option = if (length(repos) == 1L) unname(repos) else "<multiple>")
if (!identical(actual, unlist(runtime[1L, setdiff(names(runtime),
                                                  "schema_version")],
                               use.names = TRUE))) {
  stop("R runtime differs from r-runtime-spec.csv")
}

packages <- read.csv(package_path, stringsAsFactors = FALSE,
                     check.names = FALSE, colClasses = "character")
packages$schema_version <- as.integer(packages$schema_version)
lock_writer_validate_schema_v1(
  packages, lock_writer_schema_v1("r_package"), "r-package-lock.csv")
for (i in seq_len(nrow(packages))) {
  package <- packages$package[[i]]
  installed <- tryCatch(utils::packageDescription(package)$Version,
                        error = function(e) NA_character_)
  if (is.na(installed) || !identical(installed, packages$version[[i]])) {
    stop("installed package differs from lock: ", package)
  }
}
cat("R RUNTIME PREFLIGHT PASS:", nrow(packages), "locked packages\n")
