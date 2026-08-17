#!/usr/bin/env Rscript

source("R/functions/lock-writer-v1.R")
blas_path <- normalizePath(unname(extSoftVersion()[["BLAS"]]),
                           winslash = "/", mustWork = TRUE)
lapack_path <- normalizePath(La_library(), winslash = "/", mustWork = TRUE)
rng <- RNGkind()
repos <- getOption("repos")
if (length(repos) != 1L || !identical(unname(repos), "@CRAN@")) {
  stop("runtime repository option must be literal @CRAN@")
}
runtime <- data.frame(
  schema_version = 1L,
  r_version = R.version.string,
  platform = R.version$platform,
  rng_kind = rng[[1L]], normal_kind = rng[[2L]], sample_kind = rng[[3L]],
  blas_path = blas_path,
  blas_sha256 = lock_writer_sha256_file_v1(blas_path),
  lapack_path = lapack_path,
  lapack_sha256 = lock_writer_sha256_file_v1(lapack_path),
  repository_option = unname(repos),
  stringsAsFactors = FALSE, check.names = FALSE)
write_r_runtime_spec_v1(
  runtime, file.path("reproducibility", "r-runtime-spec.csv"))
cat("R RUNTIME LOCK GENERATED\n")
